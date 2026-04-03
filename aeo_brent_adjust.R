library(eia2)
library(readxl)

setwd("~/EIA/lng_vs_oil/")

CACHE_DIR <- ".steo_feb_cache"
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, showWarnings = FALSE)

NOMINAL_SERIES <- "prce_nomp_ten_NA_brn_NA_usa_ndlrpbrl"
REAL_SERIES <- "prce_rlp_ten_NA_brn_NA_usa_y13dlrpbbl"
MAX_HORIZON <- 25
USE_SAVED_DATA <- TRUE

download_with_retry <- function(url, dest, tries = 3) {
  for (i in seq_len(tries)) {
    ok <- tryCatch({
      download.file(url, dest, mode = "wb", quiet = TRUE)
      file.exists(dest) && file.info(dest)$size > 0
    }, error = function(e) FALSE)
    if (ok) return(TRUE)
  }
  FALSE
}

get_available_aeo_vintages <- function() {
  x <- eia2("aeo")
  routes <- as.character(unlist(x$routes))
  vint <- suppressWarnings(as.integer(routes))
  sort(vint[!is.na(vint)])
}

get_aeo_reference_curve <- function(vintage, series_id) {
  route <- paste0("aeo/", vintage)
  ref_code <- paste0("ref", vintage)
  safe_eia2 <- function(...) {
    for (i in 1:3) {
      x <- tryCatch(eia2(...), error = function(e) NULL)
      if (!is.null(x)) return(x)
    }
    data.frame()
  }

  x <- safe_eia2(
    route = route,
    facets = list(seriesId = series_id, scenario = ref_code),
    data_cols = "value",
    frequency = "annual",
    sort = data.frame(column = "period", direction = "asc")
  )

  if (nrow(x) == 0) {
    all_scen <- safe_eia2(
      route = route,
      facets = list(seriesId = series_id),
      data_cols = "value",
      frequency = "annual",
      sort = data.frame(column = "period", direction = "asc")
    )
    if ("scenarioDescription" %in% names(all_scen)) {
      x <- all_scen[all_scen$scenarioDescription == "Reference case", ]
    } else if ("scenario" %in% names(all_scen)) {
      x <- all_scen[grepl("^ref", all_scen$scenario, ignore.case = TRUE), ]
    }
  }

  if (nrow(x) == 0) return(data.frame())

  data.frame(
    vintage = as.integer(vintage),
    year = as.integer(unlist(x$period)),
    value = as.numeric(unlist(x$value))
  )
}

fill_down <- function(x) {
  out <- x
  last <- NA
  for (i in seq_along(out)) {
    if (!is.na(out[i])) last <- out[i]
    out[i] <- last
  }
  out
}

month_to_int <- function(x) {
  key <- c(
    jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6,
    jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, dec = 12
  )
  s <- tolower(substr(trimws(as.character(x)), 1, 3))
  as.integer(unname(key[s]))
}

resolve_feb_archive_url <- function(issue_year) {
  yy <- sprintf("%02d", issue_year %% 100)
  urls <- c(
    paste0("https://www.eia.gov/outlooks/steo/archives/feb", yy, "_base.xlsx"),
    paste0("https://www.eia.gov/outlooks/steo/archives/feb", yy, "_base.xls")
  )
  for (u in urls) {
    tf <- tempfile(fileext = ifelse(grepl("\\.xlsx$", u), ".xlsx", ".xls"))
    ok <- download_with_retry(u, tf, tries = 2)
    if (ok) return(u)
  }
  NA_character_
}

extract_feb_steo_brent_annual <- function(issue_year) {
  url <- resolve_feb_archive_url(issue_year)
  if (is.na(url)) return(data.frame())

  ext <- ifelse(grepl("\\.xlsx$", url), ".xlsx", ".xls")
  tf <- file.path(CACHE_DIR, paste0("feb", issue_year, "_base", ext))
  if (!file.exists(tf) || file.info(tf)$size <= 0) {
    if (!download_with_retry(url, tf, tries = 3)) return(data.frame())
  }

  sheets <- readxl::excel_sheets(tf)
  target_sheet <- NA_character_
  if ("2tab" %in% sheets) {
    target_sheet <- "2tab"
  } else {
    for (s in sheets) {
      probe <- tryCatch(
        readxl::read_excel(tf, sheet = s, col_names = FALSE, .name_repair = "minimal", n_max = 80),
        error = function(e) NULL
      )
      if (is.null(probe)) next
      txt <- as.character(unlist(probe, use.names = FALSE))
      if (any(grepl("^BREPUUS$", txt, ignore.case = TRUE))) {
        target_sheet <- s
        break
      }
    }
  }
  if (is.na(target_sheet)) return(data.frame())

  d <- readxl::read_excel(tf, sheet = target_sheet, col_names = FALSE, .name_repair = "minimal")
  d <- as.data.frame(d, stringsAsFactors = FALSE)

  brent_row <- which(toupper(trimws(as.character(d[[1]]))) == "BREPUUS")[1]
  if (is.na(brent_row)) {
    brent_row <- which(apply(d, 1, function(r) any(grepl("^BREPUUS$", as.character(r), ignore.case = TRUE))))[1]
  }
  if (is.na(brent_row)) return(data.frame())

  is_month_row <- function(row_vals) {
    s <- tolower(substr(trimws(as.character(row_vals)), 1, 3))
    sum(s %in% c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), na.rm = TRUE) >= 6
  }
  month_candidates <- which(apply(d[, -c(1, 2), drop = FALSE], 1, is_month_row))
  month_candidates <- month_candidates[month_candidates < brent_row]
  if (length(month_candidates) == 0) return(data.frame())
  month_row <- tail(month_candidates, 1)
  year_row <- month_row - 1
  if (year_row < 1) return(data.frame())

  vals <- suppressWarnings(as.numeric(unlist(d[brent_row, -c(1, 2)], use.names = FALSE)))
  yrs <- suppressWarnings(as.integer(unlist(d[year_row, -c(1, 2)], use.names = FALSE)))
  yrs <- fill_down(yrs)
  mos <- month_to_int(unlist(d[month_row, -c(1, 2)], use.names = FALSE))

  keep <- !is.na(vals) & !is.na(yrs) & !is.na(mos)
  z <- data.frame(year = yrs[keep], month = mos[keep], steo_nominal_brent = vals[keep])
  if (nrow(z) == 0) return(data.frame())

  out <- aggregate(steo_nominal_brent ~ year, data = z, FUN = mean)
  out$issue_year <- issue_year
  out$url <- url
  out[order(out$year), c("issue_year", "year", "steo_nominal_brent", "url")]
}

build_aeo_source <- function(vintages, nominal_series = NOMINAL_SERIES, real_series = REAL_SERIES) {
  out <- list()
  k <- 1
  for (v in vintages) {
    nom <- get_aeo_reference_curve(v, nominal_series)
    real <- get_aeo_reference_curve(v, real_series)
    if (nrow(nom) == 0 || nrow(real) == 0) next
    m <- merge(
      nom[, c("vintage", "year", "value")],
      real[, c("vintage", "year", "value")],
      by = c("vintage", "year"),
      suffixes = c("_nominal", "_real")
    )
    names(m) <- c("vintage", "year", "aeo_nominal_brent", "aeo_real_brent")
    m$real_nominal_ratio <- m$aeo_real_brent / m$aeo_nominal_brent
    out[[k]] <- m[order(m$year), ]
    k <- k + 1
  }
  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

build_steo_source <- function(issue_years) {
  out <- list()
  k <- 1
  for (y in issue_years) {
    x <- extract_feb_steo_brent_annual(y)
    if (nrow(x) == 0) next
    out[[k]] <- x
    k <- k + 1
  }
  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

build_steo_metric <- function(steo_source) {
  issue_years <- sort(unique(steo_source$issue_year))
  rows <- list()
  k <- 1
  for (y in issue_years) {
    cur <- steo_source$steo_nominal_brent[steo_source$issue_year == y & steo_source$year == y]
    nxt <- steo_source$steo_nominal_brent[steo_source$issue_year == y & steo_source$year == (y + 1)]
    if (length(cur) == 0 || length(nxt) == 0) next
    rows[[k]] <- data.frame(
      issue_year = y,
      steo_curr = cur[1],
      steo_next = nxt[1],
      steo_metric_ave = mean(c(cur[1], nxt[1]))
    )
    k <- k + 1
  }
  if (length(rows) == 0) return(data.frame())
  m <- do.call(rbind, rows)
  m <- m[order(m$issue_year), ]
  m$steo_curr_prev <- c(NA, m$steo_curr[-nrow(m)])
  m$steo_next_prev <- c(NA, m$steo_next[-nrow(m)])
  m$steo_metric_prev <- c(NA, m$steo_metric_ave[-nrow(m)])
  m$delta_steo_curr <- m$steo_curr - m$steo_curr_prev
  m$delta_steo_next <- m$steo_next - m$steo_next_prev
  m$delta_steo_ave <- m$steo_metric_ave - m$steo_metric_prev
  m
}

build_pair_status <- function(candidate_vintages, aeo_vintages_available, steo_metric_years) {
  out <- data.frame(vintage = candidate_vintages)
  out$has_aeo_new <- out$vintage %in% aeo_vintages_available
  out$has_aeo_old <- (out$vintage - 1) %in% aeo_vintages_available
  out$has_steo_new <- out$vintage %in% steo_metric_years
  out$has_steo_old <- (out$vintage - 1) %in% steo_metric_years
  out$usable <- with(out, has_aeo_new & has_aeo_old & has_steo_new & has_steo_old)
  out$reason <- ""
  out$reason[!out$has_aeo_new] <- paste(out$reason[!out$has_aeo_new], "missing AEO new;", sep = " ")
  out$reason[!out$has_aeo_old] <- paste(out$reason[!out$has_aeo_old], "missing AEO old;", sep = " ")
  out$reason[!out$has_steo_new] <- paste(out$reason[!out$has_steo_new], "missing STEO new;", sep = " ")
  out$reason[!out$has_steo_old] <- paste(out$reason[!out$has_steo_old], "missing STEO old;", sep = " ")
  out$reason <- trimws(out$reason)
  out
}

build_regression_panel <- function(aeo_source, steo_metric, vintages, dsteo_col, max_h = MAX_HORIZON) {
  rows <- list()
  k <- 1
  for (y in vintages) {
    steo_row <- steo_metric[steo_metric$issue_year == y, ]
    if (nrow(steo_row) == 0 || is.na(steo_row[[dsteo_col]][1])) next
    dsteo <- steo_row[[dsteo_col]][1]

    for (h in seq_len(max_h)) {
      y_new <- y + h
      y_old <- (y - 1) + h
      a_new <- aeo_source$aeo_nominal_brent[aeo_source$vintage == y & aeo_source$year == y_new]
      a_old <- aeo_source$aeo_nominal_brent[aeo_source$vintage == (y - 1) & aeo_source$year == y_old]
      if (length(a_new) == 0 || length(a_old) == 0) next
      rows[[k]] <- data.frame(
        vintage_new = y,
        vintage_old = y - 1,
        horizon_f = h,
        year_new = y_new,
        year_old = y_old,
        aeo_new_nominal = a_new[1],
        aeo_old_nominal = a_old[1],
        delta_aeo = a_new[1] - a_old[1],
        delta_steo_measure = dsteo
      )
      k <- k + 1
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

fit_horizon_regressions <- function(panel, max_h = MAX_HORIZON) {
  fits <- list()
  for (h in seq_len(max_h)) {
    d <- panel[panel$horizon_f == h, ]
    if (nrow(d) < 3 || length(unique(d$delta_steo_measure)) < 2) {
      fits[[as.character(h)]] <- list(ok = FALSE)
      next
    }
    fit <- lm(delta_aeo ~ delta_steo_measure, data = d)
    s <- summary(fit)
    fits[[as.character(h)]] <- list(
      ok = TRUE,
      intercept = coef(fit)[1],
      slope = coef(fit)[2],
      r_squared = s$r.squared,
      n_obs = nrow(d)
    )
  }
  fits
}

fit_summary_table <- function(fits, target_delta_steo, measure_name) {
  rows <- list()
  k <- 1
  for (nm in names(fits)) {
    h <- as.integer(nm)
    f <- fits[[nm]]
    if (!isTRUE(f$ok)) {
      rows[[k]] <- data.frame(
        horizon_f = h,
        measure = measure_name,
        intercept = NA_real_,
        slope_delta_steo = NA_real_,
        r_squared = NA_real_,
        n_obs = 0,
        target_delta_steo = target_delta_steo,
        predicted_delta_aeo = NA_real_
      )
    } else {
      pred <- as.numeric(f$intercept + f$slope * target_delta_steo)
      rows[[k]] <- data.frame(
        horizon_f = h,
        measure = measure_name,
        intercept = as.numeric(f$intercept),
        slope_delta_steo = as.numeric(f$slope),
        r_squared = as.numeric(f$r_squared),
        n_obs = as.integer(f$n_obs),
        target_delta_steo = target_delta_steo,
        predicted_delta_aeo = pred
      )
    }
    k <- k + 1
  }
  do.call(rbind, rows)
}

build_adjusted_path <- function(
  aeo_source,
  steo_source,
  steo_metric,
  fit_table,
  base_vintage = 2025,
  target_vintage = 2026,
  output_path = "brent_nominal_real_path_adjusted_to_steo2026.csv"
) {
  base <- aeo_source[aeo_source$vintage == base_vintage, ]
  if (nrow(base) == 0) stop("Base AEO vintage not found in source data.")
  base <- base[order(base$year), ]

  out <- base[, c("year", "aeo_nominal_brent", "aeo_real_brent", "real_nominal_ratio")]
  out$adjusted_nominal_brent <- out$aeo_nominal_brent
  out$adjusted_real_brent <- out$aeo_real_brent
  out$source <- "AEO base"
  out$horizon_f_applied <- NA_integer_
  out$predicted_delta_aeo_applied <- NA_real_

  m_t <- steo_metric[steo_metric$issue_year == target_vintage, ]
  m_p <- steo_metric[steo_metric$issue_year == (target_vintage - 1), ]
  if (nrow(m_t) == 0 || nrow(m_p) == 0) stop("Missing STEO metrics for target years.")
  delta_target <- fit_table$target_delta_steo[!is.na(fit_table$target_delta_steo)][1]

  # Splice near-term nominal STEO for target-1 and target years (2025 and 2026).
  feb_t <- steo_source[steo_source$issue_year == target_vintage, ]
  for (y in c(target_vintage - 1, target_vintage)) {
    sv <- feb_t$steo_nominal_brent[feb_t$year == y]
    i <- which(out$year == y)
    if (length(sv) > 0 && length(i) > 0) {
      out$adjusted_nominal_brent[i[1]] <- sv[1]
      out$adjusted_real_brent[i[1]] <- sv[1] * out$real_nominal_ratio[i[1]]
      out$source[i[1]] <- "FEB STEO splice"
    }
  }

  # Horizon-based predicted AEO deltas for years >= target_vintage + 1.
  for (h in seq_len(MAX_HORIZON)) {
    fr <- fit_table[fit_table$horizon_f == h, ]
    if (nrow(fr) == 0 || is.na(fr$predicted_delta_aeo[1])) next
    y_new <- target_vintage + h
    y_old <- base_vintage + h
    i_new <- which(out$year == y_new)
    i_old <- which(out$year == y_old)
    if (length(i_new) == 0 || length(i_old) == 0) next

    out$adjusted_nominal_brent[i_new[1]] <- out$aeo_nominal_brent[i_old[1]] + fr$predicted_delta_aeo[1]
    out$adjusted_real_brent[i_new[1]] <- out$adjusted_nominal_brent[i_new[1]] * out$real_nominal_ratio[i_new[1]]
    out$source[i_new[1]] <- "Predicted AEO revision by horizon model"
    out$horizon_f_applied[i_new[1]] <- h
    out$predicted_delta_aeo_applied[i_new[1]] <- fr$predicted_delta_aeo[1]
  }

  out$target_vintage <- target_vintage
  out$base_vintage <- base_vintage
  out$delta_steo_ave_target <- delta_target
  write.csv(out, output_path, row.names = FALSE)
  out
}

run_model <- function() {
  # Default behavior uses saved source files to avoid repeated EIA downloads.
  # If these files do not exist, set USE_SAVED_DATA <- FALSE and rerun.
  # if (!USE_SAVED_DATA) {
  #   aeo_vintages <- get_available_aeo_vintages()
  #   steo_issue_years <- 1997:2026
  #   aeo_source <- build_aeo_source(aeo_vintages)
  #   steo_source <- build_steo_source(steo_issue_years)
  #   steo_metric <- build_steo_metric(steo_source)
  #   write.csv(aeo_source, "brent_aeo_source_compiled.csv", row.names = FALSE)
  #   write.csv(steo_source, "brent_steo_feb_source_compiled.csv", row.names = FALSE)
  #   write.csv(steo_metric, "brent_steo_feb_metric_compiled.csv", row.names = FALSE)
  # } else {
  #   aeo_source <- read.csv("brent_aeo_source_compiled.csv", stringsAsFactors = FALSE)
  #   steo_source <- read.csv("brent_steo_feb_source_compiled.csv", stringsAsFactors = FALSE)
  #   steo_metric <- read.csv("brent_steo_feb_metric_compiled.csv", stringsAsFactors = FALSE)
  # }
  if (!USE_SAVED_DATA) {
    aeo_vintages <- get_available_aeo_vintages()
    steo_issue_years <- 1997:2026
    aeo_source <- build_aeo_source(aeo_vintages)
    steo_source <- build_steo_source(steo_issue_years)
    steo_metric <- build_steo_metric(steo_source)
    write.csv(aeo_source, "brent_aeo_source_compiled.csv", row.names = FALSE)
    write.csv(steo_source, "brent_steo_feb_source_compiled.csv", row.names = FALSE)
    write.csv(steo_metric, "brent_steo_feb_metric_compiled.csv", row.names = FALSE)
  } else {
    aeo_source <- read.csv("brent_aeo_source_compiled.csv", stringsAsFactors = FALSE)
    steo_source <- read.csv("brent_steo_feb_source_compiled.csv", stringsAsFactors = FALSE)
    steo_metric <- build_steo_metric(steo_source)
    write.csv(steo_metric, "brent_steo_feb_metric_compiled.csv", row.names = FALSE)
  }

  aeo_vintages <- sort(unique(aeo_source$vintage))
  candidate_vintages <- seq(min(aeo_vintages), max(aeo_vintages))

  pair_status <- build_pair_status(
    candidate_vintages = candidate_vintages,
    aeo_vintages_available = sort(unique(aeo_source$vintage)),
    steo_metric_years = sort(unique(steo_metric$issue_year))
  )
  write.csv(pair_status, "brent_pair_availability.csv", row.names = FALSE)

  usable_vintages <- pair_status$vintage[pair_status$usable]

  measures <- data.frame(
    measure = c("delta_steo_ave", "delta_steo_curr", "delta_steo_next"),
    label = c("average(curr,next)", "current-year", "next-year"),
    stringsAsFactors = FALSE
  )
  panel_all <- list()
  fit_all <- list()
  score_rows <- list()
  kk <- 1
  for (i in seq_len(nrow(measures))) {
    mcol <- measures$measure[i]
    mlabel <- measures$label[i]
    panel <- build_regression_panel(aeo_source, steo_metric, usable_vintages, dsteo_col = mcol, max_h = MAX_HORIZON)
    panel$measure <- mcol
    panel_all[[i]] <- panel

    target_delta <- steo_metric[[mcol]][steo_metric$issue_year == 2026][1]
    fits <- fit_horizon_regressions(panel, max_h = MAX_HORIZON)
    fit_table <- fit_summary_table(fits, target_delta, mcol)
    fit_all[[i]] <- fit_table

    valid <- fit_table[!is.na(fit_table$r_squared), ]
    score_rows[[kk]] <- data.frame(
      measure = mcol,
      label = mlabel,
      n_horizons_fit = nrow(valid),
      avg_r_squared = ifelse(nrow(valid) > 0, mean(valid$r_squared), NA_real_),
      avg_abs_pred_delta = ifelse(nrow(valid) > 0, mean(abs(valid$predicted_delta_aeo)), NA_real_)
    )
    kk <- kk + 1
  }

  panel_long <- do.call(rbind, panel_all)
  fit_summary <- do.call(rbind, fit_all)
  model_scores <- do.call(rbind, score_rows)
  write.csv(panel_long, "brent_horizon_regression_panel_long.csv", row.names = FALSE)
  write.csv(fit_summary, "brent_horizon_regression_fit_summary.csv", row.names = FALSE)
  write.csv(model_scores, "brent_delta_steo_measure_comparison.csv", row.names = FALSE)

  # Wide panel for quick inspection by measure and vintage.
  wide <- reshape(
    panel_long[, c("measure", "vintage_new", "horizon_f", "delta_aeo", "delta_steo_measure")],
    idvar = c("measure", "vintage_new"),
    timevar = "horizon_f",
    direction = "wide"
  )
  write.csv(wide, "brent_horizon_regression_panel_wide.csv", row.names = FALSE)

  # Select best measure by highest average R^2 across horizons.
  best_row <- model_scores[which.max(model_scores$avg_r_squared), ]
  best_measure <- best_row$measure[1]
  best_fit <- fit_summary[fit_summary$measure == best_measure, ]

  adjusted <- build_adjusted_path(
    aeo_source = aeo_source,
    steo_source = steo_source,
    steo_metric = steo_metric,
    fit_table = best_fit,
    base_vintage = 2025,
    target_vintage = 2026,
    output_path = "brent_nominal_real_path_adjusted_to_steo2026.csv"
  )
  adjusted$selected_delta_steo_measure <- best_measure
  write.csv(adjusted, "brent_nominal_real_path_adjusted_to_steo2026.csv", row.names = FALSE)

  message("Wrote brent_aeo_source_compiled.csv")
  message("Wrote brent_steo_feb_source_compiled.csv")
  message("Wrote brent_steo_feb_metric_compiled.csv")
  message("Wrote brent_pair_availability.csv")
  message("Wrote brent_horizon_regression_panel_long.csv")
  message("Wrote brent_horizon_regression_panel_wide.csv")
  message("Wrote brent_horizon_regression_fit_summary.csv")
  message("Wrote brent_delta_steo_measure_comparison.csv")
  message("Wrote brent_nominal_real_path_adjusted_to_steo2026.csv")

  adjusted
}

if (sys.nframe() == 0) {
  brent_path <- tryCatch(
    run_model(),
    error = function(e) {
      warning(paste0("Failed to build horizon-regression Brent path: ", as.character(e)))
      NULL
    }
  )
}
