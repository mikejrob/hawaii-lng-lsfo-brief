library(tidyverse)
library(lubridate)
library(slider)

# ── Data -----------------------------------------------------------------------
heco_raw  <- read_csv("/Users/mike/EIA/miscData/heco_lsfo.csv",        show_col_types = FALSE)
vlsfo_raw <- read_csv("/Users/mike/EIA/lng_vs_oil/vlsfo_prices.csv",   show_col_types = FALSE)

VLSFO_MT_TO_BBL <- 6.35
glob_col <- grep("global|20port", names(vlsfo_raw), ignore.case = TRUE, value = TRUE)[1]

heco <- heco_raw |>
  mutate(date = as.Date(floor_date(parse_date_time(Date, orders = c("mdy","ymd","dmy")), "month"))) |>
  filter(date >= as.Date("2022-02-01")) |>
  select(date, lsfo_bbl = Honolulu_LSFO) |>
  filter(!is.na(lsfo_bbl))

vlsfo <- vlsfo_raw |>
  transmute(
    date         = as.Date(date),
    sing         = singapore_vlsfo_usd_mt / VLSFO_MT_TO_BBL,
    glob         = .data[[glob_col]]      / VLSFO_MT_TO_BBL,
    avg_sing_hou = (singapore_vlsfo_usd_mt + houston_vlsfo_usd_mt) / 2 / VLSFO_MT_TO_BBL
  ) |>
  filter(!is.na(sing)) |>
  arrange(date)

# ── Predictor builder ----------------------------------------------------------
build_predictor <- function(spot, dates, w1, w2) {
  cp    <- slide_dbl(spot, mean, .before = w1 - 1L, .complete = TRUE)
  daily <- tibble(date = dates, cp = cp) |> filter(!is.na(cp))
  tibble(date = heco$date) |>
    mutate(pred = map_dbl(date, function(m) {
      anchor    <- floor_date(as.Date(m), "month") - days(7)
      win_start <- anchor - days(w2 - 1L)
      vals <- daily$cp[daily$date >= win_start & daily$date <= anchor]
      if (length(vals) == 0) NA_real_ else mean(vals, na.rm = TRUE)
    }))
}

# ── Fast OLS (closed form, no lm overhead) ------------------------------------
fast_ols_ssr <- function(y, x) {
  if (length(y) < 4) return(Inf)
  xm    <- mean(x); ym <- mean(y)
  ss_xx <- sum((x - xm)^2)
  if (ss_xx == 0) return(Inf)
  b1  <- sum((x - xm) * (y - ym)) / ss_xx
  b0  <- ym - b1 * xm
  sum((y - (b0 + b1 * x))^2)
}

fast_ols_coef <- function(y, x) {
  xm <- mean(x); ym <- mean(y)
  b1 <- sum((x - xm) * (y - ym)) / sum((x - xm)^2)
  c(intercept = ym - b1 * xm, slope = b1)
}

# ── Cache all predictors -------------------------------------------------------
POST_SERIES     <- "avg_sing_hou"
pre_series_list <- c("sing", "glob", "avg_sing_hou")

# Fine grid centred on coarse winner (w1=35, w2=30)
w1_fine <- seq(25L, 45L, by = 2L)   # 11 values
w2_fine <- seq(15L, 60L, by = 5L)   # 10 values

cat("Caching predictors...\n")
all_series <- union(pre_series_list, POST_SERIES)
pred_cache <- list()
for (s in all_series) {
  for (w1 in w1_fine) {
    for (w2 in w2_fine) {
      key <- paste(s, w1, w2, sep = "_")
      pred_cache[[key]] <- build_predictor(vlsfo[[s]], vlsfo$date, w1, w2)$pred
    }
  }
}

n_heco      <- nrow(heco)
y_all       <- heco$lsfo_bbl
sst         <- sum((y_all - mean(y_all))^2)
break_cands <- heco$date[9:(n_heco - 8)]

total_cells <- length(break_cands) * length(pre_series_list) *
               length(w1_fine)^2 * length(w2_fine)^2
cat(sprintf("H5 refined grid: %d cells\n", total_cells))

# ── Grid search ---------------------------------------------------------------
best_r2  <- -Inf
best_row <- NULL

for (bd in as.list(break_cands)) {
  bd <- as.Date(bd)
  idx_pre  <- which(heco$date <  bd)
  idx_post <- which(heco$date >= bd)
  y_pre    <- y_all[idx_pre]
  y_post   <- y_all[idx_post]

  for (sp in pre_series_list) {
    for (w1pr in w1_fine) {
      for (w2pr in w2_fine) {
        x_pre_all <- pred_cache[[paste(sp, w1pr, w2pr, sep = "_")]][idx_pre]
        ok_pre    <- !is.na(x_pre_all)
        if (sum(ok_pre) < 4) next
        ssr_pre <- fast_ols_ssr(y_pre[ok_pre], x_pre_all[ok_pre])
        if (!is.finite(ssr_pre)) next

        for (w1po in w1_fine) {
          for (w2po in w2_fine) {
            x_post_all <- pred_cache[[paste(POST_SERIES, w1po, w2po, sep = "_")]][idx_post]
            ok_post    <- !is.na(x_post_all)
            if (sum(ok_post) < 4) next

            ssr <- ssr_pre + fast_ols_ssr(y_post[ok_post], x_post_all[ok_post])
            r2  <- 1 - ssr / sst

            if (r2 > best_r2) {
              best_r2  <- r2
              best_row <- list(
                break_date = bd, series_pre = sp,
                w1_pre = w1pr, w2_pre = w2pr,
                w1_post = w1po, w2_post = w2po,
                r2 = r2, rmse = sqrt(ssr / (sum(ok_pre) + sum(ok_post)))
              )
            }
          }
        }
      }
    }
  }
}

if (is.null(best_row)) stop("H5 grid search found no valid cells — check pred_cache coverage")

# ── Report --------------------------------------------------------------------
b          <- best_row
idx_pre_b  <- which(heco$date <  b$break_date)
idx_post_b <- which(heco$date >= b$break_date)

x_pre_b  <- pred_cache[[paste(b$series_pre, b$w1_pre,  b$w2_pre,  sep = "_")]][idx_pre_b]
x_post_b <- pred_cache[[paste(POST_SERIES,  b$w1_post, b$w2_post, sep = "_")]][idx_post_b]
ok_pre_b  <- !is.na(x_pre_b)
ok_post_b <- !is.na(x_post_b)

coef_pre  <- fast_ols_coef(y_all[idx_pre_b][ok_pre_b],   x_pre_b[ok_pre_b])
coef_post <- fast_ols_coef(y_all[idx_post_b][ok_post_b], x_post_b[ok_post_b])

cat(sprintf("\n── H5 refined best ──\n"))
cat(sprintf("Break:       %s\n",         format(b$break_date)))
cat(sprintf("Pre  series: %s  w1=%d  w2=%d  b0=%.3f  b1=%.4f  n=%d\n",
            b$series_pre, b$w1_pre,  b$w2_pre,  coef_pre[1],  coef_pre[2],  sum(ok_pre_b)))
cat(sprintf("Post series: %s  w1=%d  w2=%d  b0=%.3f  b1=%.4f  n=%d\n",
            POST_SERIES,  b$w1_post, b$w2_post, coef_post[1], coef_post[2], sum(ok_post_b)))
cat(sprintf("R²=%.4f  RMSE=%.4f $/BBL\n", b$r2, b$rmse))