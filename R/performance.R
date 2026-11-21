# Period performance + matching chart ranges ---------------------------------

suppressPackageStartupMessages(library(lubridate))

PERIODS <- c(
  "1D"  = "1 day",    "5D"  = "5 days",  "1M"  = "1 month",
  "3M"  = "3 months", "6M"  = "6 months", "YTD" = "Year to date",
  "1Y"  = "1 year",   "5Y"  = "5 years"
)

# Index of the row used as the starting point of each period.
period_start_row <- function(dates, period) {
  n <- length(dates)
  last <- dates[n]
  on_or_before <- function(d) {
    i <- which(dates <= d)
    if (length(i) == 0 || d < dates[1]) NA_integer_ else max(i)
  }
  switch(period,
    "1D"  = if (n > 1) n - 1L else NA_integer_,
    "5D"  = if (n > 5) n - 5L else NA_integer_,
    "1M"  = on_or_before(last %m-% months(1)),
    "3M"  = on_or_before(last %m-% months(3)),
    "6M"  = on_or_before(last %m-% months(6)),
    "YTD" = on_or_before(floor_date(last, "year") - 1),
    "1Y"  = on_or_before(last %m-% years(1)),
    "5Y"  = on_or_before(last %m-% years(5))
  )
}

#' One row per period: label, return (fraction), start timestamp (ms) for the chart.
compute_performance <- function(prices) {
  dates <- as.Date(index(prices))
  close <- as.numeric(prices$Close)
  n <- length(close)
  to_ms <- function(d) as.numeric(as.POSIXct(d, tz = "UTC")) * 1000

  rows <- lapply(names(PERIODS), function(p) {
    i <- period_start_row(dates, p)
    data.frame(
      key    = p,
      label  = PERIODS[[p]],
      ret    = if (is.na(i)) NA_real_ else close[n] / close[i] - 1,
      min_ms = to_ms(if (is.na(i)) dates[1] else dates[i]),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  attr(out, "max_ms") <- to_ms(dates[n])
  out
}

format_pct <- function(x) {
  if (is.na(x)) return("—")
  pct <- x * 100
  body <- if (abs(pct) >= 1000) sprintf("%.2fK%%", abs(pct) / 1000)
          else sprintf("%.2f%%", abs(pct))
  if (pct < 0) paste0("−", body) else body
}
