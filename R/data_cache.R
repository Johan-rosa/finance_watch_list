# Data access with an on-disk cache -----------------------------------------
#
# Each ticker is stored as data/cache/<TICKER>.rds holding:
#   prices     xts with Open/High/Low/Close/Volume/Adjusted (full history)
#   name       long company name
#   currency   trading currency
#   fetched_at POSIXct of the last successful refresh
#
# Refresh rules:
#   * no cache                -> full download
#   * cache younger than TTL  -> use as is (no network)
#   * cache older than TTL    -> incremental download of the last few days and
#                                merge. If the overlapping closes don't match
#                                (split / restatement) -> full re-download.
#   * download fails          -> fall back to the cached copy with a warning

suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
})

cache_path <- function(ticker, cache_dir) {
  file.path(cache_dir, paste0(gsub("[^A-Za-z0-9._-]", "_", ticker), ".rds"))
}

standardize_ohlc <- function(x) {
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  x[!is.na(x$Close), ]
}

download_prices <- function(ticker, from = "1900-01-01") {
  x <- getSymbols(ticker, src = "yahoo", from = from, auto.assign = FALSE,
                  warnings = FALSE)
  standardize_ohlc(x)
}

download_meta <- function(ticker) {
  q <- tryCatch(
    getQuote(ticker, what = yahooQF(c("Name (Long)", "Currency"))),
    error = function(e) NULL
  )
  name <- if (!is.null(q) && !is.na(q$NameLong[1])) q$NameLong[1] else ticker
  currency <- if (!is.null(q) && !is.na(q$Currency[1])) q$Currency[1] else "USD"
  list(name = name, currency = currency)
}

refresh_incremental <- function(ticker, cached) {
  old <- cached$prices
  overlap_from <- index(old)[max(1, nrow(old) - 5)]
  new <- download_prices(ticker, from = overlap_from)

  common <- intersect(as.character(index(old)), as.character(index(new)))
  if (length(common) == 0) return(NULL)
  rel_diff <- abs(as.numeric(new[common, "Close"]) /
                    as.numeric(old[common, "Close"]) - 1)
  if (any(rel_diff > 0.005, na.rm = TRUE)) return(NULL)  # split etc.

  # new rows win on overlapping dates
  rbind(old[!index(old) %in% index(new), ], new)
}

#' Get full price history + metadata for a ticker, using the cache.
#' @param ttl_hours how long a cached copy is considered fresh.
get_stock <- function(ticker, cache_dir = "data/cache", ttl_hours = 6,
                      force = FALSE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- cache_path(ticker, cache_dir)
  cached <- if (file.exists(path)) readRDS(path) else NULL

  age_h <- if (is.null(cached)) Inf else
    as.numeric(difftime(Sys.time(), cached$fetched_at, units = "hours"))
  if (!force && age_h < ttl_hours) return(cached)

  fresh <- tryCatch({
    prices <- if (!is.null(cached) && !force) refresh_incremental(ticker, cached)
    if (is.null(prices)) prices <- download_prices(ticker)
    meta <- if (!is.null(cached)) cached[c("name", "currency")] else download_meta(ticker)
    c(list(ticker = ticker, prices = prices, fetched_at = Sys.time()), meta)
  }, error = function(e) {
    warning(sprintf("Download failed for %s: %s", ticker, conditionMessage(e)),
            call. = FALSE)
    NULL
  })

  if (is.null(fresh)) {
    if (is.null(cached)) return(NULL)
    return(cached)  # stale but usable
  }
  saveRDS(fresh, path)
  fresh
}
