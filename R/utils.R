
.giphy_api_key <- function()"dc6zaTOxFJmzC"

.giphy_url <- function()"http://api.giphy.com/"

search_query_parser <- function(x) {
  return(gsub("\\s+", "+", x))
}

content_exporter <- function(x) {
  map_df(x$data, ~c(
    .x[which(!names(.x) %in% c("images", "user"))],
    gif_url(.x$images),
    mp4_url(.x$images))
  )
}
