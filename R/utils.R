
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

gif_url <- function(images) {
  images <- images[which(names(images) %in% c(
    "fixed_height", "fixed_height_still", "fixed_height_downsampled",
    "fixed_width", "fixed_width_still", "fixed_width_downsampled",
    "fixed_height_small", "fixed_height_small_still", "fixed_width_small",
    "fixed_width_small_still", "downsized", "downsized_still",
    "downsized_large", "downsized_medium", "original",
    "original_still","preview_gif"
  ))]
  out <- lapply(images, function(x) {
    if ("url" %in% names(x)) {
      x[[which(names(x) == "url")]]
    }
  })
  return(out)
}

mp4_url <- function(images) {
  mp4_variables <- c(
    "fixed_height", "fixed_width", "fixed_height_small", "fixed_width_small",
    "original", "looping", "preview", "downsized_small"
  )
  images <- images[which(names(images) %in% mp4_variables)]
  out <- lapply(images, function(x) {
    if ("mp4" %in% names(x)) {
      x[[which(names(x) == "mp4")]]
    }
  })
  names(out) <- paste0(mp4_variables, "_mp4")
  return(out)
}

