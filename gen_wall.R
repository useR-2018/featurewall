# Dependencies
library(magick)
library(purrr)

# Params
sticker_row_size = 10 # The number of stickers in the first row
sticker_width = 200 # The width of each sticker in pixels

# Script
sticker_files <- list.files("stickers")
stickers <- file.path("stickers", sticker_files) %>% 
  map(image_read) %>%
  map(image_trim) %>%
  set_names(sticker_files)

# Low resolution stickers
low_res <- stickers %>%
  map_lgl(~ image_info(.x)$width < sticker_width)
which(low_res)

stickers <- stickers %>%
  map(image_scale, "200")

# Incorrectly sized stickers
bad_size <- stickers %>%
  map_lgl(~ with(image_info(.x), height < (median(height)-2) | height > (median(height) + 2)))
which(bad_size)

# Remove bad stickers
sticker_rm <- low_res | bad_size
stickers <- stickers[!sticker_rm]

message(sprintf("Automatically removed %i incompatible stickers: %s",
                sum(sticker_rm), paste0(names(sticker_rm[sticker_rm]), collapse = ", ")))

sticker_height <- stickers %>%
  map(image_info) %>%
  map_dbl("height") %>%
  median
stickers <- stickers %>%
  map(image_resize, paste0(sticker_width, "x", sticker_height, "!"))

sticker_col_size <- ceiling(NROW(image_info(stickers))/(sticker_row_size-0.5))
canvas <- image_blank(sticker_row_size*sticker_width, sticker_col_size*sticker_height)

stickers[[1]]
