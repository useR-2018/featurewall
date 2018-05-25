# Dependencies
library(magick)
library(purrr)

# Params
sticker_row_size = 10 # The number of stickers in the longest row
sticker_width = 200 # The width of each sticker in pixels

# Load stickers
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
  map(image_scale, sticker_width)

# Incorrectly sized stickers
bad_size <- stickers %>%
  map_lgl(~ with(image_info(.x), height < (median(height)-2) | height > (median(height) + 2)))
which(bad_size)

# Remove bad stickers
sticker_rm <- low_res | bad_size
stickers <- stickers[!sticker_rm]

message(sprintf("Automatically removed %i incompatible stickers: %s",
                sum(sticker_rm), paste0(names(sticker_rm[sticker_rm]), collapse = ", ")))

# Coerce sticker sizes
sticker_height <- stickers %>%
  map(image_info) %>%
  map_dbl("height") %>%
  median

stickers <- stickers %>%
  map(image_resize, paste0(sticker_width, "x", sticker_height, "!"))

# Constuct canvas
sticker_col_size <- ceiling(NROW(image_info(stickers))/(sticker_row_size-0.5))
canvas <- image_blank(sticker_row_size*sticker_width, sticker_col_size*sticker_height/1.33526, "white")

# Arrange rows of stickers into images
row_lens <- rep(c(sticker_row_size,sticker_row_size-1), length.out=sticker_col_size)
row_lens[length(row_lens)] <- row_lens[length(row_lens)]  - (length(stickers) - sum(row_lens))
sticker_rows <- map2(row_lens, cumsum(row_lens),
     ~ seq(.y-.x+1, by = 1, length.out = .x)) %>%
  map(~ stickers[.x] %>%
        invoke(c, .) %>%
        image_append)

# Add stickers to canvas
canvas <- image_blank(sticker_row_size*sticker_width, sticker_col_size*sticker_height, "white")
canvas <- reduce2(sticker_rows, seq_along(sticker_rows), 
          ~ image_composite(
            ..1, ..2,
            offset = paste0("+", ((..3-1)%%2)*sticker_width/2, "+", (..3-1)*sticker_height/1.33526)
          ),
          .init = canvas)

image_write(canvas, "featurewall.png", format = "png")
