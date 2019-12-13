library("ggplot2")
library("hexSticker")
library("rphylopic")

img <- image_data("8db421ba-4b42-4bd8-b948-5214ce122f57", size = "512")[[1]]

p <- ggplot() +
  theme_void() +
  add_phylopic(img, color = "#f0f6fa", ysize = 0.1, alpha = 1)

hexSticker::sticker(
  subplot  = p,
  package  = "argostools",
  p_size   = 8,
  p_family = "sans",
  p_x      = 1.00,
  p_y      = 0.65,
  s_x      = 1.00,
  s_y      = 1.20,
  s_width  = 0.90,
  s_height = 0.80,
  h_fill   = "#6eaecd",
  h_color  = "#f0f6fa",
  filename = "inst/figures/argostools.png",
  dpi      = 300
)
