library(cropcircles)
library(ggplot2)
library(ggpath)
library(showtext)

# choose a font from Google Fonts
font_add_google("Fira Sans", "firasans")
showtext_auto()

img_cropped <- hex_crop(
  images = here::here("hexsticker", "calcal.png"),
  bg_fill = "#fffffe",
  border_colour = "#68c3b5",
  border_size = 50
)

ggplot() +
  geom_from_path(aes(0.5, 0.5, path = img_cropped)) +
  annotate(
    "text",
    x = -0.1,
    y = 1.55,
    label = "calcal",
    family = "firasans",
    size = 24,
    colour = "#013037",
    hjust = 0,
    fontface = "bold"
  ) +
  #annotate("text",
  #         x = 0.5, y = -0.3, label = "spiderorchid", family = "firasans", size = 18, colour = "white",
  #         fontface = "bold"
  #) +
  xlim(-1, 2) +
  ylim(-1, 2) +
  theme_void() +
  coord_fixed()

ggsave("./man/figures/calcal-hex.png", height = 2.5, width = 2.5)
