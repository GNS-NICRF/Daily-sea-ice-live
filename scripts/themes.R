# Various thematic objects recycled from plots elsewhere

## Discussion of text sizes to word
# Put simply, ggplot text scaling, and the way that R scales text more broadly, sucks for the end user. DPI of output images, pts vs pxs, all effect text size.
# This is compounded by ggplot's own internal, relative scaling, which is murkier still.
# The only workaround is to experiment with text sizes, output to word, in order to derive a suitable scaling parameter that makes the text sizes in a ggplot
# resemble the desired outcome.

##### Global theme settings #####

## These parameters will affect the overall text scaling

## Scaling for normal (i.e. something that is going to be ~ 14 cm wide and around 4-5 cm tall)
textsize_normal = 9 * 2.35 # Desired textsize multiplied by a scaling factor

textsize_small  = 8 * 2.3

textsize_vsmall = 7 * 2.25

##### Small theme objects #####

# Plotting global themes. Can be overwritten on a per-plot basis.
# axis_text_size = 9
# axis_title_text_size = 10
# theme_text <- theme(
#   axis.text = element_text(size = axis_text_size),
#   axis.title = element_markdown(size = axis_title_text_size),
#   axis.title.y.right = element_markdown(size = axis_title_text_size),
#   axis.title.y.left = element_markdown(size = axis_title_text_size),
#   legend.text = element_text(size = axis_text_size),
#   plot.title = element_text(hjust = 0.5, size = axis_title_text_size)
# )

# themes <- list(
#   scale_x_continuous(expand = c(0,0)),
#   scale_y_continuous(expand = c(0,0)),
#   theme_cowplot(12)
# )

# For stacked plots
theme_rem_xaxis <- theme(
  axis.text.x = element_blank(),
  axis.line.x = element_blank(),
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank()
)
# geom.text.size = 3
# theme.size = (14/5) * geom.text.size
# inset.geom.text.size = 2
# inset.theme.size = (14/5) * inset.geom.text.size

# For dummy axes chiefly.
# Not sure how this scales just yet.
theme_blank <- theme(
  axis.ticks.y = element_blank(),
  axis.ticks.x = element_line(),
  #axis.ticks.length.x = unit(3, "pt"),
  axis.title.x = element_blank(),
  axis.line.y = element_blank(),
  #axis.line.x = element_line(size = 0.7, linetype = 'solid',colour = "black"),
  axis.line.x = element_blank(),
  axis.text.y = element_blank(),
  axis.text.x = element_text(
    margin = margin(t = -18, unit = "pt"),
    angle = 45,
    size = (14/5)*3),
  legend.title = element_blank(),
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent"))

##### Larger theme objects #####

# Intended to replace theme_cowplot. A direct adaptation of that theme, incorporating the theme_text elements.
library('ggtext')

theme_general <- function (font_size = textsize_normal, font_family = "", line_size = 1,
                      rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14)
{
  half_line <- font_size/2
  small_size <- rel_small * font_size
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(line = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
          rect = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
          text = element_text(family = font_family, face = "plain", color = "black", size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(), debug = FALSE),
          axis.line = element_line(color = "black", size = line_size, lineend = "square"),
          axis.line.x = NULL,
          axis.line.y = NULL,
          axis.text = element_text(color = "black", size = small_size),
          axis.text.x = element_text(margin = margin(t = small_size/4), vjust = 1),
          axis.text.x.top = element_text(margin = margin(b = small_size/4),  vjust = 0),
          axis.text.y = element_text(margin = margin(r = small_size/4), hjust = 1),
          axis.text.y.right = element_text(margin = margin(l = small_size/4), hjust = 0),
          axis.ticks = element_line(color = "black",  size = line_size),
          axis.ticks.length = unit(half_line/2, "pt"),
          axis.title.x = element_markdown(margin = margin(t = half_line/2), vjust = 1),#margin = margin(t = half_line/2), vjust = 1),
          axis.title.x.top = element_markdown(margin = margin(b = half_line/2), vjust = 0),#margin = margin(b = half_line/2), vjust = 0),
          axis.title.y = element_markdown(angle = 90, margin = margin(r = half_line/2), vjust = 1),
          axis.title.y.right = element_markdown(angle = -90, margin = margin(l = half_line/2), vjust = 0),
          axis.title.y.left = element_markdown(angle = 90, margin = margin(r = half_line/2), vjust = 1),
          legend.background = element_blank(),
          legend.spacing = unit(font_size, "pt"), legend.spacing.x = NULL,
          legend.spacing.y = NULL, legend.margin = margin(0, 0, 0, 0),
          legend.key = element_blank(),
          legend.key.size = unit(1.1 * font_size, "pt"),
          legend.key.height = NULL,
          legend.key.width = NULL,
          legend.text = element_text(size = rel(rel_small)),
          legend.text.align = NULL,
          legend.title = element_text(hjust = 0),
          legend.title.align = NULL,
          legend.position = "right",
          legend.direction = NULL,
          legend.justification = c("left", "center"),
          legend.box = NULL,
          legend.box.margin = margin(0, 0, 0, 0),
          legend.box.background = element_blank(),
          legend.box.spacing = unit(font_size, "pt"),
          panel.background = element_blank(),
          panel.border = element_blank(), panel.grid = element_blank(),
          panel.grid.major = NULL, panel.grid.minor = NULL,
          panel.grid.major.x = NULL, panel.grid.major.y = NULL,
          panel.grid.minor.x = NULL, panel.grid.minor.y = NULL,
          panel.spacing = unit(half_line, "pt"), panel.spacing.x = NULL,
          panel.spacing.y = NULL, panel.ontop = FALSE, strip.background = element_rect(fill = "grey80"),
          strip.text = element_text(size = rel(rel_small),margin = margin(half_line/2, half_line/2, half_line/2, half_line/2)),
          strip.text.x = NULL,
          strip.text.y = element_text(angle = -90),
          strip.placement = "inside", strip.placement.x = NULL,
          strip.placement.y = NULL, strip.switch.pad.grid = unit(half_line/2,"pt"),
          strip.switch.pad.wrap = unit(half_line/2,"pt"),
          plot.background = element_blank(),
          plot.title = element_text(face = "bold",size = rel(rel_large), hjust = 0.5, vjust = 1,margin = margin(b = half_line)),
          plot.subtitle = element_text(size = rel(rel_small),hjust = 0, vjust = 1, margin = margin(b = half_line)),
          plot.caption = element_text(size = rel(rel_tiny),hjust = 1, vjust = 1, margin = margin(t = half_line)),
          plot.tag = element_text(face = "bold", hjust = 0,vjust = 0.7),
          plot.tag.position = c(0, 1),
          plot.margin = margin(half_line,half_line, half_line, half_line),
          complete = TRUE)
}

##### Colour palettes #####

# 12-increment qualitative colour palette from colour brewer
pal12 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
