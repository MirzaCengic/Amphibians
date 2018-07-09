# Script for making slide figure for 2018 BES Macro presentation


pacman::p_load(raster, tidyverse, Rahat, data.table, sf)


#### Slide with the first explanation of graph ####
# Basic plot setup
plot_basic <- ggplot() +
  geom_hline(yintercept = 0,
             size = 0.5,
             color = "grey80",
             # alpha = 0.75,
             # linetype = "dashed",
             NULL) +
  geom_vline(xintercept = 0,
             size = 0.5,
             # alpha = 0.75,
             color = "grey80",
             # linetype = "dashed",
             NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(linetype = "dashed"),
        panel.background = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        # axis.line = element_line(colour = "black"),
        NULL) +
  labs(x = "Decrease (%)                        Increase (%)\nCombined effect",
       y = "Additive effect\nDecrease (%)                        Increase (%)") +
  xlim(-100, 100) +
  ylim(-100, 100)

# First explanation slide, with faint labels
r_intro1 <- plot_basic +
  geom_segment(aes(x = -100, xend = -100, y = -0, yend = -90), size = 2, alpha = 0.2,# lower left left
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(y = -100, yend = -100, x = -0, xend = -90), size = 2, alpha = 0.2,# LLR
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = -100, xend = -100, y = 0, yend = 100), size = 2, alpha = 0.2,# lower left left
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(y = -100, yend = -100, x = 0, xend = 100), size = 2, alpha = 0.2, # LLR
               arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = 0, y = 0,
           label = c("What are the additive\nand combined effects of\nclimate and land use change\non amphibian biodiversity?"),
           color = "#B82B22", size = 10, fontface = "bold") +
  theme(
    axis.title = element_text(color = "grey70"),
    axis.text = element_text(color = "grey70")
  )

r_intro0 <- plot_basic +
  annotate("text", x = 0, y = 60,
           label = "Question?",
           color = "#B82B22", size = 14, fontface = "bold") +
  theme(
    axis.title = element_text(color = "grey90"),
    axis.text = element_text(color = "grey90")
  )

r_intro0b <- r_intro0 +
  annotate("text", x = 0, y = -20,
           label = c("What are the additive\nand combined effects of\nclimate and land use change\non amphibian biodiversity?"),
           color = "#B82B22", size = 10, fontface = "bold")

# r_intro1b

# Second figure where labels come out
# First explanation slide, with faint labels
r_intro2 <- plot_basic +
  geom_segment(aes(x = -100, xend = -100, y = -3, yend = -90), size = 2,# lower left left
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(y = -100, yend = -100, x = -3, xend = -90), size = 2,# LLR
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = -100, xend = -100, y = 3, yend = 100), size = 2,# lower left left
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(y = -100, yend = -100, x = 3, xend = 100), size = 2, # LLR
               arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = 0, y = 0,
           label = c("What are the additive\nand combined effects of\nclimate and land use change\non amphibian biodiversity?"),
           color = "#B82B22", size = 10, fontface = "bold", alpha = 0.5)
# r_intro1
# r_intro2


####################################
# Slide with second explanation of graph
# Third one would be what's on the each side of the 1:1 line

r_intro3 <- plot_basic +
  geom_segment(aes(x = -100, xend = -100, y = -3, yend = -90), size = 2,# lower left left
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(y = -100, yend = -100, x = -3, xend = -90), size = 2,# LLR
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = -100, xend = -100, y = 3, yend = 100), size = 2,# lower left left
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(y = -100, yend = -100, x = 3, xend = 100), size = 2, # LLR
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_abline(size = 0.2,
              color = "grey30",
              linetype = "dashed") +
  annotate("text",
           x = c(-42, -21, # LL
                 45, 90, # UR
                 -45, -22, # UL
                 42, 75), # LR
           y = c(-25, -29,
                 87, 83,
                 85, 74,
                 -25, -18),
           label = c("Combined", "Additive", "Combined", "Additive",
                     "Combined", "Additive", "Combined", "Additive"), color = "black",
           angle = 45, size = 8) +
  # annotate("text", x = c(-85, 85, -85, 85), y = c(-30, 55, 55, -30),
  #          label = c("C A", "C   A",
  #                    "C  A", "C A"), color = "black", size = 9) +
  annotate("text", x = c(-50, 50, -50, 50), y = c(-50, 50, 50, -50),
           label = c("--", "++", "+-", "-+"), color = "#B82B22", size = 50, fontface = "bold") +
  NULL



# r_intro2
# r_intro3
# With focus on the plus plus part

r_intro4 <- r_intro3 +
  geom_rect(aes(xmin = 5, xmax = 95, ymin = 16, ymax = 62),
            fill = "transparent", color = "grey20", size = 2)

####


# r_intro1
# r_intro2
# r_intro3
# r_intro4
#


#### Zoomed in figure

r_intro_zoomed1 <- ggplot() +
  # geom_hline(yintercept = 0,
  #            size = 0.5,
  #            color = "grey50",
  #            # linetype = "dashed",
  #            NULL) +
  # geom_vline(xintercept = 0,
  #            size = 0.5,
  #            color = "grey50",
  #            # linetype = "dashed",
  #            NULL) +
  geom_segment(aes(x = 0, xend = 0, y = 3, yend = 100), size = 1, color = "grey20",
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(y = 0, yend = 0, x = 3, xend = 100), size = 1, color = "grey20",
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_abline(size = 0.2,
              color = "grey30",
              linetype = "dashed") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(linetype = "dashed"),
        panel.background = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        # axis.line = element_line(colour = "black"),
        NULL) +
  labs(x = "Increase (%)\nCombined effect",
       y = "Additive effect\nIncrease (%)") +
  xlim(0, 100) +
  ylim(0, 100)



triangle_x <- data.table(group = c(1,1,1),
                         polygon.x = c(0,100,100),
                         polygon.y = c(0,100,0))
triangle_y <- data.table(group = c(1,1,1),
                         polygon.x = c(0,100,0),
                         polygon.y = c(100,100,0))
#fc8d59
#ffffbf
#91bfdb
r_intro_zoomed2 <- r_intro_zoomed1 +
  geom_polygon(data = triangle_y, aes(x = polygon.x,y = polygon.y, group = group),
               fill = "#fc8d59",
               alpha = 0.5
               )

r_intro_zoomed2b <- r_intro_zoomed2 +
  annotate("text", x = 40, y = 60,
           label = "Additive effect\ngreater than combined",
           color = "grey30",
           angle = 40,
           size = 10)

# r_intro_zoomed2


r_intro_zoomed3 <- r_intro_zoomed2b +
    geom_polygon(data = triangle_x, aes(x = polygon.x,y = polygon.y, group = group),
                 fill = "#91bfdb",
                 alpha = 0.5)

r_intro_zoomed3b <- r_intro_zoomed3 +
  annotate("text", x = 62, y = 40,
           label = "Combined effect\ngreater than additive",
           color = "grey30",
           angle = 41,
           size = 10,
           NULL)

# 3b
# r_intro_zoomed3b +
#   geom_abline(size = 0.5,
#               color = "grey30"
#               ,
#               linetype = "dashed"
#               )


#### Preview
# r_intro1
# r_intro2
# r_intro3
# r_intro4
# ##
# r_intro_zoomed1
# r_intro_zoomed2
# r_intro_zoomed2b
# r_intro_zoomed3
# r_intro_zoomed3b
# Saving intro figure slides
# 1

# ggsave(filename = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro1.png" %>%
#          milkunize2(),
#        plot = r_intro1,
#        dpi = 600,
#        NULL)
svg(file = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro1.svg" %>%
      milkunize2(), width = 8, height = 7)
r_intro1
dev.off()

svg(file = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro0.svg" %>%
      milkunize2(), width = 8, height = 7)
r_intro0
dev.off()

svg(file = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro0b.svg" %>%
      milkunize2(), width = 8, height = 7)
r_intro0b
dev.off()
# 2

svg(file = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro2.svg" %>%
      milkunize2(), width = 8, height = 7)
r_intro2
dev.off()
# 3
svg(file = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro3.svg" %>%
      milkunize2(), width = 8, height = 7)
r_intro3
dev.off()
# 4
svg(file = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro4.svg" %>%
      milkunize2(), width = 8, height = 7)
r_intro4
dev.off()
# Saving zoomed ones
# 1
svg(file = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro_zoomed1.svg" %>%
      milkunize2(), width = 8, height = 7)
r_intro_zoomed1
dev.off()

# 2
svg(file = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro_zoomed2.svg" %>%
      milkunize2(), width = 8, height = 7)
r_intro_zoomed2
dev.off()
# 2b
svg(file = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro_zoomed2b.svg" %>%
      milkunize2(), width = 8, height = 7)
r_intro_zoomed2b
dev.off()
# 3
svg(file = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro_zoomed3.svg" %>%
      milkunize2(), width = 8, height = 7)
r_intro_zoomed3
dev.off()
# 3b
svg(file = "Projects/Amphibians/Documents/Presentations/BES_Macro_2018/slide_figs/r_intro_zoomed3b.svg" %>%
      milkunize2(), width = 8, height = 7)
r_intro_zoomed3b
dev.off()

##################################

# Remaking the plots

p <- ggplot() +
  geom_hline(yintercept = 0,
             size = 0.5,
             color = "grey80",
             # alpha = 0.75,
             # linetype = "dashed",
             NULL) +
  geom_vline(xintercept = 0,
             size = 0.5,
             # alpha = 0.75,
             color = "grey80",
             # linetype = "dashed",
             NULL) +
  geom_abline(size = 0.2, linetype = "dashed") +
  # geom_point() +
  theme_minimal() +
  # scale_color_jco() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(linetype = "dashed"),
        panel.background = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        # legend.position = "top",
        # axis.line = element_line(colour = "black"),
        NULL) +
  labs(x = "Decrease (%)                        Increase (%)\nCombined effect",
       y = "Additive effect\nDecrease (%)                        Increase (%)") +
  xlim(-100, 100) +
  ylim(-100, 100) +
  # annotate("text", x = c(-85, 85, -85, 85), y = c(-25, 55, 55, -25),
  #          label = c("Combined\nAdditive", "Combined\nAdditive",
  #                    "Combined\nAdditive", "Combined\nAdditive"), color = "black",
  #          angle = 90, size = 4) +
  # annotate("text", x = c(-85, 85, -85, 85), y = c(-30, 55, 55, -30),
  #          label = c("C A", "C   A",
  #                    "C  A", "C A"), color = "black", size = 9) +
  # annotate("text", x = c(-85, 85, -85, 85), y = c(-40, 40, 40, -40),
  #          label = c("--", "++", "+-", "-+"), color = "#B82B22", size = 26, fontface = "bold") +
  NULL





triangle_x1 <- data.table(group = c(1,1,1),
                         polygon.x = c(0,100,100),
                         polygon.y = c(0,100,0))
triangle_y1 <- data.table(group = c(1,1,1),
                         polygon.x = c(0,100,0),
                         polygon.y = c(100,100,0))
#

triangle_x2 <- data.table(group = c(1,1,1),
                          polygon.x = c(-100,0, 0),
                          polygon.y = c(-100,-100,0))
triangle_y2 <- data.table(group = c(1,1,1),
                          polygon.x = c(-100,-100,0),
                          polygon.y = c(0,-100,0))
#
triangle_x3 <- data.table(group = c(1,1,1),
                          polygon.x = c(-100,-100, 0),
                          polygon.y = c(100,0,0))

triangle_y3 <- data.table(group = c(1,1,1),
                          polygon.x = c(-100, 0, 0),
                          polygon.y = c(100, 100,0))
#
triangle_x4 <- data.table(group = c(1,1,1),
                          polygon.x = c(0, 100, 0),
                          polygon.y = c(0, -100, -100))

triangle_y4 <- data.table(group = c(1,1,1),
                          polygon.x = c(0, 100, 100),
                          polygon.y = c(0, 0, -100))

####
blue <- "#91bfdb"
red <- "#fc8d59"

p1 <- p +
  geom_polygon(data = triangle_y1, aes(x = polygon.x,y = polygon.y, group = group),
               fill = red,
               alpha = 0.5) +
  geom_polygon(data = triangle_x1, aes(x = polygon.x,y = polygon.y, group = group),
               fill = blue,
               alpha = 0.5) +
  geom_polygon(data = triangle_y2, aes(x = polygon.x,y = polygon.y, group = group),
               fill = red,
               alpha = 0.5) +
  geom_polygon(data = triangle_x2, aes(x = polygon.x,y = polygon.y, group = group),
               fill = blue,
               alpha = 0.5) +
  # geom_polygon(data = triangle_y3, aes(x = polygon.x,y = polygon.y, group = group),
  #              fill = blue,
  #              alpha = 0.5) +
  # geom_polygon(data = triangle_x3, aes(x = polygon.x,y = polygon.y, group = group),
  #              fill = red,
  #              alpha = 0.5) +
  # geom_polygon(data = triangle_y4, aes(x = polygon.x,y = polygon.y, group = group),
  #              fill = blue,
  #              alpha = 0.5) +
  # geom_polygon(data = triangle_x4, aes(x = polygon.x,y = polygon.y, group = group),
  #              fill = red,
  #              alpha = 0.5) +
NULL



p1 + annotate("text", x = -20, y = 20,
           label = "Additive effect\ngreater than combined",
           color = "grey30",
           angle = 45,
           size = 10)


r_intro_zoomed2b <- r_intro_zoomed2 +
  annotate("text", x = 40, y = 60,
           label = "Additive effect\ngreater than combined",
           color = "grey30",
           angle = 40,
           size = 10)

# r_intro_zoomed2


r_intro_zoomed3 <- r_intro_zoomed2b
