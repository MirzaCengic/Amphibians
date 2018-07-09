#!/usr/bin/env Rscript

#SBATCH --partition=milkun

# Analyze ESH

pacman::p_load(raster, tidyverse, Rahat, data.table, sf, ggsci)


# Load stuff
my_esh <- "Projects/Amphibians/Output/csv/Species_ESH_nodisp/" %>%
  milkunize2() %>%
  list.files(pattern = "csv", full.names = TRUE) %>%
  map(fread) %>%
  reduce(bind_rows)




my_esh_cln <- my_esh %>%
  mutate(Scenario = str_replace(Scenario, paste0(Species, "_"), ""),
         Scenario = case_when(
           Scenario == "layer.1" ~ "current_clim_current_lc",
           Scenario == "layer.2" ~ "future_clim_current_lc",
           Scenario == "layer.3" ~ "current_clim_future_SSP5_lc",
           Scenario == "layer.4" ~ "future_clim_future_SSP5_lc",
           TRUE ~ as.character(Scenario)
         )) %>%
  spread(Scenario, Area_km2) %>%
  mutate(
    change_rel_lc = ((current_clim_future_SSP5_lc - current_clim_current_lc) / current_clim_current_lc) * 100,
    change_rel_cc = ((future_clim_current_lc - current_clim_current_lc) / current_clim_current_lc) * 100,
    change_rel_both = ((future_clim_future_SSP5_lc - current_clim_current_lc) / current_clim_current_lc) * 100,
    additive = change_rel_lc + change_rel_cc,
    additive_new =   (1-((1 - (change_rel_cc / 100)) * (1 - (change_rel_lc / 100)))) * 100,
    add_diff = additive_new - additive,
    combined = change_rel_both)
#1
#head(my_esh_cln)
#summary(my_esh_cln$a
#my_esh_cln %>%
 # arrange(desc(add_diff)) %>% head


#my_esh_cln %>%
 # arrange(add_diff) %>% head

#
#
#2lc <- -20 / 100
#cc <- -10 / 100


#lc <- -0.2
#cc <- -0.1

#lc <- -0.4
#cc <- -0.4

#if (lc < 0 & cc < 0)
#  {
#  lc <- abs(lc)
#  cc <- abs(cc)

#  val <- (1- ((1 - lc) * (1 - cc)))
#  val <- -val
#} else {
#  val <- (1- ((1 - lc) * (1 - cc)))
#}



# 100 -
#  (1-((1 - lc) * (1 6- cc))) * 100

# ss <-
#   "Projects/Amphibians/biomod_wc/Cardioglossa.pulchra" %>%
#   milkunize2("archive") %>%
#   list.files(recursive = TRUE, full.names = TRUE, pattern = "grd") %>%
#   # str_subset("future") %>%
#   stack()
#
# plot(ss)

amphibians_df_raw <- "Projects/Amphibians/data/Species_data/Amphibians_shp.csv" %>%
  milkunize2() %>%
  fread()

# Select some data from the IUCN ranges shapefile to merge with the esh data
amphibians_df <- amphibians_df_raw %>%
  transmute(
    Species = binomial,
    Family = family,
    Threat_status = code,
    Range_area = shape_Area) %>%
  group_by(Species) %>%
  mutate(Range_area = sum(Range_area)) %>%
  ungroup() %>%
  distinct(Species, .keep_all = TRUE)


# Merge
my_amphibians_data <- my_esh_cln %>%
  inner_join(amphibians_df, by = "Species")


my_amphibians_data_plot <- my_amphibians_data %>%
  # select(Species, additive, combined) %>%
  mutate(
    add_sign = case_when(
      additive == 0 ~ "NC",
      additive < 0 ~ "-",
      additive > 0 ~ "+"),
    comb_sign = case_when(
      combined == 0 ~ "NC",
      combined < 0 ~ "-",
      combined > 0 ~ "+"),
    threatened = case_when(
      Threat_status == "CR" ~ "Threatened",
      Threat_status == "EN" ~ "Threatened",
      Threat_status == "VU" ~ "Threatened",
      Threat_status == "NT" ~ "Not-threatened",
      Threat_status == "LC" ~ "Not-threatened",
      Threat_status == "DD" ~ "Not-applicable",
      Threat_status == "EX" ~ "Extinct"
    ))
write_csv(my_amphibians_data_plot,"/vol/milkunB/mcengic/Projects/Amphibians/data.csv")
main_plot <- my_amphibians_data_plot %>%
  # filter(comb_sign == "+") %>%
  ggplot(aes(x = additive, y = combined,
             color = threatened,
             NULL)) +
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
  scale_color_jco() +
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

 main_plot + geom_point()



svg(file = "Projects/Amphibians/main_fig_base2.svg" %>%
      milkunize2(), width = 8, height = 7)
main_plot
dev.off()

svg(file = "Projects/Amphibians/main_fig2.svg" %>%
      milkunize2(), width = 8, height = 7)
main_plot + geom_point()
dev.off()

stop()

#my_amphibians_data_plot %>%
#  filter(additive > 50)
# library(plotly)

# ggplotly(p1, tooltip = "Species")

#nrow(climate_extinct)
#2my_amphibians_data %>% nrow
#climate_extinct <- my_amphibians_data %>%
 # filter(future_clim_current_lc == 0)


#amphibians_df %>%
 #1 arrange(desc(Range_area)) %>%
#  head()

aa <- climate_extinct %>%
  arrange(desc(Range_area)) %>%
  head(20)

my_amphibians_data %>%
  filter(change_rel_lc > 0 & change_rel_cc < 0) %>%
  arrange(desc(Range_area)) %>%
  filter(change_rel_cc != -100) %>%
  select(Species, change_rel_lc, change_rel_cc, Range_area)


aa %>% View()


#########
aa <-"Projects/Amphibians/biomod_wc/Lithobates.palmipes" %>%
  milkunize2("archive") %>%
  list.files(recursive = TRUE, full.names = TRUE, pattern = "grd") %>% stack

aa %>% hist

"Projects/Amphibians/biomod_EM_test" %>%
  milkunize2("archive") %>%
  setwd()

my_stack <- "Projects/Amphibians/biomod_EM_test" %>%
  milkunize2("archive") %>%
  list.files(recursive = TRUE, pattern = "grd", full.names  = TRUE) %>%
  str_subset("current") %>%
  str_subset("ensemble_TSS") %>%
  stack()

my_stack_f <- "Projects/Amphibians/biomod_EM_test" %>%
  milkunize2("archive") %>%
  list.files(recursive = TRUE, pattern = "grd", full.names  = TRUE) %>%
  str_subset("future") %>%
  str_subset("ensemble_TSS") %>%
  stack()

plot(my_stack)
plot(my_stack_f)



#### Find what is the cutoff value for prediction binarization ####
climate_predictions <-
  "Projects/Amphibians/biomod_wc/Lithobates.palmipes" %>%
  milkunize2("archive") %>%
  list.files(recursive = TRUE, full.names = TRUE, pattern = "grd") %>%
  # str_subset("future") %>%
  stack()

sp_range <- "Projects/Amphibians/Temp/L_palmipes_range.gpkg" %>%
  milkunize2("archive") %>%
  read_sf()


plot(climate_predictions)
plot(sp_range, add = TRUE)

library(rasterVis)
library(colorspace)
p <- levelplot(climate_predictions[[1]], margin = list(NULL), par.settings = rasterTheme(region = rev(sequential_hcl(10, power = 0.5)))) +
  layer(sp.lines(as(sp_range, "Spatial"), lwd=2, col = "black"))
p

hist(climate_predictions[[1]], xlim = c(0, 1000),
     maxpixels = ncell(climate_predictions[[1]]))
abline(v=cutoff_value,col="red") # Max TSS cutoff

hist(climate_predictions[[3]], xlim = c(0, 1000), maxpixels = ncell(climate_predictions[[3]]))
abline(v=cutoff_value,col="red")


#### Try rescaling
library(scales)
future_pred_vals <- getValues(climate_predictions[[3]])


aa <- future_pred_vals[!is.na(future_pred_vals)]

future_pred_val_rscl <- rescale(aa, to = c(minValue(climate_predictions[[1]]), maxValue(climate_predictions[[1]])))
aa %>% hist
future_pred_val_rscl %>% hist

future_pred_vals[!is.na(future_pred_vals)] <- future_pred_val_rscl
hist(future_pred_vals)
future_pred_rescaled <- setValues(climate_predictions[[3]], future_pred_vals)


plot(future_pred_rescaled)

qq <- future_pred_rescaled

qq[qq < cutoff_value] <- 0
qq[qq >= cutoff_value] <- 1

plot(climate_predictions[[2]])
plot(sp_range, add =T)
plot(qq)
plot(climate_predictions[[4]])
plot()

climate_extinct %>%
  arrange(desc(Range_area)) %>%
  head()

"Projects/Amphibians/biomod_wc" %>%
  milkunize2("archive") %>%
  setwd()

bm_out_file <- load("./Lithobates.palmipes/Lithobatespalmipes.test.models.out")

bm_out_file <- getwd() %>%
  paste0("/Lithobates.palmipes") %>%
  list.files(recursive = TRUE, pattern = "models.out", full.names = TRUE)

my_mod <- load(bm_out_file)

my_bm_out_mod  <-  get(my_mod)

cutoff_value <- my_bm_out_mod@models.evaluation@val[2,2,,,]

