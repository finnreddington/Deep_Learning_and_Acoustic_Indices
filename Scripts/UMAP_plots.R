library(tidyverse)
library(ggplot2)
library(LaCroixColoR)
library(ggpubr)
library(cowplot)

# Load data ----
## Feature embeddings ----
data_feats <- read_csv("data/UMAP_feats.csv")

# wrangle:
data_feats <- data_feats %>%
  mutate(Reef = sub("^(.*?)_.*", "\\1", minute)) %>% 
  select(Reef, everything())

# rename reefs:
data_feats <- data_feats %>%
  mutate(Reef = case_when(
    Reef %in% c("firstreef") ~ "First Reef",
    Reef %in% c("lubaluba") ~ "Lubaluba",
    Reef %in% c("lui") ~ "Lui",
    Reef %in% c("tuare") ~ "Tuare",
    Reef %in% c("inglis") ~ "Inglis",
    Reef %in% c("ema") ~ "Emma",
    Reef %in% c("joelles") ~ "Joelles",
    Reef %in% c("kimbe") ~ "Kimbe Island",
    Reef %in% c("garove") ~ "Garove",
    Reef %in% c("narega1") ~ "Narega West",
    Reef %in% c("narega2") ~ "Narega East",
    TRUE ~ Reef  
  ))

# add habitat column:
data_feats <- data_feats %>%
  mutate(Habitat = case_when(
    Reef %in% c("First Reef", "Lubaluba", "Lui") ~ "Inshore",
    Reef %in% c("Inglis", "Emma", "Joelles") ~ "Pinnacle",
    Reef %in% c("Tuare", "Kimbe Island") ~ "Island",
    Reef %in% c("Garove", "Narega West", "Narega East") ~ "Offshore Island",
    TRUE ~ "unknown" 
  )) %>% 
  select(Habitat, Reef, everything())

rm(data)

## Indices ----
data_indices <- read_csv("data/UMAP_indices.csv")

# rename cols:
data_indices <- data_indices %>% 
  select(-c(minute)) %>% 
  rename('Reef'= habitat)

data_indices <- data_indices %>%
  mutate(Reef = case_when(
    Reef %in% c("firstreef") ~ "First Reef",
    Reef %in% c("lubaluba") ~ "Lubaluba",
    Reef %in% c("lui") ~ "Lui",
    Reef %in% c("tuare") ~ "Tuare",
    Reef %in% c("inglis") ~ "Inglis",
    Reef %in% c("ema") ~ "Emma",
    Reef %in% c("joelles") ~ "Joelles",
    Reef %in% c("kimbe") ~ "Kimbe Island",
    Reef %in% c("garove") ~ "Garove",
    Reef %in% c("narega1") ~ "Narega West",
    Reef %in% c("narega2") ~ "Narega East",
    TRUE ~ Reef  
  ))

data_indices <- data_indices %>%
  mutate(Habitat = case_when(
    Reef %in% c("First Reef", "Lubaluba", "Lui") ~ "Inshore",
    Reef %in% c("Inglis", "Emma", "Joelles") ~ "Pinnacle",
    Reef %in% c("Tuare", "Kimbe Island") ~ "Island",
    Reef %in% c("Garove", "Narega West", "Narega East") ~ "Offshore Island",
    TRUE ~ "unknown" 
  )) %>% 
  select(Habitat, Reef, everything())

# Plotting ----
# Formatting for plots
custom_theme <- theme_classic() + 
  theme(
    axis.title = element_text(size = 16),    # Axis titles
    axis.text = element_text(size = 16),     # Axis text
    legend.title = element_text(size = 16),  # Legend title
    legend.text = element_text(size = 16),   # Legend text
    aspect.ratio = 1, # make it square
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1), # add a border  
    plot.margin = unit(c(0.0, 0.0, 0.0, 0.2), "cm") # make margins equal for combining
  )

# create colour palettes:
cols_reef <- c("#FFCC99", "#740AFF", "#FFE100", "#2BCE48", "#FFA8BB", 
               "#426600", "#FF0010", "#E0FF66", "#0075DC", "#4C005C", "#C20088")

cols_hab <- c("#FFCC99", "#740AFF", "#FFE100", "#2BCE48")

## Features ----
plot_feat_reef <- data_feats %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2, col = Reef), alpha = .7, stroke = .5) +
  geom_point(size = 0.45) +
  geom_hline(yintercept = 0, linetype = 'dotted', col = 'black') +
  geom_vline(xintercept = 0, linetype = 'dotted', col = 'black') +
  scale_color_manual(values = cols_reef) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  xlab("UMAP 1")+
  ylab("UMAP 2")+
  custom_theme

plot_feat_hab <- data_feats %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2, col = Habitat), alpha = .7, stroke = .5) +
  geom_point(size = 0.45) +
  geom_hline(yintercept = 0, linetype = 'dotted', col = 'black') +
  geom_vline(xintercept = 0, linetype = 'dotted', col = 'black') +
  scale_color_manual(values = cols_hab) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  xlab("UMAP 1")+
  ylab("UMAP 2")+
  custom_theme

## Indices ----
plot_ind_reef <- data_indices %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2, col = Reef), alpha = .7, stroke = .5) +
  geom_point(size = 0.45) +
  geom_hline(yintercept = 0, linetype = 'dotted', col = 'black') +
  geom_vline(xintercept = 0, linetype = 'dotted', col = 'black') +
  scale_color_manual(values = cols_reef) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  xlab("UMAP 1")+
  ylab("UMAP 2")+
  custom_theme

plot_ind_hab <- data_indices %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2, col = Habitat), alpha = .7, stroke = .5) +
  geom_point(size = 0.45) +
  geom_hline(yintercept = 0, linetype = 'dotted', col = 'black') +
  geom_vline(xintercept = 0, linetype = 'dotted', col = 'black') +
  scale_color_manual(values = cols_hab) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  xlab("UMAP 1")+
  ylab("UMAP 2")+
  custom_theme

# adding legends
# Disable legends in individual plots
plot_feat_hab <- plot_feat_hab + theme(legend.position = "none")
plot_feat_reef <- plot_feat_reef + theme(legend.position = "none")
plot_ind_hab <- plot_ind_hab + theme(legend.position = "none")
plot_ind_reef <- plot_ind_reef + theme(legend.position = "none")

plot_feat_reef <- plot_feat_reef + theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm"))
plot_feat_hab <- plot_feat_hab + theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm"))
plot_ind_reef <- plot_ind_reef + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
plot_ind_hab <- plot_ind_hab + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Extract legends
legend_hab <- cowplot::get_legend(plot_feat_hab + theme(legend.position = "right"))
legend_reef <- cowplot::get_legend(plot_feat_reef + theme(legend.position = "right"))

# Arrange plots and legends
combined_plot_both <- cowplot::plot_grid(
  cowplot::plot_grid(plot_feat_hab, plot_ind_hab, plot_feat_reef, plot_ind_reef,
                     ncol = 2, nrow = 2, 
                     labels = c("a", "b", "c", "d"),
                     align = "hv",   # Align both horizontally and vertically
                     axis = "lrtb",  # Align axes to ensure they are the same size
                     label_size = 22),  
  cowplot::plot_grid(legend_hab, legend_reef, ncol = 1),
  rel_widths = c(3, 1)  # Adjust the relative width for the legend column
)+
  theme(plot.margin = unit(c(0, 0, 0, 1), "cm")) 

# Save the final plot
ggsave("Outputs/Fig2.png", combined_plot_both, width = 10, height = 10, dpi = 300)

