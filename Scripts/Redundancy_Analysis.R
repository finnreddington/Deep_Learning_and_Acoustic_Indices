# RDA script ----

# Wrangling and setup----
## Load packages and data ----
library(tidyverse)
library(vegan)
library(corrplot)
library(car)
library(mvabund)
library(scales)
library(ggvegan)
library(ggrepel)
library(ggExtra)
library(ggfortify)
library(ggplot2)
library(GGally)
library(glmmTMB)
library(gllvm)
library(ape)
library(readxl)
library(patchwork)
library(ggpubr)
library(packfor)
library(gridExtra)

# Load data
# Load the DL features that were calulated in Python ('Kimbe_features.csv'):
# For final submission:
# data_feats <- read_csv("C:/Reef soundscapes with AI/Results/PCNN_features/Kimbe_features.csv") 
# For working:
load("Data/features.Rdata")

## Wrangle feats ----
DL_feat <- 
  features %>% rename(feat = `...1`) %>% 
  pivot_longer(cols = all_of(colnames(features)[-1]), names_to = "Filename", values_to = "val") %>% 
  #  separate_wider_delim(Filename, delim = "_", too_many = "merge", names = c("Filename", "x"), cols_remove = TRUE) %>% # split to delete extra timestamp info 
  # might need in final code but not for .RData object ^^
  #select(-x) %>% 
  pivot_wider(names_from = feat, values_from = val) 

# Format Filename to match indices:
#DL_feat$Filename <- gsub("2304", "", DL_feat$Filename) # remove the year
#DL_feat$Filename <- gsub("_(\\d{2})(\\d{2})\\d{2}$", "_\\1_\\2", DL_feat$Filename) # format as 'MM_hh_mm'

#Check for NAs:
DL_feat <- na.omit(DL_feat)
rm(features)

## Wrangle indices data_inds ----
# Load the indices from the output folder ('Kimbe_indices.csv')
# data_inds  <- read_csv("C:/Reef soundscapes with AI/Results/PCNN_features/Kimbe_indices.csv") #for final code
data_inds <- read_csv("C:/Users/fr15610/OneDrive - University of Bristol/Desktop/soundscape_AI/Reef soundscapes with AI/Results/PCNN_features/data/compound_index.csv")

data_inds <- data_inds %>% 
  rename('Filename' = minute) %>% # rename column
  select(-1) # remove row number col

data_inds <- data_inds %>%
  mutate(reef = sub("^(.*?)_.*", "\\1", Filename)) %>% 
  select(-c(fish_Hf, shrimp_Hf)) %>% # remove Hf as correlated w/ H
  select(Filename, reef, everything())

# remove st dev cols:
cols_to_remove <- grep("_std", names(data_inds)) 
data_inds <- data_inds[-cols_to_remove]

# remove full spectrum cols:
cols_to_remove <- grep("full_", names(data_inds))
data_inds <- data_inds[-cols_to_remove]

# Rename cols to be less suggestive:
data_inds <- data_inds %>% # 'fish' becomes 'low_freq'
  rename_with(~ gsub("^fish", "low_freq", .x), starts_with("fish")) 
data_inds <- data_inds %>% # 'shrimp' becomes 'high_freq'
  rename_with(~ gsub("^shrimp", "high_freq", .x), starts_with("shrimp"))

# Reformat Filename col:
data_inds$Filename <- gsub("\\.WAV$", "", data_inds$Filename, ignore.case = TRUE) # remove the '.WAV' suffix
data_inds$Filename <- gsub("2304", "", data_inds$Filename) # remove the year
data_inds$Filename <- gsub("_(\\d{2})(\\d{2})\\d{2}$", "_\\1_\\2", data_inds$Filename) # format as 'MM_hh_mm'

# Check for NAs:
data_inds <- na.omit(data_inds)

# Split time variable:
data_inds <- data_inds %>%
  extract(Filename, into = c("date", "hour", "minute"), 
          regex = ".*_(\\d{2})_(\\d{2})_(\\d{2})", remove = FALSE) %>%
  mutate(datetime = paste(date, hour, minute, sep = "_"),
         date = as.integer(date), 
         hour = as.integer(hour),
         minute = as.integer(minute)) 

# create a 'diurnal'col
data_inds <- data_inds %>%
  mutate(hour = as.integer(hour), # Ensure 'hour' is an integer for comparison
         diurnal = ifelse(hour >= 6 & hour < 18, "day", "night"))

# Rename the reefs so legend looks good:
data_inds <- data_inds %>%
  rename("Reef" = reef) %>% 
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
    TRUE ~ Reef  # In case there are any other values not covered above
  ))

# Add habitat col:
data_inds <- data_inds %>%
  mutate(Habitat = case_when(
    Reef %in% c("First Reef", "Lubaluba", "Lui") ~ "Inshore",
    Reef %in% c("Inglis", "Emma", "Joelles") ~ "Pinnacle",
    Reef %in% c("Tuare", "Kimbe Island") ~ "Island",
    Reef %in% c("Garove", "Narega West", "Narega East") ~ "Offshore Island",
    TRUE ~ "unknown" 
  ))

# Order and remove redundant cols:
data_inds <- data_inds %>% 
  select(-c(hour, minute)) %>% 
  select(Filename, Reef, Habitat, datetime, date, diurnal, everything()) 

# Prepare data for RDA ----
# get metadata, left-join ensures its the same order
metadata <- DL_feat %>% select(Filename) %>% 
  left_join(data_inds, by = "Filename")

# Check for NAs
metadata <- na.omit(metadata)

DL_feat_filtered <- inner_join(DL_feat, data_inds, by="Filename", keep = FALSE)
DL_feat_filtered <- DL_feat_filtered %>% 
  select(names(DL_feat))
DL_feat <- DL_feat_filtered

# Make sure they match:
identical(metadata[['Filename']], DL_feat[['Filename']])# check they're the same

#remove from env to avoid confusion
rm(data_inds);rm(DL_feat_filtered)

# We now have two data sets that match up: 
# metadata - contains time/place info and acoustic indices
# DL_feat - contains the deep-learning features

## Sub sampling ----
# reduce autocorrelation and simplify data
metadata_sub <- metadata %>% 
  filter(date >= 22 & date <= 26) %>% 
  filter(row_number() %% 5 == 1)# Take every 5th row

DL_sub <- DL_feat %>% 
  filter(Filename %in% metadata_sub$Filename) 

# Subset 'day' and 'night' to explore seperately
metadata_sub_day <- metadata_sub %>% 
  filter(diurnal == "day")

DL_sub_day <- DL_feat %>% 
  filter(Filename %in% metadata_sub_day$Filename) 

metadata_sub_night <- metadata_sub %>% 
  filter(diurnal == "night")

DL_sub_night <- DL_feat %>% 
  filter(Filename %in% metadata_sub_night$Filename)

rm(metadata); rm(DL_feat)

## center Y and standardize X ----
# scale Y vars:
DL_cent <- DL_sub %>% # make a centered but not scaled version
  mutate(across(-c(1), ~ {
    scaled_vector <- as.vector(scale(., scale = FALSE))
    return(scaled_vector)
  }, .names = "{col}"))  %>% 
  column_to_rownames(var = "Filename")

DL_cent_day <- DL_sub_day %>% # make a centered but not scaled version
  mutate(across(-c(1), ~ {
    scaled_vector <- as.vector(scale(., scale = FALSE))
    return(scaled_vector)
  }, .names = "{col}"))  %>% 
  column_to_rownames(var = "Filename")

DL_cent_night <- DL_sub_night %>% # make a centered but not scaled version
  mutate(across(-c(1), ~ {
    scaled_vector <- as.vector(scale(., scale = FALSE))
    return(scaled_vector)
  }, .names = "{col}"))  %>% 
  column_to_rownames(var = "Filename")

# centre and scale acoustic_indices
metadata_sd <- metadata_sub %>% 
  select(-c(Reef:diurnal)) %>% 
  column_to_rownames(var = "Filename") %>% 
  mutate(across(everything(), ~ {
    scaled_vector <- as.vector(scale(., scale = TRUE))
    return(scaled_vector)
  }, .names = "{col}"))

metadata_sd_day <- metadata_sub_day %>% 
  select(-c(Reef:diurnal)) %>% 
  column_to_rownames(var = "Filename") %>% 
  mutate(across(everything(), ~ {
    scaled_vector <- as.vector(scale(., scale = TRUE))
    return(scaled_vector)
  }, .names = "{col}"))

metadata_sd_night <- metadata_sub_night %>% 
  select(-c(Reef:diurnal)) %>% 
  column_to_rownames(var = "Filename") %>% 
  mutate(across(everything(), ~ {
    scaled_vector <- as.vector(scale(., scale = TRUE))
    return(scaled_vector)
  }, .names = "{col}"))

rm(DL_sub); rm(DL_sub_day); rm(DL_sub_night)

# Checking assumptions ----
## collinarity in X vars ----
cor.table = cor(metadata_sd) # defo some highly correlated vars
corrplot(cor.table)

# ADI and BI are correlated, NDSI is correlated. So remove ADI and NDSI
metadata_sd <- metadata_sd %>% select(-c(low_freq_ADI, high_freq_ADI))
metadata_sd <- metadata_sd %>% select(-c(NDSI))
cor.table = cor(metadata_sd) # defo some highly correlated vars
corrplot(cor.table) 

# high_freq_M is still correlated highly
metadata_sd <- metadata_sd %>% select(-c(high_freq_M))
cor.table = cor(metadata_sd) 
corrplot(cor.table) # looks pretty good now, but check VIF

# RDA ----
## All times ----
rda_full <- rda(DL_cent ~ low_freq_ACI + low_freq_H + low_freq_M +
                  low_freq_BI + high_freq_ACI + high_freq_BI + high_freq_H,
                data=metadata_sd, 
                scale=FALSE,
                na.action=na.omit)

RsquareAdj(rda_full)
summary(rda_full)
vif.cca(rda_full) 

# Extract biplot scores for table
biplot_df_full <- as.data.frame(scores(rda_full, display = "bp")) %>% 
  rownames_to_column(var='Index')%>% 
  arrange(RDA1)

# convert the loading scores to a percentage contribution to the RDA:
make_positive <- function(x) {
  ifelse(x < 0, -x, x)
}

# RDA1
biplot_df_full <- 
  biplot_df_full %>% 
  mutate(pos1 = make_positive(RDA1)) %>% # make all values positive
  mutate(sqrt1 = sqrt(pos1)) %>%  # take the square root, divide by sum and x by 100
  mutate(perc1 = round((sqrt1/sum(sqrt1)*100))) %>% 
  mutate(pos2 = make_positive(RDA2)) %>% # same for RDA2
  mutate(sqrt2 = sqrt(pos2)) %>%  
  mutate(perc2 = round((sqrt2/sum(sqrt2)*100))) %>% 
  select(Index, RDA1, perc1, RDA2, perc2)

### Plotting ----
rda_scores_full <- rda_full %>%
  fortify()

rda_data_full <- rda_scores_full %>% filter(score=='sites') %>% 
  rename(Filename = label) %>% 
  left_join(metadata_sub, by = "Filename") 

species_full <- rda_scores_full %>% filter(score=='species')
eig_full <- eigenvals(rda_full)
biplot_scores_full <- as.data.frame(rda_full$CCA$biplot)
colnames(biplot_scores_full) <- c("RDA1", "RDA2")

# select colours
cols_reef <- c("#FFCC99", "#740AFF", "#FFE100", "#2BCE48", "#FFA8BB", 
               "#426600", "#FF0010", "#E0FF66", "#0075DC", "#4C005C", "#C20088")

# Set some plot parameters:
custom_theme <- theme_classic() + 
  theme(
    axis.title = element_text(size = 16),    # Axis titles
    axis.text = element_text(size = 16),     # Axis text
    legend.title = element_text(size = 16),  # Legend title
    legend.text = element_text(size = 16),   # Legend text
    aspect.ratio = 1, # make it square
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1), # add a border  
  )

# Create ggplot
RDA_plot <- ggplot() +
  geom_point(data = rda_data_full, 
             aes(x = RDA1, y = RDA2, col = Reef), 
             alpha=0.4, 
             stroke = 0, 
             shape = 16,
             size = 2
  )+
  scale_colour_manual(values = cols_reef) +
  guides(colour = guide_legend(override.aes = list(size = 5, alpha = 0.8))) +
  
  geom_segment(data = biplot_df_full, aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.02, "npc")), color = "black", linewidth=.7) +
  
  #geom_segment(data = biplot_scores_full, aes(x = 0, y = 0, xend = -RDA1/2, yend = RDA2/2),
  #            arrow = arrow(length = unit(0.02, "npc")), color = "black", linewidth=.7) +
  
  
  geom_text_repel(data = biplot_df_full, aes(x = RDA1, y = RDA2, label = Index),
                  size = 5,  # Increase text size
                  point.padding = 0.8,  # Increase point padding
                  segment.color = 'black',
                  force = 1,
  )  +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_y_continuous(paste("RDA2", sprintf('(%0.1f%% explained var.)', eig_full[2] / sum(eig_full) * 100)),
                     breaks = seq(-5, 1, by=0.1)) +
  scale_x_continuous(paste("RDA1", sprintf('(%0.1f%% explained var.)', eig_full[1] / sum(eig_full) * 100)),
                     breaks = seq(-5, 1, by=0.1)) +
  custom_theme

RDA_plot

ggsave("outputs/Fig3.png", RDA_plot, width = 9, height = 9, dpi = 300)



## Day ----
rda_day <- rda(DL_cent_day ~ low_freq_ACI + low_freq_H + low_freq_M +
                  low_freq_BI + high_freq_ACI + high_freq_BI + high_freq_H,
                data=metadata_sd_day, 
                scale=FALSE,
                na.action=na.omit)

RsquareAdj(rda_day)
summary(rda_day)
vif.cca(rda_day) 

# Extract biplot scores for table
biplot_df_day <- as.data.frame(scores(rda_day, display = "bp")) %>% 
  rownames_to_column(var='Index') %>% 
  arrange(RDA1)

### Plotting ----
rda_scores_day <- rda_day %>%
  fortify()

rda_data_day <- rda_scores_day %>% filter(score=='sites') %>% 
  rename(Filename = label) %>% 
  left_join(metadata_sub_day, by = "Filename") 

species_day <- rda_scores_day %>% filter(score=='species')
eig_day <- eigenvals(rda_day)
biplot_scores_day <- as.data.frame(rda_day$CCA$biplot)
colnames(biplot_scores_day) <- c("RDA1", "RDA2")

# Create ggplot
p_day <- ggplot() +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  geom_point(data = rda_data_day, aes(x = RDA1, y = RDA2, color = Reef), alpha = 0.6) +
  geom_segment(data = biplot_scores_day, aes(x = 0, y = 0, xend = RDA1/2, yend = RDA2/2),
               arrow = arrow(length = unit(0.02, "npc")), color = "black", linewidth=0.7) +
  geom_text_repel(data = biplot_scores_day, aes(x = RDA1/2, y = RDA2/2, label = row.names(biplot_scores_day)),
                  size = 5,  # Increase text size
                  box.padding = 0.4,  # Increase box padding
                  point.padding = 0.8,  # Increase point padding
                  segment.color = 'grey50')+
  scale_y_continuous(paste("RDA2", sprintf('(%0.1f%%)', eig_day[2] / sum(eig_day) * 100))) +
  scale_x_continuous(paste("RDA1", sprintf('(%0.1f%% explained var.)', eig_day[1] / sum(eig_day) * 100))) +
  theme_bw(base_size = 14)+
 ggtitle("(b) Day")+
  theme(plot.title = element_text(face="bold"))

## Night ----
rda_night <- rda(DL_cent_night ~ low_freq_ACI + low_freq_H + low_freq_M +
                 low_freq_BI + high_freq_ACI + high_freq_BI + high_freq_H,
               data=metadata_sd_night, 
               scale=FALSE,
               na.action=na.omit)

RsquareAdj(rda_night)
summary(rda_night)
vif.cca(rda_night) 

biplot_df_night <- as.data.frame(scores(rda_night, display = "bp")) %>% 
  rownames_to_column(var='Index') %>% 
  arrange(RDA1)

### Plotting ----
rda_scores_night <- rda_night %>%
  fortify()

rda_data_night <- rda_scores_night %>% filter(score=='sites') %>% 
  rename(Filename = label) %>% 
  left_join(metadata_sub_night, by = "Filename") 

species_night <- rda_scores_night %>% filter(score=='species')
eig_night <- eigenvals(rda_night)
biplot_scores_night <- as.data.frame(rda_night$CCA$biplot)
colnames(biplot_scores_night) <- c("RDA1", "RDA2")

# Create ggplot
p_night <- ggplot() +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  geom_point(data = rda_data_night, aes(x = -RDA1, y = RDA2, color = Reef), alpha = 0.5) +
  geom_segment(data = biplot_scores_night, aes(x = 0, y = 0, xend = -RDA1/2, yend = RDA2/2),
               arrow = arrow(length = unit(0.02, "npc")), color = "black") +
  geom_text_repel(data = biplot_scores_night, aes(x = -(RDA1/2), y = RDA2/2, label = row.names(biplot_scores_night)),
                  box.padding = 0.35, point.padding = 0.5,
                  segment.color = 'grey50') +
  theme(legend.position = "none",
        legend.background = element_rect(fill = "white", color = "white"))+
  scale_y_continuous(paste("RDA2", sprintf('(%0.1f%%)', eig_night[2] / sum(eig_night) * 100))) +
  scale_x_continuous(paste("RDA1", sprintf('(%0.1f%% explained var.)', eig_night[1] / sum(eig_night) * 100))) +
  theme_bw(base_size = 14)+
  ggtitle("(c) Night")+
  theme(plot.title = element_text(face="bold"))


# checking for linear X~Y relationships ----
plots <- list() # List to store plots
# Loop over the predictor columns:
for (i in 15:22) {
  predictor_name <- names(rda_data_full)[i]  # Get the name of the current predictor
  plot_title <- paste("Scatter Plot of RDA1 vs", predictor_name)
  
  # Create the plot using tidy evaluation
  p <- ggplot(rda_data_full, aes(x = !!sym(predictor_name), y = !!sym("RDA1"))) +
    geom_point(alpha = 0.4, color="black") +  # Points with some transparency
    geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Linear regression line without confidence band
    labs(title = plot_title, x = predictor_name, y = "RDA1 Score")
  
  # Add the plot to the list
  plots[[length(plots) + 1]] <- p
}

# plot 1
if (length(plots) > 0) print(plots[[2]])

# plot all
grid.arrange(grobs = plots, ncol = 3) # variables look mostely linear


# model selection ----
## Forward selection of variables ----
# all times:
forward_sel_all <- forward.sel(DL_cent, metadata_sd, nperm=9999)# all are significant
# Day
forward_sel_day <- forward.sel(DL_cent_day, metadata_sd_day, nperm=9999)# all are significant
# Night
forward_sel_night <-forward.sel(DL_cent_night, metadata_sd_night, nperm=9999)# all are significant

# Hypothesis testing ----
# permutation tests for each model
# All times, full model:
anova_full <- anova.cca(rda_full, permutations = how(nperm = 9999))
# All times, X variables:
anova_vars_full <- anova.cca(rda_full, by = "terms", permutations = how(nperm = 99999))

# Day time, full model:
anova_day <-anova.cca(rda_day, permutations = how(nperm = 9999))
# day time, X variables:
anova_vars_day <- anova.cca(rda_day, by = "terms", permutations = how(nperm = 9999))

# night time, full model:
anova_night <- anova.cca(rda_night, permutations = how(nperm = 9999))
# night time, X variables:
anova_vars_night <- anova.cca(rda_night, by = "terms", permutations = how(nperm = 9999))

# All models and included variables were significant.


# Tables ----
## All times
x <- anova_vars_full %>% slice_head(n=7) 
x$p_value <- as.character(replicate(7, "<0.001"))
x <- x %>% select(-c(Variance, `Pr(>F)` ))
x <- x %>% rownames_to_column(var = "Index")

y <- inner_join(biplot_df_full,x, by="Index")

y$RDA1 <- round(y$RDA1, 2)
y$RDA2 <- round(y$RDA2, 2)
y$F <- round(y$F, 0)

library(knitr)
library(kableExtra)

kable(y, align = "c") %>%
  kable_styling(full_width = FALSE, position = "left") %>% 
  save_kable("RDA_table1.pdf")

## Daytime
x <- anova_vars_day %>% slice_head(n=7) 
x$p_value <- as.character(replicate(7, "<0.001"))
x <- x %>% select(-c(Variance, `Pr(>F)` ))
x <- x %>% rownames_to_column(var = "Index")

y <- inner_join(biplot_df_day,x, by="Index")

y$RDA1 <- round(y$RDA1, 2)
y$RDA2 <- round(y$RDA2, 2)
y$F <- round(y$F, 0)

kable(y, align = "c") %>%
  kable_styling(full_width = FALSE, position = "left") %>% 
  save_kable("RDA_table2.pdf")

## Nighttime
x <- anova_vars_night %>% slice_head(n=7) 
x$p_value <- as.character(replicate(7, "<0.001"))
x <- x %>% select(-c(Variance, `Pr(>F)` ))
x <- x %>% rownames_to_column(var = "Index")

y <- inner_join(biplot_df_night,x, by="Index")

y$RDA1 <- round(y$RDA1, 2)
y$RDA2 <- round(y$RDA2, 2)
y$F <- round(y$F, 0)

kable(y, align = "c") %>%
  kable_styling(full_width = FALSE, position = "left") %>% 
  save_kable("RDA_table3.pdf")
