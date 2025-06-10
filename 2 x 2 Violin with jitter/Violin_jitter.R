# Load necessary libraries
library(datasets)
library(ggplot2)
library(ggforce)
library(ggdist)
library(gghalves)
library(ggpubr)
library(Rmisc)
library(dplyr)
library(cowplot)
pacman::p_load(pacman, rio, ggplot2)

# Load data from Excel file
Couple <- import("C:\\Users\\55138703\\OneDrive - MMU\\Documents\\Research projects\\DCD coupling\\GGplotting.csv")
head(Couple)

# Convert 'Group' column to factor with specified levels
Couple$Group <- factor(Couple$Group, levels = c("TD", "DCD"))

# Create the violin plot with black jitter points
ggplot(Couple, aes(x = Group, y = Term_reaching, fill = Congruency)) +
  geom_violin(trim = FALSE, position = position_dodge(width = 0.8)) +
  geom_jitter(color = "black", position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1, alpha = 0.6) +
  scale_fill_manual(values = c("#FFA500", "#008080")) +  # Hex codes for orange and teal
  labs(title = NULL,
       x = NULL,
       y = "Terminal Reach (ms)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
