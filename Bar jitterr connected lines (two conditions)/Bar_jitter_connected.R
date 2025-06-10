library(ggplot2)
library(ggforce)
library(ggdist)
library(gghalves)
library(ggpubr)
library(Rmisc)
library(dplyr)
library(cowplot)
# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio, ggplot2) 

# Load the data
data <- import("C:/Users/55138703/OneDrive - MMU/Documents/Research projects/CMC Focus of attention/Force_data_for_R.csv")
head(data)
data$Condition <- factor(data$Condition)
data$Condition <- factor(data$Condition, levels = c("External", "Internal"))

# Step 1: Calculate summary statistics (mean and standard error)
data_summarys <- data %>%
  group_by(data$Condition) %>%
  summarise(
    mean_succ = mean(`Success`),
    sd_succ = sd(`Success`)
  )

# Barplot with updated color scheme and no legend
ggplot(data_summarys, aes(x=`data$Condition`, y=mean_succ)) + 
  geom_bar(stat = "identity", fill = "white", color = "black", alpha = 0.5, width = .5) +
  
  # Lines connecting participant points with pastel colors
  geom_line(data = data, 
            aes(x = Condition, y = Success, group = PPT_No, color = Suc_change),
            linewidth = 1, alpha = 0.7) +
  
  # Jittered points with larger size, transparency, and black outline
  geom_jitter(data = data, 
              aes(x = Condition, y = Success), 
              width = 0.04, height = 0, size = 5, shape = 21, fill = "white", color = "black", alpha = 0.8) +
  
  # Custom pastel colors for change
  scale_color_manual(values = c("Increase" = "blue", "Decrease" = "lightcoral")) +
  
  # Labels and theme
  labs(x = NULL, y = "Success (%)") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")  # Remove legend
