# Install required packages if not already installed
install.packages(c("ggplot2", "ggdist", "dplyr", "readr", "cowplot"))

# Load libraries
library(ggplot2)
library(ggdist)  # For half-raincloud plots
library(dplyr)    # For data manipulation
library(readr)    # For reading CSV files
library(cowplot)  # For better theme control
library(ggpubr)

# Load dataset
df <- read_csv("C:/Users/55138703/OneDrive - MMU/Documents/Research projects/DCD intervention grant/Force_data.csv")

head(df)
# Ensure Group is a factor
df$Group <- factor(df$Group, levels = c("TD", "DCD"))

# Create the half-raincloud plot
acc <- ggplot(df, aes(x = Group, y = Acc_Av, fill = Group)) +
  # Half-violin (true raincloud plot)
  stat_halfeye(
    adjust = 0.4, # Smoothness
    justification = -0.02, # Moves it to the left
    .width = 0, # Show full distribution
    point_colour = NA, # Remove median point
    slab_alpha = 0.5, scale = .6) +
  
  # Jittered raw data points
  geom_jitter(aes(x = as.numeric(Group) - 0.1, colour = Group), 
              width = 0.05, alpha = 0.5, size = 3) +
  
  # Mean (circle) and error bars (SE)
  stat_summary(fun.data = mean_sd, geom = "errorbar", 
               width = 0.05, color = "black", size = 1.5) +
  stat_summary(fun = mean, geom = "point", 
               shape = 21, size = 6, color = "black", fill = "white",stroke = 1.5) +
  
  # Aesthetics
  theme_classic2() +
  labs(title = NULL,
       x = "Group",
       y = "Force Accuracy (%)") +
  theme(legend.position = "none",
        text = element_text(size = 16)) + xlab(NULL)+
  
  # Adjust x-axis to keep groups centered
  scale_x_discrete(labels = c("TD", "DCD"))+ylim(0,100)+
  annotate("text", x = 1.5, y = 90, label = "*", size = 10)


# Create the half-raincloud plot
cov <- ggplot(df, aes(x = Group, y = CoV_Av, fill = Group)) +
  # Half-violin (true raincloud plot)
  stat_halfeye(
    adjust = 0.4, # Smoothness
    justification = -0.02, # Moves it to the left
    .width = 0, # Show full distribution
    point_colour = NA, # Remove median point
    slab_alpha = 0.5, scale = .6) +
  
  # Jittered raw data points
  geom_jitter(aes(x = as.numeric(Group) - 0.1, colour = Group), 
              width = 0.05, alpha = 0.5, size = 3) +
  
  # Mean (circle) and error bars (SE)
  stat_summary(fun.data = mean_sd, geom = "errorbar", 
               width = 0.05, color = "black", size = 1.5) +
  stat_summary(fun = mean, geom = "point", 
               shape = 21, size = 6, color = "black", fill = "white",stroke = 1.5) +
  
  # Aesthetics
  theme_classic2() +
  labs(title = NULL,
       x = "Group",
       y = "Force variability (CoV%)") +
  theme(legend.position = "none",
        text = element_text(size = 16)) + xlab(NULL)+
  
  # Adjust x-axis to keep groups centered
  scale_x_discrete(labels = c("TD", "DCD"))+ylim(0,40)+
  annotate("text", x = 1.5, y = 30, label = "**", size = 10)


both <- ggarrange(acc,cov)
both

ggsave(both,filename = "force.png", path = "C:\\Users\\55138703\\OneDrive - MMU\\Documents\\Research projects\\DCD intervention grant\\", type = "cairo", dpi = 300)


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Create the data manually
mean_data <- data.frame(
  Group = c("TD", "DCD"),
  Theta = c(0.0094, 0.0126),
  Alpha = c(0.0265, 0.0292),
  Beta = c(0.3338, 0.3451),
  Gamma = c(0.512, 0.4459)
)

sd_data <- data.frame(
  Group = c("TD", "DCD"),
  Theta = c(0.00447, 0.00618),
  Alpha = c(0.00906, 0.01327),
  Beta = c(0.03162, 0.05987),
  Gamma = c(0.048, 0.0818)
)

# Reshape to long format for ggplot
df_plot <- mean_data %>%
  pivot_longer(cols = -Group, names_to = "Band", values_to = "Mean") %>%
  left_join(
    sd_data %>%
      pivot_longer(cols = -Group, names_to = "Band", values_to = "SD"),
    by = c("Group", "Band")
  )
# Set factor levels for proper ordering
df_plot$Band <- factor(df_plot$Band, levels = c("Theta", "Alpha", "Beta", "Gamma"))
df_plot$Group <- factor(df_plot$Group, levels = c("TD", "DCD"))


# Plot
emgplot <- ggplot(df_plot, aes(x = Band, y = Mean, fill = Group)) +
  geom_bar(colour = "black",position = position_dodge(width = 0.8), stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.8),
                width = 0.2, size = 0.7) +
  theme_classic(base_size = 16) +
  labs(x = "Frequency Band", y = "EMG PSD", fill = "Group")+

annotate("text",
         x = 4,  # Gamma is the 4th level on the x-axis
         y = max(df_plot$Mean[df_plot$Band == "Gamma"]) + 0.08,
         label = "*",
         size = 10)

emgplot
ggsave(emgplot,filename = "emgplot.png", path = "C:\\Users\\55138703\\OneDrive - MMU\\Documents\\Research projects\\DCD intervention grant\\", type = "cairo", dpi = 300)
