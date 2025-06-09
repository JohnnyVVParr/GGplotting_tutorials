# ========================================
# ggplot2 Half-Raincloud Plot Tutorial
# ========================================

# 🔧 Step 1: Install required packages (only once)
# You can skip this step if you've already installed these packages
install.packages(c("ggplot2", "ggdist", "dplyr", "readr", "cowplot", "ggpubr"))

# 📚 Step 2: Load the libraries
library(ggplot2)   # For plotting
library(ggdist)    # For half-eye (raincloud) plots
library(dplyr)     # For data manipulation
library(readr)     # For reading in CSV files
library(cowplot)   # For enhanced themes
library(ggpubr)    # For easy statistical summaries

# 📁 Step 3: Load your dataset
# Replace this file path with your own if needed
df <- read_csv("C:/Users/55138703/OneDrive - MMU/Documents/Research projects/DCD intervention grant/Force_data.csv")

# 👀 Step 4: Preview the first few rows of data
head(df)

# 🧼 Step 5: Clean up your data
# Make sure 'Group' is treated as a factor and ordered
df$Group <- factor(df$Group, levels = c("TD", "DCD"))

# 📊 Step 6: Create the half-raincloud plot
acc <- ggplot(df, aes(x = Group, y = Acc_Av, fill = Group)) +
  
  # 🌧️ Half-violin (raincloud) to show distribution
  stat_halfeye(
    adjust = 0.4,          # Controls the smoothness of the curve
    justification = -0.02, # Nudges it left to create the 'half'
    .width = 0,            # Show full distribution, not intervals
    point_colour = NA,     # No median dot
    slab_alpha = 0.5,      # Transparency of the shape
    scale = 0.6            # Shrinks the height a bit
  ) +
  
  # 🔵 Add jittered raw data points
  geom_jitter(
    aes(x = as.numeric(Group) - 0.1, colour = Group), # Slightly nudged for better separation
    width = 0.05,
    alpha = 0.5,
    size = 3
  ) +
  
  # 📏 Add error bars (SD) and means
  stat_summary(
    fun.data = mean_sd,
    geom = "errorbar",
    width = 0.05,
    color = "black",
    size = 1.5
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    size = 6,
    color = "black",
    fill = "white",
    stroke = 1.5
  ) +
  
  # 🎨 Theme and labels
  theme_classic2() +
  labs(
    x = NULL,
    y = "Force Accuracy (%)"
  ) +
  theme(
    text = element_text(size = 16),
    legend.position = "none"
  ) +
  
  # 🧭 Customize x-axis and y-axis
  scale_x_discrete(labels = c("TD", "DCD")) +
  ylim(0, 100) +
  
  # ✴️ Add a significance asterisk
  annotate("text", x = 1.5, y = 90, label = "*", size = 10)

# 🖥️ Step 7: Display the plot in your R window
acc

# 💾 Step 8: Save the plot to your computer
ggsave(
  plot = acc,
  filename = "force.png",
  path = "C:/Users/55138703/OneDrive - MMU/Documents/Research projects/DCD intervention grant/",
  type = "cairo",  # High-quality rendering
  dpi = 300        # Good resolution for publication
)
