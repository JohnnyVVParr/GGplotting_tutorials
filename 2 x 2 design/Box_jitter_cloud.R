# Load relevant packages to your current session
library(ggplot2)
library(ggdist)  # For raincloud plots
library(dplyr)
library(rio)

# Load data from Excel file (YOU WILL NEED TO CHANGE THIS TO MATCH THE FILE YOU WANT TO PLOT ON YOUR DEVICE)
df <- import("C:\\Users\\55138703\\OneDrive - MMU\\Documents\\Research projects\\Balance N1\\Congruency\\Latest\\GGplotting\\N1_mean.xlsx")

# Convert categorical variables to factors
df$Size <- factor(df$Size) 
df$Condition <- factor(df$Condition)

# IMPORTANT - for my dataset, my factors are called "Size" and "Condition". For your own dataset, you will need to replace all instances of "Size" and "Condition" with your respective factor names.

# GGPLOT defaults to alphabetical order, so if you want a particular group/condition to be plotted first (i.e., left) then manually change order.
df$Condition <- factor(df$Condition, levels = c("SE", "BE"))


# Calculate means for each Size & Condition (or whatever your factors are called)
mean_df <- df %>%
  group_by(Size, Condition) %>%
  summarise(mean_COP = median(N1, na.rm = TRUE)) %>%
  mutate(Position = as.numeric(Condition) + ifelse(Size == "Big", -0.2, 0.2))  # Align with boxplots

# Create the Raincloud + Boxplot + Jitter Plot
# NOTE - "df" is the name of my Excel sheet (above). I am setting by x-axis to separate by "Condition", and setting the y-axis DV to "N1". 
# NOTE - I am also telling ggplot to separate the color of my data by their "Size" category
fig1 <- ggplot(df, aes(x = Condition, y = N1, color = Size)) +
  
  # Boxplot centered - below values can edit the size of the boxplots, whether to include outliers, and the boxplot width
  geom_boxplot(size = 1.25,outlier.shape = NA,width = 0.2, position = position_dodge(width = 0.8)) +
  
  # Jittered points shifted to the right
  geom_jitter(alpha = 0.2, size = 2,  
              position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
              aes(x = as.numeric(Condition) + ifelse(Size == "Big", -0.08, 0.32))) +
  
  # Raincloud (Halfeye density) aligned to the left
  stat_halfeye(aes(fill = Size, x = as.numeric(Condition) + ifelse(Size == "Big", -0.3, 0.1)), 
               adjust = 1.2, justification = 0, 
               .width = 0, point_colour = NA, alpha = 0.4, scale = -0.2) +
  
  # **Connecting Lines Between Mean Points**
  geom_line(data = mean_df, aes(x = Position, y = mean_COP, group = Size), 
            size = .8, linetype = "solid", alpha = .5) +  # Change color/size if needed
  # Manually set colors
  scale_color_manual(values = c("Big" = "#E64A19", "Small" = "#009688")) + 
  scale_fill_manual(values = c("Big" = "#E64A19", "Small" = "#009688"))+
  
  theme_classic() +ylab('N1 Amplitude uV')+
  ylim(-50,0)

fig1
ggsave(fig1,filename = "n1fig.tiff", path = "C:\\Users\\55138703\\OneDrive - MMU\\Documents\\Research projects\\Balance N1\\Congruency\\Latest\\GGplotting\\", type = "cairo")
