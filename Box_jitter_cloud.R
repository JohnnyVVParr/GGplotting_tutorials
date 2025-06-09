library(ggplot2)
library(ggdist)  # For raincloud plots
library(dplyr)
library(rio)

# Load data from Excel file
df <- import("C:\\Users\\55138703\\OneDrive - MMU\\Documents\\Research projects\\Balance N1\\Congruency\\Latest\\GGplotting\\N1_mean.xlsx")

# Convert categorical variables to factors
df$Size <- factor(df$Size) 
df$Condition <- factor(df$Condition)
df$Condition <- factor(df$Condition, levels = c("SE", "BE"))


# Calculate mean Peak_COP for each Size & Condition
mean_df <- df %>%
  group_by(Size, Condition) %>%
  summarise(mean_COP = median(N1, na.rm = TRUE)) %>%
  mutate(Position = as.numeric(Condition) + ifelse(Size == "Big", -0.2, 0.2))  # Align with boxplots

# Create the Raincloud + Boxplot + Jitter Plot
fig1 <- ggplot(df, aes(x = Condition, y = N1, color = Size)) +
  
  # Boxplot centered
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


# Load Data
df <- import("C:\\Users\\55138703\\OneDrive - MMU\\Documents\\Research projects\\Balance N1\\Congruency\\Latest\\GGplotting\\n1_new.xlsx")

head(df)
# Ensure Condition is a factor
df$Condition <- factor(df$Condition, levels = c("SE", "BE"))

# **Create a new merged column for coloring**
df$Condition_Size <- paste(df$Condition, df$Size, sep = "_")
# **Create a new merged column for legend labels**
df$Condition_Size <- factor(paste(df$Condition, df$Size, sep = "_"), 
                            levels = c("BE_Big", "SE_Big", "SE_Small", "BE_Small"))  # Reorder legend order

# **Manually Change Legend Labels**
legend_labels <- c(
  "BE_Big"   = "BE Big",
  "SE_Big"   = "SE Big",
  "SE_Small" = "SE Small",
  "BE_Small" = "BE Small"
)

# **Manually Define Colors**
custom_colors <- c(
  "SE_Big"   = "#E64A19",  # Lighter orange for SE Big
  "SE_Small" = "#009688",  # Lighter blue for SE Small
  "BE_Big"   = "#E64A19",  # Darker orange for BE Big
  "BE_Small" = "#009688"   # Darker blue for BE Small
)

# **Manually Define Linetypes**
custom_linetypes <- c(
  "SE_Big"   = "dotted",   # SE Big = Dashed
  "SE_Small" = "dotted",   # SE Small = Dashed
  "BE_Big"   = "solid",    # BE Big = Solid
  "BE_Small" = "solid"     # BE Small = Solid
)

# **Create the Line Plot**
fig2 <- ggplot(df, aes(x = Time, y = ERP, color = Condition_Size, alpha = Condition, linetype = Condition_Size, size = Condition)) +  
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.25, alpha = .2) + 
  
  # 🔸 Add SD shading
  geom_ribbon(aes(ymin = ERP - SD,
                  ymax = ERP + SD,
                  fill = Condition_Size,
                  group = interaction(Condition_Size, Condition)),
              alpha = 0.05,
              inherit.aes = TRUE,
              colour = NA) +
  
  geom_line(lineend = "round") +  
  scale_size_manual(values = c("SE" = 1, "BE" = 2)) +
  scale_fill_manual(values = custom_colors, guide = "none") +  # match ribbon to line color
  theme_classic2() +  
  labs(x = "Time (ms)", y = "Amplitude uV") +  
  scale_color_manual(values = custom_colors, labels = legend_labels) +  
  scale_linetype_manual(values = custom_linetypes, labels = legend_labels) +
  scale_alpha_manual(values = c("SE" = 0.9, "BE" = .3)) +  
  theme(legend.position = "right", text = element_text(size = 11),
        legend.background = element_rect(fill = "white", 
                                         color = "black", 
                                         linewidth = .5, 
                                         linetype = "solid")) +  
  guides(linetype = guide_legend(title = NULL), 
         color = guide_legend(title = NULL), 
         alpha = "none", size = "none") +
  xlim(-75, 400) + ylim(-36, 15)
fig2
ggsave(fig2,filename = "n1trace.tiff", path = "C:\\Users\\55138703\\OneDrive - MMU\\Documents\\Research projects\\Balance N1\\Congruency\\Latest\\GGplotting\\", dpi = 300, type = "cairo")
figcomb <- ggarrange(fig1,fig2)
figcomb
ggsave(figcomb,filename = "N1combo.tiff", path = "C:\\Users\\55138703\\OneDrive - MMU\\Documents\\Research projects\\Balance N1\\Congruency\\Latest\\GGplotting\\", dpi = 300, type = "cairo")

