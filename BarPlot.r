# Read the file as raw data
df <- read.csv("Age_onMale.csv", header = FALSE, skip = 2, stringsAsFactors = FALSE)

# Name the columns manually
names(df) <- c("label", "sample", "young", "old")

# Keep only the columns we need
df <- df[, c("sample", "young", "old")]

# Make sure the values are numeric
df$young <- as.numeric(df$young)
df$old <- as.numeric(df$old)

# Plot side-by-side bars
library(ggplot2)
library(tidyr)

df_long <- pivot_longer(
  df,
  cols = c("young", "old"),
  names_to = "group",
  values_to = "value"
)

p <- ggplot(df_long, aes(x = sample, y = value, fill = group)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_text(
    aes(label = sprintf("%.3f", value)),
    position = position_dodge(width = 0.8),
    vjust = ifelse(df_long$group == "young", -0.4, -0.8),
    size = 2.5,
    check_overlap = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "Effect of Age on Male Mice H3K23pr/total H4",
    x = "Sample",
    y = bquote("H3K23pr/total H4"^"†"),
    fill = "Group",
    caption = "† normalised to average of each group"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(face = "italic", hjust = 0.5, size = 10)
  )

print(p)
# Ensure figures directory exists and save plot
dir.create("figures", recursive = TRUE, showWarnings = FALSE)
ggsave(filename = "figures/age_on_male_barplot.png", plot = p, width = 8, height = 6, dpi = 150)