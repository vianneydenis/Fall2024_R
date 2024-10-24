library(tidyverse)
library(viridis)


### From Practice 2.3
rairuoho <- read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt', header = T, sep = "\t") 
str(rairuoho)
head(rairuoho)

rairuoho_long <- rairuoho %>%
  pivot_longer(day3:day8, names_to = "Day", values_to = "Length")
rairuoho_long

rairuoho_long$position <- paste(rairuoho_long$spatial1, rairuoho_long$spatial2, sep = "_")
rairuoho_long

rairuoho_long$row <- NULL
rairuoho_long$column <- NULL
rairuoho_long$spatial1 <- NULL
rairuoho_long$spatial2 <- NULL

print(rairuoho_long)

rairuoho_long$Day <- gsub("day", "", rairuoho_long$Day)
str(rairuoho_long)


###

ggplot(rairuoho_long, 
       aes(x = Day, 
           y = Length,
           fill = treatment,
           color = treatment)) +
  
  geom_boxplot() +
  
  scale_fill_manual(values = c("nutrient" = "#c4bd8b", "water" = "#8faab3")) +
  scale_color_manual(values = c("nutrient" = "#948c56", "water" = "#68919e")) +
  
  geom_jitter(color = "grey", size = 0.1, alpha = 1) +
  labs(title = "Growth patterns of plants enriched with nutrient",
       x = "Day",
       y = "Length (cm)") +
  
  theme(panel.background = element_rect(fill = "#f2f0eb"),
        plot.background = element_rect(fill = "white"))


###










?scale_fill_viridis

ggplot(rairuoho_long, 
       aes(x = Day,
           y = Length,
           group = treatment,
           color = treatment)) +
  geom_point() +
  facet_grid(~treatment)





+               # Points for each data point
  scale_color_manual(values = c("Nutrient" = "darkgreen", "Water" = "blue")) + # Custom colors
  labs(
    title = "Grass Growth in Nutrient vs. Water Treatments",
    x = "Time (days)",
    y = "Grass Height (cm)",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 15) +      # Clean, minimalist theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18), # Title formatting
    legend.position = "top",            # Move legend to the top
    axis.text = element_text(size = 12),# Axis text size
    axis.title = element_text(face = "bold") # Bold axis titles
  ) +
  annotate("text", x = 7, y = 9, label = "Nutrient growth overtakes", size = 5, color = "darkgreen") # Example annotation
