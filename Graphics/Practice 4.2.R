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

plot <- ggplot(rairuoho_long, 
       aes(x = Day, 
           y = Length,
           fill = treatment,
           color = treatment)) +
  
  geom_boxplot() +
  
  scale_fill_manual(values = c("nutrient" = "#c4bd8b", "water" = "#8faab3")) +
  scale_color_manual(values = c("nutrient" = "#948c56", "water" = "#68919e")) +
  
  geom_jitter(color = "grey", size = 0.1, alpha = 1) +
  labs(title = "Growth patterns of plants enriched with nutrient vs. water",
       x = "Day",
       y = "Length (cm)") +
  
  theme(panel.background = element_rect(fill = "#f2f0eb",
                                        colour = "grey",
                                        size = 0.3),
        plot.background = element_rect(fill = "white"),
        legend.position = c(0.9, 0.1),
        legend.background = element_rect(fill = "#f2f0eb", 
                                         size = 0.3, 
                                         linetype = "solid",
                                         colour ="grey"))


###
setwd("/Users/riversung/NTU/113-1 Fall 2024/Ocean5098 R/Rclass_Fall2024/Graphics")
ggsave(plot, filename = "Practice 4.2.pdf")
