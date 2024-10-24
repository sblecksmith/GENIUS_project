# title: genius_fig_1
# author: Sarah Blecksmith
# purpose: make plot of Chao1 diversity in subjects

library(ggplot2)
library(dplyr)

diversity <- read.csv("data/genius_cazyme_families_rounded_diversity.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(subject_id = paste0("s", subject_id),
         genius_id = case_when(subject_id == "s3" ~ "1",
                               subject_id == "s7" ~ "2",
                               subject_id == "s10" ~ "3",
                               subject_id == "s16" ~ "4",
                               subject_id == "s19" ~ "5",
                               .default = ""))
  

fiber_monos <- read.csv("data/fiber_monosaccharides.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

# calculate number of subjects above 1 standard deviation for Chao1
sum(diversity$Chao1 - mean(diversity$Chao1) >= sd(diversity$Chao1)) #3

# calculate number of subjects below 1 standard deviation for Chao1
sum(diversity$Chao1 - mean(diversity$Chao1) <= -sd(diversity$Chao1)) #3

# Figure 1A
# Chao1
F1A <- ggplot(diversity, aes(x=reorder(subject_id, Chao1), y=Chao1)) +
  geom_bar(stat = "identity", fill = "#3C5488FF") +
  geom_text(aes(label = genius_id, y=50), size = 8, color = "#E64B35FF") +
  labs(title = "Plant unique CAZyme diversity Chao1",
       y = "Chao1",
       tag = "A") + 
  xlab("Participants") +
  theme(axis.ticks.x=element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

F1A

# Figure 1B
fiber_monos_long <- fiber_monos %>% select(-c(Allose, GlcNAc, GlcA, GalNAc)) %>% 
  pivot_longer(cols = Glucose:Ribose, names_to = "monosaccharide", values_to = "percentage")
fiber_monos_long$monosaccharide <- factor(fiber_monos_long$monosaccharide, levels = c("Glucose", "Galactose", "Fructose", "Xylose", "Arabinose", "Fucose", "Rhamnose", "GalA", "Mannose", "Ribose"))
fiber_monos_long$Treat <- factor(fiber_monos_long$Treat, levels = c("MSPreBio", "SunFiber", "CocoFlour", "13Bean", "Flax", "Kale", "Banana"))
F1B <- ggplot(fiber_monos_long, aes(fill=monosaccharide, y=percentage, x=Treat)) + 
  geom_bar(position = position_fill(reverse = TRUE), stat="identity") +
  scale_fill_npg() +
  labs(title = "Composition of Fibers", tag = "B") +
  theme(axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=18),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_blank())



F1 <- grid.arrange(F1A, F1B, clip="off", ncol = 2)
ggsave("figure_1.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", F1, bg = "white")