# title: genius_fig_2
# author: Sarah Blecksmith
# purpose: Make plots of pH 


library(tidyverse)
library(data.table)
library(ggpubr)
library(ggsci)


pH <- read.csv("data/pH.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5","Media & fiber", "Media only"),
    labels=c("1", "2", "3", "4", "5", "Media & fiber", "Media only")))

# line graphs
p1 <- pH %>%
  filter(Treat == "MSPrebio") %>%
  ggplot(aes(x = Day, y = pH, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="MSPrebiotic", tag = "A") +
  theme(legend.position = "none") +
  ylab("pH") +
  scale_color_npg() + 
  ylim(4,7.5)

p2 <- pH %>%
  filter(Treat == "Sunfiber") %>%
  ggplot(aes(x = Day, y = pH, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Sunfiber", tag = "B") +
  theme(legend.position = "none")  +
  ylab("pH") +
  scale_color_npg() + 
  ylim(4,7.5)


p3 <- pH %>%
  filter(Treat == "Kale") %>%
  ggplot(aes(x = Day, y = pH, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Kale", tag = "C") +
  theme(legend.position = "none") +
  ylab("pH") +
  scale_color_npg() + 
  ylim(4,7.5) 


p4 <- pH %>%
  filter(Treat == "13Bean") %>%
  ggplot(aes(x = Day, y = pH, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="13 Bean Soup", tag = "D") +
  theme(legend.position = "none") +
  ylab("pH") +
  scale_color_npg() + 
  ylim(4,7.5) 


p5 <- pH %>%
  filter(Treat == "CocoFlour") %>%
  ggplot(aes(x = Day, y = pH, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Coconut Flour", tag = "E") +
  theme(legend.position = "none")  +
  ylab("pH") +
  scale_color_npg() + 
  ylim(4,7.5)


p6 <- pH %>%
  filter(Treat == "Flax") %>%
  ggplot(aes(x = Day, y = pH, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Flax", tag = "F") +
  theme(legend.position = "none") +
  ylab("pH") +
  scale_color_npg() + 
  ylim(4,7.5) 


p7 <- pH %>%
  filter(Treat == "Banana") %>%
  ggplot(aes(x = Day, y = pH, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G") +
  theme(legend.position = "none")  +
  ylab("pH") +
  scale_color_npg() + 
  ylim(4,7.5)

legend_plot <- pH %>%
  filter(Treat == "Banana") %>%
  ggplot(aes(x = Day, y = pH, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G", color = "Participant") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title.position = "top",
        legend.title = element_text(size = 12),
        legend.direction = "vertical",
        #legend.byrow = TRUE,
        legend.key.size = unit(2, "line")) +
  guides(colour=guide_legend(nrow=5)) +
  scale_color_npg()

legend <- ggpubr::get_legend(legend_plot)
as_ggplot(legend)

# Change in  pH by day
pH1 <- pH %>%
  filter((Day == "d0" | Day == "d1") & Subject != "Media only" & Subject != "Media & fiber")
pH2 <- pH %>%
  filter((Day == "d1" | Day == "d2") & Subject != "Media only" & Subject != "Media & fiber")
pH3 <- pH %>%
  filter((Day == "d2" | Day == "d3") & Subject != "Media only" & Subject != "Media & fiber")

pH1_diff <- pH1 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_pH = diff(pH, lag = 1))
pH1_diff$Day <- '0-24hours'

pH2_diff <- pH2 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_pH = diff(pH, lag = 1)) 
pH2_diff$Day <- '24-48hours'

pH3_diff <- pH3 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_pH = diff(pH, lag = 1))
pH3_diff$Day <- '48-72hours'

pH_diff <- rbind(pH1_diff, pH2_diff, pH3_diff)

pH_diff$Day <- factor(pH_diff$Day, levels = c('0-24hours', '24-48hours', '48-72hours'), ordered = TRUE)

shapiro.test(pH_diff$delta_pH)

p8 <- pH_diff %>%
  ggplot(aes(x = Day, y = delta_pH)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  labs(title="Change in pH for 24 hr periods", tag = "H", color = "Participant") +
  scale_color_npg() +
  ylab("change in pH") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.02,
    hide.ns = TRUE)
p8

layout <- rbind(c(1,2,3,8,8),
                c(4,5,6,8,8),
                c(7,9,9,NA,NA))

F2 <- grid.arrange(p1, p2, p3, p4, p5, p6, p7,p8,legend, clip = "off",
                  layout_matrix = layout)
ggsave("figure_2.tiff", device = "tiff", dpi = 300, width = 15, height = 7, units = "in", path = "output", F2, bg = "white")
