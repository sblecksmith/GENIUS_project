# title: genius_fig_8
# author: Sarah Blecksmith
# purpose: Make figure of total SCFA by subject


library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)

scfa <- read.csv("data/scfa_noncontrols_all.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)


scfa$Day <- factor(scfa$Day, levels = c("d0", "d1", "d2", "d3"), ordered = TRUE)
scfa <- scfa %>%
  dplyr::rename(lactate = "Lactic Acid") %>%
  dplyr::rename(propionate = "Propionic Acid") %>%
  dplyr::rename(acetate = "Acetic Acid") %>%
  dplyr::rename(butyrate = "Butyric Acid") %>%
  mutate(total_scfa = acetate + butyrate + propionate) %>%
  #  select(Day, Subject, Rep, Treat, total_scfa)  %>%
  filter(Subject != "NoSub" & Treat != "ctl-s") %>%
  filter(!(Subject == "Sub3" & Treat == "Flax" & Rep == "C"))


samples_total_scfa <- scfa %>%
  select(Day, Subject, Rep, Treat, total_scfa)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5"),
    labels=c("1", "2", "3", "4", "5")))

samples_total_scfa_diff <- samples_total_scfa %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_total_scfa = diff(total_scfa, lag = 1)) 
samples_total_scfa_diff$Subject = reorder(samples_total_scfa_diff$Subject, samples_total_scfa_diff$delta_total_scfa, median)
F8 <- ggplot(samples_total_scfa_diff, aes(x = Subject, y = delta_total_scfa)) +
  geom_point(size = 2.5, aes(color = Treat), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Total SCFA", color = "Participant") +
  ylab("change in total SCFA") +
  xlab("") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0,
    hide.ns = TRUE,
    tip.length = .01,
    label.size = 3,
    step.increase = 0.08)

F8
ggsave("figure_8.tiff", device = "tiff", dpi = 300, width = 13, height = 7, units = "in", path = "output", F8)
