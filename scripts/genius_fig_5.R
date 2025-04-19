# title: genius_fig_5
# author: Sarah Blecksmith
# purpose: change in shannon diversity by substrate and change in free monos


library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsci)
library(gridExtra)

# Figure 5A - change in shannon diversity
shannon <- read.csv("data/shannon_diversity.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

shannon1 <- shannon %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "Ctl-S") %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub01", "Sub02", "Sub03", "Sub04", "Sub05"),
    labels=c("1", "2", "3", "4", "5")))

shannon1_diff <- shannon1 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_shannon = diff(Shannon, lag = 1)) 


shannon1_diff$Treat = reorder(shannon1_diff$Treat, shannon1_diff$delta_shannon, median)
F5A <- ggplot(shannon1_diff, aes(x = Treat, y = delta_shannon)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  ggtitle("Change in Shannon diversity over first 24 hours for each substrate") +
  labs(tag = "A", color = "Participant") +
  ylab("change in Shannon diversity") +
  xlab("Fiber") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.02,
    hide.ns = TRUE)
F5A

# Figure 5B - change in free monosaccharides
free_monos <- read.csv("data/free_monos_noncontrols.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
free_monos1 <- free_monos %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "Ctl-S") %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5"),
    labels=c("1", "2", "3", "4", "5")))
free_monos1$free_monos <- rowSums(free_monos1[,5:18])

free_monos1_diff <- free_monos1 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_free_monos = diff(free_monos, lag = 1)) 


free_monos1_diff$Treat = reorder(free_monos1_diff$Treat, free_monos1_diff$delta_free_monos, median)
F5B <- ggplot(free_monos1_diff, aes(x = Treat, y = delta_free_monos)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  ggtitle("Change in free monosaccharides over first 24 hours for each substrate") +
  labs(tag = "B", color = "Participant") +
  ylab("change in free monosaccharides\n(mg per mL sample") +
  xlab("Fiber") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.02,
    hide.ns = TRUE)

F5<- grid.arrange(F5A, F5B, clip="off", ncol = 2)
ggsave("figure_5.tiff", device = "tiff", dpi = 600, width = 15, height = 7, units = "in", path = "output", F5)
