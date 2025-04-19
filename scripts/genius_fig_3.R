# title: genius_fig_3
# author: Sarah Blecksmith
# purpose: make plots of Shannon diversity and total SCFA by time period for figure 3

library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsci)
library(gridExtra)
library(CGPfunctions)


shannon <- read.csv("data/shannon_diversity.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) %>% 
  mutate(subj_rep = paste0(Subject, Rep)) %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub01", "Sub02", "Sub03", "Sub04", "Sub05"),
    labels=c("1", "2", "3", "4", "5")))

# Figure 3A
# Change in Shannon diversity by day
shannon1 <- shannon %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "Ctl-S")
shannon2 <- shannon %>%
  filter((Day == "d1" | Day == "d2") & Subject != "NoSub" & Treat != "Ctl-S")
shannon3 <- shannon %>%
  filter((Day == "d2" | Day == "d3") & Subject != "NoSub" & Treat != "Ctl-S")

shannon1_diff <- shannon1 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_shannon = diff(Shannon, lag = 1)) 
shannon1_diff$Day <- '0-24hours'

shannon2_diff <- shannon2 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_shannon = diff(Shannon, lag = 1)) 
shannon2_diff$Day <- '24-48hours'

shannon3_diff <- shannon3 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_shannon = diff(Shannon, lag = 1))
shannon3_diff$Day <- '48-72hours'

shannon_diff <- rbind(shannon1_diff, shannon2_diff, shannon3_diff)

shannon_diff$Day = reorder(shannon_diff$Day, shannon_diff$delta_shannon, median)
F3A <- ggplot(shannon_diff, aes(x = Day, y = delta_shannon)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  labs(title = "Change in Shannon diversity for 24 hour periods", tag = "A", color = "Participant") +
  scale_color_npg() +
  ylab("change in Shannon Diversity") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.02,
    hide.ns = TRUE)

# Figure 3B
# Change in total SCFA by day
scfa <- read.csv("data/scfa_noncontrols_all.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) 
 
scfa$Day <- factor(scfa$Day, levels = c("d0", "d1", "d2", "d3"), ordered = TRUE)
scfa <- scfa %>%
  dplyr::rename(lactate = "Lactic Acid") %>%
  dplyr::rename(propionate = "Propionic Acid") %>%
  dplyr::rename(acetate = "Acetic Acid") %>%
  dplyr::rename(butyrate = "Butyric Acid") %>%
  mutate(total_scfa = acetate + butyrate + propionate) %>%
  select(Day, Subject, Rep, Treat, total_scfa)  %>%
  filter(Subject != "NoSub" & Treat != "ctl-s") %>%
  filter(!(Subject == "Sub3" & Treat == "Flax" & Rep == "C"))

scfa <- scfa %>% 
  mutate(subj_rep = paste0(Subject, Rep)) %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5"),
    labels=c("1", "2", "3", "4", "5")))

# Change in total SCFA by day
total_scfa1 <- scfa %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "Ctl-S")
total_scfa2 <- scfa %>%
  filter((Day == "d1" | Day == "d2") & Subject != "NoSub" & Treat != "Ctl-S")
total_scfa3 <- scfa %>%
  filter((Day == "d2" | Day == "d3") & Subject != "NoSub" & Treat != "Ctl-S")

total_scfa1_diff <- total_scfa1 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_total_scfa = diff(total_scfa, lag = 1))
total_scfa1_diff$Day <- '0-24hours'

total_scfa2_diff <- total_scfa2 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_total_scfa = diff(total_scfa, lag = 1)) 
total_scfa2_diff$Day <- '24-48hours'

total_scfa3_diff <- total_scfa3 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_total_scfa = diff(total_scfa, lag = 1))
total_scfa3_diff$Day <- '48-72hours'

total_scfa_diff <- rbind(total_scfa1_diff, total_scfa2_diff, total_scfa3_diff)

total_scfa_diff$Day <- factor(total_scfa_diff$Day, levels = c('0-24hours', '24-48hours', '48-72hours'), ordered = TRUE)
F3B <- ggplot(total_scfa_diff, aes(x = Day, y = delta_total_scfa)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  ylab(paste0("change in total SCFA\n(","\u03bc","g per mL sample)")) +
  labs(title="Change in total SCFA for 24 hour periods", tag = "B", color = "Participant") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.03,
    hide.ns = TRUE)

F3B

F3 <- grid.arrange(F3A, F3B, clip = "off", ncol = 2)

ggsave("figure_3.tiff", device = "tiff", dpi = 600, width = 15, height = 7, units = "in", path = "output", F3, bg = "white")



