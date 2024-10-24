# title: genius_fig_6
# author: Sarah Blecksmith
# purpose: change in SCFA by substrate


library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsci)
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
  filter(!(Subject == "Sub3" & Treat == "Flax" & Rep == "C")) %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5"),
    labels=c("1", "2", "3", "4", "5")))


# total scfa
samples_total_scfa <- scfa %>%
  select(Day, Subject, Rep, Treat, total_scfa)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") #%>%

samples_total_scfa_diff <- samples_total_scfa %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_total_scfa = diff(total_scfa, lag = 1)) 
samples_total_scfa_diff$Treat = reorder(samples_total_scfa_diff$Treat, samples_total_scfa_diff$delta_total_scfa, median)
F6A <- ggplot(samples_total_scfa_diff, aes(x = Treat, y = delta_total_scfa)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Total SCFA", tag = "A", color = "Participant") +
  ylab("change in total SCFA") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0,
    hide.ns = TRUE,
    tip.length = .01,
    label.size = 3,
    step.increase = 0.08)

# acetate
samples_acetate <- scfa %>%
  select(Day, Subject, Rep, Treat, acetate)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") #%>%

samples_acetate_diff <- samples_acetate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_acetate = diff(acetate, lag = 1)) 
samples_acetate_diff$Treat = reorder(samples_acetate_diff$Treat, samples_acetate_diff$delta_acetate, median)
F6B <- ggplot(samples_acetate_diff, aes(x = Treat, y = delta_acetate)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Acetate", tag = "B", color = "Participant") +
  ylab("change in acetate") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0,
    hide.ns = TRUE,
    tip.length = .01,
    label.size = 3,
    step.increase = 0.08)


# butyrate
samples_butyrate <- scfa %>%
  select(Day, Subject, Rep, Treat, butyrate)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") #%>%

samples_butyrate_diff <- samples_butyrate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_butyrate = diff(butyrate, lag = 1)) 
samples_butyrate_diff$Treat = reorder(samples_butyrate_diff$Treat, samples_butyrate_diff$delta_butyrate, median)
F6C <- ggplot(samples_butyrate_diff, aes(x = Treat, y = delta_butyrate)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Butyrate", tag = "C", color = "Participant") +
  ylab("change in butyrate") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0,
    hide.ns = TRUE,
    tip.length = .01,
    label.size = 3,
    step.increase = 0.08)

# propionate
samples_propionate <- scfa %>%
  select(Day, Subject, Rep, Treat, propionate)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") #%>%

samples_propionate_diff <- samples_propionate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_propionate = diff(propionate, lag = 1)) 
samples_propionate_diff$Treat = reorder(samples_propionate_diff$Treat, samples_propionate_diff$delta_propionate, median)
F6D <- ggplot(samples_propionate_diff, aes(x = Treat, y = delta_propionate)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Propionate", tag = "D", color = "Participant") +
  ylab("change in propionate") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0,
    hide.ns = TRUE,
    tip.length = .01,
    label.size = 3,
    step.increase = 0.08)


F6 <- grid.arrange(F6A, F6B, F6C, F6D, clip="off", ncol = 2)
ggsave("figure_6.tiff", device = "tiff", dpi = 300, width = 20, height = 10, units = "in", path = "output", F6)