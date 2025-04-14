# title: genius_supplemental_figures
# author: Sarah Blecksmith
# purpose: make supplemental plots

library(ggplot2)
library(ggsci)
library(dplyr)
library(ggpubr)
library(gridExtra)

diversity <- read.csv("data/genius_cazyme_families_rounded_diversity.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(subject_id = paste0("s", subject_id),
         genius_id = case_when(subject_id == "s3" ~ "1",
                               subject_id == "s7" ~ "2",
                               subject_id == "s10" ~ "3",
                               subject_id == "s16" ~ "4",
                               subject_id == "s19" ~ "5",
                               .default = ""))

# Figure S2
# Shannon diversity of plant unique CAZymes in the metagenomes of the 18 participants.
S1 <- ggplot(diversity, aes(x=reorder(subject_id, Shannon), y=Shannon))+
  labs(title = "Plant unique CAZyme Shannon diversity",
       y = "Shannon") +
  xlab("Participants") +
  geom_bar(stat = "identity", fill = "#3C5488FF") +
  geom_text(aes(label = genius_id, y=2.25), size = 8, color = "#E64B35FF") +
  theme(axis.ticks.x=element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
S1
ggsave("figure_S2.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S2, bg = "white")


# Figure S3
# Observed plant unique CAZymes in the metagenomes of the 18 participants
S3 <- ggplot(diversity, aes(x=reorder(subject_id, Observed), y=Observed)) +
  geom_bar(stat = "identity", fill = "#3C5488FF") +
  geom_text(aes(label = genius_id, y=14), size = 8, color = "#E64B35FF") +
  labs(title = "Plant unique CAZyme diversity Observed",
       y = "Observed") +
  xlab("Participants") +
  theme(axis.ticks.x=element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
S3
ggsave("figure_S3.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S3, bg = "white")


# Figure S4 
# shannon diversity line graphs
shannon <- read.csv("data/shannon_diversity.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(subj_rep = paste0(Subject, Rep)) %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub01", "Sub02", "Sub03", "Sub04", "Sub05"),
    labels=c("1", "2", "3", "4", "5")))

S4A <- shannon %>%
  filter(Treat == "MSPrebio") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="MSPrebiotic", tag = "A") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S4B <- shannon %>%
  filter(Treat == "Sunfiber") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Sunfiber", tag = "B") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S4C <- shannon %>%
  filter(Treat == "Kale") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Kale", tag = "C") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S4D <- shannon %>%
  filter(Treat == "13Bean") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="13 Bean Soup",tag = "D") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S4E <- shannon %>%
  filter(Treat == "CocoFlour") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Coconut Flour", tag = "E") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S4F <- shannon %>%
  filter(Treat == "Flax") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Flax", tag = "F") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S4G <- shannon %>%
  filter(Treat == "Banana") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)


S4_legend_plot <- shannon %>%
  filter(Treat == "Banana") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G", color = "Participant") +
  scale_color_npg() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title.position = "top",
        legend.title = element_text(size = 12),
        legend.key.size = unit(4, "line"))

legend_S4 <- ggpubr::get_legend(S4_legend_plot)
as_ggplot(legend_S4)

layout_S4 <- rbind(c(1,2,3),
                c(4,5,6),
                c(7,8,8))

S4 <- grid.arrange(S4A, S4B, S4C, S4D, S4E, S4F, S4G, legend_S4, clip = "off",
                  layout_matrix = layout_S4)

ggsave("figure_S4.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S4, bg = "white")


# Figure S5 
# total SCFA line graphs
scfa <- read.csv("data/scfa_nonstoolcontrols_all.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

scfa$Day <- factor(scfa$Day, levels = c("d0", "d1", "d2", "d3"), ordered = TRUE)
scfa <- scfa %>%
  dplyr::rename(lactate = "Lactic Acid") %>%
  dplyr::rename(propionate = "Propionic Acid") %>%
  dplyr::rename(acetate = "Acetic Acid") %>%
  dplyr::rename(butyrate = "Butyric Acid") %>%
  mutate(total_scfa = acetate + butyrate + propionate) %>%
  select(Day, Subject, Rep, Treat, total_scfa)  %>%
  filter(!(Subject == "Sub3" & Treat == "Flax" & Rep == "C")) %>%
  mutate(subj_rep = paste0(Subject, Rep),
         Subject = factor(
           Subject,
          levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5","Media & fiber", "Media only"),
          labels=c("1", "2", "3", "4", "5", "Media & fiber", "Media only")))
          
S5A <- scfa %>%
  filter(Treat == "MSPrebio") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="MSPrebiotic", tag = "A") +
  ylab(paste0("total SCFA\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
  theme(legend.position = "none")  +
  ylim(0,60000)

S5B <- scfa %>%
  filter(Treat == "Sunfiber") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Sunfiber", tag ="B") +
  ylab(paste0("total SCFA\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
  theme(legend.position = "none")  +
  ylim(0,60000)

S5C <- scfa %>%
  filter(Treat == "Kale") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Kale", tag = "C") +
  ylab(paste0("total SCFA\n(","\u03bc","g per mL sample)")) +
  theme(legend.position = "none")  +
  scale_color_npg() +
  ylim(0,60000)

S5D <- scfa %>%
  filter(Treat == "13Bean") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="13 Bean Soup", tag = "D") +
  ylab(paste0("total SCFA\n(","\u03bc","g per mL sample)")) +
  theme(legend.position = "none")  +
  scale_color_npg() +
  ylim(0,60000)

S5E <- scfa %>%
  filter(Treat == "CocoFlour") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Coconut Flour", tag = "E") +
  ylab(paste0("total SCFA\n(","\u03bc","g per mL sample)")) +
  theme(legend.position = "none")  +
  scale_color_npg() +
  ylim(0,60000)

S5F <- scfa %>%
  filter(Treat == "Flax") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Flax", tag = "F") +
  ylab(paste0("total SCFA\n(","\u03bc","g per mL sample)")) +
  theme(legend.position = "none")  +
  scale_color_npg() +
  ylim(0,60000)

S5G <- scfa %>%
  filter(Treat == "Banana") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G") +
  ylab(paste0("total SCFA\n(","\u03bc","g per mL sample)")) +
  theme(legend.position = "none")  +
  scale_color_npg() +
  ylim(0,60000)

S5_legend_plot <- scfa %>%
  filter(Treat == "Banana") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G", color = "Participant") +
  scale_color_npg() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title.position = "top",
        legend.title = element_text(size = 12),
        legend.direction = "vertical",
        legend.key.size = unit(2, "line")) +
  guides(colour=guide_legend(nrow=5))


legend_S5 <- ggpubr::get_legend(S5_legend_plot)
as_ggplot(legend_S5)

layout_S5 <- rbind(c(1,2,3),
                  c(4,5,6),
                  c(7,8,NA))

S5 <- grid.arrange(S5A, S5B, S5C, S5D, S5E, S5F, S5G, legend_S5, clip = "off",
                   layout_matrix = layout_S5)

ggsave("figure_S5.tiff", device = "tiff", dpi = 300, width = 9, height = 6, units = "in", path = "output", S5, bg = "white")


# Figure S6
# Change in pielou's evenness
pielou <- read.csv("data/pielou_evenness_bac_only.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "Ctl-S") %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub01", "Sub02", "Sub03", "Sub04", "Sub05"),
    labels=c("1", "2", "3", "4", "5")))

pielou1_diff <- pielou %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_pielou = diff(pielou, lag = 1)) 


pielou1_diff$Treat = reorder(pielou1_diff$Treat, pielou1_diff$delta_pielou, median)
S6 <- ggplot(pielou1_diff, aes(x = Treat, y = delta_pielou)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Change in Pielou's evenness over first 24 hours for each substrate", color = "Participant") +
  ylab("change in Pielou's evenness") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.02,
    hide.ns = TRUE)

S6
ggsave("figure_S6.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S6)


# Figure S7 
# change in total monosaccharides
total_monos <- read.csv("data/total_monos_noncontrols.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "Ctl-S") %>%
  mutate(Subject = factor(
  Subject,
  levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5"),
  labels=c("1", "2", "3", "4", "5")))
total_monos$total_monos <- rowSums(total_monos[,5:18])

total_monos1_diff <- total_monos %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_total_monos = diff(total_monos, lag = 1)) 

total_monos1_diff$Treat = reorder(total_monos1_diff$Treat, total_monos1_diff$delta_total_monos, median)
S7 <- ggplot(total_monos1_diff, aes(x = Treat, y = delta_total_monos)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Total monosaccharides", color = "Participant") +
  ylab("change in total monosaccharides\n(mg per mL sample") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.02,
    hide.ns = TRUE)
S7
ggsave("figure_S7.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S7, bg = "white")


# Figure S8 
# Changes in lactate by fiber
scfa <- read.csv("data/scfa_nonstoolcontrols_all.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

scfa$Day <- factor(scfa$Day, levels = c("d0", "d1", "d2", "d3"), ordered = TRUE)
scfa <- scfa %>%
  dplyr::rename(lactate = "Lactic Acid") %>%
  dplyr::rename(propionate = "Propionic Acid") %>%
  dplyr::rename(acetate = "Acetic Acid") %>%
  dplyr::rename(butyrate = "Butyric Acid") %>%
  mutate(total_scfa = acetate + butyrate + propionate) %>%
  filter(!(Subject == "Sub3" & Treat == "Flax" & Rep == "C")) 

samples_lactate <- scfa %>%
  select(Day, Subject, Rep, Treat, lactate)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "Media only" & Subject != "Media & fiber" & Treat != "ctl-s") %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5"),
    labels=c("1", "2", "3", "4", "5")))

samples_lactate_diff <- samples_lactate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_lactate = diff(lactate, lag = 1)) 
samples_lactate_diff$Treat = reorder(samples_lactate_diff$Treat, samples_lactate_diff$delta_lactate, median)

S8 <- ggplot(samples_lactate_diff, aes(x = Treat, y = delta_lactate)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Lactate", color = "Participant") +
  ylab(paste0("change in lactate\n(","\u03bc","g per mL sample)")) +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0,
    hide.ns = TRUE,
    tip.length = .01,
    label.size = 3,
    step.increase = 0.08)  
S8

ggsave("figure_S8.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S8, bg = "white")

# Figure S9
# Lactate over time
scfa <- read.csv("data/scfa_nonstoolcontrols_all.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

scfa$Day <- factor(scfa$Day, levels = c("d0", "d1", "d2", "d3"), ordered = TRUE)

samples_lactate <- scfa %>%
  dplyr::rename(lactate = "Lactic Acid") %>%
  select(Day, Subject, Rep, Treat, lactate)  %>%
  filter(!(Subject == "Sub3" & Treat == "Flax" & Rep == "C")) %>%
  #filter(Subject != "NoSub" & Treat != "ctl-s") %>% 
  mutate(subj_rep = paste0(Subject, Rep),
         Subject = factor(
           Subject,
           levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5","Media & fiber", "Media only"),
           labels=c("1", "2", "3", "4", "5", "Media & fiber", "Media only")))


S9A <- samples_lactate %>%
  filter(Treat =="MSPrebio") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="MSPrebiotic", tag = "A") +
  theme(legend.position = "none") +
  ylab(paste0("lactate\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
  ylim(0, 10000)


S9B <- samples_lactate %>%
  filter(Treat =="Sunfiber") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Sunfiber", tag = "B") +
  theme(legend.position = "none") +
  ylab(paste0("lactate\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
  ylim(0, 10000)


S9C <- samples_lactate %>%
  filter(Treat =="Kale") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Kale", tag = "C") +
  theme(legend.position = "none") +
  ylab(paste0("lactate\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
  ylim(0, 10000)


S9D<- samples_lactate %>%
  filter(Treat =="13Bean") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="13 Bean Soup",tag = "D") +
  theme(legend.position = "none") +
  ylab(paste0("lactate\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
  ylim(0, 10000)

S9E <- samples_lactate %>%
  filter(Treat =="CocoFlour") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Coconut Flour", tag = "E") +
  theme(legend.position = "none") +
  ylab(paste0("lactate\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
  ylim(0, 10000)


S9F <- samples_lactate %>%
  filter(Treat =="Flax") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Flax", tag = "F") +
  theme(legend.position = "none") +
  ylab(paste0("lactate\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
  ylim(0, 10000)

S9G <- samples_lactate %>%
  filter(Treat =="Banana") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G") +
  theme(legend.position = "none") +
  ylab(paste0("lactate\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
  ylim(0, 10000)


S9_legend_plot <- samples_lactate %>%
  filter(Treat =="Banana") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G", color = "Participant") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title.position = "top",
        legend.title = element_text(size = 12),
        legend.direction = "vertical",
        legend.key.size = unit(2, "line")) +
  guides(colour=guide_legend(nrow=5)) +
  scale_color_npg()

legend_S9 <- ggpubr::get_legend(S9_legend_plot)
as_ggplot(legend_S9)

layout_S9 <- rbind(c(1,2,3),
                    c(4,5,6),
                    c(7,8,NA))

S9 <- grid.arrange(S9A, S9B, S9C, S9D, S9E, S9F, S9G, legend_S9, clip = "off",
                    layout_matrix = layout_S9)

ggsave("figure_S9.tiff", device = "tiff", dpi = 300, width = 9, height = 7, units = "in", path = "output", S9, bg = "white")


# Figure S10. Plot of within cluster sum of squares (wss) for kmeans clustering in SOM
# Generated in script genius_fig_7.R


# Figure S11
# changes in SCFA by participant
scfa <- read.csv("data/scfa_nonstoolcontrols_all.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) %>%
  dplyr::rename(lactate = "Lactic Acid") %>%
  dplyr::rename(propionate = "Propionic Acid") %>%
  dplyr::rename(acetate = "Acetic Acid") %>%
  dplyr::rename(butyrate = "Butyric Acid") %>%
  mutate(total_scfa = acetate + butyrate + propionate) %>%
  filter(!(Subject == "Sub3" & Treat == "Flax" & Rep == "C")) %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5"),
    labels=c("1", "2", "3", "4", "5")))

# acetate
samples_acetate <- scfa %>%
  select(Day, Subject, Rep, Treat, acetate)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") 
  

samples_acetate_diff <- samples_acetate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_acetate = diff(acetate, lag = 1)) 
samples_acetate_diff$Subject = reorder(samples_acetate_diff$Subject, samples_acetate_diff$delta_acetate, median)
S11A <- ggplot(samples_acetate_diff, aes(x = Subject, y = delta_acetate)) +
  geom_point(size = 2.5, aes(color = Treat), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  labs(title = "Acetate", tag = "A") +
  xlab("Participant") +
  ylab(paste0("change in acetate\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
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
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") 

samples_butyrate_diff <- samples_butyrate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_butyrate = diff(butyrate, lag = 1)) 
samples_butyrate_diff$Subject = reorder(samples_butyrate_diff$Subject, samples_butyrate_diff$delta_butyrate, median)
S11B <- ggplot(samples_butyrate_diff, aes(x = Subject, y = delta_butyrate)) +
  geom_point(size = 2.5, aes(color = Treat), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Butyrate", tag = "B") +
  xlab("Participant") +
  ylab(paste0("change in butyrate\n(","\u03bc","g per mL sample)")) +
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
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") 

samples_propionate_diff <- samples_propionate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_propionate = diff(propionate, lag = 1)) 
samples_propionate_diff$Subject = reorder(samples_propionate_diff$Subject, samples_propionate_diff$delta_propionate, median)
S11C <- ggplot(samples_propionate_diff, aes(x = Subject, y = delta_propionate)) +
  geom_point(size = 2.5, aes(color = Treat), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  labs(title = "Propionate", tag = "C") +
  xlab("Participant") +
  ylab(paste0("change in propionate\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0,
    hide.ns = TRUE,
    tip.length = .01,
    label.size = 3,
    step.increase = 0.08)


# Lactate
samples_lactate <- scfa %>%
  select(Day, Subject, Rep, Treat, lactate)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") 

samples_lactate_diff <- samples_lactate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_lactate = diff(lactate, lag = 1)) 
samples_lactate_diff$Subject = reorder(samples_lactate_diff$Subject, samples_lactate_diff$delta_lactate, median)
S11D <- ggplot(samples_lactate_diff, aes(x = Subject, y = delta_lactate)) +
  geom_point(size = 2.5, aes(color = Treat), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  labs(title = "Lactate", tag = "D") +
  xlab("Participant") +
  ylab(paste0("change in lactate\n(","\u03bc","g per mL sample)")) +
  scale_color_npg() +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0,
    hide.ns = TRUE,
    tip.length = .01,
    label.size = 3,
    step.increase = 0.08)  

S11 <- grid.arrange(S11A, S11B, S11C, S11D, clip="off", ncol = 2)
ggsave("figure_S11.tiff", device = "tiff", dpi = 300, width = 10, height = 8, units = "in", path = "output", S11)
