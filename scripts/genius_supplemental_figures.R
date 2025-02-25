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


# Figure S1
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
ggsave("figure_S1.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S1, bg = "white")


# Figure S2
# Observed plant unique CAZymes in the metagenomes of the 18 participants
S2 <- ggplot(diversity, aes(x=reorder(subject_id, Observed), y=Observed)) +
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
S2
ggsave("figure_S2.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S2, bg = "white")


# Figure S3 
# shannon diversity line graphs
shannon <- read.csv("data/shannon_diversity.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(subj_rep = paste0(Subject, Rep)) %>%
  mutate(Subject = factor(
    Subject,
    levels=c("Sub01", "Sub02", "Sub03", "Sub04", "Sub05"),
    labels=c("1", "2", "3", "4", "5")))

S3A <- shannon %>%
  filter(Treat == "MSPrebio") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="MSPrebiotic", tag = "A") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S3B <- shannon %>%
  filter(Treat == "Sunfiber") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Sunfiber", tag = "B") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S3C <- shannon %>%
  filter(Treat == "Kale") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Kale", tag = "C") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S3D <- shannon %>%
  filter(Treat == "13Bean") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="13 Bean Soup",tag = "D") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S3E <- shannon %>%
  filter(Treat == "CocoFlour") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Coconut Flour", tag = "E") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S3F <- shannon %>%
  filter(Treat == "Flax") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Flax", tag = "F") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)

S3G <- shannon %>%
  filter(Treat == "Banana") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G") +
  theme(legend.position = "none") +
  ylab("Shannon diversity") +
  scale_color_npg() +
  ylim(0, 4.5)


S3_legend_plot <- shannon %>%
  filter(Treat == "Banana") %>%
  ggplot(aes(x = Day, y = Shannon, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G", color = "Participant") +
  scale_color_npg() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title.position = "top",
        legend.title = element_text(size = 12),
        legend.key.size = unit(4, "line"))

legend_S3 <- ggpubr::get_legend(S3_legend_plot)
as_ggplot(legend_S3)

layout_S3 <- rbind(c(1,2,3),
                c(4,5,6),
                c(7,8,8))

S3 <- grid.arrange(S3A, S3B, S3C, S3D, S3E, S3F, S3G, legend_S3, clip = "off",
                  layout_matrix = layout_S3)

ggsave("figure_S3.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S3, bg = "white")


# Figure S4 
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
          
S4A <- scfa %>%
  filter(Treat == "MSPrebio") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="MSPrebiotic", tag = "A") +
  ylab("total SCFA") +
  scale_color_npg() +
  theme(legend.position = "none")  +
  ylim(0,60000)

S4B <- scfa %>%
  filter(Treat == "Sunfiber") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Sunfiber", tag ="B") +
  ylab("total SCFA") +
  scale_color_npg() +
  theme(legend.position = "none")  +
  ylim(0,60000)

S4C <- scfa %>%
  filter(Treat == "Kale") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Kale", tag = "C") +
  ylab("total SCFA") +
  theme(legend.position = "none")  +
  scale_color_npg() +
  ylim(0,60000)

S4D <- scfa %>%
  filter(Treat == "13Bean") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="13 Bean Soup", tag = "D") +
  ylab("total SCFA") +
  theme(legend.position = "none")  +
  scale_color_npg() +
  ylim(0,60000)

S4E <- scfa %>%
  filter(Treat == "CocoFlour") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Coconut Flour", tag = "E") +
  ylab("total SCFA") +
  theme(legend.position = "none")  +
  scale_color_npg() +
  ylim(0,60000)

S4F <- scfa %>%
  filter(Treat == "Flax") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Flax", tag = "F") +
  ylab("total SCFA") +
  theme(legend.position = "none")  +
  scale_color_npg() +
  ylim(0,60000)

S4G <- scfa %>%
  filter(Treat == "Banana") %>%
  ggplot(aes(x = Day, y = total_scfa, group = subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G") +
  ylab("total SCFA") +
  theme(legend.position = "none")  +
  scale_color_npg() +
  ylim(0,60000)

S4_legend_plot <- scfa %>%
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


legend_S4 <- ggpubr::get_legend(S4_legend_plot)
as_ggplot(legend_S4)

layout_S4 <- rbind(c(1,2,3),
                  c(4,5,6),
                  c(7,8,NA))

S4 <- grid.arrange(S4A, S4B, S4C, S4D, S4E, S4F, S4G, legend_S4, clip = "off",
                   layout_matrix = layout_S4)

ggsave("figure_S4.tiff", device = "tiff", dpi = 300, width = 9, height = 6, units = "in", path = "output", S4, bg = "white")


# Figure S5
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
S5 <- ggplot(pielou1_diff, aes(x = Treat, y = delta_pielou)) +
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

S5
ggsave("figure_S5.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S5)


# Figure S6 
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
S6 <- ggplot(total_monos1_diff, aes(x = Treat, y = delta_total_monos)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Total monosaccharides", color = "Participant") +
  ylab("change in total monosaccharides") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0.02,
    hide.ns = TRUE)
S6
ggsave("figure_S6.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S6, bg = "white")


# Figure S7 
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
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") %>%
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

S7 <- ggplot(samples_lactate_diff, aes(x = Treat, y = delta_lactate)) +
  geom_point(size = 2.5, aes(color = Subject), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Lactate", color = "Participant") +
  ylab("change in lactate") +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0,
    hide.ns = TRUE,
    tip.length = .01,
    label.size = 3,
    step.increase = 0.08)  
S7

ggsave("figure_S7.tiff", device = "tiff", dpi = 300, width = 9, height = 9, units = "in", path = "output", S7, bg = "white")

# Figure S8
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


S8A <- samples_lactate %>%
  filter(Treat =="MSPrebio") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="MSPrebiotic", tag = "A") +
  theme(legend.position = "none") +
  ylab("lactate") +
  scale_color_npg() +
  ylim(0, 10000)


S8B <- samples_lactate %>%
  filter(Treat =="Sunfiber") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Sunfiber", tag = "B") +
  theme(legend.position = "none") +
  ylab("lactate") +
  scale_color_npg() +
  ylim(0, 10000)


S8C <- samples_lactate %>%
  filter(Treat =="Kale") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Kale", tag = "C") +
  theme(legend.position = "none") +
  ylab("lactate") +
  scale_color_npg() +
  ylim(0, 10000)


S8D <- samples_lactate %>%
  filter(Treat =="13Bean") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="13 Bean Soup",tag = "D") +
  theme(legend.position = "none") +
  ylab("lactate") +
  scale_color_npg() +
  ylim(0, 10000)

S8E <- samples_lactate %>%
  filter(Treat =="CocoFlour") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Coconut Flour", tag = "E") +
  theme(legend.position = "none") +
  ylab("lactate") +
  scale_color_npg() +
  ylim(0, 10000)


S8F <- samples_lactate %>%
  filter(Treat =="Flax") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Flax", tag = "F") +
  theme(legend.position = "none") +
  ylab("lactate") +
  scale_color_npg() +
  ylim(0, 10000)

S8G <- samples_lactate %>%
  filter(Treat =="Banana") %>%
  ggplot(aes(x = Day, y = lactate, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G") +
  theme(legend.position = "none") +
  ylab("lactate") +
  scale_color_npg() +
  ylim(0, 10000)


S8_legend_plot <- samples_lactate %>%
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

legend_S8 <- ggpubr::get_legend(S8_legend_plot)
as_ggplot(legend_S8)

layout_S8 <- rbind(c(1,2,3),
                    c(4,5,6),
                    c(7,8,NA))

S8 <- grid.arrange(S8A, S8B, S8C, S8D, S8E, S8F, S8G, legend_S8, clip = "off",
                    layout_matrix = layout_S8)

ggsave("figure_S8.tiff", device = "tiff", dpi = 300, width = 9, height = 7, units = "in", path = "output", S8, bg = "white")



# Figure SX
# Total monosaccharides over time
total_monos <- read.csv("data/total_monos_nonstoolcontrols.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
total_monos$total_monos <- rowSums(total_monos[,5:18])
total_monos$Day <- factor(total_monos$Day, levels = c("d0", "d1", "d2", "d3"), ordered = TRUE)

total_monos <- total_monos %>%
  select(Day, Subject, Rep, Treat, total_monos)  %>%
  filter(!(Subject == "Sub3" & Treat == "Flax" & Rep == "C")) %>%
  mutate(subj_rep = paste0(Subject, Rep),
         Subject = factor(
           Subject,
           levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5","Media & fiber", "Media only"),
           labels=c("1", "2", "3", "4", "5", "Media & fiber", "Media only")))


S8A <- total_monos %>%
  filter(Treat =="MSPrebio") %>%
  ggplot(aes(x = Day, y = total_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="MSPrebiotic", tag = "A") +
  theme(legend.position = "none") +
  ylab("total_monos") +
  scale_color_npg() +
  ylim(0, 22)


S8B <- total_monos %>%
  filter(Treat =="Sunfiber") %>%
  ggplot(aes(x = Day, y = total_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Sunfiber", tag = "B") +
  theme(legend.position = "none") +
  ylab("total_monos") +
  scale_color_npg() +
  ylim(0, 22)


S8C <- total_monos %>%
  filter(Treat =="Kale") %>%
  ggplot(aes(x = Day, y = total_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Kale", tag = "C") +
  theme(legend.position = "none") +
  ylab("total_monos") +
  scale_color_npg() +
  ylim(0, 22)


S8D <- total_monos %>%
  filter(Treat =="13Bean") %>%
  ggplot(aes(x = Day, y = total_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="13 Bean Soup",tag = "D") +
  theme(legend.position = "none") +
  ylab("total_monos") +
  scale_color_npg() +
  ylim(0, 22)

S8E <- total_monos %>%
  filter(Treat =="CocoFlour") %>%
  ggplot(aes(x = Day, y = total_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Coconut Flour", tag = "E") +
  theme(legend.position = "none") +
  ylab("total_monos") +
  scale_color_npg() +
  ylim(0, 22)


S8F <- total_monos %>%
  filter(Treat =="Flax") %>%
  ggplot(aes(x = Day, y = total_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Flax", tag = "F") +
  theme(legend.position = "none") +
  ylab("total_monos") +
  scale_color_npg() +
  ylim(0, 22)

S8G <- total_monos %>%
  filter(Treat =="Banana") %>%
  ggplot(aes(x = Day, y = total_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G") +
  theme(legend.position = "none") +
  ylab("total_monos") +
  scale_color_npg() +
  ylim(0, 22)


S8_legend_plot <- total_monos %>%
  filter(Treat =="Banana") %>%
  ggplot(aes(x = Day, y = total_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G", color = "Participant") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title.position = "top",
        legend.title = element_text(size = 12),
        legend.direction = "vertical",
        legend.key.size = unit(2, "line")) +
  guides(colour=guide_legend(nrow=5)) +
  scale_color_npg()

legend_S8 <- ggpubr::get_legend(S8_legend_plot)
as_ggplot(legend_S8)

layout_S8 <- rbind(c(1,2,3),
                   c(4,5,6),
                   c(7,8,NA))

S8 <- grid.arrange(S8A, S8B, S8C, S8D, S8E, S8F, S8G, legend_S8, clip = "off",
                   layout_matrix = layout_S8)

ggsave("figure_S8.tiff", device = "tiff", dpi = 300, width = 9, height = 7, units = "in", path = "output", S8, bg = "white")


# Figure SX
# free monosaccharides over time
free_monos <- read.csv("data/free_monos_nonstoolcontrols.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
free_monos$free_monos <- rowSums(free_monos[,5:18])
free_monos$Day <- factor(free_monos$Day, levels = c("d0", "d1", "d2", "d3"), ordered = TRUE)

free_monos <- free_monos %>%
  select(Day, Subject, Rep, Treat, free_monos)  %>%
  filter(!(Subject == "Sub3" & Treat == "Flax" & Rep == "C")) %>%
  mutate(subj_rep = paste0(Subject, Rep),
         Subject = factor(
           Subject,
           levels=c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5","Media & fiber", "Media only"),
           labels=c("1", "2", "3", "4", "5", "Media & fiber", "Media only")))


S8A <- free_monos %>%
  filter(Treat =="MSPrebio") %>%
  ggplot(aes(x = Day, y = free_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="MSPrebiotic", tag = "A") +
  theme(legend.position = "none") +
  ylab("free_monos") +
  scale_color_npg() +
  ylim(0, 3.5)


S8B <- free_monos %>%
  filter(Treat =="Sunfiber") %>%
  ggplot(aes(x = Day, y = free_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Sunfiber", tag = "B") +
  theme(legend.position = "none") +
  ylab("free_monos") +
  scale_color_npg() +
  ylim(0, 3.5)


S8C <- free_monos %>%
  filter(Treat =="Kale") %>%
  ggplot(aes(x = Day, y = free_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Kale", tag = "C") +
  theme(legend.position = "none") +
  ylab("free_monos") +
  scale_color_npg() +
  ylim(0, 3.5)


S8D <- free_monos %>%
  filter(Treat =="13Bean") %>%
  ggplot(aes(x = Day, y = free_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="13 Bean Soup",tag = "D") +
  theme(legend.position = "none") +
  ylab("free_monos") +
  scale_color_npg() +
  ylim(0, 3.5)

S8E <- free_monos %>%
  filter(Treat =="CocoFlour") %>%
  ggplot(aes(x = Day, y = free_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Coconut Flour", tag = "E") +
  theme(legend.position = "none") +
  ylab("free_monos") +
  scale_color_npg() +
  ylim(0, 3.5)


S8F <- free_monos %>%
  filter(Treat =="Flax") %>%
  ggplot(aes(x = Day, y = free_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Flax", tag = "F") +
  theme(legend.position = "none") +
  ylab("free_monos") +
  scale_color_npg() +
  ylim(0, 3.5)

S8G <- free_monos %>%
  filter(Treat =="Banana") %>%
  ggplot(aes(x = Day, y = free_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G") +
  theme(legend.position = "none") +
  ylab("free_monos") +
  scale_color_npg() +
  ylim(0, 3.5)


S8_legend_plot <- free_monos %>%
  filter(Treat =="Banana") %>%
  ggplot(aes(x = Day, y = free_monos, group=subj_rep, color = Subject)) +
  geom_line(linewidth = 1) + labs(title="Banana", tag = "G", color = "Participant") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title.position = "top",
        legend.title = element_text(size = 12),
        legend.direction = "vertical",
        legend.key.size = unit(2, "line")) +
  guides(colour=guide_legend(nrow=5)) +
  scale_color_npg()

legend_S8 <- ggpubr::get_legend(S8_legend_plot)
as_ggplot(legend_S8)

layout_S8 <- rbind(c(1,2,3),
                   c(4,5,6),
                   c(7,8,NA))

S8 <- grid.arrange(S8A, S8B, S8C, S8D, S8E, S8F, S8G, legend_S8, clip = "off",
                   layout_matrix = layout_S8)

ggsave("figure_S8.tiff", device = "tiff", dpi = 300, width = 9, height = 7, units = "in", path = "output", S8, bg = "white")


# Figure S9
# Within cluster sum of squares for SOM plot
# code in genius_figure_7

# Figure S10
# changes in SCFA by subject
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
S10A <- ggplot(samples_acetate_diff, aes(x = Subject, y = delta_acetate)) +
  geom_point(size = 2.5, aes(color = Treat), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  labs(title = "Acetate", tag = "A") +
  xlab("Participant") +
  ylab("change in acetate") +
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
S10B <- ggplot(samples_butyrate_diff, aes(x = Subject, y = delta_butyrate)) +
  geom_point(size = 2.5, aes(color = Treat), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  scale_color_npg() +
  labs(title = "Butyrate", tag = "B") +
  xlab("Participant") +
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
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") 

samples_propionate_diff <- samples_propionate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_propionate = diff(propionate, lag = 1)) 
samples_propionate_diff$Subject = reorder(samples_propionate_diff$Subject, samples_propionate_diff$delta_propionate, median)
S10C <- ggplot(samples_propionate_diff, aes(x = Subject, y = delta_propionate)) +
  geom_point(size = 2.5, aes(color = Treat), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  labs(title = "Propionate", tag = "C") +
  xlab("Participant") +
  ylab("change in propionate") +
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
S10D <- ggplot(samples_lactate_diff, aes(x = Subject, y = delta_lactate)) +
  geom_point(size = 2.5, aes(color = Treat), position=position_jitterdodge(dodge.width=0.3))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  labs(title = "Lactate", tag = "D") +
  xlab("Participant") +
  ylab("change in lactate") +
  scale_color_npg() +
  geom_pwc(
    method = "wilcox_test", label = "p.adj.format",
    p.adjust.method = "bonferroni",
    bracket.nudge.y = 0,
    hide.ns = TRUE,
    tip.length = .01,
    label.size = 3,
    step.increase = 0.08)  

S10 <- grid.arrange(S10A, S10B, S10C, S10D, clip="off", ncol = 2)
ggsave("figure_S10.tiff", device = "tiff", dpi = 300, width = 10, height = 8, units = "in", path = "output", S10)
