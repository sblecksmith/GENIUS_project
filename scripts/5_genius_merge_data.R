# title: 5_genius_merge_data
# author: Sarah Blecksmith
# purpose: merge the glycomics, SCFA and shannon diversity data


library(tidyverse)
library(data.table)
library(pzfx)

scfa <- read.csv("data/scfa_noncontrols_all.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
free_monos <- read.csv("data/free_monos_noncontrols.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
total_monos <- read.csv("data/total_monos_noncontrols.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
shannon <- read.csv("data/shannon_diversity.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)


# change in total monos
total_monos1 <- total_monos %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "Ctl-S")
total_monos1$total_monos <- rowSums(total_monos1[,5:18])
total_monos1 <- total_monos1 %>% select(Day, Subject, Treat, Rep, total_monos)
total_monos1_diff <- total_monos1 %>%
  select(Day, Subject, Treat, Rep, total_monos) %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(delta_total_monos = diff(total_monos, lag = 1))
 

# change in free monos
free_monos1 <- free_monos %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "Ctl-S")
free_monos1$free_monos <- rowSums(free_monos1[,5:18])

free_monos1_diff <- free_monos1 %>%
  select(Day, Subject, Treat, Rep, free_monos) %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_free_monos = diff(free_monos, lag = 1)) 

# change in SCFA and lactate
scfa$Day <- factor(scfa$Day, levels = c("d0", "d1", "d2", "d3"), ordered = TRUE)
scfa <- scfa %>%
  dplyr::rename(lactate = "Lactic Acid") %>%
  dplyr::rename(propionate = "Propionic Acid") %>%
  dplyr::rename(acetate = "Acetic Acid") %>%
  dplyr::rename(butyrate = "Butyric Acid") %>%
  mutate(total_scfa = acetate + butyrate + propionate) %>%
  filter(Subject != "NoSub" & Treat != "ctl-s") %>%
  filter(!(Subject == "Sub3" & Treat == "Flax" & Rep == "C"))


# total scfa
samples_total_scfa <- scfa %>%
  select(Day, Subject, Rep, Treat, total_scfa)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") #%>%

samples_total_scfa_diff <- samples_total_scfa %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_total_scfa = diff(total_scfa, lag = 1)) 

# acetate
samples_acetate <- scfa %>%
  select(Day, Subject, Rep, Treat, acetate)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") #%>%

samples_acetate_diff <- samples_acetate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_acetate = diff(acetate, lag = 1)) 

# butyrate
samples_butyrate <- scfa %>%
  select(Day, Subject, Rep, Treat, butyrate)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") #%>%

samples_butyrate_diff <- samples_butyrate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_butyrate = diff(butyrate, lag = 1)) 

# propionate
samples_propionate <- scfa %>%
  select(Day, Subject, Rep, Treat, propionate)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") #%>%

samples_propionate_diff <- samples_propionate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_propionate = diff(propionate, lag = 1)) 

# lactate
samples_lactate <- scfa %>%
  select(Day, Subject, Rep, Treat, lactate)  %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "ctl-s") #%>%

samples_lactate_diff <- samples_lactate %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_lactate = diff(lactate, lag = 1)) 

# change in shannon diversity
shannon1 <- shannon %>%
  filter((Day == "d0" | Day == "d1") & Subject != "NoSub" & Treat != "Ctl-S")

shannon1_diff <- shannon1 %>%
  dplyr::group_by(Subject, Treat, Rep) %>%
  dplyr::arrange(Day, .by_group=TRUE)  %>%
  dplyr::summarise(
    delta_shannon = diff(Shannon, lag = 1))

# baseline shannon diversity
baseline_shannon <- shannon %>% filter(Day == "d0")
baseline_shannon <- dplyr::rename(baseline_shannon, baseline_shannon = Shannon)


# add the separate scfa
samples_butyrate_diff <- samples_butyrate_diff %>%
  mutate(sample_id = paste(sep = "_", Subject, Treat, Rep))

samples_propionate_diff <- samples_propionate_diff %>%
  mutate(sample_id = paste(sep = "_", Subject, Treat, Rep))
subject_24hr_data <- merge(x = samples_butyrate_diff, y= samples_propionate_diff[,c("sample_id", "delta_propionate", "pct_change_propionate")], by = "sample_id")

samples_acetate_diff <- samples_acetate_diff %>%
  mutate(sample_id = paste(sep = "_", Subject, Treat, Rep))
subject_24hr_data <- merge(x = subject_24hr_data, y= samples_acetate_diff[,c("sample_id", "delta_acetate", "pct_change_acetate")], by = "sample_id")

samples_lactate_diff <- samples_lactate_diff %>%
  mutate(sample_id = paste(sep = "_", Subject, Treat, Rep))
subject_24hr_data <- merge(x = subject_24hr_data, y= samples_lactate_diff[,c("sample_id", "delta_lactate", "pct_change_lactate")], by = "sample_id")

samples_total_scfa_diff <- samples_total_scfa_diff %>%
  mutate(sample_id = paste(sep = "_", Subject, Treat, Rep))
subject_24hr_data <- merge(x = subject_24hr_data, y= samples_total_scfa_diff[,c("sample_id", "delta_total_scfa", "pct_change_total_scfa")], by = "sample_id")

free_monos1_diff <- free_monos1_diff %>%
  mutate(sample_id = paste(sep = "_", Subject, Treat, Rep))
subject_24hr_data <- merge(x = subject_24hr_data, y= free_monos1_diff[,c("sample_id", "delta_free_monos", "pct_change_free_monos")], by = "sample_id")

total_monos1_diff <- total_monos1_diff %>%
  mutate(sample_id = paste(sep = "_", Subject, Treat, Rep))
subject_24hr_data <- merge(x = subject_24hr_data, y= total_monos1_diff[,c("sample_id", "delta_total_monos", "pct_change_total_monos")], by = "sample_id")

shannon1_diff$Subject <- gsub("0", "", shannon1_diff$Subject)
shannon1_diff$Rep <- gsub("rep", "", shannon1_diff$Rep)
shannon1_diff <- shannon1_diff %>%
  mutate(sample_id = paste(sep = "_", Subject, Treat, Rep))
subject_24hr_data <- merge(x = subject_24hr_data, y= shannon1_diff[,c("sample_id", "delta_shannon", "pct_change_shannon")], by = "sample_id")


baseline_shannon$Subject <- gsub("0", "", baseline_shannon$Subject)
baseline_shannon$Rep <- gsub("rep", "", baseline_shannon$Rep)
baseline_shannon <- baseline_shannon %>%
  mutate(sample_id = paste(sep = "_", Subject, Treat, Rep))

subject_24hr_data <- merge(x = subject_24hr_data, y= baseline_shannon[,c("sample_id", "baseline_shannon")], by = "sample_id")


write.csv(subject_24hr_data, "data/all_subject_data_24hrs_pct_change.csv", row.names = FALSE)
