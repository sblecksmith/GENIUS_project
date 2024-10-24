# title: 6_genius_pH
# author: Sarah Blecksmith
# purpose: extract and clean the pH data


library(tidyverse)
library(data.table)
library(ggpubr)
library(ggsci)
library(pzfx)

# pH file is in prism format
pzfx_tables("data/2022_05_NIH_all_pH.pzfx")
kale <- read_pzfx("data/2022_05_NIH_all_pH.pzfx", table = "kale") %>%
  dplyr::rename(Day = Var.1) %>%
  slice(-1) %>%
  data.table::transpose(make.names="Day", keep.names = "Sample") %>%
  pivot_longer(!Sample,names_to = "Day", values_to = "pH") %>%
  mutate(Treat = "Kale")

banana <- read_pzfx("data/2022_05_NIH_all_pH.pzfx", table = "banana") %>%
  dplyr::rename(Day = Var.1) %>%
  slice(-1) %>%
  data.table::transpose(make.names="Day", keep.names = "Sample") %>%
  pivot_longer(!Sample,names_to = "Day", values_to = "pH") %>%
  mutate(Treat = "Banana")

flax <- read_pzfx("data/2022_05_NIH_all_pH.pzfx", table = "flax")%>%
  dplyr::rename(Day = Var.1) %>%
  slice(-1) %>%
  data.table::transpose(make.names="Day", keep.names = "Sample") %>%
  pivot_longer(!Sample,names_to = "Day", values_to = "pH") %>%
  mutate(Treat = "Flax")

coconut_flour <- read_pzfx("data/2022_05_NIH_all_pH.pzfx", table = "coconut flour") %>%
  dplyr::rename(Day = Var.1) %>%
  slice(-1) %>%
  data.table::transpose(make.names="Day", keep.names = "Sample") %>%
  pivot_longer(!Sample,names_to = "Day", values_to = "pH") %>%
  mutate(Treat = "CocoFlour") 

bean <- read_pzfx("data/2022_05_NIH_all_pH.pzfx", table = "13 Bean Soup") %>%
  dplyr::rename(Day = Var.1) %>%
  slice(-1) %>%
  data.table::transpose(make.names="Day", keep.names = "Sample") %>%
  pivot_longer(!Sample,names_to = "Day", values_to = "pH") %>%
  mutate(Treat = "13Bean") 

msprebiotic <- read_pzfx("data/2022_05_NIH_all_pH.pzfx", table = "MS Prebiotic") %>%
  dplyr::rename(Day = Var.1) %>%
  slice(-1) %>%
  data.table::transpose(make.names="Day", keep.names = "Sample") %>%
  pivot_longer(!Sample,names_to = "Day", values_to = "pH") %>%
  mutate(Treat = "MSPrebio") 

sunfiber <- read_pzfx("data/2022_05_NIH_all_pH.pzfx", table = "Sunfiber") %>%
  dplyr::rename(Day = Var.1) %>%
  slice(-1) %>%
  data.table::transpose(make.names="Day", keep.names = "Sample") %>%
  pivot_longer(!Sample,names_to = "Day", values_to = "pH") %>%
  mutate(Treat = "Sunfiber") 

pH <- rbind(banana, bean, coconut_flour, flax, kale, msprebiotic, sunfiber) %>%
  separate(Sample, c("Subject", "Rep"), "_") %>%
  mutate(Rep = case_when(Rep == 1 ~ "A",
                         Rep == 2 ~ "B",
                         Rep == 3 ~ "C")) %>%
  mutate(Subject = gsub("ject ", "", Subject)) %>%
  mutate(Day = paste0("d", Day, ""))

pH <- pH %>% mutate(subj_rep = paste0(Subject, Rep))

write.csv(pH, "data/pH.csv", row.names = FALSE)
