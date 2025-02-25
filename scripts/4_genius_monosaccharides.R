# title: 4_genius_monosaccharides
# author: Sarah Blecksmith
# purpose: read in monosaccharide data

library(readxl)
library(purrr)
library(dplyr)

sheet = list("NIH #1",  "NIH #2",  "NIH #3",  "NIH #4",  "NIH #5",  "NIH #6",  "NIH #7")

# Total monosaccharides controls
# need to do the control sheets separately and add a control variable for them, otherwise we have duplicates
total_monos_controls1 <- 
  "NIH #8" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 65) |>
  list_rbind()
total_monos_controls1 <- total_monos_controls1 %>% filter(if_any(everything(), ~!is.na(.)))
total_monos_controls1[,"Control_batch"] = 1


total_monos_controls2 <- 
  "NIH #9" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 65) |>
  list_rbind()
total_monos_controls2 <- total_monos_controls2 %>% filter(if_any(everything(), ~!is.na(.)))
total_monos_controls2[,"Control_batch"] = 2

total_monos_controls3 <- 
  "NIH #10" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 65) |>
  list_rbind()
total_monos_controls3 <- total_monos_controls3 %>% filter(if_any(everything(), ~!is.na(.)))
total_monos_controls3[,"Control_batch"] = 3

total_monos_controls <- bind_rows(total_monos_controls1,total_monos_controls2,total_monos_controls3)

total_monos_controls <- total_monos_controls %>% separate("Sample ID", c('Subject', 'Day', 'Rep'), sep = "_")
# These stool controls don't have a treatment, so adding that
total_monos_controls[,"Treat"] = "Ctl-S"
# Remove the Exp ID and move the treat and Sample ID to the front
total_monos_controls <- dplyr::select(total_monos_controls, -'Exp ID') # don't need the exp id
total_monos_controls <- total_monos_controls %>%
  relocate(Treat, .after = Rep) %>%
  relocate(Control_batch, .after = Treat)
total_monos_controls$Subject <- gsub("0", "ub", total_monos_controls$Subject)
write.csv(total_monos_controls, "data/total_monos_stool_controls.csv", row.names = FALSE)


# Total monos
# Need to read these in separately so I can connect the fiber to the media only control
#total_monos <- 
#  sheet |>
#  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 91) |>
#  list_rbind()
#total_monos <- total_monos %>% filter(if_any(everything(), ~!is.na(.)))
#total_monos <- total_monos %>% separate('Sample ID', c('Day', 'Subject','Treat', 'Rep'), sep = "_")

total_monos_msprebiotic <- 
  "NIH #1" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 91) |>
  list_rbind()
total_monos_msprebiotic <- total_monos_msprebiotic %>% filter(if_any(everything(), ~!is.na(.)))
total_monos_msprebiotic <- total_monos_msprebiotic %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
total_monos_msprebiotic <- total_monos_msprebiotic %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                    Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                    .default = Subject))
total_monos_msprebiotic$Treat <- "MSPrebio"

total_monos_sunfiber <- 
  "NIH #2" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 91) |>
  list_rbind()
total_monos_sunfiber <- total_monos_sunfiber %>% filter(if_any(everything(), ~!is.na(.)))
total_monos_sunfiber <- total_monos_sunfiber %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
total_monos_sunfiber <- total_monos_sunfiber %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                                  Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                                  .default = Subject))
total_monos_sunfiber$Treat <- "Sunfiber"


total_monos_cocoflour <- 
  "NIH #3" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 91) |>
  list_rbind()
total_monos_cocoflour <- total_monos_cocoflour %>% filter(if_any(everything(), ~!is.na(.)))
total_monos_cocoflour <- total_monos_cocoflour %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
total_monos_cocoflour <- total_monos_cocoflour %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                            Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                            .default = Subject))
total_monos_cocoflour$Treat <- "CocoFlour"


total_monos_13Bean <- 
  "NIH #4" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 91) |>
  list_rbind()
total_monos_13Bean <- total_monos_13Bean %>% filter(if_any(everything(), ~!is.na(.)))
total_monos_13Bean <- total_monos_13Bean %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
total_monos_13Bean <- total_monos_13Bean %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                              Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                              .default = Subject))
total_monos_13Bean$Treat <- "13Bean"

total_monos_flax <- 
  "NIH #5" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 91) |>
  list_rbind()
total_monos_flax <- total_monos_flax %>% filter(if_any(everything(), ~!is.na(.)))
total_monos_flax <- total_monos_flax %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
total_monos_flax <- total_monos_flax %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                        Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                        .default = Subject))
total_monos_flax$Treat <- "Flax"

total_monos_kale <- 
  "NIH #6" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 91) |>
  list_rbind()
total_monos_kale <- total_monos_kale %>% filter(if_any(everything(), ~!is.na(.)))
total_monos_kale <- total_monos_kale %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
total_monos_kale <- total_monos_kale %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                    Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                    .default = Subject))
total_monos_kale$Treat <- "Kale"

total_monos_banana <- 
  "NIH #7" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 91) |>
  list_rbind()
total_monos_banana <- total_monos_banana %>% filter(if_any(everything(), ~!is.na(.)))
total_monos_banana <- total_monos_banana %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
total_monos_banana <- total_monos_banana %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                    Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                    .default = Subject))
total_monos_banana$Treat <- "Banana"

total_monos <- rbind(total_monos_msprebiotic, total_monos_sunfiber, total_monos_cocoflour, total_monos_13Bean, total_monos_flax, total_monos_kale, total_monos_banana)

write.csv(total_monos, "data/total_monos_nonstoolcontrols.csv", row.names = FALSE)

# Free monosaccharides controls
# need to do the control sheets separately and add a control variable for them, otherwise we have duplicates
free_monos_controls1 <- 
  "NIH #8" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 68, n_max = 65) |>
  list_rbind()
free_monos_controls1 <- free_monos_controls1 %>% filter(if_any(everything(), ~!is.na(.)))
free_monos_controls1[,"Control_batch"] = 1


free_monos_controls2 <- 
  "NIH #9" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 68, n_max = 65) |>
  list_rbind()
free_monos_controls2 <- free_monos_controls2 %>% filter(if_any(everything(), ~!is.na(.)))
free_monos_controls2[,"Control_batch"] = 2

free_monos_controls3 <- 
  "NIH #10" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 68, n_max = 65) |>
  list_rbind()
free_monos_controls3 <- free_monos_controls3 %>% filter(if_any(everything(), ~!is.na(.)))
free_monos_controls3[,"Control_batch"] = 3

free_monos_controls <- bind_rows(free_monos_controls1,free_monos_controls2,free_monos_controls3)

free_monos_controls <- free_monos_controls %>% separate("Sample ID", c('Subject', 'Day', 'Rep'), sep = "_")
# These stool controls don't have a treatment, so adding that
free_monos_controls[,"Treat"] = "Ctl-S"
# Remove the Exp ID and move the treat and Sample ID to the front
free_monos_controls <- dplyr::select(free_monos_controls, -'Exp ID') # don't need the exp id
free_monos_controls <- free_monos_controls %>%
  relocate(Treat, .after = Rep) %>%
  relocate(Control_batch, .after = Treat)
free_monos_controls$Subject <- gsub("0", "ub", free_monos_controls$Subject)
write.csv(free_monos_controls, "data/free_monos_stool_controls.csv", row.names = FALSE)


# Free monosaccharides
#free_monos <- 
#  sheet |>
#  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 94, n_max = 91) |>
#  list_rbind()

# Need to remove the blank rows
#free_monos <- free_monos %>% filter(if_any(everything(), ~!is.na(.)))
#free_monos <- free_monos %>% separate('Sample ID', c('Day', "Subject", "Treat", 'Rep'), sep = "_")

free_monos_msprebiotic <- 
  "NIH #1" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 94, n_max = 91) |>
  list_rbind()
free_monos_msprebiotic <- free_monos_msprebiotic %>% filter(if_any(everything(), ~!is.na(.)))
free_monos_msprebiotic <- free_monos_msprebiotic %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
free_monos_msprebiotic <- free_monos_msprebiotic %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                                  Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                                  .default = Subject))
free_monos_msprebiotic$Treat <- "MSPrebio"

free_monos_sunfiber <- 
  "NIH #2" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 94, n_max = 91) |>
  list_rbind()
free_monos_sunfiber <- free_monos_sunfiber %>% filter(if_any(everything(), ~!is.na(.)))
free_monos_sunfiber <- free_monos_sunfiber %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
free_monos_sunfiber <- free_monos_sunfiber %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                            Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                            .default = Subject))
free_monos_sunfiber$Treat <- "Sunfiber"


free_monos_cocoflour <- 
  "NIH #3" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 94, n_max = 91) |>
  list_rbind()
free_monos_cocoflour <- free_monos_cocoflour %>% filter(if_any(everything(), ~!is.na(.)))
free_monos_cocoflour <- free_monos_cocoflour %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
free_monos_cocoflour <- free_monos_cocoflour %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                              Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                              .default = Subject))
free_monos_cocoflour$Treat <- "CocoFlour"


free_monos_13Bean <- 
  "NIH #4" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 94, n_max = 91) |>
  list_rbind()
free_monos_13Bean <- free_monos_13Bean %>% filter(if_any(everything(), ~!is.na(.)))
free_monos_13Bean <- free_monos_13Bean %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
free_monos_13Bean <- free_monos_13Bean %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                        Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                        .default = Subject))
free_monos_13Bean$Treat <- "13Bean"

free_monos_flax <- 
  "NIH #5" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 94, n_max = 91) |>
  list_rbind()
free_monos_flax <- free_monos_flax %>% filter(if_any(everything(), ~!is.na(.)))
free_monos_flax <- free_monos_flax %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
free_monos_flax <- free_monos_flax %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                    Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                    .default = Subject))
free_monos_flax$Treat <- "Flax"

free_monos_kale <- 
  "NIH #6" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 94, n_max = 91) |>
  list_rbind()
free_monos_kale <- free_monos_kale %>% filter(if_any(everything(), ~!is.na(.)))
free_monos_kale <- free_monos_kale %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
free_monos_kale <- free_monos_kale %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                    Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                    .default = Subject))
free_monos_kale$Treat <- "Kale"

free_monos_banana <- 
  "NIH #7" |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 94, n_max = 91) |>
  list_rbind()
free_monos_banana <- free_monos_banana %>% filter(if_any(everything(), ~!is.na(.)))
free_monos_banana <- free_monos_banana %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
free_monos_banana <- free_monos_banana %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                        Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                        .default = Subject))
free_monos_banana$Treat <- "Banana"

free_monos <- rbind(free_monos_msprebiotic, free_monos_sunfiber, free_monos_cocoflour, free_monos_13Bean, free_monos_flax, free_monos_kale, free_monos_banana)

write.csv(free_monos, "data/free_monos_nonstoolcontrols.csv", row.names = FALSE)



write.csv(free_monos, "data/free_monos_noncontrols.csv", row.names = FALSE)


# fiber monosaccharides
fiber_monos <- 
  "Fiber Samples" |>
  purrr::map(read_excel, path = "data/Genius Linkage.xlsx", skip = 26, n_max = 34) |>
  list_rbind()

fiber_monos <- fiber_monos %>%
  select(-Mono) %>%
  dplyr::rename(Treat = ...2)
write.csv(fiber_monos, "data/fiber_monosaccharides.csv", row.names = FALSE)
