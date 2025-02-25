# title: 3_genius_scfa
# author: Sarah Blecksmith
# purpose: read in SCFA data

library(readxl)
library(purrr)
library(dplyr)


# need to do the stool control sheets separately and add a control variable for them, otherwise we have duplicates
scfa_controls1 <- 
  "NIH #8 Control 1" |>
  purrr::map(read_excel, path = "data/NIH GENIUS Study SCFA Data All Boxes_altered.xlsx", skip = 2, n_max = 65) |>
  list_rbind()
scfa_controls1 <- scfa_controls1 %>% filter(if_any(everything(), ~!is.na(.)))
scfa_controls1[,"Control_batch"] = 1


scfa_controls2 <- 
  "NIH #9 Control 2" |>
  purrr::map(read_excel, path = "data/NIH GENIUS Study SCFA Data All Boxes_altered.xlsx", skip = 2, n_max = 65) |>
  list_rbind()
scfa_controls2 <- scfa_controls2 %>% filter(if_any(everything(), ~!is.na(.)))
scfa_controls2[,"Control_batch"] = 2

scfa_controls3 <- 
  "NIH #10 Control 3" |>
  purrr::map(read_excel, path = "data/NIH GENIUS Study SCFA Data All Boxes_altered.xlsx", skip = 2, n_max = 65) |>
  list_rbind()
scfa_controls3 <- scfa_controls3 %>% filter(if_any(everything(), ~!is.na(.)))
scfa_controls3[,"Control_batch"] = 3

scfa_controls <- bind_rows(scfa_controls1,scfa_controls2,scfa_controls3,)


# Separate out Sample ID into components
scfa_controls <- scfa_controls %>% separate("Sample ID", c('Subject', 'Day', 'Rep'), sep = "_")
# These stool controls don't have a treatment, so adding that
scfa_controls[,"Treat"] = "Ctl-S"
# Remove the Exp ID and move the treat and Sample ID to the front
scfa_controls <- dplyr::select(scfa_controls, -'Exp ID') # don't need the exp id
scfa_controls <- scfa_controls %>%
  relocate(Treat, .after = Rep) %>%
  relocate(Control_batch, .after = Treat)
scfa_controls$Subject <- gsub("0", "ub", scfa_controls$Subject)
write.csv(scfa_controls, "data/scfa_stool_controls_all.csv", row.names = FALSE)



# Need to read these in separately so I can connect the fiber to the media only control
scfa_msprebiotic <- 
  "NIH #1 MS Prebiotic" |>
  purrr::map(read_excel, path = "data/NIH GENIUS Study SCFA Data All Boxes_altered.xlsx", skip = 2, n_max = 91) |>
  list_rbind()
scfa_msprebiotic <- scfa_msprebiotic %>% filter(if_any(everything(), ~!is.na(.)))
scfa_msprebiotic <- scfa_msprebiotic %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
scfa_msprebiotic <- scfa_msprebiotic %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                    Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                    .default = Subject))
scfa_msprebiotic$Treat <- "MSPrebio"


scfa_sunfiber <- 
  "NIH #2 SunFiber" |>
  purrr::map(read_excel, path = "data/NIH GENIUS Study SCFA Data All Boxes_altered.xlsx", skip = 2, n_max = 91) |>
  list_rbind()
scfa_sunfiber <- scfa_sunfiber %>% filter(if_any(everything(), ~!is.na(.)))
scfa_sunfiber <- scfa_sunfiber %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
scfa_sunfiber <- scfa_sunfiber %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                    Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                    .default = Subject))
scfa_sunfiber$Treat <- "Sunfiber"


scfa_cocoflour <- 
  "NIH #3 Coconut Flour" |>
  purrr::map(read_excel, path = "data/NIH GENIUS Study SCFA Data All Boxes_altered.xlsx", skip = 2, n_max = 91) |>
  list_rbind()
scfa_cocoflour <- scfa_cocoflour %>% filter(if_any(everything(), ~!is.na(.)))
scfa_cocoflour <- scfa_cocoflour %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
scfa_cocoflour <- scfa_cocoflour %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                              Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                              .default = Subject))
scfa_cocoflour$Treat <- "CocoFlour"


scfa_13Bean <- 
  "NIH #4 13Beans" |>
  purrr::map(read_excel, path = "data/NIH GENIUS Study SCFA Data All Boxes_altered.xlsx", skip = 2, n_max = 91) |>
  list_rbind()
scfa_13Bean <- scfa_13Bean %>% filter(if_any(everything(), ~!is.na(.)))
scfa_13Bean <- scfa_13Bean %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
scfa_13Bean <- scfa_13Bean %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                                Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                                .default = Subject))
scfa_13Bean$Treat <- "13Bean"


scfa_flax <- 
  "NIH #5 Flax" |>
  purrr::map(read_excel, path = "data/NIH GENIUS Study SCFA Data All Boxes_altered.xlsx", skip = 2, n_max = 91) |>
  list_rbind()
scfa_flax <- scfa_flax %>% filter(if_any(everything(), ~!is.na(.)))
scfa_flax <- scfa_flax %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
scfa_flax <- scfa_flax %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                          Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                          .default = Subject))
scfa_flax$Treat <- "Flax"

scfa_kale <- 
  "NIH #6 Kale" |>
  purrr::map(read_excel, path = "data/NIH GENIUS Study SCFA Data All Boxes_altered.xlsx", skip = 2, n_max = 91) |>
  list_rbind()
scfa_kale <- scfa_kale %>% filter(if_any(everything(), ~!is.na(.)))
scfa_kale <- scfa_kale %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
scfa_kale <- scfa_kale %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                      Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                      .default = Subject))
scfa_kale$Treat <- "Kale"


scfa_banana <- 
  "NIH #7 Banana" |>
  purrr::map(read_excel, path = "data/NIH GENIUS Study SCFA Data All Boxes_altered.xlsx", skip = 2, n_max = 91) |>
  list_rbind()
scfa_banana <- scfa_banana %>% filter(if_any(everything(), ~!is.na(.)))
scfa_banana <- scfa_banana %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")
scfa_banana <- scfa_banana %>% mutate(Subject = case_when(Subject == "NoSub" & Treat == "ctl-m" ~ "Media only",
                                                      Subject == "NoSub" & Treat != "ctl-m" ~ "Media & fiber",
                                                      .default = Subject))
scfa_banana$Treat <- "Banana"


scfa <- bind_rows(scfa_msprebiotic, scfa_sunfiber, scfa_cocoflour, scfa_13Bean, scfa_flax, scfa_kale, scfa_banana)

write.csv(scfa, "data/scfa_nonstoolcontrols_all.csv", row.names = FALSE)
