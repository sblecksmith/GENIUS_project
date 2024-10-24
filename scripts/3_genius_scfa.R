# title: 3_genius_scfa
# author: Sarah Blecksmith
# purpose: read in SCFA data

library(readxl)
library(purrr)
library(dplyr)


# need to do the control sheets separately and add a control variable for them, otherwise we have duplicates
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
write.csv(scfa_controls, "data/scfa_controls_all.csv", row.names = FALSE)


# read in non-controls
sheet = list("NIH #1 MS Prebiotic",  "NIH #2 SunFiber",  "NIH #3 Coconut Flour",  "NIH #4 13Beans", "NIH #5 Flax", "NIH #6 Kale",  "NIH #7 Banana")

scfa <- 
  sheet |>
  purrr::map(read_excel, path = "data/NIH GENIUS Study SCFA Data All Boxes_altered.xlsx", skip = 2, n_max = 91) |>
  list_rbind()

# Need to remove the blank rows
scfa <- scfa %>% filter(if_any(everything(), ~!is.na(.)))
scfa <- scfa %>% separate("Sample ID", c('Day', "Subject", "Treat", 'Rep'), sep = "_")

scfa$Treat[scfa$Treat == "Ctl-S"] <- "NoFiber"

write.csv(scfa, "data/scfa_noncontrols_all.csv", row.names = FALSE)
