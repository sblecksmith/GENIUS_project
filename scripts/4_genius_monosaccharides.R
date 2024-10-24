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
write.csv(total_monos_controls, "data/total_monos_controls.csv", row.names = FALSE)


# Total monos
total_monos <- 
  sheet |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 1, n_max = 91) |>
  list_rbind()
total_monos <- total_monos %>% filter(if_any(everything(), ~!is.na(.)))
total_monos <- total_monos %>% separate('Sample ID', c('Day', 'Subject','Treat', 'Rep'), sep = "_")

write.csv(total_monos, "data/total_monos_noncontrols.csv", row.names = FALSE)



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
write.csv(free_monos_controls, "data/free_monos_controls.csv", row.names = FALSE)


# Free monosaccharides
free_monos <- 
  sheet |>
  purrr::map(read_excel, path = "data/NIH Genius Study Glycomics Data All Boxes Updated_altered.xlsx", skip = 94, n_max = 91) |>
  list_rbind()

# Need to remove the blank rows
free_monos <- free_monos %>% filter(if_any(everything(), ~!is.na(.)))
free_monos <- free_monos %>% separate('Sample ID', c('Day', "Subject", "Treat", 'Rep'), sep = "_")

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
