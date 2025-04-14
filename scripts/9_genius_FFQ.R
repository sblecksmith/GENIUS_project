# title: 9_genius_FFQ
# author: Sarah Blecksmith
# purpose: calculate HEI from FFQ and look at consumption of test fibers


library(dplyr)
library(dietaryindex)


FFQ <- read.csv("data/FFQ_data-2021-05-15-21-41-07-raw-data-and-results_GENIUS.csv", header = T, stringsAsFactors = F, check.names = F) %>%
  dplyr::rename(metagenome_id = RESPONDENTID)

# ID mapping
metagenome_id <- c(8003,8007,8010,8016,8019)
genius_id <- c("Subject1","Subject2","Subject3","Subject4","Subject5")
subjects <- data.frame(metagenome_id, genius_id)

FFQ <- merge(x=subjects, y= FFQ, by = "metagenome_id", all.x = TRUE)


# Calculating HEI
genius_HEI <- FFQ %>% mutate(veg_legumes = V_TOTAL + V_LEGUMES,
                             green_legumes = V_DRKGR + V_LEGUMES,
                             total_prot = PF_TOTAL + PF_LEGUMES,
                             seafood_prot = PF_SEAFD_HI + PF_SEAFD_LOW + PF_SOY + PF_NUTSDS + PF_LEGUMES,
                             fatty_acid = (DT_MFAT + DT_PFAT)/DT_SFAT)


hei <- HEI2015(SERV_DATA = genius_HEI,
               RESPONDENTID = genius_HEI$RESPONDENTID,
               TOTALKCAL_HEI2015 = genius_HEI$DT_KCAL,
               TOTALFRT_SERV_HEI2015 = genius_HEI$F_TOTAL,
               FRT_SERV_HEI2015 = genius_HEI$F_WHOLE,
               VEG_SERV_HEI2015 = genius_HEI$veg_legumes,
               GREENNBEAN_SERV_HEI2015 = genius_HEI$green_legumes,
               TOTALPRO_SERV_HEI2015 = genius_HEI$total_prot,
               SEAPLANTPRO_SERV_HEI2015 = genius_HEI$seafood_prot,
               WHOLEGRAIN_SERV_HEI2015 = genius_HEI$G_WHOLE,
               DAIRY_SERV_HEI2015 = genius_HEI$D_TOTAL, 
               FATTYACID_SERV_HEI2015 = genius_HEI$fatty_acid,
               REFINEDGRAIN_SERV_HEI2015 = genius_HEI$G_REFINED,
               SODIUM_SERV_HEI2015 = genius_HEI$DT_SODI,
               ADDEDSUGAR_SERV_HEI2015 = genius_HEI$ADD_SUGARS,
               SATFAT_SERV_HEI2015 = genius_HEI$DT_SFAT)

write.csv(complete_hei[,c("subject_id", "metagenome_id", "GENIUS_subject_id", "HEI2015_ALL", "HEI2015_FRT", "HEI2015_VEG", "HEI2015_GREENNBEAN", "HEI2015_WHOLEGRAIN")], "output/genius_hei_scores.csv", row.names = FALSE)


# Checking for consumption of the test foods
test_foods <- FFQ[, grepl("metagenome_id|genius_id|FLAX|BANANA|KALE|LEGUMES|BEANS|GREENS|COCONUT", names(FFQ))]

