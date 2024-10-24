# title: 7_genius_cazy_permanova
# author: Sarah Blecksmith
# purpose: try permanova on cazyme profiles between responders and non responders

library(phyloseq)
library(dplyr)
library(microbiome)
library(vegan)

families_rounded <- read.csv("output/genius_cazyme_families_rounded.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

names(families_rounded) <- gsub("^0","",names(families_rounded))

# ID mapping
metagenomeID <- c(3,7,10,16,19)
geniusID <- c("Subject1","Subject2","Subject3","Subject4","Subject5")
responder <- c("no", "yes", "yes", "yes", "no")

subjects <- data.frame(metagenomeID, geniusID, responder)

GHPL <- families_rounded[grep("GH|PL", families_rounded$cazy_fam), ]

rownames(GHPL)<- GHPL$cazy_fam
GHPL <- GHPL %>% 
  select(-cazy_fam)

rownames(subjects)<- subjects$metagenomeID
subjects <- subjects %>% 
  select(-metagenomeID)

GHPL <- GHPL[,rownames(subjects)]
subjects$responder<- relevel(factor(subjects$responder), ref="yes")

# using adonis2
permanova <- adonis2(t(GHPL) ~ responder,
                      data = subjects, permutations=999, method = "bray") # p = 0.4

