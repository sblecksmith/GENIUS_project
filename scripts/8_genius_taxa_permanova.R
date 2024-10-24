# title: 8_genius_taxa_permanova
# author: Sarah Blecksmith
# purpose: try permanova on metaphlan taxonomy between responders and non-responders

library(phyloseq)
library(dplyr)
library(microbiome)
library(vegan)

# starting metagenome taxonomy data from metaphlan4
metaphlan <- read.csv("data/genius_merged_abundance_table.txt", skip = 1, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, sep = "\t")

rownames(metaphlan) <- metaphlan$clade_name
metaphlan <- metaphlan %>% select(-clade_name)

names(metaphlan) <- sub(".mpa4", "", names(metaphlan))
metaphlan <- metaphlan[,order(as.numeric(names(metaphlan)))]
colnames(metaphlan) <- paste0("sub", colnames(metaphlan))
metaphlan$clade_name <- rownames(metaphlan)
metaphlan <- metaphlan %>% relocate(clade_name)

# ID mapping
metagenomeID <- c("sub3","sub7","sub10","sub16","sub19")
geniusID <- c(1,2,3,4,5)
responder <- c("no", "yes", "yes", "yes", "no")

subjects <- data.frame(metagenomeID, geniusID, responder)
subjects$responder <- factor(subjects$responder)
rownames(subjects) <- subjects$metagenomeID
subjects <- subjects %>% 
  select(-metagenomeID)   

phyloseq_metaphlan <- metaphlan_to_phyloseq(mpa = metaphlan,
                                  metadata=subjects, 
                                  version = 4, 
                                  verbose = TRUE, 
                                  tax_lvl = "Genus")

subjects$responder<- relevel(factor(subjects$responder), ref="yes")
taxa <- as.data.frame(otu_table(phyloseq_metaphlan))


# adonis2
permanova <- adonis2(t(taxa) ~ responder,
                      data = subjects, permutations=999, method = "bray") # p=0.5

# function from 
# https://rdrr.io/github/g-antonello/gautils2/src/R/metaphlan_to_phyloseq.R
# with a few tweaks
metaphlan_to_phyloseq <- function(mpa,
                                  metadata = NULL,
                                  version = 4,
                                  verbose = TRUE,
                                  tax_lvl = "Species"){
  
  if(version == 4){
    if(is.character(mpa)){
      # load raw metaphlan data
      mpa <- data.table::fread(mpa) %>%
        as.data.frame()
    }
  }
  if(version == 3){
    if(is.character(mpa)){
      # load raw metaphlan data
      mpa <- data.table::fread(mpa,skip = 1) %>%
        as.data.frame()
    }
  }
  # find for each row, to which depth of taxonomy it arrives (as integers)
  tax_lengths <- mpa$clade_name %>%
    strsplit("|", fixed = T) %>%
    sapply(length)
  # remove first element,we want to keep it
  tax_lengths <- tax_lengths[-1]
  # get integer equivalent of the taxonomic level we want
  tax_lvl_int <- match(tax_lvl, c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "SGB"))
  # subset the otu table, so to keep only UNASSIGNED on top and the rows exactly to te taxonomic level
  otu_cleaned <- mpa[c(T, tax_lengths == tax_lvl_int), 2:ncol(mpa)]
  
  if(!is.null(metadata)){
    inters_names <- intersect(colnames(otu_cleaned), rownames(metadata))
    if(verbose){
      if(length(colnames(otu_cleaned)[!(colnames(otu_cleaned) %in% inters_names)])!= 0){
        cat("Metaphlan table samples lost: ")
        cat(colnames(otu_cleaned)[!(colnames(otu_cleaned) %in% inters_names)], sep = " ")
        cat("\n")
        cat("\n")
      }
      
      if(length(rownames(metadata)[!(rownames(metadata) %in% inters_names)]) != 0){
        cat("Metadata table samples lost: ")
        cat(rownames(metadata)[!(rownames(metadata) %in% inters_names)], sep = " ")
        cat("\n")
        cat("\n")
      }
    }
    
    otu_cleaned <- otu_cleaned[, inters_names]
    metadata_cleaned <- metadata[match(inters_names, rownames(metadata)),,drop = FALSE]
    
  }
  
  # create taxonomy table, filling empty columns with NA
  
  taxonomy_tab <- mpa[c(T, tax_lengths == tax_lvl_int), 1] %>%
    as.data.frame() %>%
    set_names("clade_name") %>%
    separate(col="clade_name", into = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "SGB")[1:tax_lvl_int],sep = "\\|", fill = "right") %>%
    as.matrix()
  # rename the UNKNOWN clade as all unknown, otherwise we can't assign rownames
  taxonomy_tab[1,] <- rep("UNKNOWN",tax_lvl_int)
  
  #assign rownames to the tax level chosen
  rownames(taxonomy_tab) <- taxonomy_tab[,tax_lvl_int]
  rownames(otu_cleaned) <- rownames(taxonomy_tab)
  
  profiles <- phyloseq(otu_table(as.matrix(otu_cleaned), taxa_are_rows = TRUE),
                       tax_table(taxonomy_tab),
                       sample_data(metadata_cleaned, errorIfNULL = FALSE)
  )
  
  return(profiles)
}
