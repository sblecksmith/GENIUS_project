# title: 3_genius_phyloseq
# author: Sarah Blecksmith
# purpose: calculate shannon diversity for all fermentation samples

library(dplyr)
library(phyloseq)
library(microbiome)



taxonomy <- read.csv("data/taxonomy.tsv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, sep = "\t")
map <- read.csv("data/NIHBM_map.txt", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, sep = "\t")
feature_table <- read.csv("data/NIHBM_feature_table.txt", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, sep = "\t", skip =1)


taxonomy <- dplyr::rename(taxonomy, Feature_ID = `Feature ID`)
feature_table <- dplyr::rename(feature_table, OTU_ID = `#OTU ID`)

taxonomy_sep <- taxonomy %>%
  separate(Taxon,
           into = c("Domain","Phylum", "Class", "Order","Family", "Genus", "Species"),
           sep=";")

#for otu table and tax table, need to make rownames and turn into matrices
otu_mat <- data.frame(feature_table, row.names = 1, check.names = FALSE)
tax_mat <- data.frame(taxonomy_sep, row.names = 1, check.names = FALSE)
samples <- data.frame(map, row.names = 1, check.names = FALSE)


# set the order of sample_ID.  This may not matter for Phyloseq, but it does for DESeq2
otu_mat <- otu_mat[,order(as.numeric(names(otu_mat)))]


# Checking that the rows and columns are the same 
all(rownames(samples) %in% colnames(otu_mat)) # TRUE
all(rownames(samples) == colnames(otu_mat)) # TRUE


OTU = otu_table(as.matrix(otu_mat), taxa_are_rows = TRUE)
TAX = tax_table(as.matrix(tax_mat))
samples = sample_data(samples)
genius <- phyloseq(OTU, TAX, samples)

# remove uncharacterized phyla
genius0 <- subset_taxa(genius, !is.na(Phylum) & !Phylum %in% c("", "uncharacterized"))

# Compute prevalence of each feature, store as data.frame
prevdf = apply(X = otu_table(genius0),
               MARGIN = ifelse(taxa_are_rows(genius0), yes = 1, no = 2),
               FUN = function(x){sum(x > 0)})
# Add taxonomy and total read counts to this data.frame
prevdf = data.frame(Prevalence = prevdf,
                    TotalAbundance = taxa_sums(genius0),
                    tax_table(genius0))

prevalence = plyr::ddply(prevdf, "Phylum", function(df1){cbind(mean(df1$Prevalence),sum(df1$Prevalence))})

# Define phyla to filter
filterPhyla = c( " p__Basidiomycota", " p__Campilobacterota", " p__Deinococcota", " p__Incertae_Sedis", " p__Patescibacteria", " p__Phragmoplastophyta", " p__Synergistota")

# Filter entries with unidentified Phylum.
genius_filtered = subset_taxa(genius0, !Phylum %in% filterPhyla)

# remove the media and fiber controls
genius_filtered_subj <- subset_samples(genius_filtered, Subject != "NoSub") 

# calculating Shannon diversity
genius_alpha_div <- estimate_richness(genius_filtered_subj, split = TRUE, measure = "Shannon")
# saving shannon diversity to a file
samples_shannon <- map %>%
  select(SampleID, Day, Subject, Rep, Treat)
genius_alpha_div$SampleID <- rownames(genius_alpha_div)
samples_shannon <- merge(x = samples_shannon, y= genius_alpha_div, by = "SampleID")
write.csv(samples_shannon, "data/shannon_diversity.csv", row.names = FALSE)

# calculating evenness - Pielou's evenness metric
genius_pielou_div <- evenness(genius_filtered_subj,"pielou")
# saving to file
samples_pielou <- map %>%
  select(SampleID, Day, Subject, Rep, Treat)
genius_pielou_div$SampleID <- rownames(genius_pielou_div)
samples_pielou <- merge(x = samples_pielou, y= genius_pielou_div, by = "SampleID")
write.csv(samples_pielou, "data/pielou_evenness.csv", row.names = FALSE)

