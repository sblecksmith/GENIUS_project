# title: genius_fig_4
# author: Sarah Blecksmith
# purpose: make correlation matrix

library(phyloseq)
library(microViz)
library(dplyr)

all_subject_24hr_data <- read.csv("data/all_subject_data_24hrs.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
taxonomy <- read.csv("data/taxonomy.tsv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, sep = "\t")
map <- read.csv("data/2023_09_20_NIHBM_map.txt", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, sep = "\t")
feature_table <- read.csv("data/NIHBM_feature_table.txt", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, sep = "\t", skip =1)


taxonomy <- dplyr::rename(taxonomy, Feature_ID = `Feature ID`)
feature_table <- dplyr::rename(feature_table, OTU_ID = `#OTU ID`)

taxonomy_sep <- taxonomy %>%
  separate(Taxon,
           into = c("Domain","Phylum", "Class", "Order","Family", "Genus", "Species"),
           sep=";")


map$Subject <- gsub("0", "", map$Subject)
map <- map %>% filter(Day == "d1")
map <- map %>% mutate(matcher = paste(sep = "_", Day, Subject, Treat, Rp))
map <- map %>% filter(Subject != "NoSub" & Treat != "Ctl-S") # don't want controls
map <- map %>% filter(matcher != "d1_Sub3_Flax_C") # This sample doesn't have SCFA data
all_subject_24hr_data$Day = "d1"
all_subject_24hr_data <- all_subject_24hr_data %>% mutate(matcher = paste(sep = "_", Day, Subject, Treat, Rep))
map <- merge(x = map, y= all_subject_24hr_data[,c("matcher", "delta_total_monos", "delta_free_monos", "delta_total_scfa", "delta_acetate", "delta_propionate", "delta_butyrate", "delta_lactate")], by = "matcher", all.x = TRUE)
map = map %>% relocate(matcher, .after = delta_lactate) 

#for otu table and tax table, need to make rownames and turn into matrices
otu_mat <- data.frame(feature_table, row.names = 1, check.names = FALSE)
tax_mat <- data.frame(taxonomy_sep, row.names = 1, check.names = FALSE)
samples <- data.frame(map, row.names = 1, check.names = FALSE)


otu_mat <- otu_mat[,rownames(samples)]
otu_mat <- otu_mat[,order(as.numeric(names(otu_mat)))]
samples <- samples[order(as.numeric(rownames(samples))),]

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


# Define phyla to filter (eukaryotes and archaea)
filterPhyla = c( " p__Basidiomycota", " p__Incertae_Sedis", " p__Phragmoplastophyta", " p__Euryarchaeota")
genius_filtered = subset_taxa(genius0, !Phylum %in% filterPhyla)

genius_fixed <- genius_filtered %>%
  tax_fix(
    min_length = 4,
    unknowns = c(""),
    sep = " ", anon_unique = TRUE,
    suffix_rank = "classified") %>%
  tax_rename(
    rank = "Family", sort_by = prev, pad_digits = "max", sep = "-")

genius_fixed <- genius_fixed %>% 
  tax_fix(min_length = 4,
          unknowns = c(" c__uncultured", " f__uncultured", " g__uncultured", " o__uncultured", " o__uncultured Order"),
          sep = " ", anon_unique = TRUE,
          suffix_rank = "classified") 

genius_fixed <- genius_fixed %>% 
  tax_fix(min_length = 4,
          unknowns = c(" o__uncultured"),
          sep = " ", anon_unique = TRUE,
          suffix_rank = "classified")


my_samples <- sample_data(genius_fixed)

correlations_df <- genius_fixed %>% 
  tax_rename(rank = "Family", sort_by = prev, pad_digits = "max", sep = "-") %>%
  tax_filter(min_prevalence = 0.3, undetected =0, use_counts = TRUE) %>%
  tax_model(
    trans = "clr",
    rank = "Family", 
    variables = list("delta_total_monos", "delta_total_scfa", "delta_acetate", "delta_propionate","delta_butyrate", "delta_lactate"),
    type = microViz::cor_test, 
    method = "spearman", 
    return_psx = FALSE, verbose = FALSE) %>% 
  tax_models2stats(rank = "Family")

var_order <- c('total monos', 'total SCFA', 'acetate', 'propionate','butyrate', 'lactate')

correlations_df$taxon <- gsub(" f__", "", correlations_df$taxon)
monos_order <- subset(correlations_df, correlations_df$term == "delta_total_monos")
monos_order <- dplyr::arrange(monos_order, desc(estimate))
taxa_order <- monos_order$taxon

tiff(filename = "output/figure_4.tiff", width = 11, height = 6.5, units = "in", res = 600)
correlations_df %>% 
  mutate(p.FDR = p.adjust(p.value, method = "fdr")) %>% 
  mutate(term = case_when(term == "delta_total_monos" ~ 'total monos',
                          term == "delta_free_monos" ~ 'free monos',
                          term == "delta_total_scfa" ~ 'total SCFA',
                          term == "delta_lactate" ~ 'lactate',
                          term == "delta_acetate" ~ 'acetate',
                          term == "delta_propionate" ~ 'propionate',
                          term == "delta_butyrate" ~ 'butyrate')) %>%
  ggplot(aes(x = term, y = taxon)) +
  geom_raster(aes(fill = estimate)) +
  geom_point(
    data = function(x) filter(x, p.value < 0.05),
    shape = "asterisk") +
  geom_point(
    data = function(x) filter(x, p.FDR < 0.05),
    shape = "circle", size = 3) +
  scale_y_discrete(limits = taxa_order) +
  scale_x_discrete(limits = var_order, position = "bottom") +
  scale_fill_gradient2(low = "#004785",
                       mid = "white",
                       high = "#E64B35",
                       na.value = "grey50",
                       midpoint = 0) +
  labs(
    x = NULL, y = NULL, fill = "Spearman's\nRank\nCorrelation",
  caption = paste(
    "Asterisk indicates p < 0.05, not FDR adjusted",
    "Filled circle indicates FDR corrected p < 0.05", sep = "\n")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        text = element_text(size=18, color = "black"),
        axis.ticks.y = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank())
dev.off()


