# title: genius_fig_7
# author: Sarah Blecksmith
# purpose: make nine cluster SOM

library(kohonen)
library(reshape2)
library(rcartocolor)
library(ggsci)


all_subject_24hr_data <- read.csv("data/all_subject_data_24hrs.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)


# prepare the data, "Argument 'data' should be a list of numeric vectors or matrices, or factors"
all_subject_24hr_data <- data.frame(all_subject_24hr_data, row.names = 1, check.names = FALSE)
all_subject_24hr_data <- mutate_if(all_subject_24hr_data, is.character, as.factor)

all_subject_24hr_data <- all_subject_24hr_data %>%
  mutate(delta_total_monos = case_when(delta_total_monos > 0 ~ 0,
                                       .default = delta_total_monos)) %>%
  mutate(delta_free_monos = case_when(delta_free_monos > 0 ~ 0,
                                      .default = delta_free_monos)) %>%
  mutate(delta_shannon = case_when(delta_shannon > 0 ~ 0,
                                   .default = delta_shannon))

all_subject_24hr_data$delta_total_monos <- abs(all_subject_24hr_data$delta_total_monos)
all_subject_24hr_data$delta_free_monos <- abs(all_subject_24hr_data$delta_free_monos)
all_subject_24hr_data$delta_shannon <- abs(all_subject_24hr_data$delta_shannon)

som_data <- all_subject_24hr_data[,4:12]
som_data <- som_data %>% select(-delta_total_scfa)
som_data <- som_data %>% select(-baseline_shannon)

som_data <- som_data[,c("delta_total_monos", "delta_free_monos", "delta_shannon","delta_lactate", "delta_acetate","delta_propionate", "delta_butyrate")]
som_data <- som_data %>%
  rename('change in total monos' = delta_total_monos,
         'change in free monos' = delta_free_monos,
         'change in diversity' = delta_shannon,
         'change in lactate' = delta_lactate,
         'change in acetate' = delta_acetate,
         'change in propionate' = delta_propionate,
         'change in butyrate' = delta_butyrate)
som_data <- as.matrix(som_data)

# need to run this as a block for the seed to work
set.seed(42)
som_grid <- somgrid(xdim=4, ydim=3, topo="hexagonal")
som_model <- som(scale(som_data), grid=som_grid, rlen = 600)
plot(som_model)

plot(som_model, type="changes")

par(mfrow = c(1, 2))
plot(som_model, type = "mapping", pchs = 20, main = "Samples in Nodes")
plot(som_model, main = "SOM Plot")

plot(som_model, type="count", main="Node Counts")
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")
plot(som_model, type="codes")

coolBlueHotRed <- function(n, alpha = 1) { rainbow(n, end=4/6, alpha=alpha)[n:1] }

# plot wss - Figure S9
code_data <- as.data.frame(som_model$codes) 
wss <- (nrow(code_data)-1)*sum(apply(code_data,2,var)) 
for (i in 2:11) {
  wss[i] <- sum(kmeans(code_data, centers=i)$withinss)
}

tiff(filename = "output/genius_figure_S9.tiff", width = 11, height = 6.5, units = "in", res = 300)
plot(wss)
dev.off()


# nine clusters
## use hierarchical clustering to cluster the codebook vectors
som_cluster9 <- cutree(hclust(dist(getCodes(som_model))), 9)

pal_npg("nrc")(10)
pretty_palette <- c("#B09C85FF","#4DBBD5FF", "#F39B7FFF","#00A087FF", "#E64B35FF", "#3C5488FF","#91D1C2FF", "#8491B4FF","#7E6148FF")
palette_wedges <- colorRampPalette(c(carto_pal(7, "Geyser")))

tiff(filename = "output/figure_7.tiff", width = 9, height = 7, units = "in", res = 300)
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster9], main = "", palette.name=palette_wedges)
dev.off()

# get vector with cluster value for each original data sample
cluster_assignment9 <- som_cluster9[som_model$unit.classif]
# for each of analysis, add the assignment as a column in the original data:
all_subject_24hr_data$cluster9 <- cluster_assignment9

ninecluster1 <- filter(all_subject_24hr_data[,c("Subject", "Treat", "cluster9")], cluster9 ==1)
ninecluster2 <- filter(all_subject_24hr_data[,c("Subject", "Treat", "cluster9")], cluster9 ==2)
ninecluster3 <- filter(all_subject_24hr_data[,c("Subject", "Treat", "cluster9")], cluster9 ==3)
ninecluster4 <- filter(all_subject_24hr_data[,c("Subject", "Treat", "cluster9")], cluster9 ==4)
ninecluster5 <- filter(all_subject_24hr_data[,c("Subject", "Treat", "cluster9")], cluster9 ==5)
ninecluster6 <- filter(all_subject_24hr_data[,c("Subject", "Treat", "cluster9")], cluster9 ==6)
ninecluster7 <- filter(all_subject_24hr_data[,c("Subject", "Treat", "cluster9")], cluster9 ==7)
ninecluster8 <- filter(all_subject_24hr_data[,c("Subject", "Treat", "cluster9")], cluster9 ==8)
ninecluster9 <- filter(all_subject_24hr_data[,c("Subject", "Treat", "cluster9")], cluster9 ==9)
