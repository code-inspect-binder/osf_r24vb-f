
library(tidyverse)  # its like a dialect of R
library(stringr)
library(readxl)
library(xlsx)
library(ggplot2)
library("writexl")
library(dplyr)
library(tidyr)
library("Hmisc")
library(corrplot)
library(nFactors)
library(psych)
library("clValid")

###########  GET DATA #################

folder <- "C:/Users/Jamie/Documents/R/clustering types of syn"
raw_data <- read_excel(file.path(folder, "syn_types_for_R.xlsx"))


####### FILTER Note: syn types in columns 4:167

inclusive_data <- raw_data[raw_data$TOTAL >0, ]  ## need to delete people with no types of syn at all!
inclusive_data <- inclusive_data[inclusive_data$Age >17, ]  ## people above 18 and remove demographics

mean(inclusive_data$Age)
range(inclusive_data$Age)
sd(inclusive_data$Age)
table(inclusive_data$Sex)
table(inclusive_data$hearing_motion)
table(inclusive_data$mirror_touch)

# Prevalence and filter common types

prevalence_inclusive <- colMeans(inclusive_data[4:167], na.rm=TRUE)  # how common each type is in the database
hist(prevalence_inclusive)

prevalence_common <- prevalence_inclusive[prevalence_inclusive >.2]
common_types <- inclusive_data[names(prevalence_common)]


cor_matrix <- cor(common_types, method = "pearson", use = "complete.obs")  

heatmap(x = cor_matrix, symm = TRUE, cexRow = .9, cexCol = .9, keep.dendro = FALSE)  


############ CORRELATIONS INCLUSIVE DATASET #################


cor_matrix <- cor(inclusive_data[4:167], method = "pearson", use = "complete.obs")  
round(cor_matrix,2)

# display the correlations as a histogram and heatmap

cor_matrix_half <- cor_matrix[upper.tri(cor_matrix)]
mean(cor_matrix_half)
sd(cor_matrix_half)

hist(as.vector(cor_matrix_half), breaks=24, cex.axis=2)  # Note: Novich et al. suppressed correlations of r<.4 in their visualisation
heatmap(x = cor_matrix, symm = TRUE)  


############# CLUSTERING similar correlations together ################

distances <- dist(cor_matrix, method = "euclidean", diag = FALSE, upper = FALSE)
clusters <- hclust(distances)   # note: tightest clusters appear on the left, defaults to the "complete" method

# full dendogram
plot(clusters, cex=.6)

# pruned dendogram
hcd = as.dendrogram(clusters)
plot(cut(hcd, h = 2.2)$upper, leaflab="none", ylim=c(2,4), cex.axis=2)


########  SCREE PLOT ##############

x <- length(clusters$height)

Dendogram_Height=0
for (i in 2:x) Dendogram_Height[i] <- clusters$height[i-1]
     
plot(x:1, Dendogram_Height, type="b", xlab = "sequence of merging", ylab = "Dendogram height", cex.axis=2)


#### Create stringent dataset ############

########## Exclude People with Strange Response Patterns ################
# These are identified as sub-clusters in the inclusive dendogram that group together

inclusive_data$weird_responses = inclusive_data$body_postures_shape + inclusive_data$punctuation_shape + inclusive_data$letter_shape + inclusive_data$number_shape + inclusive_data$people_name_shape + inclusive_data$english_word_shape + inclusive_data$foreign_word_shape + inclusive_data$tastes_taste + inclusive_data$smells_smell + inclusive_data$noises_noise + inclusive_data$music_music + inclusive_data$colour_colour + inclusive_data$shapes_shape + inclusive_data$smells_taste + inclusive_data$tastes_smell + inclusive_data$voices_noise + inclusive_data$voices_music + inclusive_data$noises_music + inclusive_data$music_noise

hist(inclusive_data$weird_responses)
table(inclusive_data$weird_responses)

# delete people from the upper 5% tail of weird responses

cutoff_95percent = quantile(inclusive_data$weird_responses, .95, na.rm = TRUE)

stringent_data <- inclusive_data[inclusive_data$weird_responses <= cutoff_95percent, ]

# delete columns for weird responses and add "touch-touch", "pain-pain", "touch-pain", "pain-touch"

weird <- c("body_postures_shape", "punctuation_shape", "letter_shape", "number_shape", "people_name_shape", "english_word_shape", "foreign_word_shape", "tastes_taste", "smells_smell", "noises_noise", "music_music", "colour_colour", "shapes_shape", "smells_taste", "tastes_smell", "voices_noise", "voices_music", "noises_music", "music_noise", "touch_touch", "pain_pain", "touch_pain", "pain_touch")

myvars <- names(stringent_data) %in% weird 

stringent_data <- stringent_data[!myvars]


# delete very rare types of synaesthesia with a prevalence of less then 1 percent

cutoff_1percent = nrow(stringent_data) /100

types_to_keep <- stringent_data[4:(ncol(stringent_data)-2)]

types_to_keep <- types_to_keep[, colSums(types_to_keep, na.rm = TRUE) > cutoff_1percent]

stringent_data <- cbind(stringent_data[1:3],types_to_keep)

## Prevalence within the stringent dataset

colSums(stringent_data[4:ncol(stringent_data)], na.rm = TRUE)

## need to delete people with no types of syn at all!

stringent_data <- stringent_data[ rowSums(stringent_data[4:ncol(stringent_data)], na.rm = TRUE) >0, ]  

# data summary stringent set

mean(stringent_data$Age)
range(stringent_data$Age)
sd(stringent_data$Age)
table(stringent_data$Sex)


# Prevalence and filter common types

prevalence_stringent <- colMeans(stringent_data[4:ncol(stringent_data)], na.rm=TRUE)  # how common each type is in the database
hist(prevalence_stringent)


############ CORRELATIONS STRINGENT DATA #################

# x can be a data frame, returns the r value

cor_matrix <- cor(stringent_data[4:ncol(stringent_data)], method = "pearson", use = "complete.obs")  
round(cor_matrix,2)


# display the correlations as a histogram

cor_matrix_half <- cor_matrix[upper.tri(cor_matrix)]
mean(cor_matrix_half)
sd(cor_matrix_half)

heatmap(x = cor_matrix, symm = TRUE)

hist(as.vector(cor_matrix_half), breaks=24, cex.axis=2)  # Note: Novich et al. suppressed correlations of r<.4 in their visualisation

# cluster similar correlations together

distances <- dist(cor_matrix, method = "euclidean", diag = FALSE, upper = FALSE)

clusters <- hclust(distances)

clusters$labels[clusters$order]

# full dendogram
plot(clusters, cex=.7)

# pruned dendogram
hcd = as.dendrogram(clusters)
plot(cut(hcd, h = 2)$upper, leaflab="none", ylim=c(2,3), cex.axis=2)

########  SCREE PLOT ##############

x <- length(clusters$height)

Dendogram_Height=0
for (i in 2:x) Dendogram_Height[i] <- clusters$height[i-1]

plot(x:1, Dendogram_Height, type="b", xlab = "sequence of merging", ylab = "Dendogram height", cex.axis=2)


########  CALCULATE NUMBER TYPES OF SYN EACH HAS DEPENDING ON NUMBER CLUSTERS EXTRACTED  ##############

stringent_N_clusters <- stringent_data[1:3]

for (i in 2:80) {
  
  # divides the dendogram into i branches: each type of syn is then assigned a cluster number 
  cluster_groups <- cutree(clusters, k=i)
  
  # different types of synaesthesia falling under the same cluster number are grouped
  short_data <- sapply(split.default(stringent_data[4:ncol(stringent_data)], cluster_groups), rowMeans, na.rm=TRUE)
  
  # generates true/false depending on whether each cluster of syn is present in an individual
  short_binary <- short_data>0  
  
  # each cluster is given a binary (1/0) value depending on whether it is present in an individual
  short_data[short_binary] <- 1
 
  # gives the prevalence of each cluster of data
  colMeans(short_data)
   
  N_types <- matrix(apply(short_data[,1:i], 1, sum, na.rm=TRUE))
  
  stringent_N_clusters <- cbind(stringent_N_clusters,N_types)
  
}

# gives average number of types of syn per person as a function of the theoretical maximum number of types
plot(colMeans(stringent_N_clusters[4:i]))


############# FACTOR ANALYSIS #####################

# Maximum Likelihood Factor Analysis
# entering raw data and extracting N factors,
# with varimax rotation

mydata <- na.omit(stringent_data[4:ncol(stringent_data)])

# Determine Number of Factors to Extract

ev <- eigen(cor_matrix) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.01)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# Kaiser-Meyer-Olkin factor adequacy
KMO(cor_matrix)

# Run exploratory factor analysis
fit <- factanal(mydata, 21, rotation="varimax")


options(max.print=1000000)
ld <- loadings(fit)

write_xlsx(data.frame(ld[1:112,1:21]), "C:/Users/Jamie/Documents/factor_loadings.xlsx")

capture.output(ld[1:112,1:7], file = "yourfile.txt") 


######### STUDY TWO #####################################
########## LINK NUM TYPES TO COGNITION ###################


imagery_data <- read_excel(file.path(folder, "mental_imagery_data.xlsx"))

df2 <- stringent_N_clusters
df2$ID <- stringent_N_clusters$'Part ID'

# link the imagery data to the data above which calculates the number of clusters each person has
df3 <- merge(x = imagery_data, y = df2, by = "ID", all.x = FALSE, no.dups = TRUE)

mean(df3$Age)
sd(df3$Age)
table(df3$Sex)

to_remove <-names(df3) %in% c("Age", "Sex","Part ID") 
df3 <- df3[!to_remove]

# remember that N_types goes from 2, 3, 4, 5 types etc. (i.e. not starting from zero) 

imagery_matrix <- cor(df3[7:ncol(df3)], method = "pearson", use = "complete.obs")  
imagery <- round(imagery_matrix[,1],3)
write_xlsx(data.frame(imagery), "C:/Users/Jamie/Documents/imagery_cluster_cor.xlsx")


GSQ_data <- read_excel(file.path(folder, "GSQ_data.xlsx"))

df3 <- merge(x = GSQ_data, y = df2, by = "ID", all.x = FALSE, no.dups = TRUE)

mean(df3$Age)
sd(df3$Age)
table(df3$Sex)

to_remove <-names(df3) %in% c("Age", "Sex","Part ID") 
df3 <- df3[!to_remove]

GSQ_matrix <- cor(df3[2:ncol(df3)], method = "pearson", use = "complete.obs")  
GSQ <- round(GSQ_matrix[,1],3)

write_xlsx(data.frame(GSQ), "C:/Users/Jamie/Documents/GSQ_cluster_cor.xlsx")


clan <- read_excel(file.path(folder, "clan_scores.xlsx"))

df3 <- merge(x = clan, y = df2, by = "ID", all.x = FALSE, no.dups = TRUE)

mean(df3$Age)
sd(df3$Age)
table(df3$Sex)

to_remove <-names(df3) %in% c("Age", "Sex","Part ID") 
df3 <- df3[!to_remove]


clan_matrix <- cor(df3[2:ncol(df3)], method = "pearson", use = "complete.obs")
clan_total <- round(clan_matrix[4,],3)

write_xlsx(data.frame(clan_total), "C:/Users/Jamie/Documents/clan_cluster_cor.xlsx")


######### jack-knife approach #################

# this file contains information about the 7 clusters from the stringent dataset plus hearing-motion, mirror-touch and tickertape
ten_types <- read_excel(file.path(folder, "ten_types.xlsx"))

hist(ten_types$total)
table(ten_types$total)
summary(ten_types$total)
mean(ten_types$total)
sd(ten_types$total, na.rm=TRUE)


ten_types$minus_personification <- ten_types$total - ten_types$personification
ten_types$minus_language_colour <- ten_types$total - ten_types$language_colour
ten_types$minus_language_taste <- ten_types$total - ten_types$language_taste
ten_types$minus_language_touch <- ten_types$total - ten_types$language_touch
ten_types$minus_sequence_space <- ten_types$total - ten_types$sequence_space
ten_types$minus_smell_taste <- ten_types$total - ten_types$smell_taste_concurrents
ten_types$minus_visualised_sensations <- ten_types$total - ten_types$visualised_sensations
ten_types$minus_hm <- ten_types$total - ten_types$hearing_motion
ten_types$minus_mts <- ten_types$total - ten_types$mirror_touch
ten_types$minus_tickertape <- ten_types$total - ten_types$tickertape


ten_types_correlation <- cor(ten_types[2:ncol(ten_types)], method = "pearson", use = "pairwise.complete.obs")
write_xlsx(data.frame(ten_types_correlation), "C:/Users/Jamie/Documents/ten_types_cor.xlsx")


#### simulate non-synaesthete data by adding zeroes

cor(x=ten_types$language_colour, y=ten_types$sequence_space, method = "pearson", use = "complete.obs")  
# note the value is a little different from before because the previous one excluded na.rm on a row-wise basis

zero_5_ratio = sample (c(0), length(ten_types$language_colour)*5, replace = T)
cor(x=c(ten_types$language_colour, zero_5_ratio), y=c(ten_types$sequence_space, zero_5_ratio), method = "pearson", use = "complete.obs")

zero_10_ratio = sample (c(0), length(ten_types$language_colour)*10, replace = T)
cor(x=c(ten_types$language_colour, zero_10_ratio), y=c(ten_types$sequence_space, zero_10_ratio), method = "pearson", use = "complete.obs")

zero_20_ratio = sample (c(0), length(ten_types$language_colour)*20, replace = T)
cor(x=c(ten_types$language_colour, zero_20_ratio), y=c(ten_types$sequence_space, zero_20_ratio), method = "pearson", use = "complete.obs")


# Explore how these ten clusters relate to each of the measures

df3 <- merge(x = imagery_data, y = ten_types, by = "ID", all.x = FALSE, no.dups = TRUE)
imagery_type_correlation <- cor(df3[2:ncol(df3)], method = "pearson", use = "complete.obs")
write_xlsx(data.frame(imagery_type_correlation), "C:/Users/Jamie/Documents/imagery_type_cor.xlsx")

df3 <- merge(x = GSQ_data, y = ten_types, by = "ID", all.x = FALSE, no.dups = TRUE)
GSQ_type_correlation <- cor(df3[2:ncol(df3)], method = "pearson", use = "complete.obs")
write_xlsx(data.frame(GSQ_type_correlation), "C:/Users/Jamie/Documents/GSQ_type_cor.xlsx")

df3 <- merge(x = clan, y = ten_types, by = "ID", all.x = FALSE, no.dups = TRUE)
clan_correlation <- cor(df3[2:ncol(df3)], method = "pearson", use = "complete.obs")
write_xlsx(data.frame(clan_correlation), "C:/Users/Jamie/Documents/clan_type_cor.xlsx")


#####################  END #####################################################


