data <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/NBA Project/box_advanced.csv")
three_point <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/NBA Project/three_point_shots.csv")
library(tidyverse)
library(mosaic)
library(ggrepel)
library(ggimage)
library(scatterplot3d)
three_point$X3FGP_temp <- three_point$X3FGM/three_point$X3FGA
three_point <- three_point %>%
  mutate(
    X3FGP = case_when(
      is.na(X3FGP_temp) ~ 0,
      !is.na(X3FGP_temp) ~ X3FGP_temp
    )
  )

full_data <- merge(data, three_point, by = "PersonName")

bigs <- data %>%
  filter(PositionId == 5 | PositionId == 45) %>%
  filter(MP_PG >= 12.4688 & GP >= 28)

plot(bigs$X3FGP~bigs$X3FGA)
plot(bigs$MRFGP~bigs$MRFreq)
###########################
#Clustering Offensive Bigs
###########################
bigs <- bigs %>%
  mutate(scaled_RimFreq = scale(RimFreq), scaled_X3R = scale(X3R), scaled_MRFreq = scale(MRFreq)) %>%
  mutate(scaled_ORP = scale(ORP), scaled_DRP = scale(DRP), scaled_BLKP = scale(BLKP))

distance_bigs_offense <- dplyr::select(bigs, scaled_RimFreq, scaled_X3R, scaled_MRFreq) %>%
  dist()

matrix_bigs_offense <- as.matrix(distance_bigs_offense)

hier_clust_bigs_offense <- hclust(distance_bigs_offense, method = 'complete')
scree_bigs = (nrow(bigs[2:14])-1)*sum(apply(bigs[2:14],2,var))
for (i in 2:13) scree_bigs[i] <- sum(kmeans(bigs[2:14], centers = i)$withinss)
plot(1:13, scree_bigs , type = "b", main = "C Scree Plot", xlab = "Number of Clusters", ylab = "Variance Within Clusters")
bigs_offense_cluster_hier <- cutree(hier_clust_bigs_offense, k = 3)
bigs$offense_cluster <- as.factor(bigs_offense_cluster_hier)
scatterplot3d(bigs$RimFreq, bigs$MRFreq, bigs$X3R, color = bigs$offense_cluster,angle = 60)

bigs_everybutmostly3 <- bigs %>%
  filter(offense_cluster == 1)

bigs_nothrees<- bigs %>%
  filter(offense_cluster == 2)

bigs_onlyshotsatrim<- bigs %>%
  filter(offense_cluster == 3)

##########################
#Clustering Defensive Bigs (OC 1)
##########################
distance_bigs_defense <- dplyr::select(bigs_everybutmostly3, scaled_ORP, scaled_DRP, scaled_BLKP) %>%
  dist()

matrix_bigs_defense <- as.matrix(distance_bigs_defense)

hier_clust_bigs_defense <- hclust(distance_bigs_defense, method = 'complete')
scree_bigs = (nrow(bigs_everybutmostly3[2:14])-1)*sum(apply(bigs_everybutmostly3[2:14],2,var))
for (i in 2:13) scree_bigs[i] <- sum(kmeans(bigs_everybutmostly3[2:14], centers = i)$withinss)
plot(1:13, scree_bigs , type = "b", main = "C Scree Plot", xlab = "Number of Clusters", ylab = "Variance Within Clusters")
bigs_defense_cluster_hier <- cutree(hier_clust_bigs_defense, k = 4)
bigs_everybutmostly3$defense_cluster <- as.factor(bigs_defense_cluster_hier)
scatterplot3d(bigs_everybutmostly3$ORP, bigs_everybutmostly3$DRP, bigs_everybutmostly3$BLKP, color = bigs_everybutmostly3$defense_cluster)

bigs_everybutmostly3_lowrebounds <- bigs_everybutmostly3 %>%
  filter(defense_cluster == 1)

bigs_everybutmostly3_highrebounds <- bigs_everybutmostly3 %>%
  filter(defense_cluster == 2)

bigs_everybutmostly3_blocksandrebounds <- bigs_everybutmostly3 %>%
  filter(defense_cluster == 3)

bigs_everybutmostly3_onlyblocks <- bigs_everybutmostly3 %>%
  filter(defense_cluster == 4)

##########################
#Clustering Defensive Bigs (OC 2) Scree plot issue - using 2 clusters gives a single player cluster
##########################
distance_bigs_defense <- dplyr::select(bigs_nothrees, scaled_ORP, scaled_DRP, scaled_BLKP) %>%
  dist()

matrix_bigs_defense <- as.matrix(distance_bigs_defense)

hier_clust_bigs_defense <- hclust(distance_bigs_defense, method = 'complete')
scree_bigs = (nrow(bigs_nothrees[2:14])-1)*sum(apply(bigs_nothrees[2:14],2,var))
for (i in 2:13) scree_bigs[i] <- sum(kmeans(bigs_nothrees[2:14], centers = i)$withinss)
plot(1:13, scree_bigs , type = "b", main = "C Scree Plot", xlab = "Number of Clusters", ylab = "Variance Within Clusters")
bigs_defense_cluster_hier <- cutree(hier_clust_bigs_defense, k = 3)
bigs_nothrees$defense_cluster <- as.factor(bigs_defense_cluster_hier)
scatterplot3d(bigs_nothrees$ORP, bigs_nothrees$DRP, bigs_nothrees$BLKP, color = bigs_nothrees$defense_cluster)

bigs_nothrees_rebounds <- bigs_nothrees %>%
  filter(defense_cluster == 1)

bigs_nothrees_nothingtosee <- bigs_nothrees %>%
  filter(defense_cluster == 2)

bigs_nothrees_highblock <- bigs_nothrees %>%
  filter(defense_cluster == 3)

##########################
#Clustering Defensive Bigs (OC 3)
##########################
distance_bigs_defense <- dplyr::select(bigs_onlyshotsatrim, scaled_ORP, scaled_DRP, scaled_BLKP) %>%
  dist()

matrix_bigs_defense <- as.matrix(distance_bigs_defense)

hier_clust_bigs_defense <- hclust(distance_bigs_defense, method = 'complete')
scree_bigs = (nrow(bigs_onlyshotsatrim[2:14])-1)*sum(apply(bigs_onlyshotsatrim[2:14],2,var))
for (i in 2:13) scree_bigs[i] <- sum(kmeans(bigs_onlyshotsatrim[2:14], centers = i)$withinss)
plot(1:13, scree_bigs , type = "b", main = "C Scree Plot", xlab = "Number of Clusters", ylab = "Variance Within Clusters")
bigs_defense_cluster_hier <- cutree(hier_clust_bigs_defense, k = 3)
bigs_onlyshotsatrim$defense_cluster <- as.factor(bigs_defense_cluster_hier)
scatterplot3d(bigs_onlyshotsatrim$ORP, bigs_onlyshotsatrim$DRP, bigs_onlyshotsatrim$BLKP, color = bigs_onlyshotsatrim$defense_cluster)

bigs_onlyshotsatrim_notasgood <- bigs_onlyshotsatrim %>%
  filter(defense_cluster == 1)

bigs_onlyshotsatrim_defenserebound <- bigs_onlyshotsatrim %>%
  filter(defense_cluster == 2)

bigs_onlyshotsatrim_highblock <- bigs_onlyshotsatrim %>%
  filter(defense_cluster == 3)



