data <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/box_advanced.csv")
library(tidyverse)
library(mosaic)
library(ggrepel)
library(ggimage)
tally(data$PositionId)
favstats(data$MP_PG) ## 12.4688 is Q1
favstats(data$GP) ## 28 is Q1
data <- data %>%
  mutate(
    position_words = case_when(
      PositionId == 1 ~ "Lead Guard",
      PositionId == 5 ~ "Center",
      PositionId == 12 ~ "Combo Guard",
      PositionId == 23 ~ "Wing",
      PositionId == 34 ~ "Wing",
      PositionId == 45 ~ "Stretch Big"
    )
  ) %>%
  filter(MP_PG >= 12.4688 & GP >= 28) %>%
  mutate(
    T3Freq = ATBFreq+C3Freq
  )

guards <- data %>%
  filter(PositionId == 1 | PositionId == 12)

wings <- data %>%
  filter(PositionId == 23 | PositionId == 34)

bigs <- data %>%
  filter(PositionId == 5 | PositionId == 45)


###########################
#Clustering Offensive Wings
###########################
wings <- wings %>%
  mutate(scaled_USG = scale(USG), scaled_eFG = scale(eFG)) %>%
  mutate(scaled_STLP = scale(STLP), scaled_BLKP = scale(BLKP))

distance_wings_offense <- dplyr::select(wings, scaled_USG, scaled_eFG) %>%
  dist()

matrix_wings_offense <- as.matrix(distance_wings_offense)

hier_clust_wings_offense <- hclust(distance_wings_offense, method = 'complete')
plot(hier_clust_wings_offense, labels = wings$PersonName)

wings_offense_cluster_hier <- cutree(hier_clust_wings_offense, k = 5)
wings$offense_cluster <- as.factor(wings_offense_cluster_hier)
ggplot(data = wings) + geom_point(aes(x=USG, y = eFG, color = offense_cluster)) +
  ylab("Effective FG%") + xlab("Usage Rate") + ggtitle("Offensive Clustering for Wings") 

wings_getsball_sucks <- wings %>%
  filter(offense_cluster == 1)

wings_studs <- wings %>%
  filter(offense_cluster == 2)

wings_sucks <- wings %>%
  filter(offense_cluster == 3)

wings_average <- wings %>%
  filter(offense_cluster == 4)

wings_bucket <- wings %>%
  filter(offense_cluster == 5)
#######################
#Clustering Defensive Wings (Offense Cluster 1)
#######################
distance_wings_defense1 <- dplyr::select(wings_getsball_sucks, scaled_STLP, scaled_BLKP) %>%
  dist()

matrix_wings_defense1 <- as.matrix(distance_wings_defense1)

hier_clust_wings_defense1 <- hclust(distance_wings_defense1, method = "complete")
plot(hier_clust_wings_defense1)
wings_defense_cluster_hier1 <- cutree(hier_clust_wings_defense1, k = 4)
wings_getsball_sucks$defense_cluster <- as.factor(wings_defense_cluster_hier1)
ggplot(data = wings_getsball_sucks) + geom_point(aes(x=STLP, y = BLKP, color = defense_cluster)) +
  ylab("Block Percentage") + xlab("Steal Percentage")

wings_usedbutbad_averagedefense <- wings_getsball_sucks %>%
  filter(defense_cluster == 1)

wings_usedbutbad_elitedefense <- wings_getsball_sucks %>%
  filter(defense_cluster == 2)

wings_usedbutbad_baddefense <- wings_getsball_sucks %>%
  filter(defense_cluster == 3)

wings_usedbutbad_highsteal <- wings_getsball_sucks %>%
  filter(defense_cluster == 4)

#######################
#Clustering Defensive Wings (Offense Cluster 2)
#######################
distance_wings_defense1 <- dplyr::select(wings_studs, scaled_STLP, scaled_BLKP) %>%
  dist()

matrix_wings_defense1 <- as.matrix(distance_wings_defense1)

hier_clust_wings_defense1 <- hclust(distance_wings_defense1, method = "complete")
plot(hier_clust_wings_defense1)
wings_defense_cluster_hier1 <- cutree(hier_clust_wings_defense1, k = 4)
wings_studs$defense_cluster <- as.factor(wings_defense_cluster_hier1)
ggplot(data = wings_studs) + geom_point(aes(x=STLP, y = BLKP, color = defense_cluster)) +
  ylab("Block Percentage") + xlab("Steal Percentage")
wings_studs_baddefense <- wings_studs %>%
  filter(defense_cluster == 1)

wings_studs_blocks <- wings_studs %>%
  filter(defense_cluster == 2)

wings_studs_steals <- wings_studs %>%
  filter(defense_cluster == 3)

wings_studs_elitedefense <- wings_studs %>%
  filter(defense_cluster == 4)


#######################
#Clustering Defensive Wings (Offense Cluster 3)
#######################
distance_wings_defense1 <- dplyr::select(wings_sucks, scaled_STLP, scaled_BLKP) %>%
  dist()

matrix_wings_defense1 <- as.matrix(distance_wings_defense1)

hier_clust_wings_defense1 <- hclust(distance_wings_defense1, method = "complete")
plot(hier_clust_wings_defense1)
wings_defense_cluster_hier1 <- cutree(hier_clust_wings_defense1, k = 4)
wings_sucks$defense_cluster <- as.factor(wings_defense_cluster_hier1)
ggplot(data = wings_sucks) + geom_point(aes(x=STLP, y = BLKP, color = defense_cluster)) +
  ylab("Block Percentage") + xlab("Steal Percentage")

wings_badoffense_baddefense <- wings_sucks %>%
  filter(defense_cluster == 1)

wings_badoffense_averagedefense <- wings_sucks %>%
  filter(defense_cluster == 2)

wings_badoffense_highsteals <- wings_sucks %>%
  filter(defense_cluster == 3)

wings_badoffense_highblocks <- wings_sucks %>%
  filter(defense_cluster == 4)


#######################
#Clustering Defensive Wings (Offense Cluster 4)
#######################
distance_wings_defense1 <- dplyr::select(wings_average, scaled_STLP, scaled_BLKP) %>%
  dist()

matrix_wings_defense1 <- as.matrix(distance_wings_defense1)

hier_clust_wings_defense1 <- hclust(distance_wings_defense1, method = "complete")
plot(hier_clust_wings_defense1)
wings_defense_cluster_hier1 <- cutree(hier_clust_wings_defense1, k = 3)
wings_average$defense_cluster <- as.factor(wings_defense_cluster_hier1)
ggplot(data = wings_average) + geom_point(aes(x=STLP, y = BLKP, color = defense_cluster)) +
  ylab("Block Percentage") + xlab("Steal Percentage")

wings_average_averagedefense <- wings_average %>%
  filter(defense_cluster == 1)

wings_average_baddefense <- wings_average %>%
  filter(defense_cluster == 2)

wings_average_elitedefense <- wings_average %>%
  filter(defense_cluster == 3)
#######################
#Clustering Defensive Wings (Offense Cluster 5)
#######################
distance_wings_defense1 <- dplyr::select(wings_bucket, scaled_STLP, scaled_BLKP) %>%
  dist()

matrix_wings_defense1 <- as.matrix(distance_wings_defense1)

hier_clust_wings_defense1 <- hclust(distance_wings_defense1, method = "complete")
plot(hier_clust_wings_defense1)
wings_defense_cluster_hier1 <- cutree(hier_clust_wings_defense1, k = 4)
wings_bucket$defense_cluster <- as.factor(wings_defense_cluster_hier1)
ggplot(data = wings_bucket) + geom_point(aes(x=STLP, y = BLKP, color = defense_cluster)) +
  ylab("Block Percentage") + xlab("Steal Percentage") + ggtitle("3 and D Clustering")

wings_effectivenouse_baddefense <- wings_bucket %>%
  filter(defense_cluster == 1)

wings_effectivenouse_highsteals <- wings_bucket %>%
  filter(defense_cluster == 2)

wings_effectivenouse_elitedefense <- wings_bucket %>%
  filter(defense_cluster == 3)

wings_effectivenouse_highblocks <- wings_bucket %>%
  filter(defense_cluster == 4)

##################################################
#C Scree plot
#################################################

Cn = (nrow(wings[2:14])-1)*sum(apply(wings[2:14],2,var)) 
for (i in 2:13) Cn[i] <- sum(kmeans(wings[2:14], centers=i)$withinss) 
plot(1:13, Cn, type="b", main="C Scree Plot", xlab="Number of Clusters", ylab="Variance (Sum of Squares within Clusters)")



