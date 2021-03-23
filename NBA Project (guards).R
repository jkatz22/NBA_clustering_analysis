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
  filter(MP_PG >= 12.4688 & GP >= 28)
guards <- data %>%
  filter(PositionId == 1 | PositionId == 12)

wings <- data %>%
  filter(PositionId == 23 | PositionId == 34)

bigs <- data %>%
  filter(PositionId == 5 | PositionId == 45)


###########################
#Clustering Offensive Guards
###########################
guards <- guards %>%
  mutate(scaled_USG = scale(USG), scaled_eFG = scale(eFG)) %>%
  mutate(scaled_STLP = scale(STLP), scaled_BLKP = scale(BLKP))

distance_guards_offense <- dplyr::select(guards, scaled_USG, scaled_eFG) %>%
  dist()

matrix_guards_offense <- as.matrix(distance_guards_offense)

hier_clust_guards_offense <- hclust(distance_guards_offense, method = 'complete')
plot(hier_clust_guards_offense, labels = guards$PersonName)

guards_offense_cluster_hier <- cutree(hier_clust_guards_offense, k = 4)
guards$offense_cluster <- as.factor(guards_offense_cluster_hier)
ggplot(data = guards) + geom_point(aes(x=USG, y = eFG, color = offense_cluster)) +
  ylab("Effective FG%") + xlab("Usage Rate") 

guards_shoot_average <- guards %>%
  filter(offense_cluster == 1)

guards_highshot_nouse <- guards %>%
  filter(offense_cluster == 2)

guards_sucks <- guards %>%
  filter(offense_cluster == 3)

guards_studs <- guards %>%
  filter(offense_cluster == 4)


#######################
#Clustering Defensive Guards (Offense Cluster 1)
#######################
distance_guards_defense1 <- dplyr::select(guards_shoot_average, scaled_STLP, scaled_BLKP) %>%
  dist()

matrix_guards_defense1 <- as.matrix(distance_guards_defense1)

hier_clust_guards_defense1 <- hclust(distance_guards_defense1, method = "complete")
plot(hier_clust_guards_defense1)
guards_defense_cluster_hier1 <- cutree(hier_clust_guards_defense1, k = 4)
guards_shoot_average$defense_cluster <- as.factor(guards_defense_cluster_hier1)
ggplot(data = guards_shoot_average) + geom_point(aes(x=STLP, y = BLKP, color = defense_cluster)) +
  ylab("Block Percentage") + xlab("Steal Percentage")

guards_averageshot_baddefense <- guards_shoot_average %>%
  filter(defense_cluster == 1)

guards_averageshot_averagedefense <- guards_shoot_average %>%
  filter(defense_cluster == 2)

guards_averageshot_highsteals <- guards_shoot_average %>%
  filter(defense_cluster == 3)

guards_averageshot_elitedefense <- guards_shoot_average %>%
  filter(defense_cluster == 4)

#######################
#Clustering Defensive Guards (Offense Cluster 2)
#######################
distance_guards_defense1 <- dplyr::select(guards_highshot_nouse, scaled_STLP, scaled_BLKP) %>%
  dist()

matrix_guards_defense1 <- as.matrix(distance_guards_defense1)

hier_clust_guards_defense1 <- hclust(distance_guards_defense1, method = "complete")
plot(hier_clust_guards_defense1)
guards_defense_cluster_hier1 <- cutree(hier_clust_guards_defense1, k = 4)
guards_highshot_nouse$defense_cluster <- as.factor(guards_defense_cluster_hier1)
ggplot(data = guards_highshot_nouse) + geom_point(aes(x=STLP, y = BLKP, color = defense_cluster)) +
  ylab("Block Percentage") + xlab("Steal Percentage")

guards_efficientnousage_highblock <- guards_highshot_nouse %>%
  filter(defense_cluster == 1)

guards_efficientnousage_highsteal <- guards_highshot_nouse %>%
  filter(defense_cluster == 2)

guards_efficientnousage_averagesteal <- guards_highshot_nouse %>%
  filter(defense_cluster == 3)

guards_efficientnousage_baddefense <- guards_highshot_nouse %>%
  filter(defense_cluster == 4)


#######################
#Clustering Defensive Guards (Offense Cluster 3)
#######################
distance_guards_defense1 <- dplyr::select(guards_sucks, scaled_STLP, scaled_BLKP) %>%
  dist()

matrix_guards_defense1 <- as.matrix(distance_guards_defense1)

hier_clust_guards_defense1 <- hclust(distance_guards_defense1, method = "complete")
plot(hier_clust_guards_defense1)
guards_defense_cluster_hier1 <- cutree(hier_clust_guards_defense1, k = 3)
guards_sucks$defense_cluster <- as.factor(guards_defense_cluster_hier1)
ggplot(data = guards_sucks) + geom_point(aes(x=STLP, y = BLKP, color = defense_cluster)) +
  ylab("Block Percentage") + xlab("Steal Percentage")

guards_badoffense_elitedefense <- guards_sucks %>%
  filter(defense_cluster == 1)

guards_badoffense_averagedefense <- guards_sucks %>%
  filter(defense_cluster == 2)

guards_badoffense_cantblock <- guards_sucks %>%
  filter(defense_cluster == 3)



