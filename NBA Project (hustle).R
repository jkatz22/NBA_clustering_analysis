data <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/NBA Project/box_advanced.csv")
data <- data %>%
  filter(MP_PG >= 12.4688 & GP >= 28)
hustle_data <- data %>%
  mutate(scaled_STLP = scale(STLP), scaled_RimFreq = scale(RimFreq)) %>%
  mutate(
    hustle_known = ifelse(
      PersonName == "Chris Paul" | PersonName == "Kawhi Leonard" | 
        PersonName == "Russell Westbrook" | PersonName == "Jimmy Butler" | 
        PersonName ==  "John Wall" | PersonName == "Marcus Smart" | 
        PersonName == "Draymond Green",1,0
    )
  )

distance_hustle <- dplyr::select(hustle_data, scaled_STLP, scaled_RimFreq) %>%
  dist()

matrix_hustle <- as.matrix(distance_hustle)

hier_clust_hustle <- hclust(distance_hustle, method = "complete")
plot(hier_clust_hustle)

hustle_cluster_hier <- cutree(hier_clust_hustle, k = 3)
hustle_data$hustle_cluster <- as.factor(hustle_cluster_hier)
ggplot(hustle_data, aes(x= STLP, y= RimFreq, color=hustle_cluster, label=PersonName))+
  geom_point() + geom_label_repel(aes(label=ifelse(hustle_known==1,as.character(PersonName),'')),
                                  box.padding = 0.35, point.padding = 1, segment.color = "grey50")

no_hustle <- hustle_data %>%
  filter(hustle_cluster == 1)

high_hustle <- hustle_data %>%
  filter(hustle_cluster == 2)

no_steal <- hustle_data %>%
  filter(hustle_cluster == 3)

high_hustle$X3P <- high_hustle$X3FGM_PG/high_hustle$X3FGA_PG
