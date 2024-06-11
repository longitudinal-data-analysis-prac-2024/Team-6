library(haven)
#Remember that lower scores means a higher extent---lower scores of warmth means more warmth and lower scores of hostility means more hostility.
schooltransitiondata <- read_sav("SchoolTransitionData.sav")
library(tidyverse)
PWHdata <- schooltransitiondata %>%
  select(76:97, ID) %>%
  filter(!is.na(C1_PWHAa)) %>%
  filter(C1_PWHAa == 0 | C1_PWHBa == 0)
#All of the children are living with either their mom or dad or both during the past 6 months.
MaternalPWHdata <- PWHdata %>%
  select(1:11, ID) %>%
  na.omit %>%
  mutate(hostility = C1_PWHA1 + C1_PWHA3 + C1_PWHA4 + C1_PWHA8) %>%
  mutate(warmth = C1_PWHA2 + C1_PWHA5 + C1_PWHA6 + C1_PWHA7 + C1_PWHA9 + C1_PWHA10)
PaternalPWHdata <- PWHdata %>%
  select(12:22, ID) %>%
  na.omit %>%
  mutate(hostility = C1_PWHB1 + C1_PWHB3 + C1_PWHB4 + C1_PWHB8) %>%
  mutate(warmth = C1_PWHB2 + C1_PWHB5 + C1_PWHB6 +C1_PWH7 + C1_PWHB9 +C1_PWHB10)
Maternal <- MaternalPWHdata %>%
  select(warmth, hostility, ID)
Paternal <- PaternalPWHdata %>%
  select(warmth, hostility, ID)
#I filtered out all the rows with NA data
Overall <- rbind(Maternal, Paternal)
p1 <- ggplot(Overall, aes(x = warmth)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#FF6") +
  ggtitle("Child-Perceived Wave 1")
p2 <- ggplot(Overall, aes(x = hostility)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#FF6")
install.packages("gridExtra")
library(gridExtra)
p_density <- grid.arrange(p1, p2)
ggsave(filename = "C1PWHDistribution.png",
       width = 30, height = 15, units = "cm",
       dpi = 500)
#Now I look at parent-perceived parental warmth and hostility at wave 1
PWHdata_parents <- schooltransitiondata %>%
  select(418:427, ID) %>%
  na.omit
Parent_PWHdata<- PWHdata_parents %>%
  mutate(hostility = P1_PWH1 + P1_PWH3 + P1_PWH4 + P1_PWH8) %>%
  mutate(warmth = P1_PWH2 + P1_PWH5 + P1_PWH6 + P1_PWH7 + P1_PWH9 + P1_PWH10)
p3 <- ggplot(Parent_PWHdata, aes(x = warmth_P)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#dfd")+
  ggtitle("Parent-Perceived Wave 1")
p4 <- ggplot(Parent_PWHdata, aes(x = hostility_P)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#dfd")
p_density2 <- grid.arrange(p3, p4)
ggsave(filename = "P1PWHDistribution.png",
       width = 30, height = 15, units = "cm",
       dpi = 500)
#Now I look at discrepancies (children - parents)
#First of all, I will merge the parent- and children-perceived PWH data together based on ID.
Parent_PWHdata_overall <- Parent_PWHdata %>%
  select(ID, hostility_P, warmth_P)
Parent_PWHdata<- PWHdata_parents %>%
  mutate(hostility_P = P1_PWH1 + P1_PWH3 + P1_PWH4 + P1_PWH8) %>%
  mutate(warmth_P = P1_PWH2 + P1_PWH5 + P1_PWH6 + P1_PWH7 + P1_PWH9 + P1_PWH10)
Overall_P_and_C <- merge(Overall, Parent_PWHdata, by = "ID")
Overall_P_and_C %>%
  group_by(ID) %>%
  count() %>%
  print(n = Inf)
#We need to know who wrote the parent-perceived parental warmth and hostility questionnaire (male =1, female =2)
PWHdata_parents <- schooltransitiondata %>%
  select(418:427,251, ID) %>%
  na.omit
Parent_PWHdata<- PWHdata_parents %>%
  mutate(hostility_P = P1_PWH1 + P1_PWH3 + P1_PWH4 + P1_PWH8) %>%
  mutate(warmth_P = P1_PWH2 + P1_PWH5 + P1_PWH6 + P1_PWH7 + P1_PWH9 + P1_PWH10)
Parent_PWHdata_overall <- Parent_PWHdata %>%
  select(ID, hostility_P, warmth_P, P1_ParentGender)
Overall_P_and_C <- merge(Overall, Parent_PWHdata_overall, by = "ID")
#Now we need to separate maternal and paternal warmth and hostility.
PaternalPupils <- Overall_P_and_C %>%
  filter(P1_ParentGender == 1) %>%
  select(ID) %>%
  unique() %>%
  as.list()
PaternalPupils
#These pupils had their dad filled out the questionnaire, so we should focus on paternal warmth and hostility for these pupils. 
#Let's first check if all the above IDs are in PaternalPWH data IDs
PaternalPupils %in% PaternalPWHdata$ID
#So no, not all children with their dad answering the questionnaire had their paternal PWH data. So now we need to filter out those who had their paternal PWH data.
list <- as.list(PaternalPWHdata$ID)
commonlist <- as.list(intersect(unlist(PaternalPupils), unlist(list)))
newdata_childperceived <- PaternalPWHdata %>%
  filter(ID %in% commonlist) %>%
  select(ID, hostility, warmth)
newdata_childperceived
#This is the list of child-perceived parental warmth that can be compared with father-perceived parental warmth. 
Father_PWHdata_overall <- Parent_PWHdata_overall %>%
  filter(P1_ParentGender == 1)
#This is the list of parent-perceived parental warmth (dad). Now we check if all IDs in the first list is exactly the same as the IDs in the second list.
newdata_childperceived$ID %in% Father_PWHdata_overall$ID
PaternalDataset1 <- merge(newdata_childperceived, Father_PWHdata_overall) %>%
  as_tibble
PaternalDataset1 <- PaternalDataset1 %>%
  rename( hostility_C1 = hostility,
        warmth_C1 = warmth,
        hostility_P1 = hostility_P,
        warmth_P1 = warmth_P)
PaternalDataset1 <- PaternalDataset1 %>%
  select(ID, hostility_C1, warmth_C1, hostility_P1, warmth_P1) %>%
  mutate (hostility_Discrepancies = hostility_C1 - hostility_P1) %>%
  mutate(warmth_Discrepancies = warmth_C1 - warmth_P1)
#Now we explore the distribution of the paternal discrepancies of warmth and hostility.
p5 <- ggplot(PaternalDataset1, aes(x = hostility_Discrepancies)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#F2D")+
  ggtitle("Paternal Wave 1")
p6 <- ggplot(PaternalDataset1, aes(x = warmth_Discrepancies)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#F2D")
p_density3 <- grid.arrange(p5, p6)
#We shall pause and do some interpretations here: Lower number means more: for example, a lower number of warmth means that there is MORE warmth in the parental relationship; similarly, a high number of hostility means that there is LESS hostility in the parental relationship. So if the hostility discrepancy is a large positive number, it means that the child has a higher perceived hostility score, meaning that the child thinks that the parent is being LESS hostile than the parents think they are. On the other hand, if the hostility discrepancy is a large negative number, it means that the child has a lower perceived hostility score, meaning that the child thinks that the parent is being MORE hostile than the parents thinks they are. In similar senses, a large positive warmth discrepancy number means that the child has a higher perceived warmth score, meaning that the child thinks that the parent is being LESS warm than the parents think they are. A large negative warmth discrepancy number means that the child has a lower perceived warmth score, meaning that the child thinks that the parent is being MORE warm than the parents think they are.
#We can observe from the distribution shape that hostility discrepancy is left skewed while warmth discrepancy is right skewed. And this means that parents generally tend to think they are more hostile and less warm than the children think they are. This could be a demonstration of parental guilt---parents sometime tend to over-reflect on their parental behaviors. Note that this is only the paternal data though. The next step is to see if similar patterns show on maternal data.
#Now let's look at the discrepancy for maternal data.
MaternalPupils <- Parent_PWHdata_overall %>%
  filter(P1_ParentGender == 2) %>%
  select(ID) %>%
  unique() %>%
  as.list()
list1 <- as.list(MaternalPWHdata$ID)
commonlist1 <- as.list(intersect(unlist(MaternalPupils), unlist(list1)))
newdata_childperceived_m <- MaternalPWHdata %>%
  filter(ID %in% commonlist1) %>%
  select(ID, hostility, warmth)
Mother_PWHdata_overall <- Parent_PWHdata_overall %>%
  filter(P1_ParentGender == 2)
newdata_childperceived_m$ID %in% Mother_PWHdata_overall$ID
#We have much more maternal data than paternal data; more mothers filled out the parental warmth forms than the fathers.
MaternalDataset1 <- merge(newdata_childperceived_m, Mother_PWHdata_overall) %>%
  as_tibble
MaternalDataset1 <- MaternalDataset1 %>%
  rename( hostility_C1 = hostility,
          warmth_C1 = warmth,
          hostility_P1 = hostility_P,
          warmth_P1 = warmth_P)
MaternalDataset1 <- MaternalDataset1 %>%
  select(ID, hostility_C1, warmth_C1, hostility_P1, warmth_P1) %>%
  mutate (hostility_Discrepancies = hostility_C1 - hostility_P1) %>%
  mutate(warmth_Discrepancies = warmth_C1 - warmth_P1)
p7 <- ggplot(MaternalDataset1, aes(x = hostility_Discrepancies)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#0000FF7D")+
  ggtitle("Maternal Wave 1")
p8 <- ggplot(MaternalDataset1, aes(x = warmth_Discrepancies)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#0000FF7D")
p_density4 <- grid.arrange(p7, p8)
p_overall_1 <- grid.arrange(p5, p7, p6, p8, nrow = 2, ncol = 2)
#We can see that the patterns for paternal and maternal data are actually quite similar. I would say that the hostility discrepancy resembles more of a normal distribution and the warmth discrepancy data has a larger and more prominent peak. This could potentially mean that mothers are more accurate in terms of their self-reflected parental warmth and hostility (at least more consistent with children's feedback). The degree of "parental guilt" as described before is less prominent among mothers than fathers. 
#Okay, now we can do the same thing for the Wave 2 data.
schooltransitiondatacsv <- write.csv(schooltransitiondata, file = "/Users/rluo/Documents/Longitudinal Data Analysis/Team-6/schooltransitiondata.csv", row.names = FALSE)
#So from the csv search I found out that the PWH data are available for children and parents only for wave 1 and wave 2 but NOT wave 3. So we will only focus on wave 1 and wave 2 data.
PWHdataWave2 <- schooltransitiondata %>%
  select(553:574, ID) %>%
  filter(!is.na(C2_PWHAa)) %>%
  filter(!is.na(C2_PWHBa)) %>%
  filter(C2_PWHAa == 0 | C2_PWHBa == 0) %>%
  na.omit
MaternalPWHdataWave2 <- PWHdataWave2 %>%
  select(1:11, ID) %>%
  mutate(hostility = C2_PWHA1 + C2_PWHA3 + C2_PWHA4 + C2_PWHA8) %>%
  mutate(warmth = C2_PWHA2 + C2_PWHA5 + C2_PWHA6 + C2_PWHA7 + C2_PWHA9 + C2_PWHA10)
PaternalPWHdataWave2 <- PWHdataWave2 %>%
  select(12:22, ID) %>%
  mutate(hostility = C2_PWHB1 + C2_PWHB3 + C2_PWHB4 + C2_PWHB8) %>%
  mutate(warmth = C2_PWHB2 + C2_PWHB5 + C2_PWHB6 +C2_PWHB7 + C2_PWHB9 +C2_PWHB10)
MaternalWave2 <- MaternalPWHdataWave2 %>%
  select(warmth, hostility, ID)
PaternalWave2 <- PaternalPWHdataWave2 %>%
  select(warmth, hostility, ID)
OverallWave2 <- rbind(MaternalWave2, PaternalWave2)
p9 <- ggplot(OverallWave2, aes(x = warmth)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#FF6666") +
  ggtitle("Child-Perceived Wave 2")
p10 <- ggplot(OverallWave2, aes(x = hostility)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#FF6666")
grid.arrange(p9, p10)
#I think it might be a good idea to combine child-perceived parental warmth and hostility data from wave 1 and wave 2 together so that it is easier to compare the distribution patterns.
grid.arrange(p1, p9, p2, p10, nrow = 2, ncol = 2)
#From here we can see that the child-perceived parental warmth and hostility are actually very similar for wave 1 and wave 2. One difference that is worth noting is that for wave 2 hostility data, there is a huge peak at the very right, which means that parental hostility actually goes down at wave 3. This makes sense because if we look at the far-left peak at wave 2 for warmth, it is actually higher than that in Wave 2. Generally, parents are warmer and less hostile toward their children at wave 2.
PWHdata_parents_Wave2 <- schooltransitiondata %>%
  select(732:741, ID, P2_ParentGender) %>%
  na.omit
Parent_PWHdata_Wave2<- PWHdata_parents_Wave2 %>%
  mutate(hostility_P2 = P2_PWH1 + P2_PWH3 + P2_PWH4 + P2_PWH8) %>%
  mutate(warmth_P2 = P2_PWH2 + P2_PWH5 + P2_PWH6 + P2_PWH7 + P2_PWH9 + P2_PWH10)
p11 <- ggplot(Parent_PWHdata_Wave2, aes(x = warmth_P2)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#0000FF7D")+
  ggtitle("Parent-Perceived Wave 2")
p12 <- ggplot(Parent_PWHdata_Wave2, aes(x = hostility_P2)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#0000FF7D")
grid.arrange(p11, p12)
grid.arrange(p3, p11, p4, p12)
#The parent-perceived warmth and hostility distributions at wave 1 and wave 2 are very similar. Now I think we can compare all the warmth and hostility distribution graphs together.
grid.arrange(p1, p9, p3, p11, p2, p10, p4, p12, nrow = 2, ncol = 4)
#From this graph, we can see that the discrepancy between wave 2 hostility child-perceived and parent-perceived is the greatest. We shall know more about it once we conducted discrepancy analyses.
#Now we can calculate the discrepancies at Wave 2.
#Again, we need to know who wrote the questionnaire at wave 2 (mom or dad).
PaternalPupilsWave2 <- Parent_PWHdata_Wave2 %>%
  filter(P2_ParentGender == 1) %>%
  select(ID) %>%
  unique() %>%
  as.list()
#This is the list of pupils that have their dad filled out the PWH data at wave 2.
PaternalPupilsWave2 %in% PaternalPWHdataWave2$ID
list2 <- as.list(PaternalPWHdataWave2$ID)
commonlist2 <- as.list(intersect(unlist(PaternalPupilsWave2), unlist(list2)))
newdata_childperceived_p_wave2 <- PaternalPWHdataWave2 %>%
  filter(ID %in% commonlist2) %>%
  select(ID, hostility, warmth)
Father_PWHdata_overall_wave2 <- Parent_PWHdata_Wave2 %>%
  filter(P2_ParentGender == 1) %>%
  select(ID, hostility_P2, warmth_P2)
newdata_childperceived_p_wave2$ID %in% Father_PWHdata_overall_wave2$ID
#Now we combine the child-perceived and dad-perceived data together.
PaternalDataset2 <- merge(newdata_childperceived_p_wave2, Father_PWHdata_overall_wave2) %>%
  as_tibble
PaternalDataset2 <- PaternalDataset2 %>%
  rename(hostility_C2 = hostility,
         warmth_C2 = warmth) 
PaternalDataset2 <- PaternalDataset2 %>%
  mutate(hostility_Discrepancies = hostility_C2 - hostility_P2,
         warmth_Discrepancies = warmth_C2 - warmth_P2)
p13 <- ggplot(PaternalDataset2, aes(x = hostility_Discrepancies)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#FF6")+
  ggtitle("Paternal Wave 2")
p14 <- ggplot(PaternalDataset2, aes(x = warmth_Discrepancies)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#FF6")
grid.arrange(p13, p14)
#If the hostility discrepancy is a large positive number, it means that the child has a higher perceived hostility score, meaning that the child thinks that the parent is being LESS hostile than the parents think they are. On the other hand, if the hostility discrepancy is a large negative number, it means that the child has a lower perceived hostility score, meaning that the child thinks that the parent is being MORE hostile than the parents thinks they are. In similar senses, a large positive warmth discrepancy number means that the child has a higher perceived warmth score, meaning that the child thinks that the parent is being LESS warm than the parents think they are. A large negative warmth discrepancy number means that the child has a lower perceived warmth score, meaning that the child thinks that the parent is being MORE warm than the parents think they are.
#We can observe from the distribution shape that hostility discrepancy is left skewed while warmth discrepancy is right skewed. And this means that parents generally tend to think they are more hostile and less warm than the children think they are. This could be a demonstration of parental guilt---parents sometime tend to over-reflect on their parental behaviors. Note that this is only the paternal data though. The next step is to see if similar patterns show on maternal data. Basically the same patterns maintained at wave 2. We shall compare the distribution side by side to have a better comparison.
#But we shall look at maternal data first.
MaternalPupilsWave2 <- Parent_PWHdata_Wave2 %>%
  filter(P2_ParentGender == 2) %>%
  select(ID) %>%
  unique() %>%
  as.list()
MaternalPupilsWave2 %in% MaternalPWHdataWave2$ID
list3 <- as.list(MaternalPWHdataWave2$ID)
commonlist3 <- as.list(intersect(unlist(MaternalPupilsWave2), unlist(list3)))
newdata_childperceived_m_wave2 <- MaternalPWHdataWave2 %>%
  filter(ID %in% commonlist3) %>%
  select(ID, hostility, warmth)
Mother_PWHdata_overall_wave2 <- Parent_PWHdata_Wave2 %>%
  filter(P2_ParentGender == 2) %>%
  select(ID, hostility_P2, warmth_P2)
newdata_childperceived_m_wave2$ID %in% Mother_PWHdata_overall_wave2$ID
MaternalDataset2 <- merge(newdata_childperceived_m_wave2, Mother_PWHdata_overall_wave2) %>%
  as_tibble
MaternalDataset2 <- MaternalDataset2 %>%
  rename(hostility_C2 = hostility,
         warmth_C2 = warmth) 
MaternalDataset2 <- MaternalDataset2 %>%
  mutate(hostility_Discrepancies = hostility_C2 - hostility_P2,
         warmth_Discrepancies = warmth_C2 - warmth_P2)
p15 <- ggplot(MaternalDataset2, aes(x = hostility_Discrepancies)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#FF6666")+
  ggtitle("Maternal Wave 2")
p16 <- ggplot(MaternalDataset2, aes(x = warmth_Discrepancies)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#FF6666")
grid.arrange(p15, p16)
#Again, maternal discrepancies distributions are less skewed than that of paternal ones. There is an apparent outlier in the warmth discrepancy data though which is worth noting. Now we can put wave 2 discrepancy data together.
grid.arrange(p13, p15, p14, p16, nrow = 2, ncol = 2)
#Now we can put wave 1 and wave 2 discrepancy distribution graphs together to further compare.
grid.arrange(p5, p13, p7, p15, p6, p14, p8, p16, nrow = 2, ncol = 4)
