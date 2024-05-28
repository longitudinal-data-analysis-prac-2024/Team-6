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
  geom_density(alpha = .2, fill="#FF6666")
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
p3 <- ggplot(Parent_PWHdata, aes(x = warmth)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#FF6666")
p4 <- ggplot(Parent_PWHdata, aes(x = hostility)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 colour = "black", fill = "white")+
  geom_density(alpha = .2, fill="#FF6")
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
newdata <- PaternalPWHdata %>%
  filter(ID %in% commonlist) %>%
  select(ID, hostility, warmth)
  