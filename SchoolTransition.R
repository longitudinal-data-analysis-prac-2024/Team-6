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
