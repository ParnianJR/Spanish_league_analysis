library(engsoccerdata)
library(dplyr)
library(ggplot2)


#finding  teams with most champoinship
games <- rbind( select(spain, Season, team = home, GF = hgoal, GA = vgoal),
                select(spain, Season, team = visitor, GF = vgoal, GA = hgoal) )

data0 <- games %>% group_by(Season,team) %>%
  mutate(W = sum(GF > GA), E = sum(GF == GA)) %>%
  mutate(scores = as.integer(W) * if_else(Season < 1995 , 2, 3) + as.integer(E) ) %>%
  mutate(goal_for = sum(GF)) %>%
  mutate(goal_against = sum(GA)) %>%
  mutate(goal_average = goal_for - goal_against) %>% arrange(Season)

data1 <- ungroup(data0) %>% group_by(Season) %>% filter(scores == max(scores)) %>%
  select(Season,team,scores, goal_average) %>% arrange(Season)

data2 <- distinct(data1) %>% group_by(Season) %>% 
  filter(goal_average == max(goal_average)) %>% arrange(Season)

data3 <- ungroup(data2) %>% select(team) %>% group_by(team) %>%
  summarise(num_of_win = n()) %>% arrange(desc(num_of_win))


ggplot(data3, aes(x = reorder(team, num_of_win), y = num_of_win)) + 
  geom_bar(stat = 'identity', fill = 'darkmagenta') +
  geom_text(aes(label = num_of_win),vjust=0.5, color="navy")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_flip()

#find the the most boring and the most exciting teams
data <- select(spain, Season, team = home, hgoal, vgoal,round )
#find boring teams
data1 <- data %>% filter(round == 'league') %>% 
  group_by(team) %>%
  mutate(result = if_else(hgoal < vgoal, 1, 0)) %>%
  summarise(boredom = sum(result)) %>%
  arrange(desc(boredom)) %>%
  slice(1:10)

ggplot(data1, aes(x = reorder(team, boredom), y = boredom)) + 
  geom_bar(stat = 'identity', fill = 'blue') + 
  theme(axis.title.y = element_blank()) +
  coord_flip()
#find exciting teams
data2 <- data %>% filter(round == 'league') %>% 
  group_by(team) %>%
  mutate(result = if_else(hgoal > vgoal, 1, 0)) %>%
  summarise(excitment = sum(result)) %>%
  arrange(desc(excitment)) %>%
  slice(1:10)

ggplot(data2,aes(x = reorder(team, excitment), y = excitment)) +
  geom_bar(stat = 'identity', fill = 'pink') +
  theme(axis.title.y = element_blank()) +
  coord_flip()


# half_season champion also become the champion of the season
half_data <- select(spain, Season, team = home, hgoal, vgoal)
full_data <- rbind( select(spain, Season, team = home, GF = hgoal, GA = vgoal),
                    select(spain, Season, team = visitor, GF = vgoal, GA = hgoal) )

half_result0 <- half_data %>% group_by(Season, team) %>%
  mutate(W = sum(hgoal < vgoal),
         E = sum(hgoal == vgoal)) %>%
  mutate(scores = as.integer(W) * if_else(Season < 1995, 2, 3) + as.integer(E)) %>%
  mutate(goal_for = sum(hgoal)) %>%
  mutate(goal_against = sum(vgoal)) %>%
  mutate(goal_average = goal_for - goal_against) %>% arrange(Season)

half_result1 <- ungroup(half_result0) %>% group_by(Season) %>% 
  filter(scores == max(scores)) %>% select(Season, team, scores, goal_average)

half_result <- distinct(half_result1) %>% group_by(Season) %>% 
  filter(goal_average == max(goal_average)) %>% arrange(Season)

data0 <- full_data %>% group_by(Season,team) %>%
  mutate(W = sum(GF > GA), E = sum(GF == GA)) %>%
  mutate(scores = as.integer(W) * if_else(Season < 1995 , 2, 3) + as.integer(E) ) %>%
  mutate(goal_for = sum(GF)) %>%
  mutate(goal_against = sum(GA)) %>%
  mutate(goal_average = goal_for - goal_against) %>% arrange(Season)

data1 <- ungroup(data0) %>% group_by(Season) %>% filter(scores == max(scores)) %>%
  select(Season,team,scores, goal_average) %>% arrange(Season)

data2 <- distinct(data1) %>% group_by(Season) %>% 
  filter(goal_average == max(goal_average)) %>% arrange(Season)

total_num <- ungroup(data2) %>% summarise(num = n())  
true_num <- summary(data2$team %in% half_result$team)
percent <- (51 / 89) * 100

#find dark horse of great teams
games1 <- select(spain, Season, home, visitor, hgoal, vgoal) %>%
  filter(Season > 2000 & Season < 2011) %>%
  filter(home == 'Real Madrid' | home == 'FC Barcelona') %>%
  mutate(black1 = if_else(vgoal > hgoal, 1, 0)) %>%
  filter(black1 == 1) %>% select(visitor) %>% 
  filter(visitor != 'Real Madrid' & visitor != 'FC Barcelona') %>%
  group_by(visitor) %>% mutate(num_1 = n()) %>% arrange(desc(num_1)) %>%
  select(team = visitor, num = num_1)

games2 <-  select(spain, Season, home, visitor, hgoal, vgoal) %>%
  filter(Season > 2000 & Season < 2011) %>%
  filter(visitor == 'Real Madrid' | visitor == 'FC Barcelona') %>%
  mutate(black1 = if_else(hgoal > vgoal, 1, 0)) %>%
  filter(black1 == 1) %>% select(home) %>% 
  filter(home != 'Real Madrid' & home != 'FC Barcelona') %>%
  group_by(home) %>% mutate(num_2 = n()) %>% arrange(desc(num_2)) %>%
  select(team = home, num = num_2)

games <- rbind(games1, games2) %>% group_by(team) %>% 
  summarise(totalNum = sum(num)) %>% arrange(desc(totalNum)) %>%
  slice(1:10)

ggplot(games, aes(x = reorder(team,totalNum), y = totalNum)) +
  geom_bar(stat = 'identity', fill = '#00AFBB') +
  theme(axis.title.y = element_blank()) +
  coord_flip()


