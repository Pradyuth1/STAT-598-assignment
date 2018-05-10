#Name : Pradyuth Vangur
#Assignment 3

#Problem 1: Dataframes and ggplot2

#Question a
#install package maps and ggplot2

#Installing and loading the package maps and ggplot2
library(maps)
library(ggplot2)
library(dplyr)

#Question b
#Creating dataframe
da_fr <- map_data(map = "state")

#Dimensions of dataframe
dim(da_fr)

#Column names
names(da_fr)

#Question c
#Printing the number of unique values
unique(da_fr$region)

#Question d
ggplot(da_fr, aes(x = long, y = lat, col = region)) +
  geom_point()

#Question e
ggplot(da_fr, aes(x = long, y = lat,group = group, col = region)) +
  geom_point() +
  geom_path()
#We observe that the path is being laud out in the order of points and gives a
#picture of the map.

ggplot(da_fr, aes(x = long, y = lat,group = group, col = region)) +
  geom_point() +
  geom_line()
#Here, the lines try to connect each points which belong to the same region

#Question f
#Selecting rows of new york and california
da_dr_st <- which(da_fr$region == c("new york" , "california"))

#making a dataframe of only those countries
da_fr_st <- da_fr[da_dr_st,]

#Creating plot for two states
ggplot(da_fr_st, aes(x = long, y = lat,group = group, col = region)) +
  geom_point() +
  geom_path()

#Question g
ggplot(sample(da_fr_st), aes(x = long, y = lat,group = group, col = region)) +
  geom_point() +
  geom_path()

#Question h
#states_map is da_fr in this case
ggplot() + geom_map(map = da_fr, map_id=da_fr$region, data = da_fr, aes(fill=group)) +
  expand_limits(x = da_fr$long, y = da_fr$lat)

#Question i
#Assigning correct values to states
states <- unique(da_fr$region)
group_new <- c(rep(0, nrow(da_fr)))


for( i in 1:nrow(da_fr) ){
  m <- which(da_fr$region[i] == states)
  group_new[i] <- m
}

da_fr_new <- cbind(da_fr, group_new)

ggplot() + geom_map(map = da_fr_new, map_id=da_fr_new$region, data = da_fr_new, aes(fill=group_new)) +
  expand_limits(x = da_fr_new$long, y = da_fr_new$lat)

#Question j
#Assigning each row it's murder count
data(state)
InfoValue <- c(rep(0, nrow(state.x77)))
InfoValue <- (state.x77[,1] * state.x77[,5]) / 10000
InfoValue_round <- round(InfoValue)

#Adding it onto a new data frame
state_new <- cbind(state.name, state.x77, InfoValue, InfoValue_round)
state_new_df <- as.data.frame(state_new, row.names = F)

#Question k
InfoType_1 <- rep("Murder", nrow(da_fr))
InfoType_2 <- rep("Grad", nrow(da_fr))
InfoType_n <- c(InfoType_1, InfoType_2)
InfoValue_n <- rep(0, nrow(da_fr))
new_value <- rep(0, nrow(da_fr))

state_new_df_2 <- state_new_df[-2,]
state_name_new <- state.name[-2]

#Creating a function to map values into
value <- function(data, col, states_map){
  new_column <- rep(0, nrow(data))
  for(i in 1:nrow(data)){
    new_column[i] <- as.numeric(levels(droplevels(col[i][1])))
  }

  for(i in 1:nrow(states_map)){
    l <- as.numeric(which(states_map$region[i] == tolower(state_name_new)))
    if(sum((states_map$region[i] == tolower(state_name_new))) == 0) {next}
    new_value[i] = new_column[l]
  }
  return(new_value)
}

Murder_values <- value(data = state_new_df_2, col = state_new_df_2$Murder , states_map = da_fr)
Illiteracy_values <- value(data = state_new_df_2, col = state_new_df_2$Illiteracy , states_map = da_fr)

da_fr_M <- cbind(da_fr, InfoType_1, Murder_values, group_new)
names(da_fr_M)[7] <- c("InfoType")
names(da_fr_M)[8] <- c("InfoValue")

da_fr_I <- cbind(da_fr, InfoType_2, Illiteracy_values, group_new)
names(da_fr_I)[7] <- c("InfoType")
names(da_fr_I)[8] <- c("InfoValue")

#Final dataframe
da_fr_MI <- rbind(da_fr_M, da_fr_I)

#Question l
ggplot() + geom_map(map = da_fr_MI, data = da_fr_MI, map_id=da_fr_MI$region, aes(fill=group_new)) +
  expand_limits(data = da_fr, x = da_fr$long, y = da_fr$lat) +
  facet_grid(. ~ InfoType)


#Question m
states <- unique(da_fr$region)
group_new_1 <- c(rep(0, nrow(da_fr)))
states_df <- as.data.frame(state.x77)
states_df_new <- states_df[-2,]

#Displaying a loop to map the calumns into another
for( i in 1:nrow(da_fr) ){
  m <- which(da_fr$region[i] == states)
  if(sum(da_fr$region[i] == states) == 0){next}
  group_new_1[i] <- states_df_new$Illiteracy[m]
}

da_fr_new_il <- cbind(da_fr_new, group_new_1)


vec <- vector()
MeanLat <- rep(0, length(states))
MeanLong <- rep(0, length(states))

#Finding the mean values of lat and long of each region
for(i in 1:length(states)){
  vec <- which(da_fr_new_il$region == states[i])
  MeanLong[i] <- mean(da_fr_new_il$long[vec])
  MeanLat[i] <- mean(da_fr_new_il$lat[vec])
}

data_4 <- as.data.frame(cbind(state.name[-2],MeanLat, MeanLong, ArrentCount = as.numeric(levels(droplevels(state_new_df_2$InfoValue)))))

data_4$MeanLat <- as.numeric(levels(droplevels(data_4$MeanLat)))
data_4$MeanLong <- as.numeric(levels(droplevels(data_4$MeanLong)))
data_4$ArrentCount <- as.numeric(levels(droplevels(data_4$ArrentCount)))


q <- ggplot() + geom_map(map = da_fr_new, map_id=da_fr_new$region, data = da_fr_new, aes(fill=group_new)) +
  expand_limits(x = da_fr_new$long, y = da_fr_new$lat)

q + geom_point(data = data_4, aes(x = MeanLong, y = MeanLat, col = V1, size = ArrentCount))

#Question n
#Function for calculating the mean of latitude and longitude
fun <- function(X){
  vec <- which(X == states)
  MeanLong <- mean(da_fr$long[vec])
  MeanLat <- mean(da_fr$lat[vec])
  return(c(MeanLong, MeanLat))
}

states_n <- states[-2]

#Function to calculate the values for the whole dataset
head(sapply(states, fun))