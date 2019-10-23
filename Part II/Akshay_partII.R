
library()


setwd("/Users/akshaypunwatkar/TeamProject2/team-project-2-estrogen-bioassay-and-voting-in-nc-avengers")

#Reading file with data for all the voters
all_voter = read.csv("/Users/akshaypunwatkar/TeamProject2/team-project-2-estrogen-bioassay-and-voting-in-nc-avengers/Data/voter_stats_20161108.txt", sep = '')

#Reading file with data for the voters who actually voted 
voted_voter = read.csv("/Users/akshaypunwatkar/TeamProject2/team-project-2-estrogen-bioassay-and-voting-in-nc-avengers/Data/history_stats_20161108.txt", sep = '\t')

#Making unwanted columns NULL (DATES)
all_voter$election_date <- NULL
voted_voter$election_date <- NULL
voted_voter$update_date <- NULL
voted_voter$stats_type <- NULL

#Removing Unwated rows with random data 90250
all_voter <- all_voter[-c(106022,106021),] 
voted_voter <- voted_voter[-c(90250,382555,134953,90249),] 



voted_voter[voted_voter$party_cd == 'voting_method',]

str(voted_voter)

unique(voted_voter$county_desc)
unique(voted_voter$precinct_abbrv)

dt = aggregate(voted_voter$county_desc, list(voted_voter$county_desc), length)
colnames(dt) = c('County','NbrOfObs')
dt$type = rep('voted')

dt2 = aggregate(all_voter$county_desc, list(all_voter$county_desc), length)
colnames(dt2) = c('County','NbrOfObs')
dt2$type = rep('all')

dt3 = union(dt,dt2)

ggplot(dt3)+
  geom_point(aes(x=County, y=NbrOfObs, colour=type))

table(voted_voter$age)



table(voted_voter$voting_method_desc)

nrow(unique(voted_voter))

