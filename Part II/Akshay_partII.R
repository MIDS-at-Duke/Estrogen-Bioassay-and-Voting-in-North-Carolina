
####### Voting in NC (2016 General Elections) ####### 

#Akshay Punwatkar (AP509)


library(tidyverse)
library(ggplot2)

setwd("/Users/akshaypunwatkar/TeamProject2/team-project-2-estrogen-bioassay-and-voting-in-nc-avengers")

#Reading file with data for all the voters
all_voter = read.csv("/Users/akshaypunwatkar/TeamProject2/team-project-2-estrogen-bioassay-and-voting-in-nc-avengers/Data/voter_stats_20161108.txt", sep = '')

#Reading file with data for the voters who actually voted 
voted_voter = read.csv("/Users/akshaypunwatkar/TeamProject2/team-project-2-estrogen-bioassay-and-voting-in-nc-avengers/Data/history_stats_20161108.txt", sep = '\t')


#Making unwanted columns NULL (DATES)
all_voter$election_date <- NULL
voted_voter$election_date <- NULL
all_voter$stats_type <- NULL
voted_voter$update_date <- NULL
voted_voter$stats_type <- NULL

#Removing Rows with empty data

all_voter <- all_voter %>%
              na_if("") %>%
                na.omit()

voted_voter <- voted_voter %>%
                na_if("") %>%
                  na.omit()


#renaming column in voted dataframe
colnames(voted_voter)[9] <- "voted_voters"

#merging the two dataframes 
merge_voter= merge(voted_voter, all_voter, 
                   by.y = c("county_desc" , "precinct_abbrv", "vtd_abbrv", 
                            "party_cd", "race_code","ethnic_code","sex_code",       
                            "age"),
                   by.x = c( "county_desc", "precinct_abbrv", "vtd_abbrv",          
                             "party_cd", "race_code", "ethnic_code", "sex_code",          
                             "age"))

#counting and plotting number of observations for each county
dt = aggregate(merge_voter$county_desc, list(merge_voter$county_desc), length)
colnames(dt) = c('County','NbrOfObs')

ggplot(dt)+
  geom_point(aes(x=County, y=NbrOfObs))

#Selecting 20 counties at rondom
set.seed(100)
counties = sample(as.character(unique(merge_voter$county_desc)), size = 20,replace = T)

#Couting number of observations of 20 selected counties 

print(counties)

# [1] "PITT"       "TYRRELL"    "ROBESON"   
# [4] "CLEVELAND"  "SURRY"      "PASQUOTANK"
# [7] "ANSON"      "LINCOLN"    "PASQUOTANK"
# [10] "WILSON"     "BEAUFORT"   "BEAUFORT"  
# [13] "LINCOLN"    "HARNETT"    "SAMPSON"   
# [16] "MITCHELL"   "BURKE"      "YADKIN"    
# [19] "JOHNSTON"   "PERQUIMANS"

voter_dataset = subset(merge_voter, county_desc %in% counties)

dt = aggregate(voter_dataset$county_desc, list(voter_dataset$county_desc), length)
colnames(dt) = c('County','NbrOfObs')

#Plotting the 20 couties with their count

ggplot(dt,aes(x=County, y=NbrOfObs),label=NbrOfObs)+
  geom_point(colour='Red')+
  ylim(0,15500)+
  geom_text(aes(label=NbrOfObs),hjust=0.5,vjust=-0.8)+
  ggtitle("Number of Observations for 20 Counties") +
  theme( plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,hjust = 1))

######## EDA ######## 

#colnames(voter_dataset)
# "county_desc" | "precinct_abbrv" | "vtd_abbrv" | "party_cd" | "race_code" |
# "sex_code" | "age" | "voting_method" | "voted_party_cd" | "voted_voters" 
# "voting_method_desc" | "total_voters" |  "ethnic_code"   

str(voter_dataset)
voter_dataset$total_voters = as.numeric(voter_dataset$total_voters)
voter_dataset$voted_voters = as.numeric(voter_dataset$voted_voters)

ggplot(voter_dataset, aes(x=county_desc, y=total_voters))+
  geom_boxplot(aes(fill= county_desc),width=0.2)+
  ggtitle("Distribution of Total Voters for 20 Counties") +
  xlab("Counties")+
  ylab("Total Voters")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,hjust = 1))


ggplot(voter_dataset, aes(x=county_desc, y=voted_voters))+
  geom_boxplot(aes(fill= county_desc),width=0.2)+
  ggtitle("Distribution of Voted Voters for 20 Counties") +
  xlab("Counties")+
  ylab("Number of Voters who Voted") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,hjust = 1))






