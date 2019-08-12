###############################################################################################
# getting data sets
companies <- read.delim("companies.txt", sep = "\t", stringsAsFactors = FALSE, header = TRUE)
rounds2 <- read.csv("rounds2.csv")
###############################################################################################

# unique company count in round2
library(tidyr)
library(dplyr)
library(stringr)
company_name <- distinct(rounds2,rounds2$company_permalink)
nrow(company_name)

###############################################################################################

# unique company in companies
company_name_c <- distinct(companies,companies$permalink)
nrow(company_name_c)

###############################################################################################

# Merging both data frames to create master_frame
companies$permalink <- str_to_title(companies$permalink)
rounds2$company_permalink <- str_to_title(rounds2$company_permalink)

master_frame <- merge(x = rounds2, y = companies,
                      by.x ='company_permalink',by.y = 'permalink',all.y = TRUE )

##############################################################################################

#Average Funding amount of different funding type

library(RMySQL)
con <- dbConnect(RMySQL::MySQL(),
                 dbname ="investment",
                 host ="127.0.0.1",
                 port =3306,
                 user ="root",
                 password ="think@123")

#Average funding amount for Venture type

Vfund <- "select AVG(raised_amount_usd) from rounds2 where funding_round_type='venture'"
avgVFund <- dbGetQuery(con,Vfund)

#Average Funding amount of Angel type

afund <- "select AVG(raised_amount_usd) from rounds2 where funding_round_type='angel'"
avgAFund <- dbGetQuery(con,afund)

#Average Funding amount for Seed type

sfund <- "select AVG(raised_amount_usd) from rounds2 where funding_round_type='seed' "
avgSFund <- dbGetQuery(con,sfund)

#Average Funding amount for Private equity

pfund <- "select AVG( raised_amount_usd) from rounds2 where funding_round_type ='private_equity' "
avgPFund <- dbGetQuery(con,pfund)

############################################################################################################

# grouping based on country code

company_groups <- group_by(master_frame,country_code)
summ_raised_fund <- summarise(company_groups,sum_raised_fund=sum(raised_amount_usd,na.rm = T))

# removing rows with blank country_code

summ_raised_fund <- summ_raised_fund[!(summ_raised_fund$country_code == ""), ]

# selecting top 9 countries based on total fund raised
top_coun_fund <- head(arrange(summ_raised_fund,desc(sum_raised_fund)), n = 9 )

# creating dataframe of top 9 countries
top9 <- data.frame(top_coun_fund$country_code)

###########################################################################################################################
# Checkpoint 4 sector Analysis 

# separating primary sector

newdata_separate <- separate(master_frame,category_list, into = c("prim_cat","sec_cat","ter_cat"),
                             extra = 'drop',sep = "\\|")
newdata_primary_sector <- unique(data.frame(primary_cat=newdata_separate$prim_cat))

# mapping primary sector to eight main sector and creating separate column 

mapping <- read.csv("mapping.csv")

newdata <- gather(mapping,main_sector,my_value, Automotive...Sports : Social..Finance..Analytics..Advertising)
newdata <- newdata[!(newdata$my_value == 0),]
newdata <- newdata[,-3]
x1_join <- right_join(newdata,newdata_primary_sector,by =c("category_list"="primary_cat"))

with_mainsector <- left_join(newdata_separate,x1_join, by =c("prim_cat"="category_list") )

# removing secondary_sector etc.. 

with_mainsector <- with_mainsector[ , -which(names(with_mainsector) %in% c("sec_cat","ter_cat"))]


###########################################################################################################################

# Creating Data frame D1,D2,D3 for each country

dice1 <- subset(with_mainsector,with_mainsector$funding_round_type == 'venture'
                 & with_mainsector$country_code =='USA')
rollup1 <- aggregate(dice1$raised_amount_usd, by = list(main_sector=dice1$main_sector), FUN = sum, na.rm = TRUE )

X2_merge <- merge(dice1,rollup1, by = 'main_sector')

x3 <- X2_merge %>%
  group_by(main_sector) %>%
  summarise(n = n())

D1 <- merge(X2_merge,x3, by = 'main_sector')

############################################################################################################################

dice2 <- subset(with_mainsector,with_mainsector$funding_round_type == 'venture'
                & with_mainsector$country_code =='GBR')
rollup2 <- aggregate(dice2$raised_amount_usd, by = list(main_sector=dice2$main_sector), FUN = sum, na.rm = TRUE )

X2_merge_2 <- merge(dice2,rollup2, by = 'main_sector')

x3_2 <- X2_merge_2 %>%
  group_by(main_sector) %>%
  summarise(n = n())

D2 <- merge(X2_merge_2,x3_2, by = 'main_sector')

###############################################################################################################################

dice3 <- subset(with_mainsector,with_mainsector$funding_round_type == 'venture'
                & with_mainsector$country_code =='IND')
rollup3 <- aggregate(dice3$raised_amount_usd, by = list(main_sector=dice3$main_sector), FUN = sum, na.rm = TRUE )

X2_merge_3 <- merge(dice3,rollup3, by = 'main_sector')

x3_3 <- X2_merge_3 %>%
  group_by(main_sector) %>%
  summarise(n = n())

D3 <- merge(X2_merge_3,x3_3, by = 'main_sector')

################################################################################################################################

# total number of investment
sum(x3$n)       # country 1
sum(x3_2$n)     # country 2
sum(x3_3$n)     # country 3

# total amount of investment
sum(rollup1$x)  # Country 1
sum(rollup2$x)  # country 2
sum(rollup3$x)  # country 3

# company which received highest investment based on top sector

D1_subset <- subset(D1,D1$main_sector=='Others')
D1_subset[which.max(D1_subset$x),]                  # Country 1

D2_subset <- subset(D2,D2$main_sector=='Others')
D2_subset[which.max(D2_subset$x),]                  # Country 2

D3_subset <- subset(D3,D3$main_sector=='Others')
D3_subset[which.max(D3_subset$x),]                  # Country 3

# company which received highest investment based on second best sector

D1_subset1 <- subset(D1,D1$main_sector=='Cleantech...Semiconductors')
D1_subset1[which.max(D1_subset1$x),]

D2_subset1 <- subset(D2,D2$main_sector=='Cleantech...Semiconductors')
D2_subset1[which.max(D2_subset1$x),]

D3_subset1 <- subset(D3,D3$main_sector=='News..Search.and.Messaging')
D3_subset1[which.max(D3_subset1$x),]

#####################################################################################

# End , Thanks.