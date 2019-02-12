#
# Investment Global Trend Analysis
#
# Assumptions
# Data Set Files - companies.txt, rounds2.csv,mapping.csv are available in the working directory
#

library(dplyr)

#--------------------------------------------------------------------------------------------------#
# Checkpoint 1: Data Cleaning 1
#
cat("Checkpoint 1: Data Cleaning 1\n")

# Load the companies from companies.txt and rounds data from rounds2.csv
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)
companies <- read.delim("companies.txt", stringsAsFactors = FALSE)

#
# Solve for Table 1.1
cat("***** Table 1.1 Solutions *****\n")

# 1. How many unique companies are present in rounds2?
# 2. How many unique companies are present in companies?
# 3. In the companies data frame, which column can be used as the unique key for each company? 
#    Write the name of the column.

# Unique key for each company - Permalink
# Convert all permalinks into same format (all lowercase)
rounds2$company_permalink <- tolower(rounds2$company_permalink)
companies$permalink <- tolower(companies$permalink)

# Calculate unique companies
UniqueNo_rounds <- length(unique(rounds2$company_permalink))
UniqueNo_companies <- length(unique(companies$permalink))

cat("Q1 : Total number of unique companies are present in rounds2 =",UniqueNo_rounds,"\n")
cat("Q2 : Total number of unique companies are present in companies =",UniqueNo_companies,"\n")
cat("Q3 : Unique key for each company is permalink\n")

#
# 4. Are there any companies in the rounds2 file which are not present in companies ? 
#    Answer Y/N.

# Check which companies of rounds2 are not present in companies
# difference between two sets using setdiff (dplyr package)

mismatched_companies <- length(setdiff(rounds2$company_permalink,companies$permalink))
cat("Q4 : No of companies in rounds2 not present in companies data set =",mismatched_companies,"\n")

#
# 5. Merge the two data frames so that all variables (columns) in the companies frame are added 
#    to the rounds2 data frame. Name the merged frame master_frame. How many observations are 
#    present in master_frame?

# Change the column name in rounds2 data frame to match with companies data frame column name 
colnames(rounds2)[1] <- "permalink"

# Merge rounds & companies data frames
master_frame <- merge(rounds2, companies, by="permalink")

cat("Q5 : Total no of observation in master_frame =",nrow(master_frame),"\n")


#--------------------------------------------------------------------------------------------------#
# Checkpoint 2: Funding Type Analysis
#
cat("Checkpoint 2: Funding Type Analysis\n")

# Data cleaning - Replace NA with 0 in raised_amount_usd column
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0
# output 1 for tableau : write.csv(master_frame, "6_1funding_type_analysis.csv")

#
# Solve for Table 2.1
cat("***** Table 2.1 Solutions *****\n")

#
# 1. Average funding amount of venture type
# 2. Average funding amount of angel type
# 3. Average funding amount of seed type
# 4. Average funding amount of private equity type

# Group companies into data frames based on their funding round type
VentureFund <- subset(master_frame, funding_round_type=="venture")
AngelFund <- subset(master_frame, funding_round_type=="angel")
SeedFund <- subset(master_frame, funding_round_type=="seed")
PrvEqFund <- subset(master_frame, funding_round_type=="private_equity")

# Calculate avg of funding amount of each type
AvgVenture <- mean(VentureFund$raised_amount_usd)
AvgAngel <- mean(AngelFund$raised_amount_usd)
AvgSeed <- mean(SeedFund$raised_amount_usd)
AvgPrvEq <- mean(PrvEqFund$raised_amount_usd)

cat("Q1 : Average funding amount of venture type =",AvgVenture,"\n")
cat("Q2 : Average funding amount of angel type =",AvgAngel,"\n")
cat("Q3 : Average funding amount of seed type =",AvgSeed,"\n")
cat("Q4 : Average funding amount of private equity type =",AvgPrvEq,"\n")

#
# 5. Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round,
#    which investment type is the most suitable for them?

# Based on avg values, venture type best fits the contraint between 5 to 15 million USD
cat("Q5 : 'Venture' is the best suitable funding type\n")

#--------------------------------------------------------------------------------------------------#
# Checkpoint 3: Country Analysis
#

cat("Checkpoint 3: Country Analysis\n")

#Top nine countries which have received the highest total funding 
#(across ALL sectors for the chosen investment type) 
##---> Our Chosen investment type = "venture"

#For the chosen investment type, make a data frame named top9 
#with the top nine countries (based on the total investment 
#amount each country has received)

#create the top9 countries data frame

#using dplyr %>% function, 
#capture master_frame, filter on venture funds, group by country, 
#summarise on average raised_amount_usd, remove blank country,
#sort by investment descending and finally select top_n where n=9

top9 <- master_frame %>%
  filter(funding_round_type == "venture") %>%
  group_by(country_code) %>%
  summarise(raised_amount_usd = sum(raised_amount_usd)) %>%
  filter(country_code != "") %>%
  arrange(desc(raised_amount_usd)) %>%
  top_n(9, raised_amount_usd) %>% as.data.frame


#top9
#Top9 countries and investments:
# > top9
# country_code raised_amount_usd
# 1          USA      422510842796
# 2          CHN       39835418773
# 3          GBR       20245627416
# 4          IND       14391858718
# 5          CAN        9583332317
# 6          FRA        7259536732
# 7          ISR        6907514579
# 8          DEU        6346959822
# 9          JPN        3363676611


#create a data frame of english speaking country codes
df_en<- data.frame(country_code=c("ATG",
                                  "AUS",
                                  "BRB",
                                  "BLZ",
                                  "BWA",
                                  "CMR",
                                  "CAN",
                                  "DMA",
                                  "ERI",
                                  "ETH",
                                  "FSM",
                                  "FJI",
                                  "GHA",
                                  "GRD",
                                  "GUY",
                                  "IND",
                                  "IRL",
                                  "JAM",
                                  "KEN",
                                  "KIR",
                                  "LSO",
                                  "LBR",
                                  "MWI",
                                  "MLT",
                                  "MHL",
                                  "MUS",
                                  "NAM",
                                  "NRU",
                                  "NZL",
                                  "NGA",
                                  "PAK",
                                  "PLW",
                                  "PNG",
                                  "PHL",
                                  "RWA",
                                  "KNA",
                                  "LCA",
                                  "VCT",
                                  "WSM",
                                  "SYC",
                                  "SLE",
                                  "SGP",
                                  "SLB",
                                  "ZAF",
                                  "SDN",
                                  "SDN",
                                  "SWZ",
                                  "TZA",
                                  "BHS",
                                  "GMB",
                                  "TON",
                                  "TTO",
                                  "TUV",
                                  "UGA",
                                  "GBR",
                                  "USA",
                                  "VUT",
                                  "ZMB",
                                  "ZWE"))

#new column marking whether country is english speaking or not
top9$english_speaking <- top9$country_code %in% df_en$country_code

#output for Tableau
#write.csv(top9, "6_2top9.csv")

#find top 3 english speaking countries
df_top3_en <- arrange(merge(top9, df_en, by="country_code"), desc(raised_amount_usd)) [1:3,]

cat("Solution to Checkpoint 3: Top 3 english speaking countries: ",df_top3_en[1,1],",",df_top3_en[2,1],",",df_top3_en[3,1])


#--------------------------------------------------------------------------------------------------#
# Checkpoint 4: Sector Analysis 1
#
cat("Checkpoint 4: Sector Analysis 1\n")

mapping <- read.csv("mapping.csv",stringsAsFactors = F)

#removing blanks and na from mapping.csv
mapping<-mapping[!(is.na(mapping$category_list) | mapping$category_list==""), ]

#replacing 0 with na in category_list column.eg-A0lytics to analytics.
mapping$category_list <-gsub("0","na",mapping$category_list)

#1.extract primary sector of each category list from the category_list column
master_frame$primary_sector<-sapply(strsplit(master_frame$category_list,split='\\|'),"[", 1)

#2.Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors

library(tidyr)
#mapping file- convert wide to long format
mappingLong <- gather(mapping, sector, my_val, Automotive...Sports:Social..Finance..Analytics..Advertising)
#drop asymmetric variables by filtering my_val to 1
mappingLong <- filter(mappingLong, my_val == 1)

#replace .. with / and . with space 
mappingLong$sector <- gsub("[.]{2}","/",mappingLong$sector)
mappingLong$sector <- gsub("[.]{1}"," ",mappingLong$sector)

#rename the primary_sector column so that it's easy to merge/join with master_frame df
#current value is "category_list"
colnames(mappingLong)[1] <- "primary_sector"

#drop last column my_val which is not needed any more
mappingLong <- mappingLong[,1:ncol(mappingLong)-1]

#join with companies df. we could have used merge and would've got same results.
master_frame <- merge(master_frame, mappingLong, by = "primary_sector")

#remove NA sectors
master_frame <- master_frame[which(!is.na(master_frame$sector)),]

#master_frame dataframe now has the sector information.

#following shows the 8 unique sectors in the master_frame
#unique(master_frame$sector) 


#--------------------------------------------------------------------------------------------------#
# Checkpoint 5: Sector Analysis 2
#
cat("Checkpoint 5: Sector Analysis 2\n")


#force R not to print exponents like 3.3e+07
options(scipen = 999)

#select only US,UK and IND from master_frame dataset
master_frame <- subset(master_frame, country_code %in% df_top3_en[1:3,1]) %>%
                filter(funding_round_type=="venture") %>% 
                group_by(sector) %>%
                mutate(sector_investment_count = length(permalink),
                       sector_inv_amounts = sum(raised_amount_usd)) %>%
                filter(raised_amount_usd > 5000000 & raised_amount_usd < 15000000) %>%
                as.data.frame

D1 <- subset(master_frame, country_code == "USA") %>% arrange(desc(raised_amount_usd))
D2 <- subset(master_frame, country_code == "GBR") %>% arrange(desc(raised_amount_usd))
D3 <- subset(master_frame, country_code == "IND") %>% arrange(desc(raised_amount_usd))

#D1 <- arrange(D1, desc(raised_amount_usd))
#D2 <- arrange(D2, desc(raised_amount_usd))
#D3 <- arrange(D3, desc(raised_amount_usd))

# Number of investments

#USA
nrow(D1)
#[1] 9942

#GBR
nrow(D2)
#[1] 559

#IND
nrow(D3)
#[1] 243

# Amount of investment
#USA
sum(D1$raised_amount_usd)
#[1] 88,638,294,664

#GBR
sum(D2$raised_amount_usd)
#[1] 4,875,228,300

#IND
sum(D3$raised_amount_usd)
#[1] 2,174,543,602

# D1 : USA : Top 3 Sectors based on # of investments
arrange(aggregate(permalink~country_code+sector, data = D1, FUN = "length"), desc(permalink))[1:3,]

# D2 : GBR : Top 3 Sectors based on # of investments
arrange(aggregate(permalink~country_code+sector, data = D2, FUN = "length"), desc(permalink))[1:3,]

# D3 : IND : Top 3 Sectors based on # of investments
arrange(aggregate(permalink~country_code+sector, data = D3, FUN = "length"), desc(permalink))[1:3,]

# Top sector based on count of investments
#USA
arrange(aggregate(permalink~sector, data = D1, FUN = "length"), desc(permalink))[1,]
#D1 Others 2423

#GBR
arrange(aggregate(permalink~sector, data = D2, FUN = "length"), desc(permalink))[1,]
#D2 Others 129

#IND
arrange(aggregate(permalink~sector, data = D3, FUN = "length"), desc(permalink))[1,]
#D1 Others 78

# Second best sector based on count of investments
#USA
arrange(aggregate(permalink~sector, data = D1, FUN = "length"), desc(permalink))[2,]
#                                 sector permalink
#2 Social/Finance/Analytics/Advertising      2216

#GBR
arrange(aggregate(permalink~sector, data = D2, FUN = "length"), desc(permalink))[2,]
#                     sector permalink
#2 Cleantech/ Semiconductors       124

#IND
arrange(aggregate(permalink~sector, data = D3, FUN = "length"), desc(permalink))[2,]
#                                sector permalink
#2 Social/Finance/Analytics/Advertising        45

# Third best sector based on count of investments

#USA
arrange(aggregate(permalink~sector, data = D1, FUN = "length"), desc(permalink))[3,]
#sector permalink
#3 Cleantech/ Semiconductors      1951

#GBR
arrange(aggregate(permalink~sector, data = D2, FUN = "length"), desc(permalink))[3,]
#sector permalink
#3 Social/Finance/Analytics/Advertising       118

#IND
arrange(aggregate(permalink~sector, data = D3, FUN = "length"), desc(permalink))[3,]
#sector permalink
#3 News/Search and Messaging        41


#Top 3 sector+company combination based on # of investments
arrange(aggregate(permalink~country_code+sector+name, data = D1, FUN = "length"), desc(permalink))[1:3,]
arrange(aggregate(permalink~country_code+sector+name, data = D2, FUN = "length"), desc(permalink))[1:3,]
arrange(aggregate(permalink~country_code+sector+name, data = D3, FUN = "length"), desc(permalink))[1:3,] 


#Q9 Top company in best sector
#Top sector count wise is for sector = 'Others' in all countries

D1 %>%
  subset(sector == 'Others') %>%
  aggregate(raised_amount_usd~country_code+sector+name, data= ., FUN= "sum") %>%
  arrange(desc(raised_amount_usd))%>% top_n(1)

D2 %>%
  subset(sector == 'Others') %>%
  aggregate(raised_amount_usd~country_code+sector+name, data= ., FUN= "sum") %>%
  arrange(desc(raised_amount_usd))%>% top_n(1)

D3 %>%
  subset(sector == 'Others') %>%
  aggregate(raised_amount_usd~country_code+sector+name, data= ., FUN= "sum") %>%
  arrange(desc(raised_amount_usd))%>% top_n(1)


#Q10 Top company in second best sector in each country

#US: Social/Finance/Analytics/Advertising
D1 %>%
  subset(sector == 'Social/Finance/Analytics/Advertising') %>%
  aggregate(raised_amount_usd~country_code+sector+name, data= ., FUN= "sum") %>%
  arrange(desc(raised_amount_usd))%>% top_n(1)
#SST Inc. (Formerly ShotSpotter)


#GBR: Cleantech/ Semiconductors
D2 %>%
  subset(sector == 'Cleantech/ Semiconductors') %>%
  aggregate(raised_amount_usd~country_code+sector+name, data= ., FUN= "sum") %>%
  arrange(desc(raised_amount_usd))%>% top_n(1)
#EUSA Pharma

#IND: Social/Finance/Analytics/Advertising
D3 %>%
  subset(sector == 'Social/Finance/Analytics/Advertising') %>%
  aggregate(raised_amount_usd~country_code+sector+name, data= ., FUN= "sum") %>%
  arrange(desc(raised_amount_usd))%>% top_n(1)
#inTarvo


#--------------------------------------------------------------------------------------------------#



