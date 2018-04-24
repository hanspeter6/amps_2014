# libraries
library(stringr)
library(tidyverse)
library(readstata13)
library(caret)

print_14 <- read.dta13("stata/amps-2014-newspaper-magazine-readership-v1.1.dta")
electr_14 <- read.dta13("stata/amps-2014-electronic-media-v1.1.dta")
internet_14 <- read.dta13("stata/amps-2014-cellphone-and-internet-v1.1.dta")
demogrs_14 <- read.dta13("stata/amps-2014-demographics-v1.1.dta")
lsm_14 <- read.dta13("stata/amps-2014-lsm-and-saarf-segmentations-v1.1.dta")
attitudes_14 <- read.dta13("stata/amps-2014-attitudes-v1.1.dta")
lifestage_14 <- read.dta13("stata/amps-2014-lifestages-v1.1.dta")
personal_14 <- read.dta13("stata/amps-2014-personal-v1.1.dta")
# 
save(print_14, electr_14, internet_14, demogrs_14, personal_14, lsm_14, lifestage_14, attitudes_14, file = "input_14.RData")

load("input_14.RData")
# 
print_14_labels <- readLines("stata/amps_2014_print_variable_labels.txt")
electr_14_labels <- readLines("stata/amps_2014_electonic_variable_labels.txt")
internet_14_labels <- readLines("stata/amps_2014_internet_variable_labels.txt")
demogrs_14_labels <- readLines("stata/amps_2014_demographics_variable_labels.txt")
personal_14_labels <- readLines("stata/amps_2014_personal_variable_labels.txt")
lsm_14_labels <- readLines("stata/amps_2014_lsm_variable_labels.txt")
lifestage_14_labels <- readLines("stata/amps_2014_lifestages_variable_labels.txt")
attitudes_14_labels <- readLines("stata/amps_2014_attitudes_variable_labels.txt")

save(print_14_labels, electr_14_labels, internet_14_labels, demogrs_14_labels, personal_14_labels, lsm_14_labels, lifestage_14_labels, attitudes_14_labels, file = "labels_14.RData")

load("labels_14.RData")

## 1st Print (newspapers and magazines) Media Set

names_issues_print_14 <- str_subset(print_14_labels, 'Number of different issues usually read') %>%
        str_replace('.+\\s-', '') %>%
        str_trim()
vars_issues_print_14 <- str_subset(print_14_labels, 'Number of different issues usually read') %>%
        str_replace('Number\\sof\\sdifferent.+', '') %>%
        str_trim()

##Newspapers
# fix names and get rid of some and save
# names_newspapers_14_issues <- names_issues_print_14[c(1:49,136)]
# fix(names_newspapers_14_issues)
# saveRDS(names_newspapers_14_issues, "names_newspapers_14_issues.rds")
names_newspapers_14_issues <- readRDS("names_newspapers_14_issues.rds")

# vector of variables
vars_newspapers_14_issues <- vars_issues_print_14[c(1:49, 136)]
issues_newspapers_14 <- print_14[,vars_newspapers_14_issues]

# Magazines
# fix names and get rid of some (including MNet guides and save
# names_magazines_14_issues <- names_issues_print_14[c(51:63,65:79,81:87,89:110,112:126,128:135,138,140:152)]
# fix(names_magazines_14_issues)
# saveRDS(names_magazines_14_issues, "names_magazines_14_issues.rds")
names_magazines_14_issues <- readRDS("names_magazines_14_issues.rds")

# vector of variables
vars_magazines_14_issues <- vars_issues_print_14[c(51:63,65:79,81:87,89:110,112:126,128:135,138,140:152)]
issues_magazines_14 <- print_14[,vars_magazines_14_issues]


## THOUROUGHLY
names_thorough_print_14 <- str_subset(print_14_labels, 'How thoroughly respondent usually read') %>%
        str_replace('.+\\s-', '') %>%
        str_replace("\\'",'') %>%
        str_trim()
vars_thorough_print_14 <- str_subset(print_14_labels, 'How thoroughly respondent usually read') %>%
        str_replace('How\\sthoroughly.+', '') %>%
        str_trim()

##Newspapers
# get names and get rid of some and save (already sorted above)
# names_newspapers_14_thorough <- names_thorough_print_14[c(1:39,77)]
# fix(names_newspapers_14_thorough)
# saveRDS(names_newspapers_14_issues, "names_newspapers_14_issues.rds")

# vector of variables
vars_newspapers_14_thorough <- vars_thorough_print_14[c(1:35,37:50,36)]
thorough_newspapers_14 <- print_14[,vars_newspapers_14_thorough]
thorough_newspapers_14 <- 7 - thorough_newspapers_14

# Magazines
# fix names and get rid of some and save
# names_magazines_14_thorough <- names_thorough_print_14[c(77:99,103:107,109:157)]
# fix(names_magazines_14_issues)
# saveRDS(names_magazines_14_issues, "names_magazines_14_issues.rds")

# vector of variables
vars_magazines_14_thorough <- vars_thorough_print_14[c(52:64,66:80,82:88,90:111,113:127,129:136,139:140,142:154)]
thorough_magazines_14 <- print_14[,vars_magazines_14_thorough]

# Elle Decoration missing in "issues". Remove
thorough_magazines_14 <- thorough_magazines_14[,-81]

# # need to reverse numbering to serve as weights (see value_lables text file):
thorough_magazines_14 <- 7 - thorough_magazines_14

# create datasets ...for newspapers and magazines:
newspapers_engagement_14_all <- issues_newspapers_14 * thorough_newspapers_14
names(newspapers_engagement_14_all) <- names_newspapers_14_issues
magazines_engagement_14_all <- issues_magazines_14 * thorough_magazines_14
names(magazines_engagement_14_all) <- names_magazines_14_issues

newspapers_engagement_14_simple_all <- issues_newspapers_14
names(newspapers_engagement_14_simple_all) <- names_newspapers_14_issues
magazines_engagement_14_simple_all <- issues_magazines_14
names(magazines_engagement_14_simple_all) <- names_magazines_14_issues

# # # replace NAs with zeros
newspapers_engagement_14_all[is.na( newspapers_engagement_14_all)] <- 0
magazines_engagement_14_all[is.na(magazines_engagement_14_all)] <- 0

newspapers_engagement_14_simple_all[is.na(newspapers_engagement_14_simple_all)] <- 0
magazines_engagement_14_simple_all[is.na(magazines_engagement_14_simple_all)] <- 0

# save (alls)
saveRDS(newspapers_engagement_14_all, "newspapers_engagement_14_all.rds")
saveRDS(magazines_engagement_14_all, "magazines_engagement_14_all.rds")
saveRDS(newspapers_engagement_14_simple_all, "newspapers_engagement_14_simple_all.rds")
saveRDS(magazines_engagement_14_simple_all, "magazines_engagement_14_simple_all.rds")

## CLEAN UP and reduce variables

# for newspapers: include The Zimbabwean as Other
other.news <- newspapers_engagement_14_all[,49]
newspapers_engagement_14 <- newspapers_engagement_14_all %>%
        mutate(other.news = other.news)
newspapers_engagement_14 <- newspapers_engagement_14[,-c(49)]

other.news_simple <- newspapers_engagement_14_simple_all[,49]
newspapers_engagement_14_simple <- newspapers_engagement_14_simple_all %>%
        mutate(other.news = other.news_simple)
newspapers_engagement_14_simple <- newspapers_engagement_14_simple[,-c(49)]

# for magazines - dealt with it in vehicle_cleaning project and read in the resulting object
magazines_engagement_14 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_14.rds")
magazines_engagement_14_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_14_simple.rds")


# save them in this project
saveRDS(newspapers_engagement_14, "newspapers_engagement_14.rds")
saveRDS(magazines_engagement_14, "magazines_engagement_14.rds")
saveRDS(newspapers_engagement_14_simple, "newspapers_engagement_14_simple.rds")
saveRDS(magazines_engagement_14_simple, "magazines_engagement_14_simple.rds")

magazines_engagement_14 <- readRDS("magazines_engagement_14.rds")
newspapers_engagement_14 <- readRDS("newspapers_engagement_14.rds")
magazines_engagement_14_simple <- readRDS("magazines_engagement_14_simple.rds")
newspapers_engagement_14_simple <- readRDS("newspapers_engagement_14_simple.rds")

## 2nd Electronic Media Set
# RADIO

# use 4 weeks for names vector
names_radio_14_4w <- electr_14_labels %>%
        str_subset('ca64co\\d{2}_\\d') %>%
        str_replace('.+\\s-\\s','') %>%
        str_trim()
# # get rid of tail lines
# names_radio_14_4w <- names_radio_14_4w[1:100] # get rid of summaries & "unsure" &"none"

# names_radio_14 <- names_radio_14_4w
# 
# check_radio_12 <- readRDS("names_radio_12_copy.rds")
# 
# fix(names_radio_14)

# saveRDS(names_radio_14, "names_radio_14.rds")
names_radio_14 <- readRDS('names_radio_14.rds')

# get data...
radio4weeks_14 <- electr_14[,str_detect(names(electr_14), 'ca64co\\d{2}_\\d')]
radio4weeks_14 <- radio4weeks_14[,c(1:100,104)] # get rid of "unsure" and "none" etc..

radio7days_14 <- electr_14[,str_detect(names(electr_14), 'ca65co\\d{2}_\\d')]
radio7days_14 <- radio7days_14[,c(1:91, 95)]  # get rid of "unsure" and "none" etc...

radioYesterday_14 <- electr_14[,str_detect(names(electr_14), 'ca66co\\d{2}_\\d')]
radioYesterday_14 <- radioYesterday_14[,c(1:68, 72)]  # get rid of "unsure" and "none" etc..

# identifying missing stations by changing all to "64"
a <- names(radio4weeks_14)
b <- names(radio7days_14)
c <- names(radioYesterday_14)
b_adj <- b %>%
        str_replace("65", "64")
c_adj <- c %>%
        str_replace("66", "64")

names(radio7days_14) <- b_adj
names(radioYesterday_14) <- c_adj

ind_7 <- which(names(radio4weeks_14) %in% names(radio7days_14))
ind_y <- which(names(radio4weeks_14) %in% names(radioYesterday_14))

# adding up
radio4weeks_14[,ind_7] <- radio4weeks_14[,ind_7] + radio7days_14
radio4weeks_14[,ind_y] <- radio4weeks_14[,ind_y] + radioYesterday_14

# creating engagement set:
radio_engagement_14_all <- radio4weeks_14
names(radio_engagement_14_all) <- names_radio_14

saveRDS(radio_engagement_14_all, "radio_engagement_14_all.rds")
radio_engagement_14_all <- readRDS("radio_engagement_14_all.rds")

# AFTER CLEANING (see vehicle cleaning project)
radio_engagement_14 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/radio_engagement_14.rds")



# tv need to include "other"...
# then deal with simple internet
# then onto explore14

## TV (this year, included specific dstv and toptv channels (will include them))

# check_tv_names_12 <- readRDS("names_tv_12_copy.rds")

names_tv_14 <- c("e tv",
                 "SABC 1",
                 "SABC 2",
                 "SABC 3",
                 "IKZN TV",
                 "Bay TV",
                 "Cape Town TV",
                 "Soweto TV",
                 "Tswane TV",
                 "Top TV",
                 "DSTV",
                 "Other TV")

saveRDS(names_tv_14, "names_tv_14.rds")
names_tv_14 <- readRDS("names_tv_14.rds")

# want to isolate only past 4 weeks and get rid of ("UNSURE", and "no TV") 
# also NB for 2014 topTV changed name to Starsat. Will stick to topTV here.
tv4weeks_14 <- electr_14[,c('ca45co49_1',
                            'ca45co49_4',
                            'ca45co49_5',
                            'ca45co49_6',
                            'ca45co49_7',
                            'ca45co49_8',
                            'ca45co49_9',
                            'ca45co50_1',
                            'ca45co50_2',
                            'ca46co72_3',
                            'ca46co72_8',
                            'ca45co50_5')] 

# want to isolate only past 7 days...
tv7days_14 <- electr_14[,c('ca45co51_1',
                           'ca45co51_4',
                           'ca45co51_5',
                           'ca45co51_6',
                           'ca45co51_7',
                           'ca45co51_8',
                           'ca45co51_9',
                           'ca45co52_1',
                           'ca45co52_2',
                           'ca46co74_3',
                           'ca46co74_8',
                           'ca45co52_5')] 

# want to isolate only yesterday...(indexes w.r.t 4weeks that are missing here: 7, 10)
tvYesterday_14 <- electr_14[,c('ca45co53_1',
                               'ca45co53_4',
                               'ca45co53_5',
                               'ca45co53_6',
                               'ca45co53_7',
                               'ca45co53_8',
                               'ca45co53_9',
                               'ca45co54_1',
                               'ca45co54_2',
                               'ca46co76_3',
                               'ca46co76_8',
                               'ca45co53_5')]

# combining into a tv engagement dataset (using tv4weeks_14 as basis):

tv_engagement_14 <- tv4weeks_14 + tv7days_14 +tvYesterday_14
names(tv_engagement_14) <- names_tv_14

saveRDS(tv_engagement_14, "tv_engagement_14.rds")

tv_engagement_14 <- readRDS("tv_engagement_14.rds")

## 3rd Internet Media Set

## accessed: sum of 12 months, 4weeks, 7days and yesterday
internet_level1 <- internet_14[,str_detect(names(internet_14), 'ca49co(63_1)|(63_2)|(63_3)|(63_4)')]

internet_level1 <- rowSums(internet_level1)

# what internet was accessed for...
##  (maybe could use similar to vehicles?? as well as add up and multiply with first eng):

int_search <- internet_14[,c('ca49co15_1',
                             'ca49co15_2')] %>%
        mutate(sum = ca49co15_1 + ca49co15_2)
        
int_search <- ifelse(int_search$sum == 2, 1, int_search$sum)

int_social <- internet_14[,c('ca49co18_1',
                             'ca49co18_2',
                             'ca49co19_1',
                             'ca49co19_2',
                             'ca49co20_1',
                             'ca49co20_2')]
int_social <- rowSums(int_social)
int_social <- as.vector(ifelse(int_social != 0, 1, int_social))

int_print <- internet_14[,c('ca49co30_1',
                             'ca49co30_2')] %>%
        mutate(sum = ca49co30_1 + ca49co30_2)

int_print <- ifelse(int_print$sum == 2, 1, int_print$sum)

int_news <- internet_14[,c('ca49co31_1',
                            'ca49co31_2')] %>%
        mutate(sum = ca49co31_1 + ca49co31_2)

int_news <- ifelse(int_news$sum == 2, 1, int_news$sum)

int_tv <- internet_14[,c('ca49co36_1',
                           'ca49co36_2')] %>%
        mutate(sum = ca49co36_1 + ca49co36_2)

int_tv <- ifelse(int_tv$sum == 2, 1, int_tv$sum)

int_radio <- internet_14[,c('ca49co38_1',
                         'ca49co38_2')] %>%
        mutate(sum = ca49co38_1 + ca49co38_2)

int_radio <- ifelse(int_radio$sum == 2, 1, int_radio$sum)


internet_level2 <- data.frame(int_search,
                              int_social,
                              int_print,
                              int_news,
                              int_tv,
                              int_radio)

## create single dataframe for internet multiplying internet_level1 with sum of internet_level2:
internet_engagement_14 <- internet_level2  * internet_level1
internet_engagement_14_simple <- internet_level1

saveRDS(internet_engagement_14, "internet_engagement_14.rds")
saveRDS(internet_engagement_14_simple, "internet_engagement_14_simple.rds")

internet_engagement_14 <- readRDS("internet_engagement_14.rds")
internet_engagement_14_simple <- readRDS("internet_engagement_14_simple.rds")

## create single dataframe for media14, including total_engagement columns 
# 
# Level 1: Type
media_type_14 <- data.frame(cbind(qn = demogrs_14$qn,
                                  rowSums(newspapers_engagement_14),
                                  rowSums(magazines_engagement_14),
                                  rowSums(radio_engagement_14),
                                  rowSums(tv_engagement_14),
                                  rowSums(internet_engagement_14)))
names(media_type_14) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
media_type_14 <- media_type_14 %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet))


media_type_14_simple <- data.frame(cbind(qn = demogrs_14$qn,
                                  rowSums(newspapers_engagement_14_simple),
                                  rowSums(magazines_engagement_14_simple),
                                  rowSums(radio_engagement_14),
                                  rowSums(tv_engagement_14),
                                  internet_engagement_14_simple))
names(media_type_14_simple) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
media_type_14_simple <- media_type_14_simple %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet))

# Level 2: Vehicles
media_vehicles_14 <- data.frame(cbind(qn = demogrs_14$qn,
                                      newspapers_engagement_14,
                                      magazines_engagement_14,
                                      radio_engagement_14,
                                      tv_engagement_14,
                                      internet_engagement_14))
media_vehicles_14_simple <- data.frame(cbind(qn = demogrs_14$qn,
                                      newspapers_engagement_14_simple,
                                      magazines_engagement_14_simple,
                                      radio_engagement_14,
                                      tv_engagement_14,
                                      internet_eng = internet_engagement_14_simple))

saveRDS(media_type_14, 'media_type_14.rds')
saveRDS(media_type_14_simple, 'media_type_14_simple.rds')

saveRDS(media_vehicles_14, 'media_vehicles_14.rds')
saveRDS(media_vehicles_14_simple, 'media_vehicles_14_simple.rds')

media_type_14 <- readRDS('media_type_14.rds')
media_vehicles_14 <- readRDS('media_vehicles_14.rds')
media_type_14_simple <- readRDS('media_type_14_simple.rds')
media_vehicles_14_simple <- readRDS('media_vehicles_14_simple.rds')


## 4th Demographics Set (see notes for descriptions)

age <- personal_14[,'ca56co34']
sex <- demogrs_14[,'ca91co51a']
edu <- demogrs_14[,'ca91co48']
for(i in 1: length(edu)) {
        if(edu[i] %in% c(6,7)) {
                edu[i] <- edu[i] + 1
        }
        else if(edu[i] == 8) {
                edu[i] <- 6
        }
}
hh_inc <- demogrs_14[,'ca91co50']
race <- demogrs_14[,'ca91co51b']
province <- demogrs_14[,'ca91co56']
metro1 <- demogrs_14[,'ca91co57']
metro2 <- demogrs_14[,'ca91co58'] + 9
metro <- rowSums(cbind(metro1,
                       metro2), na.rm = TRUE)

# collect and code into single metro set:
#0 = no metro
#1 Cape Town
#2 Cape Town Fringe Area
#3 Port Elizabeth/Uitenhage
#4 East London
#5 Durban
#6 Bloemfontein
#7 Greater Johannesburg
#8 Reef
#9 Pretoria
#10 Kimberley
##11 Pietermaritzburg
##12 Vaal
##13 Welkom/Witbank??

metro <- ifelse(metro == 19, 7, metro) # add soweto back to greater jhb
metro <- ifelse(metro == 13, 12, metro) # vaal
metro <- ifelse(metro == 15, 13, metro) # welkom/witbank
table(metro)

lang <- as.numeric(demogrs_14[,'ca91co75']) # here no need to change 0 to 1...??
lifestages <- demogrs_14[,'ca91co77']
mar_status <- personal_14[,'ca56co09']
# pers_inc1 <- personal_14[,'ca57co61']
# pers_inc2 <- personal_14[,'ca57co62'] + 10
# pers_inc3 <- personal_14[,'ca57co63'] + 20
# pers_inc4 <- personal_14[,'ca57co64'] + 30
# for(i in 1: length(pers_inc4)) {
#         if(!is.na(pers_inc4[i])) {
#                 if(pers_inc4[i] == 31) {
#                         pers_inc4[i] <- 0
#                 }
#                 if(pers_inc4[i] == 32) {
#                         pers_inc4[i] <- 60
#                 }
#         }
# }
# pers_inc <- rowSums(cbind(pers_inc1,
#                           pers_inc2,
#                           pers_inc3,
#                           pers_inc4), na.rm = TRUE)
lsm <- lsm_14[,'ca91co64']
lsm <- ifelse(lsm == 0,10,lsm)


# lifestyle groups total groups lsm groups 1:10
#Value	Category (changed by 1)
#0	None
#1	Cell Sophisticates
#2	Outdoors
#3	Avid Readers
#4	Sports
#5	Traditionals
#6	Gamers
#7	Studious
#8	Showgoers
lifestyle <- lsm_14[,'ca58co19'] + 1 # to get rid of zero

# attitudes::
# want:
#1: None 
#2: Now Generation
#3: Nation Builders
#4: Distants_Survivors
#5: Distants_Established
#6: Rooted
#7: Global Citizens
attitudesA <- lsm_14[,'ca72co10'] + 1 # to get rid of zeros
attitudesB <- lsm_14[,'ca72co10_lsm']

attitudesA <- ifelse(is.na(attitudesA), 0, attitudesA)
attitudesB <- ifelse(is.na(attitudesB), 0, attitudesB)

attitudesB <- ifelse(attitudesB == 8, 4, attitudesB)
attitudesB <- ifelse(attitudesB == 9, 5, attitudesB)

attitudesA <- ifelse(attitudesA == 4, 0, attitudesA)
attitudesA <- ifelse(attitudesA == 5 | attitudesA == 6, attitudesA + 1, attitudesA)

attitudes <- attitudesA + attitudesB
table(attitudes) # check


demographics_14 <- data.frame(qn = demogrs_14$qn,
                              pwgt = demogrs_14$pwgt,
                              age,
                              sex,
                              edu,
                              hh_inc,
                              race,
                              province,
                              metro,
                              lang,
                              lifestages,
                              mar_status,
                              lsm,
                              lifestyle,
                              attitudes)


#reducing levels of categorical variables and setting factor types for demographics:
# age:
demographics_14$age <- ifelse(demographics_14$age %in% c(1,2), 1, demographics_14$age)
demographics_14$age <- ifelse(demographics_14$age %in% c(3,4), 2, demographics_14$age)
demographics_14$age <- ifelse(demographics_14$age %in% c(5,6), 3, demographics_14$age)
demographics_14$age <- ifelse(demographics_14$age %in% c(7,8), 4, demographics_14$age)
demographics_14$age <- factor(demographics_14$age, ordered = TRUE)

# sex:
demographics_14$sex <- factor(demographics_14$sex, ordered = FALSE)

#edu:
demographics_14$edu <- ifelse(demographics_14$edu %in% c(1,2,3,4), 1, demographics_14$edu)
demographics_14$edu <- ifelse(demographics_14$edu %in% c(5), 2, demographics_14$edu)
demographics_14$edu <- ifelse(demographics_14$edu %in% c(6,7,8), 3, demographics_14$edu)
demographics_14$edu <- factor(demographics_14$edu, ordered = TRUE)

#hh_inc
demographics_14$hh_inc <- ifelse(demographics_14$hh_inc %in% c(1,2,3,4), 1, demographics_14$hh_inc)
demographics_14$hh_inc <- ifelse(demographics_14$hh_inc %in% c(5,6), 2, demographics_14$hh_inc)
demographics_14$hh_inc <- ifelse(demographics_14$hh_inc %in% c(7), 3, demographics_14$hh_inc)
demographics_14$hh_inc <- ifelse(demographics_14$hh_inc %in% c(8), 4, demographics_14$hh_inc)
demographics_14$hh_inc <- factor(demographics_14$hh_inc, ordered = TRUE)

demographics_14$race <- factor(demographics_14$race, ordered = FALSE)
demographics_14$province <- factor(demographics_14$province, ordered = FALSE)
demographics_14$metro <- factor(demographics_14$metro, ordered = FALSE)
demographics_14$lang <- factor(demographics_14$lang, ordered = FALSE)
demographics_14$lifestages <- factor(demographics_14$lifestages, ordered = FALSE)
demographics_14$mar_status <- factor(demographics_14$mar_status, ordered = FALSE)

# lsm
demographics_14$lsm <- ifelse(demographics_14$lsm %in% c(1,2), 1, demographics_14$lsm)
demographics_14$lsm <- ifelse(demographics_14$lsm %in% c(3,4), 2, demographics_14$lsm)
demographics_14$lsm <- ifelse(demographics_14$lsm %in% c(5,6), 3, demographics_14$lsm)
demographics_14$lsm <- ifelse(demographics_14$lsm %in% c(7,8), 4, demographics_14$lsm)
demographics_14$lsm <- ifelse(demographics_14$lsm %in% c(9,10), 5, demographics_14$lsm)
demographics_14$lsm <- factor(demographics_14$lsm, ordered = TRUE)

demographics_14$lifestyle <- factor(demographics_14$lifestyle, ordered = FALSE)
demographics_14$attitudes <- factor(demographics_14$attitudes, ordered = FALSE)

saveRDS(demographics_14, "demographics_14.rds")
demographics_14 <- readRDS("demographics_14.rds")

# read datafiles again if necessary
magazines_engagement_14 <- readRDS("magazines_engagement_14.rds")
magazines_engagement_14_simple <- readRDS("magazines_engagement_14_simple.rds")
newspapers_engagement_14 <- readRDS("newspapers_engagement_14.rds")
newspapers_engagement_14_simple <- readRDS("newspapers_engagement_14_simple.rds")
radio_engagement_14 <- readRDS("radio_engagement_14.rds")
tv_engagement_14 <- readRDS("tv_engagement_14.rds")
internet_engagement_14 <- readRDS("internet_engagement_14.rds")
internet_engagement_14_simple <- readRDS("internet_engagement_14_simple.rds")

media_type_14 <- readRDS("media_type_14.rds")
media_vehicles_14 <- readRDS("media_vehicles_14.rds")
media_type_14_simple <- readRDS("media_type_14_simple.rds")
media_vehicles_14_simple <- readRDS("media_vehicles_14_simple.rds")

demographics_14 <- readRDS("demographics_14.rds")



# #create single dataset minus non metropolitans
set14 <- demographics_14 %>%
        left_join(media_type_14) %>%
        left_join(media_vehicles_14) %>%
        filter(metro != 0)
set14_simple <- demographics_14 %>%
        left_join(media_type_14_simple) %>%
        left_join(media_vehicles_14_simple) %>%
        filter(metro != 0)


# get rid of zero variances:
ind_14 <- nearZeroVar(set14[,16:ncol(set14)], saveMetrics = TRUE)
good_set <- set14[,16:ncol(set14)][,!ind_14$zeroVar]
set14 <- data.frame(cbind(set14[,1:15], good_set))

ind_14_simple <- nearZeroVar(set14_simple[,16:ncol(set14_simple)], saveMetrics = TRUE)
good_set_simple <- set14_simple[,16:ncol(set14_simple)][,!ind_14_simple$zeroVar]
set14_simple <- data.frame(cbind(set14_simple[,1:15], good_set_simple))

# scale media type and media vehicles
set14[,16:ncol(set14)] <- scale(set14[,16:ncol(set14)])
set14_simple[,16:ncol(set14_simple)] <- scale(set14_simple[,16:ncol(set14_simple)])

# save them:
saveRDS(set14, "set14.rds")
saveRDS(set14_simple, "set14_simple.rds")

