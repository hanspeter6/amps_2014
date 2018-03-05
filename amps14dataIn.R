# libraries
library(stringr)
library(tidyverse)
library(readstata13)

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
# print_14_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2014/csv/metadata/variable_labels/amps-2014-newspaper-magazine-readership-v1.1_variable_labels.txt")
# electr_14_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2014/csv/metadata/variable_labels/amps-2014-electronic-media-v1.1_variable_labels.txt")
# internet_14_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2014/csv/metadata/variable_labels/amps-2014-cellphone-and-internet-v1.1_variable_labels.txt")
# demogrs_14_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2014/csv/metadata/variable_labels/amps-2014-demographics-v1.1_variable_labels.txt")
# personal_14_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2014/csv/metadata/variable_labels/amps-2014-personal-v1.2_variable_labels.txt")
# lsm_14_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2014/csv/metadata/variable_labels/amps-2014-lsm-saarf-segmentations-v1.1_variable_labels.txt")
# lifestage_14_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2014/csv/metadata/variable_labels/amps-2014-lifestage-v1.1_variable_labels.txt")
# attitudes_14_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2014/csv/metadata/variable_labels/amps-2014-attitudes-v1.1_variable_labels.txt")
# 
# 
# # 
# save(print_14_labels, electr_14_labels, internet_14_labels, demogrs_14_labels, personal_14_labels, lsm_14_labels, lifestage_14_labels, attitudes_14_labels, file = "labels_14.RData")
# 
# load("labels_14.RData")












## 1st Print (newspapers and magazines) Media Set

# names_print_14 <- str_subset(print_14_labels, 'Number of different issues usually read or page through') %>%
#         str_replace('.+\\s-', '')

saveRDS(names_print_14, "names_print_14.rds")

names_print_14 <- readRDS("names_print_14.rds")

names_dailies_14 <- names_print_14[1:22]
names_biweeklies_14 <- names_print_14[23]
names_weeklies_14 <- names_print_14[24:52]

# # NBNB: Not community papers in 2014...
# names_community_cape_town <- names_print[40:51]
# names_community_restCape <- names_print[52:61]
# names_community_FreeState <- names_print[62:66]
# names_community_NWest <- names_print[67:68]
# names_community_Jhb <- names_print[69]
# names_community_ERand <- names_print[70:71]
# names_community_KZn <- names_print[72:74]

names_mags_weekly_14 <- names_print_14[53:65]
names_fortnightly_mags_14 <- names_print_14[66:67]
names_monthly_news_14 <- names_print_14[68:69]
names_monthly_mags_14 <- names_print_14[70:147]

names_alt_monthly_14 <- names_print_14[150:161]
names_quarterly_mags_14 <- names_print_14[164:168]

names_monthly_store_mags_14 <- names_print_14[148:149]
names_alt_month_store_mags_14 <- names_print_14[162:163]
names_quarterly_store_mags_14 <- names_print_14[169:172]

# create print dataset:
issues_14 <- print_14[,str_detect(names(print_14), 'ca[345678]co\\d{2}')]
issues_14 <- issues_14[,-which(names(issues_14) == "ca6co40")] # get rid of one variable name = '0'. All NAs
names(issues_14) <- names_print_14


saveRDS(issues_14, "issues_14.rds")

thorough_14 <- print_14[,str_detect(names(print_14), 'ca((34)|(35)|(36)|(37)|(38)|(39))co\\d{2}')]
thorough_14 <- thorough_14[,-which(names(thorough_14) == 'ca38co50'| names(thorough_14) == 'ca38co51')] # get rid of six-week mags (zigzag and saltwater girl) not in issues

names(thorough_14) <- names_print_14

# # need to reverse numbering to serve as weights (see value_lables text file):
thorough_14 <- 7 - thorough_14

saveRDS(thorough_14, "thorough_14.rds")
# create single print dataset:

print_engagement_14 <- issues_14 * thorough_14

# replace nas with zero's:
print_engagement_14[is.na(print_engagement_14)] <- 0

saveRDS(print_engagement_14, "print_engagement_14.rds")

print_engagement_14 <- readRDS("print_engagement_14.rds")

newspapers_engagement_14 <- print_engagement_14[,c(1:52,68,69)]
magazines_engagement_14 <- print_engagement_14[,c(53:67,70:172)]

saveRDS(newspapers_engagement_14, "newspapers_engagement_14.rds")
saveRDS(magazines_engagement_14, "magazines_engagement_14.rds")

magazines_engagement_14 <- readRDS("magazines_engagement_14.rds")
newspapers_engagement_14 <- readRDS("newspapers_engagement_14.rds")

## 2nd Electronic Media Set
# RADIO

names_radio_14_4w <- electr_14_labels %>%
        str_subset('ca64co\\d{2}_\\d') %>%
        str_replace('.+listened.+4\\sweeks\\s-\\s','')
names_radio_14_4w <- names_radio_14_4w[-c(98,99)] # get rid of "unsure" and "none"

names_radio_14_7 <- electr_14_labels %>%
        str_subset('ca65co\\d{2}_\\d') %>%
        str_replace('.+listened.+7\\sdays\\s-\\s','')
names_radio_14_7 <- names_radio_14_7[-c(81, 87,88)] # get rid of "unsure" and "none" & empty one 

names_radio_14_y <- electr_14_labels %>%
        str_subset('ca66co\\d{2}_\\d') %>%
        str_replace('.+listened\\sto\\syesterday\\s-\\s','')
names_radio_14_y <- names_radio_14_y[-c(64,65)] # get rid of "unsure" and "none"


# # most radio stations in 4 weeks, so use that to create names list
# names_radio_14 <- names_radio_14_4w
# fix(names_radio_14)
saveRDS(names_radio_14, "names_radio_14.rds")
names_radio_14 <- readRDS('names_radio_14.rds')

# get data...
radio4weeks_14 <- electr_14[,str_detect(names(electr_14), 'ca64co\\d{2}_\\d')]
radio4weeks_14 <- radio4weeks_14[,-c(98,99)] # get rid of "unsure" and "none"

radio7days_14 <- electr_14[,str_detect(names(electr_14), 'ca65co\\d{2}_\\d')]
radio7days_14 <- radio7days_14[,-c(81, 87,88)]  # get rid of "unsure" and "none" & empty one 

radioYesterday_14 <- electr_14[,str_detect(names(electr_14), 'ca66co\\d{2}_\\d')]
radioYesterday_14 <- radioYesterday_14[,-c(64,65)]  # get rid of "unsure" and "none"

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
radio_engagement_14 <- radio4weeks_14
names(radio_engagement_14) <- names_radio_14

saveRDS(radio_engagement_14, "radio_engagement_14.rds")
radio_engagement_14 <- readRDS("radio_engagement_14.rds")

## TV (this year, included specific dstv and toptv channels (will include them))
names_tv_14 <- electr_14_labels %>%
        str_subset('Watched.+4\\sWEEKS') %>%
        str_replace('.+Watched\\s','') %>%
        str_replace('in\\sthe\\sPAST\\s4\\sWEEKS','') %>%
        str_trim()

saveRDS(names_tv_14, "names_tv_14.rds")
names_tv_14 <- readRDS("names_tv_14.rds")

# want to isolate only past 4 weeks and get rid of ("UNSURE", and "no TV")
tv4weeks_14 <- electr_14[,c('ca45co30_1',
                            'ca45co30_2',
                            'ca45co30_3',
                            'ca45co30_4',
                            'ca45co30_5',
                            'ca45co30_6',
                            'ca45co30_7',
                            'ca45co30_8',
                            'ca45co30_9',
                            'ca45co31_0',
                            'ca45co72_3',
                            'ca45co72_8'
                            )] 

# want to isolate only past 7 days...
tv7days_14 <- electr_14[,c('ca45co32_1',
                           'ca45co32_2',
                           'ca45co32_3',
                           'ca45co32_4',
                           'ca45co32_5',
                           'ca45co32_6',
                           'ca45co32_7',
                           'ca45co32_8',
                           'ca45co32_9',
                           'ca45co33_0',
                           'ca45co74_3',
                           'ca45co74_8'
                           )] 

# want to isolate only yesterday...(indexes w.r.t 4weeks that are missing here: 7, 10)
tvYesterday_14 <- electr_14[,c('ca45co34_1',
                               'ca45co34_2',
                               'ca45co34_3',
                               'ca45co34_4',
                               'ca45co34_5',
                               'ca45co34_6',
                               'ca45co34_8',
                               'ca45co34_9',
                               'ca45co76_3',
                               'ca45co76_8'
                               )]

# combining into a tv engagement dataset (using tv4weeks_14 as basis):

tv_engagement_14 <- tv4weeks_14 + tv7days_14
tv_engagement_14[,-c(7,10)] <- tv_engagement_14[,-c(7,10)] + tvYesterday_14
names(tv_engagement_14) <- names_tv_14

saveRDS(tv_engagement_14, "tv_engagement_14.rds")

tv_engagement_14 <- readRDS("tv_engagement_14.rds")

## 3rd Internet Media Set

## accessed: sum of 14 months, 4weeks, 7days and yesterday
internet_level1 <- internet_14[,str_detect(names(internet_14), 'ca49co(45)|(46)|(47)|(48)')]

#change all 2 = "No" and NA's' to 0
for(i in 1: nrow(internet_level1)) {
        for(j in 1: ncol(internet_level1)) {
                if(is.na(internet_level1[i,j]) | internet_level1[i,j] == 2){
                        internet_level1[i,j] <- 0
                }
        }
}

internet_level1 <- rowSums(internet_level1)

# what internet was accessed for...
##  (maybe could use similar to vehicles?? as well as add up and multiply with first eng):

internet_level2 <- internet_14[,str_detect(names(internet_14), 'ca49co(55)|(58)|(63)|(64)|(69)|(71)')]

# change NA and 3 = 0; 1,2,4 = 1
for(i in 1: nrow(internet_level2)) {
        for(j in 1: ncol(internet_level2)) {
                if(is.na(internet_level2[i,j]) | internet_level2[i,j] == 3){
                        internet_level2[i,j] <- 0
                }
                else {
                        internet_level2[i,j] <- 1
                }
        }
}

names(internet_level2) <- c('int_search',
                          'int_social',
                          'int_print',
                          'int_news',
                          'int_tv',
                          'int_radio')

## create single dataframe for internet multiplying internet_level1 with sum of internet_level2:
internet_engagement_14 <- internet_level2  * internet_level1

saveRDS(internet_engagement_14, "internet_engagement_14.rds")

internet_engagement_14 <- readRDS("internet_engagement_14.rds")

## create single dataframe for media14, including total_engagement columns (consider using media groupings .. follow up on this!)

# Level 1: Type
media_type_14 <- data.frame(cbind(qn = print_14$qn,
                                  scale(rowSums(newspapers_engagement_14)),
                                  scale(rowSums(magazines_engagement_14)),
                                  scale(rowSums(radio_engagement_14)),
                                  scale(rowSums(tv_engagement_14)),
                                  scale(rowSums(internet_engagement_14))))
names(media_type_14) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")
# Level 2: Vehicles
media_vehicles_14 <- data.frame(cbind(qn = print_14$qn,
                                      newspapers_engagement_14,
                                      magazines_engagement_14,
                                      radio_engagement_14,
                                      tv_engagement_14,
                                      internet_engagement_14))

saveRDS(media_type_14, 'media_type_14.rds')
saveRDS(media_vehicles_14, 'media_vehicles_14.rds')

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
#as in '95 need to sort out double count of Soweto....
# seems that all the 19s are the sum of 7 & 14s (ie, Soweto)
# # code as such, ie all 19s are actually 14s (this also eliminates double count in the 7s ( so exlude Soweto)) >NB double check this is same in '95!!!
metro <- ifelse(metro == 19, 14, metro)
lang <- demogrs_14[,'ca91co75'] + 1 # change 0 to 1, so add one to all
lifestages <- demogrs_14[,'ca91co77']
mar_status <- personal_14[,'ca56co09']
pers_inc1 <- personal_14[,'ca57co61']
pers_inc2 <- personal_14[,'ca57co62'] + 10
pers_inc3 <- personal_14[,'ca57co63'] + 20
pers_inc4 <- personal_14[,'ca57co64'] + 30
for(i in 1: length(pers_inc4)) {
        if(!is.na(pers_inc4[i])) {
                if(pers_inc4[i] == 31) {
                        pers_inc4[i] <- 0
                }
                if(pers_inc4[i] == 32) {
                        pers_inc4[i] <- 60
                }
        }
}
pers_inc <- rowSums(cbind(pers_inc1,
                          pers_inc2,
                          pers_inc3,
                          pers_inc4), na.rm = TRUE)
lsm <- lsm_14[,'ca91co64']
lsm <- ifelse(lsm == 0,10,lsm)

lifestyle <- lsm_14[,'ca58co39'] + 1 # to get rid of zero

attitudesA <- lsm_14[,'ca67co10'] + 1 # to get rid of zeros
attitudesB <- lsm_14[,'ca67co10_lsm']
attitudesA <- ifelse(is.na(attitudesA), 0, attitudesA)
attitudesB <- ifelse(is.na(attitudesB), 0, attitudesB)
attitudes <- attitudesA + attitudesB
attitudes <- ifelse(attitudes == 8, 4, attitudes)
attitudes <- ifelse(attitudes == 5 | attitudes == 6, attitudes + 1, attitudes)
attitudes <- ifelse(attitudes == 9, 5, attitudes)
table(attitudes) # check


demographics_14 <- data.frame(qn = print_14$qn,
                              pwgt = print_14$pwgt,
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
                              pers_inc,
                              lsm,
                              lifestyle,
                              attitudes)


# save as

saveRDS(demographics_14, "demographics_14.rds")
demographics_14 <- readRDS("demographics_14.rds")
