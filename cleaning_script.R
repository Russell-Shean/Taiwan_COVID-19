#Covid JSon clean

library(rjson)
library(jsonlite)
library(dplyr)
covid_cases <- fromJSON("https://od.cdc.gov.tw/eic/Day_Confirmation_Age_County_Gender_19CoV.json", flatten=TRUE)

#rename variables with English names
# because Chinese characters and encoding
# are a huge pains in scripts
# we can Chinese labels back later if needed

colnames(covid_cases) <- c("disease_type",
                           "assigned_onset_date",
                           "city",
                           "district",
                           "sex",
                           "imported",
                           "age_range",         
                           "case_count")

#makes site id column, because there are about 22 zhongzheng
# districts, one for each city/county in Taiwan
covid_cases$site_id <- paste(covid_cases$city, 
                             covid_cases$district, 
                             sep = "")

# convert Taiwan CDC dates into a Date format that R 
# recognizes 

library(lubridate)
covid_cases$assigned_onset_date <- as.Date(covid_cases$assigned_onset_date, format = "%Y%m%d")

#changes case count to a number 
covid_cases$case_count <- as.numeric(covid_cases$case_count)

# change age category names

covid_cases[covid_cases$age_range=="0",]$age_range <- "pop_0" 
covid_cases[covid_cases$age_range=="1",]$age_range <- "pop_1" 
covid_cases[covid_cases$age_range=="2",]$age_range <- "pop_2" 
covid_cases[covid_cases$age_range=="3",]$age_range <- "pop_3" 
covid_cases[covid_cases$age_range=="4",]$age_range <- "pop_4" 
covid_cases[covid_cases$age_range=="5-9",]$age_range <- "pop_5to9" 
covid_cases[covid_cases$age_range=="10-14",]$age_range <- "pop_10to14" 
covid_cases[covid_cases$age_range=="15-19",]$age_range <- "pop_15to19" 
covid_cases[covid_cases$age_range=="20-24",]$age_range <- "pop_20to24" 
covid_cases[covid_cases$age_range=="25-29",]$age_range <- "pop_25to29" 
covid_cases[covid_cases$age_range=="30-34",]$age_range <- "pop_30to34" 
covid_cases[covid_cases$age_range=="35-39",]$age_range <- "pop_35to39" 
covid_cases[covid_cases$age_range=="40-44",]$age_range <- "pop_40to44" 
covid_cases[covid_cases$age_range=="45-49",]$age_range <- "pop_45to49" 
covid_cases[covid_cases$age_range=="50-54",]$age_range <- "pop_50to54" 
covid_cases[covid_cases$age_range=="55-59",]$age_range <- "pop_55to59" 
covid_cases[covid_cases$age_range=="60-64",]$age_range <- "pop_60to64" 
covid_cases[covid_cases$age_range=="65-69",]$age_range <- "pop_65to69" 
covid_cases[covid_cases$age_range=="70+",]$age_range <- "pop_over70" 










#here's population by age, sex, and district
require(readxl)
pop_by_sex2 <- read_xlsx("pop_by_sex2.xlsx", sheet = 1)
pop_by_sex2 <- as.data.frame(pop_by_sex2)

#change column names
new.names <-  c("sex","area_code","area_name","total_pop",
                paste("pop_at_age_",
                      seq(0,100,1),
                      sep = ""))

colnames(pop_by_sex2)<- new.names

#remove first three rows
pop_by_sex2 <- pop_by_sex2 %>%
  slice(5:n())


#make the site_id variable

#extract city information from area code
pop_by_sex2$area_code <- as.numeric(pop_by_sex2$area_code)


pop_by_sex2$city_code <- round(pop_by_sex2$area_code/1000)


#create index for matching city code with city name
Names14 <- data.frame(city = c("新北市",
                               "台北市",
                               "桃園市",
                               "台中市",
                               "台南市",
                               "高雄市",
                               "宜蘭縣",
                               "新竹縣",
                               "苗栗縣",
                               "彰化縣",
                               "南投縣",
                               "雲林縣",
                               "嘉義縣",
                               "屏東縣",
                               "台東縣",
                               "花蓮縣",
                               "澎湖縣",
                               "基隆市",
                               "新竹市",
                               "嘉義市",
                               "金門縣",
                               "連江縣"),
                      area_code = c(65000000,	63000000,68000000,
                                    66000000,67000000,64000000,	
                                    10002000,	10004000,10005000,10007000,10008000,
                                    10009000, 10010000,	10013000,		10014000,10015000,
                                    10016000,10017000,10018000,10020000,9020000,9007000))

Names14$city_code <- round(Names14$area_code/1000)

pop_by_sex2 <- pop_by_sex2 %>% 
  left_join(Names14, by=c("city_code"="city_code")) 

#create the site_id field
pop_by_sex2$site_id <- paste(pop_by_sex2$city,
                             pop_by_sex2$area_name, 
                             sep = "")


'%notin%' <- function(x,y)!('%in%'(x,y))


# create correct age ranges
#pop_by_sex3 <- cbind(pop_by_sex2[,1:4],apply(pop_by_sex2[,3:105],2,as.numeric),pop_by_sex2[,106:109])
pop_by_sex2$pop_at_age_100 <- as.numeric(pop_by_sex2$pop_at_age_100)

pop_by_sex3 <- pop_by_sex2 %>%
  mutate(pop_0 =pop_at_age_0,
         pop_1 =pop_at_age_1,
         pop_2 =pop_at_age_2,
         pop_3 = pop_at_age_3,
         pop_4 = pop_at_age_4,
         pop_5to9 =rowSums(across(pop_at_age_5:pop_at_age_9)),
         pop_10to14=rowSums(across(pop_at_age_10:pop_at_age_14)),
         pop_15to19=rowSums(across(pop_at_age_15:pop_at_age_19)),
         pop_20to24 =rowSums(across(pop_at_age_20:pop_at_age_24)),
         pop_25to29 =rowSums(across(pop_at_age_25:pop_at_age_29)),
         pop_30to34 =rowSums(across(pop_at_age_30:pop_at_age_34)),
         pop_35to39 =rowSums(across(pop_at_age_35:pop_at_age_39)),
         pop_40to44 =rowSums(across(pop_at_age_40:pop_at_age_44)),
         pop_45to49 =rowSums(across(pop_at_age_45:pop_at_age_49)),
         pop_50to54 =rowSums(across(pop_at_age_50:pop_at_age_54)),
         pop_55to59 =rowSums(across(pop_at_age_55:pop_at_age_59)),
         pop_60to64 =rowSums(across(pop_at_age_60:pop_at_age_64)),
         pop_65to69 =rowSums(across(pop_at_age_65:pop_at_age_69)),
         pop_over70 =rowSums(across(pop_at_age_70:pop_at_age_100)))%>%
  dplyr::select(!(pop_at_age_0:pop_at_age_100))%>%
  dplyr::select(!area_code.x)%>%
  dplyr::select(!area_code.y)%>%
  dplyr::select(!city_code)%>%
  dplyr::select(!(area_name:city))%>%
  filter(site_id %notin% c("NA總計",
                           "NA福建省",
                           "新北市新北市",
                           "台北市台北市",
                           "桃園市桃園市",
                           "台中市台中市",
                           "台南市台南市",
                           "高雄市高雄市",
                           "NA臺灣省",
                           "宜蘭縣宜蘭縣",
                           "新竹縣新竹縣",
                           "苗栗縣苗栗縣",
                           "彰化縣彰化縣",
                           "南投縣南投縣",
                           "雲林縣雲林縣",
                           "嘉義縣嘉義縣",
                           "屏東縣屏東縣",
                           "台東縣台東縣",
                           "花蓮縣花蓮縣",
                           "澎湖縣澎湖縣",
                           "基隆市基隆市",
                           "新竹市新竹市",
                           "金門縣金門縣",
                           "連江縣連江縣",
                           "嘉義市嘉義市"))



###########################################################
##########                              ################################
######     stratification  + incidence     #######################
##########                              ###########################
########################################################

# Sex

# 1. Here's population stratified by sex,age, and district
require(reshape2)            
pop_by_SAD <- melt(pop_by_sex3, id.vars = c("sex","site_id"))%>%
  rename(age_range = "variable",
         population = "value")

pop_by_SAD$population <- as.numeric(pop_by_SAD$population)

inc.maker <- function(pop){
  inc <- pop %>%
    full_join(covid_cases,
              by=c("site_id"="site_id",
                   "age_range"="age_range",
                   "sex"="sex"))%>%
    mutate(new_perCapita = case_count/population*100000)
  inc
}

inc_by_SAD <- inc.maker(pop_by_SAD)

# 2. Here's population stratified by sex and age
pop_by_SA <- aggregate(population~sex+age_range, data= pop_by_SAD, FUN = sum)

covid_cases2 <- covid_cases %>%
  filter(imported=="否",
         assigned_onset_date > "2021-04-01")%>%
  dplyr::select(assigned_onset_date,
                case_count,
                site_id,
                age_range,
                sex)

cases_by_SA <- aggregate(case_count~assigned_onset_date+age_range+sex, data=covid_cases2, FUN = sum)

inc_by_SA <- pop_by_SA %>%
  full_join(cases_by_SA,
            by=c("age_range"="age_range",
                 "sex"="sex"))%>%
  mutate(new_perCapita = case_count/population*100000)

# 3. Here's population stratified by sex and district
pop_by_SD <- aggregate(population~sex+site_id, data= pop_by_SAD, FUN = sum)

cases_by_SD <- aggregate(case_count~assigned_onset_date+site_id+sex, data = covid_cases2, FUN = sum)

inc_by_SD <- pop_by_SD %>%
  full_join(cases_by_SD,
            by=c("site_id"="site_id",
                 "sex"="sex"))%>%
  mutate(new_perCapita = case_count/population*100000)



# 4. Here's population stratified by sex
pop_by_S <- aggregate(population~sex, data= pop_by_SAD, FUN = sum)

cases_by_S <- aggregate(case_count~assigned_onset_date+sex, data=covid_cases2, FUN = sum)

inc_by_S <- pop_by_S %>%
  full_join(cases_by_S,
            by=c("sex"="sex"))%>%
  mutate(new_perCapita = case_count/population*100000)

# Age 

# 4. Here's population stratified by age and district
pop_by_AD <-  aggregate(population~age_range+site_id, data= pop_by_SAD, FUN = sum)

cases_by_AD <- aggregate(case_count~assigned_onset_date+age_range+site_id, data = covid_cases2, FUN = sum)

inc_by_AD <- pop_by_AD %>%
  full_join(cases_by_AD,
            by=c("age_range"="age_range",
                 "site_id"="site_id"))%>%
  mutate(new_perCapita = case_count/population*100000)

# 5. Here's population stratified by age
pop_by_A <- aggregate(population~age_range, data= pop_by_SAD, FUN = sum)

cases_by_A <- aggregate(case_count~assigned_onset_date+age_range, data = covid_cases2, FUN=sum)

inc_by_A <- pop_by_A %>%
  full_join(cases_by_A,
            by=c("age_range"="age_range"))%>%
  mutate(new_perCapita = case_count/population*100000)

# 6. Here's population stratified by district
pop_by_D <-  aggregate(population~site_id, data= pop_by_SAD, FUN = sum)

cases_by_D <- aggregate(case_count~site_id+assigned_onset_date, data = covid_cases2,FUN = sum)

inc_by_D <- pop_by_D %>%
  full_join(cases_by_D,
            by=c("site_id"="site_id"))%>%
  mutate(new_perCapita = case_count/population*100000)

##################################################################################################
###########   This makes a function to calculate cumulative prevalence ##############################
####################################################################################################

prevalator <- function(df=inc_by_D, var1="site_id"){

require(lubridate)
require(dplyr)

date_range <- seq(from= min(inc_by_D$assigned_onset_date,
                            na.rm = TRUE),
                      to= max(inc_by_D$assigned_onset_date,
                              na.rm = TRUE),
                          by="days")

district_list <- unique(inc_by_D$site_id)

date_range_index <- as.data.frame(matrix(rep(date_range,times=length(unique(district_list))),
                                         nrow = length(unique(district_list)),byrow = TRUE))

for (i in seq_along(date_range)) {
  date_range_index[,i] <- date_range[i]
  
} 

date_range_index <- cbind(district_list,date_range_index)
 
require(reshape2)

date_range_index <- melt(date_range_index)

date_range_index <- date_range_index[,-2]

colnames(date_range_index) <- c("site_id","assigned_onset_date")



df <- df %>%
  full_join(date_range_index,
            by=c("site_id"="site_id",
                "assigned_onset_date"="assigned_onset_date"))




df[is.na(df$new_perCapita),]$new_perCapita <- 0

testik <- df %>% 
  arrange(site_id, assigned_onset_date)%>%
  filter(!is.na(assigned_onset_date))

cumsums <- numeric()

  
for (i in unique(testik[,var1])) {
 cumsums2 <- cumsum(testik[testik[,var1]==i,]$new_perCapita)
 cumsums <- c(cumsums,cumsums2)                  
}

#works to here
  
testik$Cumul_cases_perCapita <- cumsums  

testik
}

###########################################################################################3
### here's the function in action:

inc_by_D <- prevalator(df=inc_by_D)




####################################################
#####    Remove extra datasets and functions  ######
####################################################

rm(cases_by_A, cases_by_AD, cases_by_D,
   cases_by_S,cases_by_SA, cases_by_SD,
   pop_by_A, pop_by_AD, pop_by_D,
   pop_by_S,pop_by_SD,pop_by_SAD,pop_by_SA,
   Names14, pop_by_sex2, pop_by_sex3, new.names)


#####################################################
#############               #########################
#########    Maps and Graphs    ######################
#############               #########################
#######################################################









# this is all unsorted stuff and can be ignored
#library(xlsx)


#covid_cases <- covid_cases %>%
# full_join(pop_by_sex_age, 
# by=c("site_id"="site_id",
#    "age_range"="age_range",
#    "sex"="sex"))

#covid_cases$population <- as.numeric(covid_cases$population)

#covid_cases$daily_incidence <- covid_cases$case_count / covid_cases$population *100000

#stratified_daily_incidence <- covid_cases%>% 
#  select(assigned_onset_date,site_id,sex,age_range,case_count,population,daily_incidence)


#write.xlsx(stratified_daily_incidence, file = "stratified_daily_incidence.xlsx")








#%>%
#dplyr::select(site_id,people_total)






#aggregate by site_id

#import and attach population 
#district_population <- read_excel("C:/Users/rshea/Desktop/old computer/Covid/district.population.xlsx")
#dist_pop2 <- fromJSON("https://od.moi.gov.tw/api/v1/rest/datastore/301000000A-000605-055.json", flatten = TRUE)



#covid_test1 <- covid_cases %>%
#  filter(district=="萬華區"|
#        district=="板橋區"|
#       district=="空值")

# equivalents~~

#covid_test1 <- covid_cases %>%
# filter(district %in% c("萬華區","板橋區","空值"))

#covid_cases %>%
# filter(district %in% c("萬華區","板橋區"),
#                       assigned_onset_date > "2021-04-23")%>%
#count(assigned_onset_date,district,wt=case_count)%>%
#ggplot(aes(x=assigned_onset_date,y=n,color=district))+
# geom_line()


#covid_cases %>%
# filter(city %in% c("台北市","新北市"),
#   assigned_onset_date > "2021-04-23")%>%
# count(assigned_onset_date,district,wt=case_count)%>%
# ggplot(aes(x=assigned_onset_date,y=n,color=district))+
# geom_line()


#remove covid column, we know....
#remove imported cases
#remove cases before april 23
#Remove everything but case count and site_id
#because for now that's all we care about
#aggregate by site_id and date

#local_cases <- covid_cases %>%
# filter(imported=="否",
#     assigned_onset_date>"2021-04-22")%>%
#dplyr::select(site_id,assigned_onset_date, case_count)%>% 
#count(site_id,assigned_onset_date,wt=case_count)


