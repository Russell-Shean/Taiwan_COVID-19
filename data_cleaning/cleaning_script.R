
#Taiwan's COVID-19 data import
# and calculation of incidence and cumulative incidence 
#stratified by district, age, sex
#and combinations of all three

#first load some libraries
library(rjson)
library(jsonlite)
library(dplyr)

# this imports covid data directly from Taiwan CDC website
# this website is updated daily, so this will automatically download the most recent numbers
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
covid_cases$assigned_onset_date <- as.Date(covid_cases$assigned_onset_date, format = "%Y/%m/%d")

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



# this downloads the population data from the web
# so now people should be able to run this script
# directly without having to download the population 
# data first
# population data originally came from the ministry of interior website
# https://ws.moi.gov.tw/001/Upload/OldFile/site_stuff/321/2/year/year.html
# 15.鄉鎮市區人口數按性別及單一年齡分


##### This is an alternate way of downloading the data from github (just in case MOI moves their data)

#download population data from github
# this creates a temporary file
#temp <- tempfile(fileext = ".xlsx")
#dataURL <- "https://github.com/Russell-Shean/Covid_SHINY_MAP/raw/main/pop_by_sex2.xlsx"
#download.file(dataURL, destfile=temp, mode='wb')
#pop_by_sex2 <- read_xlsx(temp)


# This downloads population data from MOI website
temp1 <- tempfile(fileext = ".xls")
dataURL1 <- "https://www.ris.gov.tw/info-popudata/app/awFastDownload/file/y1sg-00000.xls/y1sg/00000/"
download.file(dataURL1,destfile = temp1, mode = "wb")


#here's population by age, sex, and district
require(readxl)
pop_by_sex2 <- read_xls(temp1)
pop_by_sex2 <- as.data.frame(pop_by_sex2)

# and then we don't need to keep the temporary file, so we'll delete it
#file.remove("temp1.xls")

#change column names
new.names <-  c("sex","area_code","area_name","total_pop",
                paste("pop_at_age_",
                      seq(0,100,1),
                      sep = ""))

colnames(pop_by_sex2)<- new.names

#remove first four rows and last row
pop_by_sex2 <- pop_by_sex2 %>%
  dplyr::slice(5:(n()-1))


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
  dplyr::left_join(Names14, by=c("city_code"="city_code")) 

#create the site_id field
pop_by_sex2$site_id <- paste(pop_by_sex2$city,
                             pop_by_sex2$area_name, 
                             sep = "")


'%notin%' <- function(x,y)!('%in%'(x,y))


# create correct age ranges
#pop_by_sex3 <- cbind(pop_by_sex2[,1:4],apply(pop_by_sex2[,3:105],2,as.numeric),pop_by_sex2[,106:109])
pop_by_sex2$pop_at_age_100 <- as.numeric(pop_by_sex2$pop_at_age_100)

pop_by_sex2 <- pop_by_sex2 %>%
  dplyr::mutate(across(total_pop:pop_at_age_100,as.numeric))



pop_by_sex3 <- pop_by_sex2 %>%
  dplyr::mutate(pop_0 =pop_at_age_0,
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
  dplyr::filter(site_id %notin% c("NA總計",
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

### This simplifies the data
### In this step we remove all cases that happened before April 1st 2021 
###  And all imported cases

covid_cases2 <- covid_cases %>%
  dplyr::filter(imported=="否",
                assigned_onset_date > "2021-04-01")%>%
  dplyr::select(assigned_onset_date,
                case_count,
                site_id,
                age_range,
                sex)




# Sex

# 1. Here's population stratified by sex,age, and district
require(reshape2)            
pop_by_SAD <- melt(pop_by_sex3, 
                   id.vars = c("sex","site_id"))%>%
  dplyr::rename(age_range = "variable",
         population = "value")

pop_by_SAD$population <- as.numeric(pop_by_SAD$population)

inc.maker <- function(pop, case_data){
  inc <- pop %>%
    dplyr::full_join(case_data,
              by=c("site_id"="site_id",
                   "age_range"="age_range",
                   "sex"="sex"))%>%
    dplyr::mutate(new_perCapita = case_count/population*100000)
  inc
}

inc_by_SAD <- inc.maker(pop=pop_by_SAD, case_data = covid_cases2)

# 2. Here's population stratified by sex and age
pop_by_SA <- aggregate(population~sex+age_range, 
                       data= pop_by_SAD, 
                       FUN = sum)


cases_by_SA <- aggregate(case_count~assigned_onset_date+age_range+sex,
                         data=covid_cases2,
                         FUN = sum)

inc_by_SA <- pop_by_SA %>%
  dplyr::full_join(cases_by_SA,
            by=c("age_range"="age_range",
                 "sex"="sex"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# 3. Here's population stratified by sex and district
pop_by_SD <- aggregate(population~sex+site_id,
                       data= pop_by_SAD, 
                       FUN = sum)

cases_by_SD <- aggregate(case_count~assigned_onset_date+site_id+sex, 
                         data = covid_cases2, 
                         FUN = sum)

inc_by_SD <- pop_by_SD %>%
  dplyr::full_join(cases_by_SD,
            by=c("site_id"="site_id",
                 "sex"="sex"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)



# 4. Here's population stratified by sex
pop_by_S <- aggregate(population~sex, 
                      data= pop_by_SAD, 
                      FUN = sum)

cases_by_S <- aggregate(case_count~assigned_onset_date+sex, 
                        data=covid_cases2, 
                        FUN = sum)

inc_by_S <- pop_by_S %>%
  dplyr::full_join(cases_by_S,
            by=c("sex"="sex"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# Age 

# 4. Here's population stratified by age and district
pop_by_AD <-  aggregate(population~age_range+site_id,
                        data= pop_by_SAD, 
                        FUN = sum)

cases_by_AD <- aggregate(case_count~assigned_onset_date+age_range+site_id,
                         data = covid_cases2, 
                         FUN = sum)

inc_by_AD <- pop_by_AD %>%
  dplyr::full_join(cases_by_AD,
            by=c("age_range"="age_range",
                 "site_id"="site_id"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# 5. Here's population stratified by age
pop_by_A <- aggregate(population~age_range, 
                      data= pop_by_SAD,
                      FUN = sum)

cases_by_A <- aggregate(case_count~assigned_onset_date+age_range,
                        data = covid_cases2, 
                        FUN=sum)

inc_by_A <- pop_by_A %>%
  dplyr::full_join(cases_by_A,
            by=c("age_range"="age_range"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# 6. Here's population stratified by district
pop_by_D <-  aggregate(population~site_id, 
                       data= pop_by_SAD,
                       FUN = sum)

cases_by_D <- aggregate(case_count~site_id+assigned_onset_date, 
                        data = covid_cases2,
                        FUN = sum)

inc_by_D <- pop_by_D %>%
  dplyr::full_join(cases_by_D,
            by=c("site_id"="site_id"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)


####################################################
#####    Remove extra datasets and functions  ######
####################################################

rm(cases_by_A, 
   cases_by_AD, 
   cases_by_D,
   cases_by_S,
   cases_by_SA, 
   cases_by_SD,
   #pop_by_A, 
   #pop_by_AD, 
   #pop_by_D,
   #pop_by_S,
   #pop_by_SD,
   #pop_by_SAD,
   #pop_by_SA,
   #Names14, 
   #pop_by_sex2, 
   #pop_by_sex3, 
   new.names,
   dataURL1, 
   temp1)




##################################################################################################
###########   This makes three functions to calculate stratified cumulative prevalence ##############################
####################################################################################################

#one variable stratification
prevalator <- function(df=inc_by_D, var1="site_id"){
  
  require(lubridate)
  require(dplyr)
  
  #This makes a sequence between the minimum and max dates in the data set
  # this is needed so that we can assign zeros to dates without any covid cases
  date_range <- seq(from= min(df$assigned_onset_date,
                              na.rm = TRUE),
                    to= max(df$assigned_onset_date,
                            na.rm = TRUE),
                    by="days")
  
  #this creates a list of all the unique districts in Taiwan
  district_list <- unique(df[,var1])
  


  
  #this creates a blank (ish) data frame for use in the loop later
  date_range_index <- as.data.frame(
                                    matrix(
                                           rep(date_range,
                                           times=length(unique(district_list))),
                                           nrow = length(unique(district_list)),
                                           byrow = TRUE))
  
# that step just created a matrix of all the dates in the date range
# with each row containing a vector of all the dates
# and the number of rows equal to the number of districts


# then this step changes the data frame we just created
# and makes each column a vector of the same date repeated for each district (row)
# e.g.     date 1  date 2  date 3
#  dist 1   1/11   1/12    1/13
#  dist 2   1/11   1/12    1/13
#  dist 3   1/11   1/12    1/13
  
  for (i in seq_along(date_range)) {
    date_range_index[,i] <- date_range[i]
  } 
 


  #this binds the date matrix we just created to the list of districts
  #first column is districts
  #second column is the date matrix
  
  date_range_index <- cbind(district_list,date_range_index)
  

   # this changes the format to long form
  require(reshape2)
  
  date_range_index <- melt(date_range_index)
  


  # this removes the second column
  date_range_index <- date_range_index[,-2]
  
  # this changes column names
  colnames(date_range_index) <- c(var1,"assigned_onset_date")
 
 
  # now we have a long data frame with all of the district date combinations

  # this joins the covid cases data to the district-date combos dataframe that we just made
  df <- df %>%
    dplyr::full_join(date_range_index,
              by = names(select(., {{var1}},"assigned_onset_date")))%>%
    dplyr::filter(!is.na(assigned_onset_date))
    

  
  # this changes all the NA incidence values to zero
  df[is.na(df$new_perCapita),]$new_perCapita <- 0
  
  # and case counts to zero
  df[is.na(df$case_count),]$case_count <- 0
  

  
  #this rearranges the data frame by putting all the districts together 
  #and then arranging the dates sequentially
  #e.g   dist 1    1/11
  #      dist 1    1/12
  #      dist 1    1/13
  #      dist 2    1/11
  #      dist 2    1/12
  #      dist 2    1/13
  
  stratified_cases <- df %>% 
    dplyr::arrange(.data[[var1]], assigned_onset_date)%>%
    dplyr::filter(!is.na(assigned_onset_date))
  


  # this creates an empty vector
  cumulative_sums <- numeric()
  
  # this loops through all the districts
  # and calculates cumulative sums for each districts daily incidence
  # then strings all the cumulative sums together into one long vector
  #e.g  site_id    onset_date   incidence
  #      dist 1    1/11            1
  #      dist 1    1/12            20
  #      dist 1    1/13            2
  #      dist 2    1/11            3
  #      dist 2    1/12            3
  #      dist 2    1/13            3
  
  # to:   c(1,21,23,3,6,9)
  
  for (i in unique(stratified_cases[,var1])) {
    cumulative_sums2 <- cumsum(stratified_cases[stratified_cases[,var1]==i,]$new_perCapita)
    cumulative_sums <- c(cumulative_sums,cumulative_sums2)                  
  }
  
 
  # then this attaches the cumulative sums (daily cumulative incidence by district)
  # to the dataframe as a final column
  #e.g  site_id    onset_date   incidence   cumulative incidence
  #      dist 1    1/11            1              1
  #      dist 1    1/12            20             21
  #      dist 1    1/13            2              23
  #      dist 2    1/11            3              3
  #      dist 2    1/12            3              6
  #      dist 2    1/13            3              9
  
  stratified_cases$Cumul_cases_perCapita <- cumulative_sums  
  
  stratified_cases
}

#two variable stratification
prevalator2 <- function(df=inc_by_SAD, vars=c("site_id","age_range")){
  require(lubridate)
  require(dplyr)
  
  #This makes a sequence between the minimum and max dates in the data set
  # this is needed so that we can assign zeros to dates without any covid cases
  date_range <- seq(from= min(df$assigned_onset_date,
                              na.rm = TRUE),
                    to= max(df$assigned_onset_date,
                            na.rm = TRUE),
                    by="days")
  

  
  #this creates a list of all the unique districts in Taiwan
  district_list <- unique(df[,vars[1]])
  # and this creates a list of all the unique age groups
  age_list <- unique(df[,vars[2]])
  
  #this creates an empty vector
  bin_id <- character()
  
  #this creates a vector with all the possible district,date, age combinations
  for(i in district_list){
    bin_id <- c(bin_id, paste(i,age_list, sep = "."))
  }

  #this removes intermediate data structures
  # maybe this will make the function faster?
  rm(age_list,district_list)
  
  #this creates a blank (ish) dataframe for use in the loop later
  date_range_index <- as.data.frame(
                                    matrix(rep(date_range,
                                    times=length(unique(bin_id))),
                                    nrow = length(unique(bin_id)),
                                    byrow = TRUE))

  
  # that step just created a matrix of all the dates in the date range
  # with each row containing a vector of all the dates
  # and the number of rows equal to the number of unique district_AgeGroup combinations
  
  # then this step changes the data frame we just created
  # and makes each column a vector of the same date repeated for each district (row)
  # e.g.                 date 1  date 2  date 3
  #  dist 1 ages 1to5          1/11   1/12    1/13
  #  dist 1 ages 5to10         1/11   1/12    1/13
  #  dist 1 ages 10to20        1/11   1/12    1/13
  #  dist 2 ages 1to5          1/11   1/12    1/13
  #  dist 2 ages 5to10         1/11   1/12    1/13
  #  dist 2 ages 10to20        1/11   1/12    1/13
  #  dist 3 ages 1to5          1/11   1/12    1/13
  #  dist 3 ages 5to10         1/11   1/12    1/13
  #  dist 3 ages 10to20        1/11   1/12    1/13
  
  for (i in seq_along(date_range)) {
    date_range_index[,i] <- date_range[i]
    
  } 
  
  #this removes intermediate data structures
  # maybe this will make the function faster?
  rm(date_range)
  
  #this binds each unique district-age combination to the date matrix we just made
  
  date_range_index <- cbind(bin_id,date_range_index)
  

  
  #this removes intermediate data structures
  # maybe this will make the function faster?
  rm(bin_id)
  
  #this puts data in long format
  require(reshape2)
  
  date_range_index <- melt(date_range_index)
  

  # this removes second column
  date_range_index <- date_range_index[,-2]
  

  # this renames date column
  colnames(date_range_index)[2] <- "assigned_onset_date"
  
  # this takes only the id columns (age group and district) from the covid data dataframe 
  df_simplified <- df%>%    
    dplyr::select(., {{vars}}) 
  
  # this pastes the age groups and districts together
  # and then attaches the groupings to the original dataframe
  df$bin_id <-paste(df_simplified[,1],df_simplified[,2],sep = ".")
  
  #this removes intermediate data structures
  # maybe this will make the function faster?
  rm(df_simplified)

  # this removes the age group and district groups from the covid data
  # the same information is still contained in the bin_id
  # I'm not sure why I did this....
  
  df <- df %>% 
    dplyr::select(., -{{vars}})
  

 # this joins the covid data to all the possible date, district and age group combinations
# and removes any NA  dates
  
  df <- df %>%
    dplyr::full_join(date_range_index,
                     by = c("bin_id"="bin_id",
                            "assigned_onset_date"="assigned_onset_date"))%>%
    dplyr::filter(!is.na(assigned_onset_date))


  #this removes intermediate data structures
  # maybe this will make the function faster?
  rm(date_range_index)
  
  #this changes NA incidence values to zero
  df[is.na(df$new_perCapita),]$new_perCapita <- 0
  
  # and case counts to zero
  df[is.na(df$case_count),]$case_count <- 0
 

  #this rearranges the data frame by putting all the district age group combinations together 
  #and then arranges the dates sequentially
  # e.g.                       
  #       bin_id               date   
  #  dist 1 ages 1to5          1/11  
  #  dist 1 ages 1to5          1/12
  #  dist 1 ages 1to5          1/13
  #  dist 1 ages 5to10         1/11 
  #  dist 1 ages 5to10         1/12
  #  dist 1 ages 5to10         1/13
  #  dist 1 ages 10to20        1/11
  #  dist 1 ages 10to20        1/12  
  #  dist 1 ages 10to20        1/13  
  #  dist 2 ages 1to5          1/11 
  #  dist 2 ages 1to5          1/12
  #  dist 2 ages 1to5          1/13
 
  
  stratified_cases <- df %>% 
    dplyr::arrange(bin_id, assigned_onset_date)%>%
    dplyr::filter(!is.na(assigned_onset_date))
  

  
  # this creates an empty vector
  cumulative_sums <- numeric()
  cumulative_sums2 <- numeric()
 
  # this loops through all the districts
  # and calculates cumulative sums for each district-age group combination's daily incidence
  # then strings all the cumulative sums together into one long vector
  # e.g.                       
  #       bin_id               date       incidence
  #  dist 1 ages 1to5          1/11          1
  #  dist 1 ages 1to5          1/12          20
  #  dist 1 ages 1to5          1/13          2
  #  dist 1 ages 5to10         1/11          3
  #  dist 1 ages 5to10         1/12          3
  #  dist 1 ages 5to10         1/13          3
  #  dist 1 ages 10to20        1/11          4
  #  dist 1 ages 10to20        1/12          0
  #  dist 1 ages 10to20        1/13          16
  #  dist 2 ages 1to5          1/11          1
  #  dist 2 ages 1to5          1/12          1
  #  dist 2 ages 1to5          1/13          1
  
  # to:   c(1,21,23,3,6,9,4,0,20,1,2,3)
  
  for (i in unique(stratified_cases$bin_id)) {
    cumulative_sums2 <- cumsum(stratified_cases[stratified_cases$bin_id==i,]$new_perCapita)
    cumulative_sums <- c(cumulative_sums,cumulative_sums2)                  
  }
  

  
  #then this attaches that vector onto the data frame
  # e.g.                       
  #       bin_id               date       incidence   cumulative incidence
  #  dist 1 ages 1to5          1/11          1                 1
  #  dist 1 ages 1to5          1/12          20                21
  #  dist 1 ages 1to5          1/13          2                 23
  #  dist 1 ages 5to10         1/11          3                 3
  #  dist 1 ages 5to10         1/12          3                 6
  #  dist 1 ages 5to10         1/13          3                 9
  #  dist 1 ages 10to20        1/11          4                 4
  #  dist 1 ages 10to20        1/12          0                 4
  #  dist 1 ages 10to20        1/13          16                20
  #  dist 2 ages 1to5          1/11          1                 1
  #  dist 2 ages 1to5          1/12          1                 2
  #  dist 2 ages 1to5          1/13          1                 3
  
  # to:   c(1,21,23,3,6,9,4,4,20,1,2,3)
  stratified_cases$Cumul_cases_perCapita <- cumulative_sums  
  

  #this removes intermediate data structures
  # maybe this will make the function faster?
  rm(cumulative_sums,cumulative_sums2)
  
  # This splits the bin_id back into district and age group
  # and creates a vector of alternating districts and age groups
  # for all the old bin ids
  #e.g.     c(dist1, ages 1to5, dist1, ages 1to5, dist1, ages 1to 5, dist1, ages 5to10)
  split_bin_ids <- unlist(strsplit(stratified_cases$bin_id,"[.]"))
  

  #this creates one sequence of odd numbers
  #and one sequence of even numbers
  # for the length (number of rows) of the dataframe
  df_column_name_index1 <- seq(1,length(split_bin_ids),2)
  df_column_name_index2 <- seq(2,length(split_bin_ids),2)
  
  #because of the alternating dist, age group, dist pattern
  # the odds will be districts and the evens will be age groups
  
  districts <- split_bin_ids[df_column_name_index1]
  age_groups <- split_bin_ids[df_column_name_index2]
  
  

  #this creates a vector with all the column names from the main data frame
  covid_columns <- colnames(stratified_cases)
  
  #this binds the districts and age groups back onto the main dataframe
  stratified_cases <- cbind(districts,age_groups,stratified_cases)
  
  
  # this fixes the variable names
  colnames(stratified_cases)<- c(vars,covid_columns)

  
  stratified_cases
  
}


# three variable stratification
prevalator3 <- function(df=inc_by_SAD, vars=c("site_id","age_range","sex")){
  
  #load  date-time and data manipulation libraries
  require(lubridate)
  require(dplyr)
  

  #This makes a sequence between the minimum and max dates in the data set
  # this is needed so that we can assign zeros to dates without any covid cases
  date_range <- seq(from= min(df$assigned_onset_date,
                              na.rm = TRUE),
                    to= max(df$assigned_onset_date,
                            na.rm = TRUE),
                    by="days")
  
  #this creates a list of all the unique districts in Taiwan
  # unique age groups and unique sexes (male and female)
  district_list <- unique(df[,vars[1]])
  age_list <- unique(df[,vars[2]])
  sex_list <- unique(df[,vars[3]])
  
  #this creates an empty vector
  bin_id2 <- character()
  
  # this creates a list of district and age combinations
  for(i in age_list){
    bin_id2 <- c(bin_id2, paste(district_list,i, sep = "."))
  }
  
  
  #this creates a list of district, age and sex combinations
  bin_id <- character()
  
  for(i in sex_list){
    bin_id <- c(bin_id, paste(bin_id2,i, sep = "."))
  }
  
  #this removes intermediate data structures
  # maybe this will make the function faster?
  rm(age_list,district_list, sex_list, bin_id2)
  
  
  #this creates a blank (ish) dataframe for use in the loop later
  date_range_index <- as.data.frame(
    matrix(
          rep(date_range,
          times=length(unique(bin_id))),   #this creates multiple copies of each date
                                           #one for each district,age,sex combination
          nrow = length(unique(bin_id)),                   
          byrow = TRUE))
  
  # that step just created a matrix of all the dates in the date range
  # with each row containing a vector of all the dates
  # and the number of rows equal to the number of unique district_AgeGroup_sex combinations
  
  # then this step changes the data frame we just created
  # and makes each column a vector of the same date repeated for each district (row)
  # e.g.                            date 1  date 2  date 3
  #  dist 1 ages 1to5_male          1/11   1/12    1/13
  #  dist 1 ages 5to10_male         1/11   1/12    1/13
  #  dist 1 ages 10to20_male        1/11   1/12    1/13
  #  dist 2 ages 1to5_male          1/11   1/12    1/13
  #  dist 2 ages 5to10_male         1/11   1/12    1/13
  #  dist 2 ages 10to20_male        1/11   1/12    1/13
  #  dist 3 ages 1to5_male          1/11   1/12    1/13
  #  dist 3 ages 5to10_male         1/11   1/12    1/13
  #  dist 3 ages 10to20_male        1/11   1/12    1/13
  #  dist 1 ages 1to5_female        1/11   1/12    1/13
  #  dist 1 ages 5to10_female       1/11   1/12    1/13
  
  
  
  for (i in seq_along(date_range)) {
    date_range_index[,i] <- date_range[i]
    
  } 
  
  #this removes intermediate data structures
  # maybe this will make the function faster?
  rm(date_range)
  

  #this binds each unique district-age combination to the date matrix we just made (see above example)
  
  date_range_index <- cbind(bin_id,date_range_index)
  
  #this removes intermediate data structures
  # maybe this will make the function faster?
  rm(bin_id)
  

  #this puts the data frame we just made into the long form
  require(reshape2)
  
  date_range_index <- melt(date_range_index)
  

  
  
  # this removes the second column
  date_range_index <- date_range_index[,-2]
  
 
  
  
  #this changes the column name 
  colnames(date_range_index)[2] <- "assigned_onset_date"
 
  # this selects the age groups, districts and sexes from the original covid data
  df_simplified <- df%>%    
    dplyr::select(., {{vars}}) 

  

  #and pastes them together to create a bin_id column on the original data set
  df$bin_id <-paste(df_simplified[,1],df_simplified[,2],df_simplified[,3],sep = ".")
  

  #this removes intermediate data structures
  # maybe this will make the function faster?
  rm(df_simplified)
  

  

  #this removes the duplicate copy we're about to create
  df <- df %>% 
    dplyr::select(., -{{vars}})
  

  
  #this creates a full (outer join) between all the possible dates 
  # and the dates we actually had covid on
  df <- df %>%
    dplyr::full_join(date_range_index,
                     by = c("bin_id"="bin_id",
                            "assigned_onset_date"="assigned_onset_date"))%>%
    dplyr::filter(!is.na(assigned_onset_date))
  
  
  # this changes NA incidence vaules to zero
  df[is.na(df$new_perCapita),]$new_perCapita <- 0
  
  # and case counts to zero
  df[is.na(df$case_count),]$case_count <- 0
  
  
  # this rearranges the data frame by unique bin_id 
  #and then secondarily by date
  stratified_cases <- df %>% 
    dplyr::arrange(bin_id, assigned_onset_date)%>%
    dplyr::filter(!is.na(assigned_onset_date))
  

  
  #this creates an empty vector
  cumulative_sums <- numeric()
  cumulative_sums2 <- numeric()
  
  # this calculates cumulative sums for all the dates
  # for each age-sex-district combination
  # see prevelator2() for an example
  for (i in unique(stratified_cases$bin_id)) {
    cumulative_sums2 <- cumsum(stratified_cases[stratified_cases$bin_id==i,]$new_perCapita)
    cumulative_sums <- c(cumulative_sums,cumulative_sums2)                  
  }
  

  

  # this attaches the cumulative sums onto the original data frame
  stratified_cases$Cumul_cases_perCapita <- cumulative_sums  
  
  #this removes intermediate data structures
  # maybe this will make the function faster?
  rm(cumulative_sums2,cumulative_sums)
  

  # This splits the bin_id back into district and age group
  # and creates a vector of alternating districts and age groups
  # for all the old bin ids
  #e.g.     c(dist1, ages 1to5, dist1, ages 1to5, dist1, ages 1to 5, dist1, ages 5to10)
  split_bin_ids <- unlist(strsplit(stratified_cases$bin_id,"[.]"))
  
  #this creates one sequence of odd numbers
  #and one sequence of even numbers
  # for the length (number of rows) of the dataframe
  df_column_name_index1 <- seq(1,length(split_bin_ids),3)
  df_column_name_index2 <- seq(2,length(split_bin_ids),3)
  df_column_name_index3 <- seq(3,length(split_bin_ids),3)
  
  
  #because of the alternating dist, age group, dist pattern
  # the odds will be districts and the evens will be age groups
  
  districts <- split_bin_ids[df_column_name_index1]
  age_groups <- split_bin_ids[df_column_name_index2]
  sexes     <-  split_bin_ids[df_column_name_index3]

  
  #this creates a vector with all the column names from the main data frame
  covid_columns <- colnames(stratified_cases)
  
  #this binds the districts and age groups back onto the main dataframe
  stratified_cases <- cbind(districts,age_groups,sexes, stratified_cases)
  
  # this fixes the variable names
  colnames(stratified_cases)<- c(vars,covid_columns)
  

  stratified_cases
  
}


# The final data set with all three variables (4 if you include time)
# will probably be easier and faster to run outside of a general function...
# Especially because we only have one of these to modify



 


###########################################################################################3
### here's the function in action:

prev_by_D <- prevalator(df=inc_by_D)
prev_by_A <- prevalator(df=inc_by_A,var1 ="age_range")
prev_by_S <- prevalator(df=inc_by_S, var1 = "sex")

####################################################
###  the age-district stratification needs to be broken up
###         bc  otherwise takes way too long
#########################################################

# the drawback of this, is that the dates don't all start in the same place
# this would be fixable by adding a date range term to the prevalators

prev_by_AD_list  <- list()

for(i in 1:19){
  prev_by_AD_list[[i]] <- prevalator2(df=inc_by_AD[inc_by_AD$age_range==unique(inc_by_AD$age_range)[i],])
  
  
}

#this does the same things but is waaay slower
#prev_by_AD <- prevalator2(df=inc_by_AD)


###################################################


prev_by_SA <- prevalator2(df=inc_by_SA, vars = c("sex","age_range"))
prev_by_SD <- prevalator2(df=inc_by_SD, vars = c("site_id","sex"))

time771 <- Sys.time()



#############################################################################
#############################################################################
### this also needs to be broken up
##      bc otherwise R breaks lol
###############################################################################

prev_by_SAD_list  <- list()

for(i in 1:19){
  prev_by_AD_list[[i]] <- prevalator3(df=inc_by_SAD[inc_by_SAD$age_range==unique(inc_by_SAD$age_range)[i],])
  
  
}

#prev_by_SAD <- prevalator3()






