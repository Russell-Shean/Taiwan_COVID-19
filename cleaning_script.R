# 6/25 RS edits
# new in this version: 
# 1. changed population data to download from the internet (no need to separately download
#     data outside of the script)

# 2. added namespaces to reduce the risk of conflicts (mostly dplyr::)

# 3. Cumulative prevalence
#   Adds a function that calculates cumulative prevalence
#    for age, sex, and district. I'm working on a version
#    of the function that will work on combinations of the 
#    three variables. This also solves the problem of
#    date or district holes in the data by making sure
#    all date and variable combinations are present in the
#    dataframe either with cases or as zeros

# 4. Added additional explanations and documentation for what the script does

### future plans
# 1. make shapefiles download directly from the internet
# 2. Finish cumulative prevalence for sex-age, district-sex, district-age,and SAD
# 3. graphs and shiny tweeks.....ideas for graphs to add?

## unresolved problems
# 1. Shiny doesn't work on firefox (and maybe other browsers too?), seems to be a widespread problem
#     especially for leaflet

time28 <- Sys.time()

#Covid JSon clean

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


# This download population data from MOI website
temp1 <- tempfile(fileext = ".xls")
dataURL1 <- "https://www.ris.gov.tw/info-popudata/app/awFastDownload/file/y1sg-00000.xls/y1sg/00000/"
download.file(dataURL1,destfile = temp1, mode = "wb")


#here's population by age, sex, and district
require(readxl)
pop_by_sex2 <- read_xls(temp1)
pop_by_sex2 <- as.data.frame(pop_by_sex2)



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
pop_by_SAD <- melt(pop_by_sex3, id.vars = c("sex","site_id"))%>%
  dplyr::rename(age_range = "variable",
         population = "value")

pop_by_SAD$population <- as.numeric(pop_by_SAD$population)

inc.maker <- function(pop){
  inc <- pop %>%
    dplyr::full_join(covid_cases2,
              by=c("site_id"="site_id",
                   "age_range"="age_range",
                   "sex"="sex"))%>%
    dplyr::mutate(new_perCapita = case_count/population*100000)
  inc
}

inc_by_SAD <- inc.maker(pop_by_SAD)

# 2. Here's population stratified by sex and age
pop_by_SA <- aggregate(population~sex+age_range, data= pop_by_SAD, FUN = sum)


cases_by_SA <- aggregate(case_count~assigned_onset_date+age_range+sex, data=covid_cases2, FUN = sum)

inc_by_SA <- pop_by_SA %>%
  dplyr::full_join(cases_by_SA,
            by=c("age_range"="age_range",
                 "sex"="sex"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# 3. Here's population stratified by sex and district
pop_by_SD <- aggregate(population~sex+site_id, data= pop_by_SAD, FUN = sum)

cases_by_SD <- aggregate(case_count~assigned_onset_date+site_id+sex, data = covid_cases2, FUN = sum)

inc_by_SD <- pop_by_SD %>%
  dplyr::full_join(cases_by_SD,
            by=c("site_id"="site_id",
                 "sex"="sex"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)



# 4. Here's population stratified by sex
pop_by_S <- aggregate(population~sex, data= pop_by_SAD, FUN = sum)

cases_by_S <- aggregate(case_count~assigned_onset_date+sex, data=covid_cases2, FUN = sum)

inc_by_S <- pop_by_S %>%
  dplyr::full_join(cases_by_S,
            by=c("sex"="sex"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# Age 

# 4. Here's population stratified by age and district
pop_by_AD <-  aggregate(population~age_range+site_id, data= pop_by_SAD, FUN = sum)

cases_by_AD <- aggregate(case_count~assigned_onset_date+age_range+site_id, data = covid_cases2, FUN = sum)

inc_by_AD <- pop_by_AD %>%
  dplyr::full_join(cases_by_AD,
            by=c("age_range"="age_range",
                 "site_id"="site_id"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# 5. Here's population stratified by age
pop_by_A <- aggregate(population~age_range, data= pop_by_SAD, FUN = sum)

cases_by_A <- aggregate(case_count~assigned_onset_date+age_range, data = covid_cases2, FUN=sum)

inc_by_A <- pop_by_A %>%
  dplyr::full_join(cases_by_A,
            by=c("age_range"="age_range"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# 6. Here's population stratified by district
pop_by_D <-  aggregate(population~site_id, data= pop_by_SAD, FUN = sum)

cases_by_D <- aggregate(case_count~site_id+assigned_onset_date, data = covid_cases2,FUN = sum)

inc_by_D <- pop_by_D %>%
  dplyr::full_join(cases_by_D,
            by=c("site_id"="site_id"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)


####################################################
#####    Remove extra datasets and functions  ######
####################################################

rm(cases_by_A, cases_by_AD, cases_by_D,
   cases_by_S,cases_by_SA, cases_by_SD,
   pop_by_A, pop_by_AD, pop_by_D,
   pop_by_S,pop_by_SD,pop_by_SAD,pop_by_SA,
   Names14, pop_by_sex2, pop_by_sex3, new.names,
   dataURL1, temp1)




##################################################################################################
###########   This makes a function to calculate cumulative prevalence ##############################
####################################################################################################

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
  

  

  
  #this creates a blank (ish) data fram for use in the loop later
  date_range_index <- as.data.frame(matrix(rep(date_range,times=length(unique(district_list))),
                                           nrow = length(unique(district_list)),byrow = TRUE))
  

  
  
  for (i in seq_along(date_range)) {
    date_range_index[,i] <- date_range[i]
    
  } 
  

  
  date_range_index <- cbind(district_list,date_range_index)
 

   
  require(reshape2)
  
  date_range_index <- melt(date_range_index)
  

  
  date_range_index <- date_range_index[,-2]
  
  colnames(date_range_index) <- c(var1,"assigned_onset_date")
  

  
  df <- df %>%
    dplyr::full_join(date_range_index,
              by = names(select(., {{var1}},"assigned_onset_date")))%>%
    dplyr::filter(!is.na(assigned_onset_date))
    

  
  
  df[is.na(df$new_perCapita),]$new_perCapita <- 0
  
  testik <- df %>% 
    dplyr::arrange(.data[[var1]], assigned_onset_date)%>%
    dplyr::filter(!is.na(assigned_onset_date))
  

  
  cumsums <- numeric()
  
  
  for (i in unique(testik[,var1])) {
    cumsums2 <- cumsum(testik[testik[,var1]==i,]$new_perCapita)
    cumsums <- c(cumsums,cumsums2)                  
  }
  
 
  
  testik$Cumul_cases_perCapita <- cumsums  
  
  testik
}

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
  age_list <- unique(df[,vars[2]])
  
  bin_id <- character()
  
  for(i in district_list){
    bin_id <- c(bin_id, paste(i,age_list, sep = "."))
  }

  
  
  #this creates a blank (ish) dataframe for use in the loop later
  date_range_index <- as.data.frame(matrix(rep(date_range,times=length(unique(bin_id))),
                                           nrow = length(unique(bin_id)),byrow = TRUE))
  
  
  
  
  for (i in seq_along(date_range)) {
    date_range_index[,i] <- date_range[i]
    
  } 
  
  
  
  date_range_index <- cbind(bin_id,date_range_index)
  
  
  
  require(reshape2)
  
  date_range_index <- melt(date_range_index)
  

  
  date_range_index <- date_range_index[,-2]
  

  
  colnames(date_range_index)[2] <- "assigned_onset_date"
  
  
  sheep <- df%>%    
    dplyr::select(., {{vars}}) 
  
  sheep$bin_id <-paste(sheep[,1],sheep[,2],sep = ".")
  

  
  df$bin_id <- sheep$bin_id
  
  df <- df %>% 
    dplyr::select(., -{{vars}})
  

 
  
  df <- df %>%
    dplyr::full_join(date_range_index,
                     by = c("bin_id"="bin_id",
                            "assigned_onset_date"="assigned_onset_date"))%>%
    dplyr::filter(!is.na(assigned_onset_date))

  

  df[is.na(df$new_perCapita),]$new_perCapita <- 0
  
  testik <- df %>% 
    dplyr::arrange(bin_id, assigned_onset_date)%>%
    dplyr::filter(!is.na(assigned_onset_date))
  
  
  cumsums <- numeric()
 

  
  for (i in unique(testik$bin_id)) {
    cumsums2 <- cumsum(testik[testik$bin_id==i,]$new_perCapita)
    cumsums <- c(cumsums,cumsums2)                  
  }
  
  testik$Cumul_cases_perCapita <- cumsums  
  
  # This fixes names and stuff
  sheep2 <- unlist(strsplit(testik$bin_id,"[.]"))
  
  sheep.seq1 <- seq(1,length(sheep2),2)
  sheep.seq2 <- seq(2,length(sheep2),2)
  
  poodle1 <- sheep2[sheep.seq1]
  poodle2 <- sheep2[sheep.seq2]
  
  

  # sensible variable names hahahahahaha
  
  Goatss <- colnames(testik)
  
  testik <- cbind(poodle1,poodle2,testik)
  
  
  
  colnames(testik)<- c(vars,Goatss)

  
  testik
  
}

# The final dataset with all three variables (4 if you include time)
# will probably be easier and faster to run outside of a generilzable function...
# Especially because we only have one of these to modify

prevalator3 <- function(df=inc_by_SAD, vars=c("site_id","age_range","sex")){
  require(lubridate)
  require(dplyr)
  
  message("This function will break at some point in the future because as the number of dates increase the memory requirements for this function will exceed the computer's ram without additional action from the user. It broke for me around 7 million rows. The number of rows is so large because nrow = n(age groups)*n(days in the date range)*n(districts)*n(sex). For possible workarounds see: https://stackoverflow.com/questions/5233769/practical-limits-of-r-data-frame")
  
  #This makes a sequence between the minimum and max dates in the data set
  # this is needed so that we can assign zeros to dates without any covid cases
  date_range <- seq(from= min(df$assigned_onset_date,
                              na.rm = TRUE),
                    to= max(df$assigned_onset_date,
                            na.rm = TRUE),
                    by="days")
  
  #this creates a list of all the unique districts in Taiwan
  district_list <- unique(df[,vars[1]])
  age_list <- unique(df[,vars[2]])
  sex_list <- unique(df[,vars[3]])
  
  bin_id2 <- character()
  
  for(i in age_list){
    bin_id2 <- c(bin_id2, paste(district_list,i, sep = "."))
  }
  
  
  
  bin_id <- character()
  
  for(i in sex_list){
    bin_id <- c(bin_id, paste(bin_id2,i, sep = "."))
  }
  

  
  #this creates a blank (ish) dataframe for use in the loop later
  date_range_index <- as.data.frame(matrix(rep(date_range,times=length(unique(bin_id))),
                                           nrow = length(unique(bin_id)),byrow = TRUE))
  

  
  
  
  
  for (i in seq_along(date_range)) {
    date_range_index[,i] <- date_range[i]
    
  } 
  

  
  
  date_range_index <- cbind(bin_id,date_range_index)
  

  
  require(reshape2)
  
  date_range_index <- melt(date_range_index)
  

  
  
  
  date_range_index <- date_range_index[,-2]
  
 
  
  
  
  colnames(date_range_index)[2] <- "assigned_onset_date"
 
  
  sheep <- df%>%    
    dplyr::select(., {{vars}}) 

  

  
  sheep$bin_id <-paste(sheep[,1],sheep[,2],sheep[,3],sep = ".")
  

  
  df$bin_id <- sheep$bin_id
  

  
  df <- df %>% 
    dplyr::select(., -{{vars}})
  

  
  
  df <- df %>%
    dplyr::full_join(date_range_index,
                     by = c("bin_id"="bin_id",
                            "assigned_onset_date"="assigned_onset_date"))%>%
    dplyr::filter(!is.na(assigned_onset_date))
  
  
  
  df[is.na(df$new_perCapita),]$new_perCapita <- 0
  
  df
  
  
  
  testik <- df %>% 
    dplyr::arrange(bin_id, assigned_onset_date)%>%
    dplyr::filter(!is.na(assigned_onset_date))
  

  
  
  cumsums <- numeric()
  
  
  
  for (i in unique(testik$bin_id)) {
    cumsums2 <- cumsum(testik[testik$bin_id==i,]$new_perCapita)
    cumsums <- c(cumsums,cumsums2)                  
  }
  
  

  
  testik$Cumul_cases_perCapita <- cumsums  
  

  
  # This fixes names and stuff
  sheep2 <- unlist(strsplit(testik$bin_id,"[.]"))
  
  sheep.seq1 <- seq(1,length(sheep2),3)
  sheep.seq2 <- seq(2,length(sheep2),3)
  sheep.seq3 <- seq(3,length(sheep2),3)
  
  poodle1 <- sheep2[sheep.seq1]
  poodle2 <- sheep2[sheep.seq2]
  poodle3 <- sheep2[sheep.seq3]
  
  
  # sensible variable names hahahahahaha
  
  Goatss <- colnames(testik)
  
  testik <- cbind(poodle1,poodle2,poodle3,testik)
  
  
  
  colnames(testik)<- c(vars,Goatss)
  
  
  testik
  
}






 


###########################################################################################3
### here's the function in action:

inc_by_D <- prevalator(df=inc_by_D)
inc_by_A <- prevalator(df=inc_by_A,var1 ="age_range")
inc_by_S <- prevalator(df=inc_by_S, var1 = "sex")

time762 <- Sys.time()

inc_by_AD <- prevalator2()

time766 <- Sys.time()

inc_by_SA <- prevalator2(df=inc_by_SA, vars = c("sex","age_range"))
inc_by_SD <- prevalator2(df=inc_by_SD, vars = c("site_id","sex"))

time771 <- Sys.time()

inc_by_SAD <- prevalator3()

time775 <- Sys.time()


###################################################
####   Time report    ############################
##################################################






#####################################################
#############               #########################
#########    Maps and Graphs    ######################
#############               #########################
#######################################################





