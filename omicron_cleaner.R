#omicron update
#we're going to only look at domestic transmission starting 11/2021
# this assumes your run the cleaning_script file first
#cleaning_script available here: https://github.com/Russell-Shean/Covid_SHINY_MAP/blob/main/cleaning_script.R

#here's the libraries we'll need again
require(dplyr)

#filter out cases before nov 1 2021
covid_cases_omi <- covid_cases2 %>%
                   filter(assigned_onset_date > "2021-10-31")


# Sex

# 1. Here's omicron incidence stratified by sex,age, and district


inc_by_SAD_omi <- inc.maker(pop=pop_by_SAD, case_data = covid_cases_omi)

# 2. Here's omicron incidence  stratified by sex and age



cases_by_SA_omi <- aggregate(case_count~assigned_onset_date+age_range+sex, data=covid_cases_omi, FUN = sum)

inc_by_SA_omi <- pop_by_SA %>%
  dplyr::full_join(cases_by_SA_omi,
                   by=c("age_range"="age_range",
                        "sex"="sex"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# 3. Here's omicron incidence  stratified by sex and district

cases_by_SD_omi <- aggregate(case_count~assigned_onset_date+site_id+sex, data = covid_cases_omi, FUN = sum)

inc_by_SD_omi <- pop_by_SD %>%
  dplyr::full_join(cases_by_SD_omi,
                   by=c("site_id"="site_id",
                        "sex"="sex"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)



# 4. Here's omicron incidence  stratified by sex

cases_by_S_omi <- aggregate(case_count~assigned_onset_date+sex, data=covid_cases_omi, FUN = sum)

inc_by_S_omi <- pop_by_S %>%
  dplyr::full_join(cases_by_S_omi,
                   by=c("sex"="sex"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# Age 

# 4. Here's omicron incidence stratified by age and district
cases_by_AD_omi <- aggregate(case_count~assigned_onset_date+age_range+site_id, data = covid_cases_omi, FUN = sum)

inc_by_AD_omi <- pop_by_AD %>%
  dplyr::full_join(cases_by_AD_omi,
                   by=c("age_range"="age_range",
                        "site_id"="site_id"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# 5. Here's omicron incidence  stratified by age
cases_by_A_omi <- aggregate(case_count~assigned_onset_date+age_range, data = covid_cases_omi, FUN=sum)

inc_by_A_omi <- pop_by_A %>%
  dplyr::full_join(cases_by_A_omi,
                   by=c("age_range"="age_range"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)

# 6. Here's omicron incidence  stratified by district

cases_by_D_omi <- aggregate(case_count~site_id+assigned_onset_date, data = covid_cases_omi,FUN = sum)

inc_by_D_omi <- pop_by_D %>%
  dplyr::full_join(cases_by_D_omi,
                   by=c("site_id"="site_id"))%>%
  dplyr::mutate(new_perCapita = case_count/population*100000)


####################################################
#####    Remove extra datasets and functions  ######
####################################################

rm(pop_by_A, pop_by_AD, pop_by_D,
   pop_by_S,pop_by_SD,pop_by_SAD,pop_by_SA,
   Names14, pop_by_sex2, pop_by_sex3)




##################################################################################################




prev_by_D_omi <- prevalator(df=inc_by_D_omi)
prev_by_A_omi <- prevalator(df=inc_by_A_omi,var1 ="age_range")
prev_by_S_omi <- prevalator(df=inc_by_S_omi, var1 = "sex")

prev_by_AD_omi <- prevalator2(df=inc_by_SAD_omi)
prev_by_SA_omi <- prevalator2(df=inc_by_SA_omi, vars = c("sex","age_range"))
prev_by_SD_omi <- prevalator2(df=inc_by_SD_omi, vars = c("site_id","sex"))

prev_by_SAD_omi <- prevalator3(df=inc_by_SAD_omi)

















