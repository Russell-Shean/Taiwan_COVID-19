# epi curve

require(ggplot2)

ggplot(covid_cases_omi)+ geom_col(aes(x=assigned_onset_date,
                                      y=case_count))



covid_cases %>% 
  filter(assigned_onset_date > "2021-11-30")%>%
  ggplot()+
  geom_col(aes(x=assigned_onset_date,
               y=case_count,
               fill = imported),
           position = "dodge")
