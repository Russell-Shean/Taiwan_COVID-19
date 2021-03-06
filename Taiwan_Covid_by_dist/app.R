
## Preliminary data cleaning stuff

#Here are the libraries we'll need
library(sf)
library(tmap)
require(dplyr)
library(shiny)
library(ggplot2)


#Here's the shape file import:

load("./data/new.taiwan.district.rda")

# and here's where we load the data


load("./data/inc_by_A.rda")
load("./data/inc_by_D.rda")


#########################################################
############ make maps   ############################
###########################################################

# attach incidence to shape file
test1 <- new.taiwan.district %>%
    full_join(inc_by_D,
              by=c("site_id"="site_id"))%>%
    dplyr::filter(!is.na(assigned_onset_date))%>% 
    dplyr::filter(Cumul_cases_perCapita >0)%>%
    dplyr::relocate(site_id,.before = value)




# this standardizes the naming scheme for the different age ranges


age_levels <- c("pop_0","pop_1","pop_2","pop_3","pop_4","pop_5to9","pop_10to14","pop_15to19",
                "pop_20to24","pop_25to29","pop_30to34","pop_35to39","pop_40to44",
                "pop_45to49","pop_50to54","pop_55to59","pop_60to64","pop_65to69","pop_over70")


#this improves the labeling of the age ranges
inc_by_A$age_range <- ordered(inc_by_A$age_range, 
                              levels=age_levels,
                              labels=c("1","2","3","4","5","5-9",
                                       "10-14","15-19","20-24","25-29",
                                       "30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70+"))




################################################################################

##     The Actual Shiny!!!!

#################################################################################





ui <- navbarPage("Taiwan's May 2021 COVID-19 outbreak visualization",
                 tabPanel("Age stratified over time",
                          fluidPage(
    fluidRow(
        column(3, selectizeInput("perCap_type","Total or new per capita", choices= c("new_perCapita","Cumul_cases_perCapita"))),
        column(9, checkboxGroupInput("age","Ages", inline =TRUE,selected = c("65-69","70+"), choices = levels(inc_by_A$age_range)))
    ),
    
    fluidRow(
        plotOutput("ageinc"))
)),

tabPanel("District stratified over time",
fluidPage(
    fluidRow(
        dateInput("day","Choose a date", min = "2021-04-23",
                  max=Sys.Date()-1, value = "2021-05-23"),
        selectInput("prevorinc", 
                    "Please choose prevalence or Incidence", 
                    choices = c("new_perCapita",
                                "Cumul_cases_perCapita"))
    ),
    tmapOutput("inc_map", height = 1000)
)))



server<- function(input, output, session){
    output$inc_map <- renderTmap({tm_shape(new.taiwan.district)+
            tm_borders()+
            tm_shape(test1[test1$assigned_onset_date== input$day,])+
            tm_polygons(input$prevorinc,
                        title="New cases per 10^6")})
    
    output$ageinc <- renderPlot(ggplot(data=inc_by_A %>%
                                           filter(age_range %in% input$age))+
                                    geom_line(aes(x=assigned_onset_date, y= .data[[input$perCap_type]] , color=age_range))
    )
    
}

shinyApp(ui,server)
