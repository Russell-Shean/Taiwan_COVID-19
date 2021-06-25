library(shiny)

age_levels <- c("pop_0","pop_1","pop_2","pop_3","pop_4","pop_5to9","pop_10to14","pop_15to19",
                "pop_20to24","pop_25to29","pop_30to34","pop_35to39","pop_40to44",
                "pop_45to49","pop_50to54","pop_55to59","pop_60to64","pop_65to69","pop_over70")


inc_by_A$age_range <- ordered(inc_by_A$age_range, 
                              levels=age_levels,
                              labels=c("1","2","3","4","5","5-9",
                                       "10-14","15-19","20-24","25-29",
                                       "30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70+"))


ui <- fluidPage(
  fluidRow(
    column(3, selectizeInput("perCap_type","Total or new per capita", choices= c("new_perCapita","Cumul_cases_perCapita"))),
  column(9, checkboxGroupInput("age","Ages", inline =TRUE,selected = c("65-69","70+"), choices = levels(inc_by_A$age_range)))
  ),
  
  fluidRow(
plotOutput("ageinc"))
)

server <- function(input,output, session){
  output$ageinc <- renderPlot(ggplot(data=inc_by_A %>%
                      filter(age_range %in% input$age))+
               geom_line(aes(x=assigned_onset_date, y= .data[[input$perCap_type]] , color=age_range))
  )
}

shinyApp(ui,server)


