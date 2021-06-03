### mapssss

########################################################
##     First let's load the data we'll need     ######
########################################################


## Incidence

source("cleaning_script_Big5.R")


## shape files

#Here are the spatial libraries we'll need
library(sf)
library(rgeos)
library(GISTools)
library(sp)
library(tmap)


#Here's the shape file import:

load("C:/Users/rshea/Desktop/old computer/TB final/new.taiwan.district.rda")

#convert names b/c apparently different branches of 
# the government can't decide if it’s 台 or 臺

new.taiwan.district[new.taiwan.district$COUNTYNAME=="臺東縣",]$COUNTYNAME <- "台東縣"
new.taiwan.district[new.taiwan.district$COUNTYNAME=="臺中市",]$COUNTYNAME <- "台中市"
new.taiwan.district[new.taiwan.district$COUNTYNAME=="臺北市",]$COUNTYNAME <- "台北市"
new.taiwan.district[new.taiwan.district$COUNTYNAME=="臺南市",]$COUNTYNAME <- "台南市"


#this creates a combined city district column
new.taiwan.district$site_id <- paste(new.taiwan.district$COUNTYNAME,
                                     new.taiwan.district$TOWNNAME,
                                     sep = "")






#########################################################
############ make maps   ############################
###########################################################

# attach incidence to shape file
test1 <- new.taiwan.district %>%
  full_join(inc_by_D,
            by=c("site_id"="site_id"))%>%
  filter(!is.na(assigned_onset_date))%>% 
  relocate(site_id,.before = value)

# 1. GIF

#a. All of Taiwan
require(gifski)
library(lubridate)
quanguo_facet <- tm_shape(new.taiwan.district)+
  tm_borders()+
  tm_shape(test1) +
  tm_polygons("new_perCapita",
              title="New cases per 10^6") +
  tm_layout(title = "Daily Incidence by District")+
  tm_facets(along ="assigned_onset_date", as.layers = TRUE)

tmap_animation(quanguo_facet,filename = "quanguo_inc.gif",delay = 100)

#2. Northern taiwan
test2 <- test1 %>%
  filter(COUNTYNAME %in% c("台北市", "新北市","桃園市","基隆市"))

beibu_facet <- tm_shape(new.taiwan.district%>%
                            filter(COUNTYNAME %in% c("台北市", "新北市","桃園市","基隆市")))+
  tm_borders()+
  tm_shape(test2) +
  tm_polygons("new_perCapita",
              title="New cases per 10^6") +
  tm_layout(title = "Daily Incidence by District")+
  tm_facets(along ="assigned_onset_date", as.layers = TRUE)

tmap_animation(beibu_facet,filename = "beibu_inc.gif",delay = 100)

###################################################
#############                       #################
########            Shiny!!             ############
#############                       ###################
######################################################



# shiny 
library(shiny)

ui <- fluidPage(
  dateInput("day","Choose a date", min = "2021-04-23",
            max=Sys.Date()-1, value = "2021-05-23"),
  tmapOutput("inc_map")
)

server<- function(input, output, session){
  output$inc_map <- renderTmap({tm_shape(new.taiwan.district)+
      tm_borders()+
      tm_shape(test1[test1$assigned_onset_date== input$day,])+
      tm_polygons("new_perCapita",
                  title="New cases per 10^6")})
}

shinyApp(ui,server)



