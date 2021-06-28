# Maps, GIFS and Shinies

Cleaning script
 - Includes the latest version of my script for cleaning Covid and population data
 - Automatically imports covid data from Taiwan CDC JSON
 - Automatically imports population data from Ministry of interior website
 - Calculates daily incidence stratified by various combinations of age,sex,and district.    
 - 06-258-2021 update: Calculates cumulative prevalence for all combinations of age,sex, and district

Maps and gifs script
- Makes animated maps of daily incidence by district
- 06-04-2021 update: makes animated map of daily cumulative cases per population by district
- Makes a shiny that allows users to select date and view incidence by district for that day
 - Shiny: I need help. I can make them, but I can't figure out how to get them online...

## Workflow

1. Download new taiwan districts, cleaning script, and maps and gifs script
2. Run cleaning script    
       - (OR directly open maps and gifs and run the source() line)
3. Play around with code in maps and GIFS script to make GIFS, maps and shiny


## Naming scheme
inc_by_SAD = incidence stratified by sex, age, and district   
inc_by_AD  = incidence stratified by age and district   
S= sex, A =age, D = district   

## Potential pitfalls
 -Chinese characters, especially traditional characters as used in Taiwan, don't always encode super well. Different R functions appear to prefer different encoding formats. Finding the right encoding also probably depends on your computer's system language setting. That's why there's both a UTF8 and a BIG5 version of the cleaning scripts. If one doesn't work, try the other.
 -If you run source() the encoding will matter. 
 -Shiny apparently doesn't work well on firefox (especially for leaflet QQ) 
 -And I'm not the only one!: https://stackoverflow.com/questions/65116942/firefox-leaflet-not-displaying-map-shapes-in-shiny-app-default-page-when-publish 

## If you just want to look at the pictures

<img src="https://github.com/Russell-Shean/Covid_SHINY_MAP/raw/main/quanguo_inc.gif" width="400" height="auto" /><img src="https://github.com/Russell-Shean/Taiwancovid/raw/main/beibu_inc.gif" width="400" height=auto />

Left side: New locally acquired cases per 100,000 people from 4/23/2021    
Right side: Northern Taiwan (Taipei, New Taipei, Taoyuan, Keelung)   
   
<img src="https://github.com/Russell-Shean/Covid_SHINY_MAP/raw/main/quanguo_prev.gif" width="400" height="auto" /><img src="https://github.com/Russell-Shean/Taiwancovid/raw/main/beibu_prev.gif" width="400" height=auto />

Left side: cumulative locally acquired cases per 100,000 people from 4/23/2021    
Right side: Northern Taiwan (Taipei, New Taipei, Taoyuan, Keelung)      
         
         


Here's what the Shiny looks like:    

<img src="https://github.com/Russell-Shean/Covid_SHINY_MAP/blob/main/192618317_223019976297967_895173108556869321_n.png" width="400" height=auto />



