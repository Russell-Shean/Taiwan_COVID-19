# Maps, GIFS and Shinies

- Cleaning script
 -- Includes the latest version of my script for cleaning Covid and population data
 - automatically imports covid data from Taiwan CDC JSON
 - calculates daily incidence stratified by various combinations of age,sex,and district 
  - (Someone could probably make this into daily cumulative prevalence fairly easily)

- Maps and gifs script
 - Makes animated maps of daily incidence by district
 - Makes a shiny that allows users to select date and view incidence by district for that day
  - Shiny: I need help. I can make them, but I can't figure out how to upload them~~~

## Workflow

1. Download new taiwan districts, pop_by_sex2, cleaning script, and maps and gifs script
2. Run cleaning script 
 - (OR directly open maps and gifs and run the source() line)
3. Play around with code in maps and GIFS script to make GIFS, maps and shiny
4. If you figure out how to get shiny online, pleaseee email me link and explanation of how you did it hahaha
5. No, but seriously if you do, email me: russshean@gmail.com 

## potential pitfalls
Chinese characters, especially traditional characters as used in Taiwan, don't always encode super well. That's why there's both a UTF8 and a BIG5 version of cleaning scripts.
If you run source() the encoding will matter. 

