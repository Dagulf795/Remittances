#
### This is is a Shiny app - this file needs to be renamed app.R, and placed in a folder with the relevant CSVs - for this app, it's country_coords.csv, country-capitals.csv, pridata.csv and rem_tot.csv. 
#

library(shiny)
library(tidyverse)
library(janitor)
library(maps)
library(geosphere)
library(countrycode)
library(scales)
library(ggmap)
library(mapdata)
library(shinyWidgets)


####DATA PREP


all_remittances <- read.csv("rem_tot.csv", stringsAsFactors = FALSE, header = TRUE) %>%
  clean_names() %>%
  filter(receiver != "Channel Islands" & sender != "Channel Islands") %>%
  mutate(sender = countrycode(sender, origin="country.name", destination = "iso3c") %>%
           countrycode(origin = "iso3c", destination = "country.name"),
         receiver = countrycode(receiver, origin="country.name", destination = "iso3c") %>%
           countrycode(origin = "iso3c", destination = "country.name")) %>%
  filter(sender != "United Kingdom" & receiver != "United Kingdom") 


remittances <- all_remittances[,1:3] %>%
  filter(usd > 0) %>%
  rbind(read.csv("pridata.csv") %>%
          filter(receiver != "Channel Islands" & sender != "Channel Islands") %>%
          filter(receiver != "Kosovo" & sender != "Kosovo") %>%
          filter(!is.na(usd)) %>%
          mutate(sender = countrycode(sender, origin="country.name", destination = "iso3c") %>%
                   countrycode(origin = "iso3c", destination = "country.name"),
                 receiver = countrycode(receiver, origin="country.name", destination = "iso3c") %>%
                   countrycode(origin = "iso3c", destination = "country.name"))
  ) %>%
  filter(sender != "Palestinian Territories" & receiver != "Palestinian Territories") %>%
  filter(sender != "Hong Kong SAR China" & receiver != "Hong Kong SAR China")

countries <- sort(unique(c(remittances[,1], remittances[,2])))

bilateral_remittance_matrix <- expand.grid(countries, countries) %>%
  setNames(c("sender", "receiver")) %>%
  left_join(remittances) %>%
  mutate(usd = ifelse(is.na(usd), 0, usd)) %>%
  pivot_wider(id_cols = sender, names_from = receiver, values_from = usd) %>%
  arrange(sender) %>% 
  column_to_rownames("sender") %>% 
  select(sort(tidyselect::peek_vars())) ###this sorts columns by name

##first - nodes 

country_nodes <- data.frame(country = countries, continent = countrycode(countries, origin="country.name", destination = "continent")) %>%
  mutate(continent = ifelse(is.na(continent), "Europe", continent)) %>%
  left_join(
    rbind(select(remittances, sender, usd) %>%
            group_by(sender) %>%summarise(usd = sum(usd)) %>%
            rename(country = sender),
          select(remittances, receiver, usd) %>%
            group_by(receiver) %>%summarise(usd = sum(usd)) %>%
            rename(country = receiver)
    ) %>%
      group_by(country) %>%
      summarise(total = sum(usd))
  )%>%
  arrange(desc(total)) %>%
  mutate(id = c(0:(length(countries)-1)),
         node_size = log(total)) %>%   ###then add country codes and capitals 
  mutate(country_code = countrycode(country, origin="country.name", destination = "iso3c")) %>%
  left_join(read.csv("country-capitals.csv", header = TRUE, stringsAsFactors = FALSE)[1:2] %>%
              mutate(country_code = countrycode(country, origin="country.name", destination = "iso3c")) %>%
              mutate(capital = ifelse(country == "Israel", "Tel Aviv", capital)) %>%
              select(country_code, capital) %>%
              filter(capital != "Sucre"), ###the data I used had two capitals listed for Bolivia - I'm happy to go with La Paz
            by = "country_code") %>%
  distinct() %>%
  filter(capital != "Belfast")


##this gives you a function which makes green 0 and red 1
greenred <- colour_ramp(c("green", "red"))

###this gives all the cases where the sender is ahead or the same as the receiver alphabetically 
bilateral <- remittances %>% setNames(c("alpha", "beta", "b2a")) %>% 
  full_join(remittances %>% setNames(c("beta", "alpha", "a2b"))) %>%
  select(alpha, beta, a2b, b2a) %>%
  arrange(alpha, beta) %>% 
  mutate(a2b = ifelse(is.na(a2b), 0, a2b),
         b2a = ifelse(is.na(b2a), 0, b2a)) %>%
  filter(a2b >= b2a) %>%
  filter(!(a2b == 0 & b2a == 0)) %>%
  mutate(a2b_net = a2b - b2a,
         a2b_percentage = 2*((a2b/(a2b+b2a))-0.5),
         total = a2b + b2a) %>%
  mutate(link_colour = greenred(a2b_percentage)) %>%
  left_join(select(country_nodes, country, id) %>% setNames(c("alpha", "Source"))) %>%
  left_join(select(country_nodes, country, id) %>% setNames(c("beta", "Target"))) %>%
  mutate(link_value = log10(total)) 

###get country coordinates and join on the main table - I'm doing this because geocode generates two columns 
country_nodes <- cbind(country_nodes, read.csv("country_coords.csv", header = TRUE, stringsAsFactors = FALSE)) 


###create the world map 

world <- map_data("world") %>%
  filter(region != "Antarctica") %>%
  mutate(continent = countrycode(region, origin="country.name", destination = "continent")) %>%
  mutate(continent = case_when(region %in% c("Siachen Glacier") ~ "Asia",
                               region %in% c("Barbuda", "Bonaire", "Grenadines", "Saba",  "Sint Eustatius", "South Georgia", "South Sandwich Islands", "Virgin Islands") ~ "Americas",
                               region %in% c("Kosovo") ~ "Europe",
                               region %in% c("Micronesia") ~ "Oceania",
                               TRUE ~ continent)
  ) %>%
  mutate(continent = ifelse(region == "Russia" & long > 41, NA, continent)) %>%
  mutate(continent = ifelse(region %in% c("Pitcairn Islands", "Cocos Islands", "French Polynesia", "Christmas Island"), NA, continent))


###arc vectorised approach 

##get the coords for each bilateral flow

bilateral_coords <- bilateral %>%
  left_join(select(country_nodes, country, lon, lat) %>% setNames(c("alpha", "alpha_lon", "alpha_lat"))) %>%
  left_join(select(country_nodes, country, lon, lat) %>% setNames(c("beta", "beta_lon", "beta_lat"))) %>% 
  mutate(group = paste(alpha, beta)) %>%
  select(group, alpha_lon, alpha_lat, beta_lon, beta_lat) 

###create a custom function that takes a vector as an input
arc_coords <- function(x) {
  gcIntermediate(x[2:3], x[4:5], n=98, addStartEnd=TRUE )
}
###apply that function to every row of bilateral_coords

arc <- data.frame(group = rep(bilateral_coords$group, each = 100),
           do.call("rbind", lapply(split(bilateral_coords, 1:nrow(bilateral_coords)),  arc_coords))) %>%
  group_by(group) %>%
  mutate(check = (max(lon) - min(lon)) > 180) %>%
  mutate(group = ifelse(check, paste(ifelse(lon > 0, "", "Negative "), group, sep = ""), group))

###this negative thing sorts out the whole issue with arcs going round the back of the world getting connected wrongly 


  


ui <- 
  fluidPage(
    titlePanel("Global Remittance Flows"),
    
    plotOutput('map'),
    
    hr(),
    
    fluidRow(
      column(3,
             sliderInput("min_flow",
                         "Set a minimum total value for a flow to be included (in million USD):",
                         min = 0,
                         max = 1000,
                         value = 300,
                         step = 20),
             sliderInput("width",
                         "Set the width of the flows:",
                         min = 0,
                         max = 3,
                         value = 0.8,
                         step = 0.1),
             sliderInput("arrow_size",
                         "Set the size of the arrows:",
                         min = 0,
                         max = 5,
                         value = 1,
                         step = 0.5)
      ),
      column(4, offset = 1,
             multiInput(
               inputId = "countries", label = "Click on a country to exclude it from the map:",
               choices = sort(unique(c(remittances$sender, remittances$receiver)))
             )
      ),
      column(4,
             selectInput("continent", "Focus on a particular region:", 
                         choices = c("World", "Africa", "Americas", "Asia", "Europe", "Oceania"), selected = "World"),
             multiInput(
               inputId = "ego", label = "Focus on the flows to and from particular countries:",
               choices = sort(unique(c(remittances$sender, remittances$receiver)))
             )
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
    output$map <- renderPlot({
      
      ###first, reduce down according to the filter level selected
      
      
      ##reduce down by TOTAL
      bilateral_filtered <- bilateral %>%
        filter(total > input$min_flow*1000000) %>%
        select(-c("Source", "Target"))
      
      country_nodes_filtered <- country_nodes %>%
        filter(country %in% unique(c(bilateral_filtered$alpha, bilateral_filtered$beta))) %>%
        select(-id) 
      country_nodes_filtered$id <- c(0:(nrow(country_nodes_filtered)-1))
      
      bilateral_filtered <- bilateral_filtered %>%
        left_join(select(country_nodes_filtered, country, id) %>% setNames(c("alpha", "Source"))) %>%
        left_join(select(country_nodes_filtered, country, id) %>% setNames(c("beta", "Target")))
      
      
      ###reduce down by SELECTED CONTINENTS
      
      bilateral_filtered <- bilateral_filtered %>%
        mutate(continent_alpha = countrycode(alpha, origin="country.name", destination = "continent"),
               continent_beta = countrycode(beta, origin="country.name", destination = "continent")) %>%
        filter((input$continent == continent_alpha & input$continent == continent_beta) | input$continent == "World")
      
      
      country_nodes_filtered <- country_nodes_filtered %>%
        filter(input$continent == continent | input$continent == "World")
      
      ###reduce down to SELECTED COUNTRIES
      
      
      bilateral_filtered <- bilateral_filtered %>%
        filter(!(alpha %in% input$countries) & !(beta %in% input$countries)) %>%
        filter(case_when(length(input$ego) >= 1 ~ alpha %in% input$ego | beta %in% input$ego,
                         TRUE ~ TRUE))
      
      country_nodes_filtered <- country_nodes_filtered %>%
        filter(!(country %in% input$countries))
      
      
      ###now get the arcs
      ##we've already created all of them - so filter that down to the ones from bilateral_filtered
      
      arc_filtered <- arc %>%
        filter(group %in% paste(bilateral_filtered$alpha, bilateral_filtered$beta) | group %in% paste("Negative ", bilateral_filtered$alpha, " ", bilateral_filtered$beta, sep = ""))
        
      
    ###filter the world map 
      
     world <- world %>%
        filter(input$continent == continent | input$continent == "World") 
      
    ##then change the longitude for Oceania - centre on IDL
     
     if(input$continent == "Oceania") {
       world <- world %>%
         mutate(long = -1*(long/abs(long))*(180-abs(long)))
       
       country_nodes_filtered <- country_nodes_filtered %>%
         mutate(lon = -1*(lon/abs(lon))*(180-abs(lon)))
       
       arc_filtered <- arc_filtered %>%
         mutate(lon = -1*(lon/abs(lon))*(180-abs(lon)))
     }
     else {
     }
     
       ###now sort the alpha and colour of the arcs. 
     
     
     arc_aes <- select(arc_filtered, group) %>%
       distinct() %>%
       mutate(alpha_beta = gsub("Negative ", "", group)) %>%
       left_join(bilateral_filtered %>%
                   select(alpha, beta, link_colour, total) %>%
                   mutate(alpha_beta = paste(alpha, beta)) %>%
                   select(alpha_beta, link_colour, total)) %>%
       select(group, link_colour, total) %>%
       ungroup() %>%
       mutate(total = log10(total)) %>%
       mutate(total = (total - min(total))/(max(total) - min(total)))
     
     arc_filtered <- arc_filtered %>%
       left_join(arc_aes)
       
       
      
      
      ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "#384d78", colour = "white", size = 0.1) + 
        coord_fixed(1.3) +  geom_point(data = country_nodes_filtered, aes(x = lon, y = lat), color = "#2596be", size = 1) +
        geom_path(data = arc_filtered, aes(x = lon, y = lat, group = group), color = arc_filtered$link_colour, alpha = arc_filtered$total, 
                  size = input$width, arrow = grid::arrow(length = unit(input$arrow_size, "mm"), type = "closed")) + 
        theme(legend.position = "none") + 
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
