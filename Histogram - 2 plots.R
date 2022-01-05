#
### This is is a Shiny app - this file needs to be renamed app.R, and placed in a folder with the relevant CSVs - for this app, it's pridata.csv and rem_tot.csv. 
#


library(shiny)
library(tidyverse)
library(janitor)
library(scales)
library(shinyWidgets)
library(gridExtra)
library(countrycode)


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


# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    titlePanel("Bilateral Remittance Flows"),
    sidebarLayout(
      position = "left",
      sidebarPanel(selectInput("country", "Select country", choices = sort(unique(c(remittances$sender, remittances$receiver))), selected = "United Kingdom"),
                   width = 3),
      mainPanel(
        plotOutput("myhist", height = "700px"), width = 9
      )
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output) {

  output$myhist <- renderPlot({
    out_plot <- remittances %>%
      filter(sender == input$country) %>%
      mutate(usd = usd/1000000) %>%
      arrange(desc(usd)) %>%
      head(15) %>%
      ggplot() + geom_col(aes(x = reorder(receiver, desc(usd)), y = usd), colour = "#2596be", fill = "#384d78") +
      labs(x = "Receiver", y = "Remittances (million USD)", title =  paste(input$country, "Remittance Outflows (2017)")) + 
      scale_y_continuous(label=comma) +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.text=element_text(size=10),
            axis.title=element_text(size=10,face="bold"),
            axis.text.x = element_text(hjust = 1, angle = 45))
    
    in_plot <-  remittances %>%
      filter(receiver == input$country) %>%
      mutate(usd = usd/1000000) %>%
      arrange(desc(usd)) %>%
      head(15) %>%
      ggplot() + geom_col(aes(x = reorder(sender, desc(usd)), y = usd), colour = "#2596be", fill = "#384d78") +
      labs(x = "Sender", y = "Remittances (million USD)", title =  paste(input$country, "Remittance Inflows (2017)")) + 
      scale_y_continuous(label=comma) +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.text=element_text(size=10),
            axis.title=element_text(size=10,face="bold"),
            axis.text.x = element_text(hjust = 1, angle = 45))
    
    grid.arrange(out_plot, in_plot, ncol = 1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


