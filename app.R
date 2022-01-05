#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(janitor)
library(scales)
library(countrycode)



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
      sidebarPanel(selectInput("country", "Select country:", choices = sort(unique(c(remittances$sender, remittances$receiver))), selected = "United Kingdom"),
                   br(),
                   selectInput("order", "Order by:", choices = c("Inflows", "Outflows", "Total"), selected = "Inflows"),
                   width = 3),
      mainPanel(
        plotOutput("myhist", height = "700px"), width = 9
      )
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$myhist <- renderPlot({
    
    plot_data <- rbind(
      remittances %>%
        filter(sender == input$country) %>%
        mutate(usd = usd/1000000, 
               type = "out") %>%
        rename(alpha = sender, beta = receiver),
      remittances %>%
        filter(receiver == input$country) %>%
        mutate(usd = usd/1000000, 
               type = "in") %>%
        rename(alpha = receiver, beta = sender)
    ) 
    
    ###get the data pivoted wider - and then order it properly 
    ordered_plot_data <- plot_data %>% 
      filter(type == "in") %>% rename(usd_in = usd) %>% select(-type) %>%
      full_join(
        filter(plot_data, type == "out") %>% rename(usd_out = usd) %>% select(-type)
      ) %>%
      arrange(desc(usd_in), desc(usd_out)) %>%
      mutate(total = usd_in + usd_out) %>%
      arrange(case_when(input$order == "Inflows" ~ desc(usd_in),
                        input$order == "Total" ~ desc(total),
                        TRUE ~ desc(usd_out)))
    
    ordered_plot_data$rank = c(1:nrow(ordered_plot_data))
    
    
    cbind(merge(ordered_plot_data$beta, c("in", "out")), input$country) %>% 
      setNames(c("beta", "type", "alpha")) %>%
      left_join(plot_data) %>%
      mutate(usd = ifelse(is.na(usd), 0, usd)) %>%
      left_join(select(ordered_plot_data, beta, rank)) %>%
      arrange(rank) %>%
      head(30) %>%
      ggplot(aes(x = reorder(beta, rank), y = usd, fill = type)) +
      geom_bar(stat = "identity", position = "dodge") + 
      scale_fill_manual(values = c("#2596be", "#384d78"), labels=c("Inflows", "Outflows")) +
      labs(x = "Country", y = "Remittances (million USD)", title =  paste(input$country, "Remittances (2017)")) + 
      scale_y_continuous(label=comma) +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.text=element_text(size=10),
            axis.title=element_text(size=10,face="bold"),
            axis.text.x = element_text(hjust = 1, angle = 45),
            legend.text = element_text(size=10),
            legend.title = element_blank())
  })
}
# Run the application 
shinyApp(ui = ui, server = server)