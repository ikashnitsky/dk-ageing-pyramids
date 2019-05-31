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
library(extrafont)
library(hrbrthemes); import_roboto_condensed()
source("helpers.R")

# df_path <- "https://ikashnitsky.github.io/share/1905-shiny-dk-ageing-pyramids/for-shiny.rda"
# load(url(df_path))

# deal with data
pop.Cons <- readRDS("data/pop.Cons.rds")
pop.Dream <- readRDS("data/pop.Dream.rds")
pop.OV2 <- readRDS("data/pop.OV2.rds")
pop.OVcu <- readRDS("data/pop.OVcu.rds")

popD65.f<-colSums(pop.Dream$projF[66:111,])
popO65.f<-colSums(pop.OVcu$projF[66:111,])
popC65.f<-colSums(pop.Cons$projF[66:111,])
pop265.f<-colSums(pop.OV2$projF[66:111,])

popD65.m<-colSums(pop.Dream$projM[66:111,])
popO65.m<-colSums(pop.OVcu$projM[66:111,])
popC65.m<-colSums(pop.Cons$projM[66:111,])
pop265.m<-colSums(pop.OV2$projM[66:111,])



# load("data/for-shiny.rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # titlePanel(title = 'Tricolore: A flexible color scale for ternary compositions'),
    
    sidebarLayout(
        
        # INPUT
        sidebarPanel(width = 3,
            sliderInput(inputId = 'year', label = 'Projection year', ticks = TRUE,
                        min = 2020, max = 2070, step = 5, value = 2050, animate = T),
            sliderInput(inputId = 'cutoff', label = 'Age cut-off', ticks = TRUE,
                        min = 0, max = 100, step = 5, value = 65)
        ),
        
        # OUTPUT
        mainPanel(plotOutput(outputId = 'pyramid'))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$pyramid <- renderPlot(res = 120, width = 1000, height = 800, {
        
        draw_pyramid(proj_year = input$year, cut_off_age = input$cutoff)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
