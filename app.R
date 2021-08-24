library(shiny)
library(ggplot2)
library(tidyverse)



sum_data <- read.csv("https://github.com/guillaumed83/shiny_juveniles_restigouche/blob/master/Summary_year_d_avg_FL_R.csv")


list_lf <- c("0+","1+","2+")

col_catch1 <- c(rgb(61,173,138,alpha=170,maxColorValue = 255),rgb(217,95,2,alpha=170,maxColorValue = 255),rgb(136,133,188,alpha=170,maxColorValue = 255),
                rgb(234,74,154,alpha=170,maxColorValue = 255),rgb(124,180,62,alpha=170,maxColorValue = 255),rgb(232,183,39,alpha=170,maxColorValue = 255),
                rgb(179,138,62,alpha=170,maxColorValue = 255))

temp_cex=c(0.8,1,1.5,2,2.5)

max_dens <- c(5,2,1)
min_FL <- c(20,60,70)
max_FL <- c(85,110,170)

MyPalette <- c(
  "RES" = col_catch1[1],
  "KED" = col_catch1[2],
  "LMR" = col_catch1[3],
  "UPS" = col_catch1[4],
  "PAT" = col_catch1[5],
  "MAT" = col_catch1[6],
  "OTH" = col_catch1[7]
  
)

setwd("C:/Users/DauphinGu/Desktop/DFO projects/Geo_spatial/shiny")

ui <- fluidPage(
  
  titlePanel("Summary juvenile data in the Restigouche watershed"),
  
  sidebarLayout(
    
    #Inputs: Select variables to plot
    sidebarPanel(
    #fluidRow(  
      # Select Year
      selectInput(inputId = "Year",
                  label = "Year",
                  choices = sort(unique(sum_data$SURVEY)),
                  selected = max(unique(sum_data$SURVEY))),
 
      
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices =  c("JULIAN_DAY","d_0","d_1","d_2","AVG_LENGTH_0","AVG_LENGTH_1","AVG_LENGTH_2"),
        selected = "JULIAN_DAY"
      ),
      
      # # Select variable for y-axis
       selectInput(
         inputId = "y",
         label = "Y-axis:",
         choices = c( "d_0","d_1","d_2","AVG_LENGTH_0","AVG_LENGTH_1","AVG_LENGTH_2") ,
         selected = "d_0"
       ),
      
      width=1





 
      
      
    ),
    
    mainPanel(
    tabsetPanel(
      tabPanel(title = "Plot",
               plotOutput("plot_0")
      ),
      tabPanel(title = "Table",
               tableOutput("table_0")
      )
      
      
    )
    )
    
    # # Output: Show scatterplot
    # mainPanel(
    #   
    #   plotOutput(outputId = "scatterplot"),
    #   
    #   
    #   height = 10
    #   
    #   
    #   
    # )
  )
)  






# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  
  
   output$plot_0 <- renderPlot({
     
     x_vector <- sum_data %>% filter(SURVEY==input$Year) %>% select(input$x)
     y_vector <- sum_data %>% filter(SURVEY==input$Year) %>% select(input$y)
     
     if(input$x == "JULIAN_DAY"){
       x_limits <- c(170,300)
       
     }else{
       x_limits <-  c(min(pretty(x_vector[,1])),max(pretty(x_vector[,1]))  )
     }
     
     
     if(input$y == "JULIAN_DAY"){
       y_limits <- c(170,300)
     }else{
       y_limits <-  c(min(pretty(y_vector[,1])),max(pretty(y_vector[,1]))  )
     }
     
     
     
     
       
     ggplot(data = sum_data %>% filter(SURVEY==input$Year), aes_string(x = input$x, y = input$y,colour="Sub_catchment_index",size="TOT_COUNT_0")) +
       
       scale_x_continuous(limits = x_limits ) +
       scale_y_continuous(limits = y_limits ) +
       geom_point(shape=16)+
       scale_size_area(max_size = 25,
                       breaks = c(1, 10, 30, 100),
                       labels = c("1", "10", "30", "100+"),
                       guide = "legend",
                       limits = c(0, 100),
                       oob = scales::squish
                       )+#,guide=F) +
       # scale_size(range = c(1, 10),
       #            breaks = c(0, 10, 30, 50, 100),
       #            labels = c("0", "10", "30", "50", "100"),
       #            guide = "legend"
       # ) +
       # 
       
       geom_smooth(method=lm,aes(col = NULL),show.legend = F)+
       scale_colour_manual(values = MyPalette)+ 
       guides(colour = guide_legend(override.aes = list(size=10)))
   }, height=900,width=1200)


  output$table_0 <- renderTable({
       sum_data %>% filter(SURVEY==input$Year)
       
   })
 
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)


