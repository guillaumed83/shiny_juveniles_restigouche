library(shiny)
library(ggplot2)
library(tidyverse)



sum_data <- read.csv("https://raw.githubusercontent.com/guillaumed83/shiny_juveniles_restigouche/master/Summary_year_d_avg_FL_R.csv")


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

ui <- fluidPage(
  
  titlePanel("Summary juvenile data in the Restigouche watershed"),

  tabsetPanel(
  
    tabPanel(title = "Description",
             
             h2("Foreword"),
             
             h4("This app is a companion tool to the 2021 Report:",tags$a(href="https://waves-vagues.dfo-mpo.gc.ca/Library/40931493.pdf", "JUVENILE ATLANTIC SALMON (Salmo salar) MONITORING ACTIVITIES
                  IN THE RESTIGOUCHE RIVER (SOUTHERN GULF OF ST. LAWRENCE, CANADA) 1972 TO 2019"),"."),
             h4("This report summarises the sampling program, site characteristics, and the estimated densities 
                  and biological characteristics of Atlantic Salmon (Salmo salar) juveniles sampled from the
                  Restigouche River watershed between 1972 and 2019. 
                The changes in sampling methodology(e.g. number and duration of electrofishing sweeps, type of gear) and design are documented.
                A hierarchical model for estimating abundance of salmon juveniles at individual sites that accounts
                for the changes in sampling methodology over years is described.
                The model also provides estimates of the average annual densities of juveniles at the subwatershed scale. 
                The data collected and analysed are from an ongoing science program to monitor changes in the relative
                abundance of juvenile salmon and other species in the Restigouche watershed."
             
             ),
             h4("It is strongly advised to consult the report to understand what is presented in this app."),
                               
             h2("Word of warning"),
             h4("This app is not intended as an analysis platform but rather a visualization tool that allows the quick exploration of the results generated in Dauphin et al. (2021). "),
             h4("The user can select a year of interest and plot two variables of their choice. on each plot, a dot correspond to one site, sub-watershed are color-coded as indicated in the legend, 
                and the size of the dot is proportionate to the number of juvenile that had their FL measured."),
             
             h4("In the Visualization tab, the plots are automatically generated with a linear model fitted to the selected data. This allow a quick look at the general trend however,
                this is likely not the most appropriate way to analyse the data and therefore this should be kept in mind while exploring the dataset"),
             
             h2("Glossary"),
             h4("The visualization tab allows the user to plot different variable listed below:"),
             h4(tags$b("JULIAN_DAY"), ": Calendar day of sampling/electrofishing"),
             h4(tags$b("d_0"), ": Average density of 0+ juveniles"),
             h4(tags$b("d_1"), ": Average density of 1+ juveniles"),
             h4(tags$b("d_2"), ": Average density of 2+ juveniles"),
             h4(tags$b("AVG_LENGTH_0"), ": Average fork length of 0+ juveniles"),
             h4(tags$b("AVG_LENGTH_1"), ": Average fork length of 1+ juveniles"),
             h4(tags$b("AVG_LENGTH_2"), ": Average fork length of 2+ juveniles"),
             
             
             h2("Contact"),
             h4(tags$a(href="mailto:guillaume.dauphin@dfo-mpo.gc.ca","guillaume.dauphin@dfo-mpo.gc.ca")),
             
             h2("Reference"),
             h4("Dauphin, G.J.R., Arsenault, M., Benwell, I., Biron, M., Cameron, P., Olive, A., Pickard, R., and
               Chaput, G. 2021. Juvenile Atlantic Salmon (Salmo salar) monitoring activities in the
               Restigouche River (southern Gulf of St. Lawrence, Canada), 1972 to 2019. Can. Data
               Rep. Fish. Aquat. Sci. 1321: xiv + 324 p")
             
             ),
 
    tabPanel(
      title = "Visualization",
      sidebarLayout(
    
        #Inputs: Select variables to plot
        sidebarPanel(
        #fluidRow(  
          # Select Year
          selectInput(inputId = "Year",
                      label = "Year",
                      choices = sort(unique(sum_data$SURVEY)),
                      selected = max(unique(sum_data$SURVEY))),
          
          radioButtons(inline=T,"age", "Age",
                       c("NA","0+", "1+","2+"), selected = "NA"),
          
          h5("These buttons control the size of the points to be proportionate to the number of juveniles of the given lifestage that were measured. If NA, all points have the same size."),
          
          
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
          
          width=2
    
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

      )
    )
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
     
     ##Showing the size of the dots only if x or y are average size
     
     if(input$age == "NA"){size_vector = 10}
     if(input$age == "0+"){size_vector = "TOT_COUNT_0"}
     if(input$age == "1+"){size_vector = "TOT_COUNT_1"}
     if(input$age == "2+"){size_vector = "TOT_COUNT_2"}   
    
#        
     ggplot(data = sum_data %>% filter(SURVEY==input$Year), aes_string(x = input$x, y = input$y,colour="Sub_catchment_index",size=size_vector)) +
       
       scale_x_continuous(limits = x_limits ) +
       scale_y_continuous(limits = c(-100,200) ) +
       geom_point(shape=16)+
       scale_size_area(max_size = 25,
                       breaks = c(1, 10, 30, 100),
                       labels = c("1", "10", "30", "100+"),
                       guide = "legend",
                       limits = c(0, 100),
                       oob = scales::squish
                       )+
       geom_smooth(method=lm,aes(col = NULL,size=NULL),show.legend = F)+
       coord_cartesian( xlim = x_limits ,ylim=y_limits )+
       scale_colour_manual(values = MyPalette)+ 
       guides(colour = guide_legend(override.aes = list(size=10),order=1) )
       
       
       
   }, height=900,width=1200)


  output$table_0 <- renderTable({
       sum_data %>% filter(SURVEY==input$Year)
       
   })
 
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)


