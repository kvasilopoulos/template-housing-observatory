library(shiny)
library(shinydashboard)
library(dashboardthemes)

library(DT)
library(tidyverse)

# Set Options -------------------------------------------------------------

src <- FALSE
load <- FALSE
primary <<- FALSE
download_primary <<- FALSE
secondary <<- FALSE

# Source Scripts ----------------------------------------------------------

if (src) {
  suppressMessages(
    list.files(c("scripts"), full.names = TRUE, pattern = ".R") %>% 
      map(source)
  )
}

# Load everything ---------------------------------------------------------

if(load) {
  store <- c((items <- c("price", "income")),
           c("cnames"),
           paste0("summary_", items),
           paste0("datestamp_", items),
           paste0("plot_", items),
           paste0("autoplot_", items),
           paste0("autoplot_datestamp_", items))

  path_store <- paste0("data/", store, ".rds")

  for (i in seq_along(path_store)) assign(store[i], readRDS(file = path_store[i]))
}

# Header ------------------------------------------------------------------

mytitle = titlePanel(
  HTML('<a href="#shiny-tab-home" data-toggle="tab">
       <p style="font-size:20px;color:white;">
       <span> &nbsp; </span>
       <span style="background-color: rgb(189, 201, 213); border-radius: 3px;"> 
       &nbsp;<font color="white" size="2"> Beta </font> &nbsp; </span> </p>
       </a>')
  ,
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "nerd.png")
  )
  )

header <- dashboardHeader(
  
  title = mytitle,
  titleWidth = 450,
  
  # Return to original website
  tags$li(
    a(href = "",
      icon("power-off"),
      title = "Back to"),
    class = "dropdown"
  )
)

# Sidebar -----------------------------------------------------------------


sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "home", icon = icon("home"),
                         selected = TRUE),
                menuItem("Overview", tabName = "overview", 
                         icon = icon("globe",  lib = "glyphicon")),
                menuItem("Analysis", tabName = "analysis", icon = icon("table")),
                menuItem("Downloads",  icon = icon("file-text-o"),
                         menuItem("Raw", tabName = "raw", 
                                  icon = icon("angle-right"),
                                  menuSubItem("Price", tabName = "price"),
                                  menuSubItem("Price to Income", tabName = "income")
                         ),
                         menuItem("Estimation", tabName = "estimation",
                                  icon = icon("angle-right"),
                                  menuSubItem("Price", tabName = "est_price"),
                                  menuSubItem("Price to Income", tabName = "est_income")
                         ),
                         menuItem("Critical Values", tabName = "sequence",
                                  icon = icon("angle-right")
                         )
                ),
                hr()
    )
  )
)



# Body --------------------------------------------------------------------


body <- dashboardBody(
  
  # Make theme "html/theme.R"
  theme_boe_website,
  
  ######## Customization #################
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  ),
  
  # Customize the red to red_lanc
  # tags$style(
  #   type = 'text/css', 
  #   '.bg-blue {background-color: rgb(49, 70, 88)!important; }'
  # ),
  
  
# Analysis --------------------------------------------------------------------

  tabItems(
    tabItem(tabName = "home",
            includeHTML("home.html"),
            includeCSS("style.css")
            ),
    
    tabItem(tabName = "overview",
            fluidPage(style = "padding:0;",
              h2("Overview", style = "padding:1em 2em 0 2em;"),
              fluidRow(
                box(title = "Real House Prices",
                  plotOutput("autoplot_datestamp_price")
                ),
                box(title = "Real Price to Disposable Income Ratio (Housing Affordability)",
                  plotOutput("autoplot_datestamp_income")
                )
              )
            )
    ),
    
    tabItem(tabName = "analysis",
            
            div(class = "row", style = "text-align:left;padding:3em;",
                h2("Analysis"),
                column(6, 
                       p("This page provides figures for real house prices and
                         house price to disposable income ratios (housing affordability) 
                         starting in 1975, exuberance statistics, as well as date-stamping 
                         of the specific periods of exubernace.")
                       ),
                column(6,
                      offset = 1,
                      # selectInput("country", "Select Country:", cnames)
                )
               
            ),
            
          
            fluidRow(
              box(
                title = "Real House Prices",
                plotOutput("plot1"),
                width = 6
              ),
              box(
                title = "Real Price to Disposable Income Ratio (Housing Affordability)", 
                plotOutput("plot3"),
                width = 6
              )
            ),
            
            fluidRow(
              box(
                width = 12,
                background = "blue",
                p("Exuberance Statistics", style = "font-size:22px;text-align:center;")
              )
            ),
            
            fluidRow(
              box(
                title = "Real House Prices",
                tableOutput("table1")
              ),
              box(
                title = "Real Price to Disposable Income Ratio (Housing Affordability)", 
                tableOutput("table2")
              )
            ),
            
            
            fluidRow(
              box(
                width = 12,
                background = "blue",
                p("Date Stamping Periods of Exuberance", style = "font-size:22px;text-align:center;")
              )
            ),
            
            fluidRow(
              box(
                title = "Real House Prices",
                plotOutput("plot2"),
                p("There is exuberance when the statistic (blue line) exceeds the critical value 
                  (red line)", style = "text-align:center;"),
                width = 6),
              box(
                title = "Real Price to Disposable Income Ratio (Housing Affordability)", 
                plotOutput("plot4"),
                width = 6)
              ),
            
            fluidRow(
              box(
                width = 12,
                background = "blue",
                p("Date Stamping Periods of Exuberance Table", style = "font-size:22px;text-align:center;")
              )
            ),
            
            
            fluidRow(
              box(
                title = "Real House Prices",
                dataTableOutput("table3")
              ),
              box(
                title = "Real Price to Disposable Income Ratio (Housing Affordability)", 
                dataTableOutput("table4")
              )
            )
          
    ),
    

  
    # Data --------------------------------------------------------------------
    
    
    tabItem(tabName = "price",
            fluidPage(style = "padding-top:2em;",
              dataTableOutput("data_price")
            )
    ),
    tabItem(tabName = "income",
            fluidPage(style = "padding-top:2em;",
              dataTableOutput("data_income")
            )
    ),
    
    ### Estimation
    
    tabItem(tabName = "est_price",
            fluidPage(style = "padding-top:2em;",
              dataTableOutput("estimation_price")
            )
    ),
    tabItem(tabName = "est_income",
            fluidPage(style = "padding-top:2em;",
              dataTableOutput("estimation_income")
            )
    ),
    
    ### Critical Values
    
    tabItem(tabName = "stat",
            fluidPage(style = "padding-top:2em;",
              dataTableOutput("cv")
            )
            
    ),
    tabItem(tabName = "sequence",
            fluidPage(style = "padding-top:2em;",
                      box(width = 12,
                          dataTableOutput("cv_seq")
                      )
            )
    )
  )
)

# ui ----------------------------------------------------------------------



ui <- dashboardPage(#skin = "blue",
                    dashboardHeader(title = "Exuberance Analysis"),
                    sidebar, 
                    body)
  

# server ------------------------------------------------------------------



server <- function(input, output, session) {
  
# overview ----------------------------------------------------------------
  
  output$autoplot_datestamp_price <- 
    renderPlot({
      autoplot_datestamp_price
      })
  output$autoplot_datestamp_income <- 
    renderPlot({
      autoplot_datestamp_income
    })


# Analysis ----------------------------------------------------------------

  
  output$table1 <- renderTable({summary_price[[input$country]]},
                               striped = TRUE, bordered = TRUE,  
                               width = '100%', rownames = TRUE,
                               align = 'ccccc')
  output$table2 <- renderTable({summary_income[[input$country]]},
                               striped = TRUE, bordered = TRUE,  
                               width = '100%', rownames = TRUE,
                               align = 'ccccc')
  output$table3 <- renderDataTable({datestamp_price[[input$country]]},
                                   options = list(searching = FALSE, 
                                                  # bPaginate=FALSE,
                                                  ordering = FALSE,
                                                  dom = "t"))
  output$table4 <- renderDataTable({datestamp_income[[input$country]]},
                                   options = list(searching = FALSE, 
                                                  # bPaginate=FALSE,
                                                  ordering = FALSE,
                                                  dom = "t"))
  

    

# Plots -------------------------------------------------------------------

  
  
  output$plot1 <- renderPlot({plot_price[[input$country]]})
  
  output$plot2 <- renderPlot({autoplot_price[[input$country]]})
  
  output$plot3 <- renderPlot({plot_income[[input$country]]})
  
  output$plot4 <- renderPlot({autoplot_income[[input$country]]})
  

# Data --------------------------------------------------------------------


  ### Data section
  citation_data <- 
    "We would appreciate that anyone wishing to use this dataset, modified or 
otherwise, acknowledge the source of the data publicly available through this 
website with a citation of the working paper: for example, including a statement 
such as, 'The authors acknowledge use of the dataset described in Mack and Martínez-García (2011).'"
  citation_estimation <- 
    "We would appreciate that anyone wishing to use this dataset, modified or 
  otherwise, acknowledge the source of the data publicly available through this 
  website with a citation of the working paper: for example, including a statement 
  such as, 'The authors acknowledge use of the dataset described in Pavlidis et al. (2016).'"
  
  # Raw
  
  output$data_price <- DT::renderDataTable({
    make_DT(price, "price", citation_data)
  })
  
  
  output$data_income <- renderDataTable({
    make_DT(income, "price", citation_data)
  })
  
  ### Estimation
  
  output$estimation_price <- renderDataTable({
    make_DT(estimation_price, "estimation_price", citation_estimation)
  })
  
  output$estimation_income <- renderDataTable({
    make_DT(estimation_price, "estimation_price", citation_estimation)
  })
  
  output$cv_seq <- renderDataTable({
    make_DT_general(cv_seq, "cv_seq")
  })
  
}


# Launch ------------------------------------------------------------------

shinyApp(ui = dashboardPage(header, sidebar, body), server)
