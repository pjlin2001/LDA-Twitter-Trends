library(modules) # must load
# load packages
packages.Self <- modules::use("core/libs.R")
packages.Self$getPackages("ui")
library(shiny) 
library(plotly) 
library(leaflet)
library(ggplot2)
library(miceadds) 
library(shinytoastr)
library(shinyWidgets) 
library(shinydashboard) 
library(dashboardthemes)
library(shinydashboardPlus)

# source widgets
source.all("widgets/", grepstring="\\.R")
source("modules/tweet_analysis_module.R")


# Define UI for the application
ui <- shinydashboardPlus::dashboardPage(
    dashboardHeader(
      title = "Twitter Trend Analysis",
      disable = FALSE
    ),

    dashboardSidebar(
      width = 0,
      sidebarMenu(
        id = "tabs",
        menuItem("Twitter Data", tabName = "database", icon = icon("database")),
        menuItem("Our Code", icon = icon("code"), tabName = "code"),
        menuItem("Documentations", icon = icon("dochub"), tabName = "document")
      )),

    #dashboard body starts here
    dashboardBody(
      # changing theme
      shinyDashboardThemes(
        theme = "blue_gradient"
      ),
      useToastr(), # for alert on data

      # add css file here
      tags$head( tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),

      navbarPage("",
         position = c("fixed-bottom"),
         collapsible = TRUE,
         fluid = TRUE,
         
         #**The Trend Maps Tab** 
         tabPanel( 
           title ="Trend Maps", 
           icon = icon("globe-americas"), 
           #**Add the states map widget**
           states_map_widget(),
           # Separating box
           vs_box(),
           #**Add the state map widget**
           counties_map_widget(),
           # Separating box
           vs_box(),
           #**Add Tweets table analysis widget**
           tweets_table_widget(),
           vs_box(),
           #**Add LDA analysis widget**
           tweets_lda_widget(),
           vs_box()
           ),
         
         #**Usage Guidelines Tab**
         tabPanel( 
           title ="Usage Guidelines", 
           icon = icon("info-circle"), 
         ),
         
         #**Our write-up Tab**
         tabPanel( 
           title ="Milestone Summary", 
           icon = icon("file"), 
         ),
      )) 
)

return(ui)
