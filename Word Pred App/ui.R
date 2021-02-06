suppressPackageStartupMessages(c(
    library(shinythemes),
    library(shiny)
))

appTitle = ("Coursera: Data Science Specialization (Capstone Project)")
browserText = "Capstone"

# create tabs and panels
shinyUI(fluidPage(titlePanel(appTitle,browserText),
                  
                  hr(), # styling
                  tags$head(tags$style(HTML("
    #final_text {
      text-align: center;
    }
    div.box-header {
      text-align: center;
    }
    "))),
                  
                  theme = shinytheme("cerulean"),
                  
                  navbarPage("Next Word Predictor",id ="navpanel",
                             
                             # Home tab is panel with a sidebar and main sections  
                             tabPanel("Word Predictor",
                                      sidebarLayout(
                                          
                                          #sidebar - Instructions 
                                          sidebarPanel(id="sidebarPanel"
                                                       , includeMarkdown("./Instructions.Rmd")
                                          ), 
                                          
                                          # mainpanel - text prediction app
                                          mainPanel(id="mainpanel",
                                                    tags$div(textInput("text", 
                                                                       label = h4("Input"),
                                                                       value = ),
                                                             br(),
                                                             tags$hr(),
                                                             
                                                             h4("Suggested next word:"),
                                                             tags$span(style="color:black",
                                                                       tags$strong(tags$h3(textOutput("nextWords")))),
                                                             br(),
                                                             tags$hr(),
                                                             
                                                             h4("Your input:"),
                                                             tags$span(style="color:black",
                                                                       tags$em(tags$h3(textOutput("inputWords")))),
                                                             align="center"))
                                      ),
                             ),
                             
                             # Analysis - the Milestone Report
                             tabPanel("Exploratory Analysis", includeMarkdown("./Milestone Project.Rmd")),
                            
                             # About - R, Shiny, and me
                             tabPanel("Miscellanea",includeMarkdown("./Miscelanea.Rmd")),
                             
                             tags$hr()
                  )))