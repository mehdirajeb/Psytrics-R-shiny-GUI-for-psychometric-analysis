library(shiny)
library(shinythemes)
library(bslib)


my_theme <- bs_theme(
  bg = "#202123", fg = "#B8BCC2", primary = "#EA80FC", 
  base_font = font_google("Grandstander"),
  "font-size-base" = "1.1rem"
)


shinyUI( fluidPage(theme = bs_theme_update(my_theme, fg = "#D01717", primary = "#710428", 
                                           font_scale = NULL, bootswatch = "flatly", bg = "#FFFFFF"),

  navbarPage( title = "Psytrics for Psychometric Analaysis" ,

                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel("Data Upload", width = 3,
                                fileInput("file","Please upload data file"),
                                helpText("Maximum file size should be 5MB. Also, the data set should include only item related variables."),
                                tags$hr(),
                                h5(helpText("Select the table parameters below")),
                                checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                                checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),

                                br(),
                                radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),

                              ),

                            mainPanel("Uploaded Data", width = 9,
                              tableOutput("table")
                              )
                            )),
############################################################################################### end of first page
                    tabPanel("Summary Statistics", tableOutput("sum")),
                

############################### Transformed scores#############################################

                    tabPanel("Transformed Scores",
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("col", "Select a Variable", character(0)),
                                 submitButton("Calculate scores"),
                                 p(" Please click to calculate psychometric scores. Also, please note that if your data contains missing values(i.e., NA), 
                                   missing values will be replaced by 0 for score calculations")

                               ),

                             mainPanel(
                               tableOutput("score")
                             ))

                             ),

######################################################################################################

navbarMenu("Plots",
           ##########Histogram#####################
           #tabPanel("Histogram",plotOutput("hist")
           
           tabPanel("Histogram",
                    sidebarLayout(
                      sidebarPanel( width = 4,
                                    actionButton("Histogram", "Histogram"),
                                    helpText("Please click on the Histogram button for data visualizations.")
                      ),
                      
                      mainPanel("Distributions of all variables", width =8,
                                plotOutput("hist")
                      )
                    )),
           
           #tabPanel("Q-Q plot", plotOutput("qq")),
           tabPanel("Q-Q Plot",
                    sidebarLayout(
                      sidebarPanel( width = 4,
                                    actionButton("Q", " Create Q-Q plot"),
                                    helpText("Please click on the Q-Q plot button for for Q-Q plots of all variables.")
                      ),
                      
                      mainPanel("Normal Q-Q plot of all variables", width =8,
                                plotOutput("qq")
                      )
                    )),
           tabPanel("Correlation Plot", plotOutput("corr"))
           
),
#######################################################################################################################

                   navbarMenu("Reliability",
                              tabPanel("Alpha",
                                       sidebarLayout(
                                         sidebarPanel("Chronbach's Alpha", width = 3,
                                           actionButton("calculate", "Calculate Alpha")
                                         ),

                                         mainPanel("Alpha estimates", width =9,
                                           verbatimTextOutput("alpha")
                                         )
                                       )),
                     tabPanel("Split-half reliability", verbatimTextOutput("sp")),
                     #tabPanel("Omega", verbatimTextOutput("omega")),
                     tabPanel("Omega",
                              sidebarLayout(
                                sidebarPanel("Omega", width = 3,
                                  actionButton("calculate1", "Calculate Omega")
                                ),

                                  mainPanel("Omega estimates", width =9,
                                  verbatimTextOutput("omega")
                                )
                              ))
                     ),
                   
                   navbarMenu("Item Analaysis",
                              tabPanel("Inter-item correlations", tableOutput("ic")),
                              tabPanel("Item Statistics", tableOutput("is")),
                              tabPanel("Discrimination Analaysis", tableOutput("disc")),
                              tabPanel("Option Analysis", plotOutput("oa")),
                              

                   ),

                   navbarMenu("Dimensionality Analaysis",
                              tabPanel("Parallel Analysis", tableOutput("pa")),
                              #tabPanel("Factor Analysis", verbatimTextOutput("fa")),
                              tabPanel("Factor Analysis",
                                       sidebarLayout(
                                         sidebarPanel(" Factor Analysis", width = 4,
                                          numericInput("nfactor", "Specify the number of factors",1),
                                          selectInput("rotate", "Specify the rotation", c("none","oblimin", "Varimax","quartimax", "bentlerT",
                                                                                          "equamax","varimin", "geominT","bifactor") ),
                                           paste("To run the factor analysis, please click the following button"),
                                           submitButton( "Run Factor Analysis"),
                                           paste("thank you")
                                         ),

                                         mainPanel("Estimates from factor analysis", width = 8,
                                           verbatimTextOutput("fa"),

                                         )
                                       )),
                   ),


                   ),



)
)
