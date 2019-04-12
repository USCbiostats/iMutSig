library(shiny)
library(dplyr)
library(rvest)
library(DT)
library(shinythemes)
library(shinyjs)


source('functions/extraFunctions.R')
source("Background.R")

library(shinydashboard)

ui <- dashboardPage(
  
    header <- dashboardHeader(
      title = "iMutSig: identifying the most similar mutational signature",
      titleWidth = 550,
      dropdownMenuOutput("menu")
    ),
    

    sidebar <- dashboardSidebar(    
      sidebarMenu(
      id="mysidebar",
      menuItem("COSMIC to pmsignature", icon = icon("chart-bar"), tabName = "page1"),
      menuItem("pmsignature to COSMIC", icon = icon("cube"), tabName = "page2"),
      menuItem("Input a signature", icon = icon("sign-in-alt"),  startExpanded = TRUE,
               menuSubItem('COSMIC signature', tabName = 'page3'),
               menuSubItem('pmsignature', tabName = 'page4')
        ), 
      menuItem("About iMutSig", icon = icon("info-circle"), tabName = "page5")
      )
    ), 
      
    
    body <- dashboardBody(
      tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #222D32;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #222D32;
        }
        .skin-blue .main-header .navbar {
          background-color: #222D32;
        }
        
      '))),

      useShinyjs(),
      
      tabItems(
        tabItem(tabName = "page1",
                # 1st row 
                fluidRow(
                  box(
                    width = 5, 
                    title = paste("Chosen a COSMIC signature"),
                    plotOutput("selected_sig_1", height = 200)
                  ),
                  
                  box(
                    width = 7, 
                    title = "Its membership among 40 cancer types", 
                    plotOutput("corrplot1_2", height = 200)
                  )
                ),
                
                # 2nd row 
                fluidRow(
                  box(width = 4,
                      style="font-size:120%",
                      dataTableOutput("mytable1")
                  ),
                  
                  box(width = 8, 
                      title = "Its cosine similarity to the pmsignatures",
                      plotOutput("corrplot1_1", height = 180))
                ),
                
                fluidRow(
                  valueBox(width = 2, color = "black",
                           "Choose", "a COSMIC signature"),
                  
                  box(width = 2, height = "100px",
                      selectInput(inputId = "N_F", label="COSMIC",
                                  choices = paste0("C",1:30),
                                  selected = 1)
                  ),
                  
                  valueBoxOutput(width = 2,
                                 "selected1_1"),
                  
                  valueBoxOutput(width = 2,
                                 "highest"),
                  
                  valueBoxOutput(width = 2,
                                 "selected1_2"),
                  
                  box(width = 2, height = "100px",
                      selectInput(inputId = "N_S_D", label="pmsignature",
                                  choices = paste0("P",1:27),
                                  selected = 3)
                  )
                ),
                
                # 3nd row 
                fluidRow(
                  box( 
                    width = 4,
                    title = "COSMIC signature:",
                    status = "primary", solidHeader = TRUE,
                    uiOutput("selected_sig_text_1")
                  ),
                
                  box(
                    width = 6,
                    plotOutput("selected_sig_full_1", height = 180)
                  ),
                  
                  box(
                    width = 2
                  )

                ),
                
                fluidRow(
                  box(
                    width = 4,             
                    status = "success", solidHeader = TRUE,
                    title = "Most similar pmsignature:",
                    uiOutput(paste0('selected_sig_text_1_', 1))
                  ), 
                  
                  box(
                    width = 6, 
                    plotOutput(paste0('selected_sig_pm_full_1_', 1), height = 180)
                  ),
                  
                  box(
                    width = 2,                                 
                    plotOutput(paste0('selected_sig_full_1_', 1), height = 180)
                  )
                ), 
                
                fluidRow(
                  box(
                    width = 4, 
                    title = "Selected pmsignature:",
                    status = "warning", solidHeader = TRUE,
                    uiOutput(paste0('selected_sig_text_1_', 2))
                  ), 

                  box(
                    width = 6,
                    plotOutput(paste0('selected_sig_pm_full_1_', 2), height = 180)
                  ),
                  
                  box(
                    width = 2,                                 
                    plotOutput(paste0('selected_sig_full_1_', 2), height = 180)
                  )
                )
        ),

        tabItem(tabName = "page2",
                # First tab content
                fluidRow(
                  box(
                    width = 4,
                    title = paste("Chosen pmsignature"),
                    plotOutput("selected_sig_2", height = 200)
                  ),
                  
                  box(
                    width = 8, 
                    title = "Its membership among 30 cancer types",
                    plotOutput("corrplot2_1", height = 200)
                  )
                  
                ),
                
                # 2nd Row 
                
                fluidRow(
                  box(width = 4,
                      style="font-size:120%",
                      dataTableOutput("mytable2")
                  ),
                  
                  box(width = 8, 
                      title = "Its cosine similarity to the COSMIC signatures",
                      plotOutput("corrplot2_2", height = 200))
                ),
                
                # 3rd Row
                
                fluidRow(
                  valueBox(width = 2, color = "black",
                           "Choose", "a pmsignature"),
                  
                  box(width = 2, height = "100px",
                      selectInput(inputId = "N_D", label="pmsignature",
                                  choices = paste0("P",1:27),
                                  selected = 1)
                      
                  ),
                  
                  valueBoxOutput(width = 2,
                                 "selected2_1"),
                  
                  
                  valueBoxOutput(width = 2,
                                 "highest2"),
                  
                  valueBoxOutput(width = 2,
                                 "selected2_2"),
                  
                  box(width = 2, height = "100px",
                      selectInput(inputId = "N_D_S", label="COSMIC",
                                  choices = paste0("C",1:30),
                                  selected = 3)
                  )
                ),
                
              
                fluidRow(
                  box(
                    width = 4,
                    title = "pmsignature in COSMIC signature style:",
                    status = "primary", solidHeader = TRUE,
                    uiOutput("selected_sig_text_2")
                  ), 
                  
                  box(
                    width = 6, 
                    plotOutput("selected_sig_full_2", height = 180)
                  ), 
                  
                  box(
                    width = 2, 
                    plotOutput("selected_sig_2_1", height = 180)
                  )
                  
                ),
                
                
                fluidRow(
                  box(
                    width = 4,             
                    status = "success", solidHeader = TRUE,
                    title = "Most similar COSMIC signature:",
                    uiOutput(paste0('selected_sig_text_2_', 1))
                  ), 
                  
                  box(
                    width = 6, 
                    plotOutput(paste0('selected_sig_full_2_', 1), height = 180)
                  ),
                  
                  box(
                    width = 2
                  )
                ), 
                
                fluidRow(
                  box(
                    width = 4, 
                    title = "Selected COSMIC signature:",
                    status = "warning", solidHeader = TRUE,
                    uiOutput(paste0('selected_sig_text_2_', 2))
                  ), 
                  
                  box(
                    width = 6,
                    plotOutput(paste0('selected_sig_full_2_', 2), height = 180)
                  ), 
                  
                  box(
                    width = 2
                  )
                )
        ),
        
        tabItem(tabName = "page3",
                fluidRow( 
                  box(height = "280px",
                      title = "About input:", status = "warning", solidHeader = TRUE,
                      width = 4, 
                      "Please prepare a CSV file by putting your pmsignature into a column with no quotes.",
                      downloadButton('downloadData', label = "Download a sample CSV file", class = NULL),
                      br(), br(),
                      fileInput("file1", "Choose CSV File",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")),
                      checkboxInput("header1", "Header", TRUE)
                      
                  ), 
                  
                  column(width = 4, 
                    box(width = NULL, height = "280px",
                        title = "COSMIC signature",
                        status = "primary", solidHeader = TRUE,
                        style="font-size:130%",
                        dataTableOutput("fu_table")
                    )
                  ),
                  
                  column(width = 4, 
                     box(width = NULL, height = "280px",
                         title = "pmsignature", 
                         status = "success", solidHeader = TRUE,
                         style="font-size:130%",
                         dataTableOutput("fu_table2")
                    )
                  )
                ),
                

                
                fluidRow(
                  box(width = 4, 
                      title = "Input COSMIC signature", status = "warning", solidHeader = TRUE,
                      plotOutput("similar_full", height = 170)
                  ), 
                  
                  box(width = 4,
                      title = textOutput("fu_text"), 
                      status = "primary", solidHeader = TRUE,
                      plotOutput("fu_plot", height = 170)
                  ), 
                  
                  box(width = 4,
                      title = textOutput("fu_text2"), 
                      status = "success", solidHeader = TRUE,
                      plotOutput("fu_plot2", height = 170)
                  )
                ), 
                
                fluidRow(
                  column(width = 4, offset = 4, 
                         valueBoxOutput(width = NULL, "fu_box")
                  ),
                  column(width = 4,  
                         valueBoxOutput(width = NULL, "fu_box2"))
                ), 
                
                fluidRow(
                  column(width = 4),
                  
                  box(width = 8, title="Cancer membership in both COSMIC and pmsignature",  
                      status = "danger", solidHeader = TRUE,
                         uiOutput(paste0('selected_sig_text_3_', 1))
                  )
                ),
                
                fluidRow(
                  column(width = 4), 
                  
                  box(width = 4, title="Cancer membership only in COSMIC", status = "primary", 
                      solidHeader = TRUE,
                      uiOutput(paste0('selected_sig_text_3_', 2))
                  ),
                  
                  box(width = 4, title="Cancer membership only in pmsignature", status = "success", 
                      solidHeader = TRUE,
                      uiOutput(paste0('selected_sig_text_3_', 3))
                  )
                )
        ), 
        
        tabItem(tabName = "page4",
              fluidRow( 
                 box(height = "280px",
                   title = "About input:", status = "warning", solidHeader = TRUE,
                   width = 4, 
                   "Please prepare a CSV file by putting your pmsignature into a matrix with no quotes.",
                   downloadButton('downloadData2', label = "Download a sample CSV file", class = NULL),
                   br(), br(),
                   fileInput("file2", "Choose CSV File",
                             multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   checkboxInput("header2", "Header", TRUE)
                   
                 ), 
                 
                 box(width = 4, height = "280px",
                     title = "pmsignature",
                     status = "primary", solidHeader = TRUE,
                     style="font-size:130%",
                     dataTableOutput("pm_table")
                 ), 
                 
                 box(width = 4, height = "280px",
                     title = "COSMIC signature", 
                     status = "success", solidHeader = TRUE,
                     style="font-size:130%",
                     dataTableOutput("pm_table2")
                 )
              ),
              

                
              fluidRow(
                box(width = 4, 
                    title = "Input a pmsignature", status = "warning", solidHeader = TRUE,
                    plotOutput("similar_pm", height = 200)
                ), 
                
                box(width = 4,
                    title = textOutput("pm_text"), 
                    status = "primary", solidHeader = TRUE,
                    plotOutput("pm_plot", height = 200)
                ), 
                
                box(width = 4,
                    title = textOutput("pm_text2"), 
                    status = "success", solidHeader = TRUE,
                    plotOutput("pm_plot2", height = 200)
                )
              ), 
              
              fluidRow(
                column(width = 4, offset = 4, 
                       valueBoxOutput(width = NULL, "pm_box")
                ),
                valueBoxOutput(width = 4, "pm_box2")
              ), 
              
              fluidRow(
                column(width = 4),
                
                box(width = 8, title="Cancer membership in both COSMIC and pmsignature",  status = "danger", solidHeader = TRUE,
                    uiOutput(paste0('selected_sig_text_4_', 1))
                )
              ),
              
              fluidRow(
                column(width = 4), 
                
                box(width = 4, title="Cancer membership only in pmsignature", status = "primary", solidHeader = TRUE,
                    uiOutput(paste0('selected_sig_text_4_', 2))
                ),
                
                box(width = 4, title="Cancer membership only in COSMIC", status = "success", solidHeader = TRUE,
                    uiOutput(paste0('selected_sig_text_4_', 3))
                )
              )
            ),
        
        tabItem(tabName = "page5",
                
                # box(width = 4, title = "The iMutSig R pacakage",
                #     h3(textOutput("caption"))
                # ), 
                # 
                # box(width = 4, title = "Publication",
                #     h3(textOutput("caption"))
                # ), 
                # 
                # box(width = 4, title = "Contact Author",
                #     h3(textOutput("caption"))
                # ),
                
                fluidRow(
                box(width = 4, title = "About iMutSig", status = "danger", solidHeader = TRUE,
                    tags$h3("This Shiny app is dependent on:"),
                    h4("- Catalogue of Somatic Mutations in Cancer (COSMIC)."),
                    h4("- pmsignature developed by Shiraishi et al."),
                    br(),
                    actionButton("about1",
                                 label = "COSMIC",
                                 icon = icon("laptop"),
                                 onclick = sprintf("window.open('%s')", "https://cancer.sanger.ac.uk/cosmic")),
                    actionButton("about2",
                                 label = "pmsignature",
                                 icon = icon("github"),
                                 onclick = sprintf("window.open('%s')", "https://github.com/friend1ws/pmsignature"))
                ),                
                
                box(width = 4, title = "R Package", status = "primary", solidHeader = TRUE,
                    tags$h3("On the Github page, you can: "),
                    h4("- install the iMutSig R pacakge and run it locally."),
                    h4("- report any issues and leave suggestions."),
                    br(),
                    actionButton("R1",
                                 label = "Download",
                                 icon = icon("github"),
                                 onclick = sprintf("window.open('%s')", "http://www.github.com/USCbiostats/iMutSig")),
                    actionButton("R2",
                                 label = "Issues",
                                 icon = icon("bug"),
                                 onclick = sprintf("window.open('%s')", "http://www.github.com/USCbiostats/iMutSig/issues"))                    
                    ),
                
                box(width = 4, title = "Share", status = "success", solidHeader = TRUE,
                    tags$h3("We're glad that you found iMutSig helpful."),
                    h4("- please tweet about us."),
                    h4("- please cite our paper."),
                    br(),
                    actionButton("share1",
                                 label = "Share",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url_share)), 
                    actionButton("share2",
                                 label = "Cite",
                                 icon = icon("edit"),
                                 onclick = sprintf("window.open('%s')", url_cite))
                    )
              )
            )        
        
        ) 
      
      ),
    
    
    dashboardPage(header, sidebar, body)
    
)

