library(shinyjs)
library(shinydashboard)
library(shiny)
library(dplyr)
library(DT)
library(corrplot)
library(stringr)
library(pmsignature)

source("Background.R")


ui <- dashboardPage(
  
    header <- dashboardHeader(
      title = "iMutSig: identifying the most similar mutational signature",
      titleWidth = 550,
      dropdownMenuOutput("menu")
    ),
    

    sidebar <- dashboardSidebar(    
      width = 210,
      sidebarMenu(
      id="mysidebar",
      menuItem("COSMIC to pmsignature", icon = icon("chart-bar"), startExpanded = TRUE,
               menuSubItem('v3.1 (latest)', tabName = "page1-v3"),
               menuSubItem('v2', tabName = 'page1-v2')
               ),
      menuItem("pmsignature to COSMIC", icon = icon("cube"), startExpanded = TRUE,
               menuSubItem('v3.1 (latest)', tabName = "page2-v3"),
               menuSubItem('v2', tabName = 'page2-v2')
               ),
      menuItem("Input a signature", icon = icon("sign-in-alt"),  startExpanded = TRUE,
               menuSubItem('COSMIC signature', tabName = 'page3'),
               menuSubItem('pmsignature', tabName = 'page4')
               ), 
      menuItem("Heatmaps", icon = icon("chess-board"), tabName = "page5"),
      menuItem("About iMutSig", icon = icon("info-circle"), tabName = "page6")
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

        .skin-blue .main-header .navbar .sidebar-toggle{
          display:none;
        }
      '))),

      tabItems(
        tabItem(tabName = "page1-v3",
                fluidRow(
                  valueBox(width = 3, color = "black",
                           "Choosing", "a COSMIC v3.1 signature"),
                  
                  box(width = 2, height = "102px",
                      selectInput(inputId = "N_F_v3", label="COSMIC",
                                  choices = colnames(sig_full_v3)[-c(1,2)],
                                  selected = 1) #N_F_v3
                  ),
                  
                  box(width = 2, height = "102px",
                      selectInput(inputId = "method_1_v3", label="Method",
                                  choices = c("Expand", "Collapse"),
                                  selected = "Expand")
                  )
                  
                ),
                # 1st row 
                fluidRow(
                  box(
                    width = 5, 
                    title = paste("Chosen a COSMIC v3.1 signature"),
                    plotOutput("selected_sig_1_v3", height = 200)
                  ),
                  
                  box(
                    width = 7, 
                    title = "Its membership among 37 cancer types", 
                    plotOutput("corrplot1_2_v3", height = 200)
                  )
                ),
                
                # 2nd row 
                fluidRow(
                  box(width = 4,
                      style="font-size:120%",
                      dataTableOutput("mytable1_v3")
                  ),
                  
                  box(width = 8, 
                      title = "Its cosine similarity to the pmsignatures",
                      plotOutput("corrplot1_1_v3", height = 180))
                ),
                
                fluidRow(
                  valueBox(width = 2, color = "yellow",
                           "Choose", "a 2nd COSMIC v3.1 signature"),
                  
                  box(width = 2, height = "102px",
                      selectInput(inputId = "N_S_D_v3", label="pmsignature",
                                  choices = paste0("P",1:27),
                                  selected = 3) #N_S_D_v3
                  ),
                  
                  valueBoxOutput(width = 2,
                                 "selected1_v3_1"),
                  
                  valueBoxOutput(width = 2,
                                 "highest_v3"),
                  
                  valueBoxOutput(width = 2,
                                 "selected1_v3_2")
                  
                ),
                
                # 3nd row 
                fluidRow(
                  box( 
                    width = 4,
                    title = "COSMIC signature:",
                    status = "primary", solidHeader = TRUE,
                    uiOutput("selected_sig_text_1_v3")
                  ),
                  
                  box(
                    width = 6,
                    plotOutput("selected_sig_full_1_v3", height = 180)
                  ),
                  
                  box(
                    width = 2,
                    plotOutput("selected_sig_full_pm_1_v3", height = 180)
                  )
                  
                ),
                
                fluidRow(
                  box(
                    width = 4,             
                    status = "success", solidHeader = TRUE,
                    title = "Most similar pmsignature:",
                    uiOutput(paste0('selected_sig_text_1_v3_', 1))
                  ), 
                  
                  box(
                    width = 6, 
                    plotOutput(paste0('selected_sig_pm_full_1_v3_', 1), height = 180)
                  ),
                  
                  box(
                    width = 2,                                 
                    plotOutput(paste0('selected_sig_full_1_v3_', 1), height = 180)
                  )
                ), 
                
                fluidRow(
                  box(
                    width = 4, 
                    title = "Selected pmsignature:",
                    status = "warning", solidHeader = TRUE,
                    uiOutput(paste0('selected_sig_text_1_v3_', 2))
                  ), 
                  
                  box(
                    width = 6,
                    plotOutput(paste0('selected_sig_pm_full_1_v3_', 2), height = 180)
                  ),
                  
                  box(
                    width = 2,                                 
                    plotOutput(paste0('selected_sig_full_1_v3_', 2), height = 180)
                  )
                )
        ),
        tabItem(tabName = "page1-v2",
                # 1st row 
                fluidRow(
                  valueBox(width = 3, color = "black",
                           "Choosing", "a COSMIC v2 signature"),
                  
                  box(width = 2, height = "102px",
                      selectInput(inputId = "N_F_v2", label="COSMIC",
                                  choices = paste0("C",1:30),
                                  selected = 1) #N_F_v2
                  ),
                  
                  box(width = 2, height = "102px",
                      selectInput(inputId = "method_1_v2", label="Method",
                                  choices = c("Expand", "Collapse"),
                                  selected = "Expand")
                  )
                ),
                
                fluidRow(
                  box(
                    width = 5, 
                    title = paste("Chosen a COSMIC v2 signature"),
                    plotOutput("selected_sig_1_v2", height = 200)
                  ),
                  
                  box(
                    width = 7, 
                    title = "Its membership among 40 cancer types", 
                    plotOutput("corrplot1_2_v2", height = 200)
                  )
                ),
                
                # 2nd row 
                fluidRow(
                  box(width = 4,
                      style="font-size:120%",
                      dataTableOutput("mytable1_v2")
                  ),
                  
                  box(width = 8, 
                      title = "Its cosine similarity to the pmsignatures",
                      plotOutput("corrplot1_1_v2", height = 180))
                ),
                
                fluidRow(
                  valueBox(width = 2, color = "yellow",
                           "Choose", "a 2nd COSMIC v2 signature"),
                  
                  box(width = 2, height = "102px",
                      selectInput(inputId = "N_S_D_v2", label="2nd pmsignature",
                                  choices = paste0("P",1:27),
                                  selected = 3) #N_S_D_v2
                  ),
                  
                  valueBoxOutput(width = 2,
                                 "selected1_v2_1"),
                  
                  valueBoxOutput(width = 2,
                                 "highest_v2"),
                  
                  valueBoxOutput(width = 2,
                                 "selected1_v2_2")
                ),
                
                # 3nd row 
                fluidRow(
                  box( 
                    width = 4,
                    title = "COSMIC signature:",
                    status = "primary", solidHeader = TRUE,
                    uiOutput("selected_sig_text_1_v2")
                  ),
                
                  box(
                    width = 6,
                    plotOutput("selected_sig_full_1_v2", height = 180)
                  ),
                  
                  box(
                    width = 2,
                    plotOutput("selected_sig_full_pm_1_v2", height = 180)
                  )

                ),
                
                fluidRow(
                  box(
                    width = 4,             
                    status = "success", solidHeader = TRUE,
                    title = "Most similar pmsignature:",
                    uiOutput(paste0('selected_sig_text_1_v2_', 1))
                  ), 
                  
                  box(
                    width = 6, 
                    plotOutput(paste0('selected_sig_pm_full_1_v2_', 1), height = 180)
                  ),
                  
                  box(
                    width = 2,                                 
                    plotOutput(paste0('selected_sig_full_1_v2_', 1), height = 180)
                  )
                ), 
                
                fluidRow(
                  box(
                    width = 4, 
                    title = "Selected pmsignature:",
                    status = "warning", solidHeader = TRUE,
                    uiOutput(paste0('selected_sig_text_1_v2_', 2))
                  ), 

                  box(
                    width = 6,
                    plotOutput(paste0('selected_sig_pm_full_1_v2_', 2), height = 180)
                  ),
                  
                  box(
                    width = 2,                                 
                    plotOutput(paste0('selected_sig_full_1_v2_', 2), height = 180)
                  )
                )
        ),

        tabItem(tabName = "page2-v3",
                fluidRow(
                  valueBox(width = 3, color = "black",
                           "Choosing", "a pmsignature"),
                  
                  box(width = 1, height = "102px",
                      selectInput(inputId = "N_D_v3", label="",
                                  choices = paste0("P",1:27),
                                  selected = 1)
                      
                  ),
                  
                  box(width = 2, height = "102px",
                      selectInput(inputId = "method_2_v3", label="Method",
                                  choices = c("Expand", "Collapse"),
                                  selected = "Expand")
                  )
                ),
                
                # First tab content
                fluidRow(
                  box(
                    width = 4,
                    title = paste("Chosen pmsignature"),
                    plotOutput("selected_sig_2_v3", height = 200)
                  ),
                  
                  box(
                    width = 8, 
                    title = "Its membership among 30 cancer types",
                    plotOutput("corrplot2_v3_1", height = 200)
                  )
                  
                ),
                
                # 2nd Row 
                
                fluidRow(
                  box(width = 4,
                      style="font-size:120%",
                      dataTableOutput("mytable2_v3")
                  ),
                  
                  box(width = 8, 
                      title = "Its cosine similarity to the COSMIC signatures",
                      plotOutput("corrplot2_v3_2", height = 200))
                ),
                
                # 3rd Row
                
                fluidRow(
                  valueBox(width = 2, color = "yellow",
                           "Choose", "a 2nd pmsignature"),
                  
                  box(width = 2, height = "102px",
                      selectInput(inputId = "N_D_S_v3", label="COSMIC",
                                  choices = colnames(sig_full_v3)[-c(1,2)],
                                  selected = 3)),
                      
                  valueBoxOutput(width = 2,
                                 "selected2_v3_1"),
                  
                  valueBoxOutput(width = 2,
                                 "highest2_v3"),
                  
                  valueBoxOutput(width = 2,
                                 "selected2_v3_2")
                ),
                
                fluidRow(
                  box(
                    width = 4,
                    title = "pmsignature in COSMIC signature style:",
                    status = "primary", solidHeader = TRUE,
                    uiOutput("selected_sig_text_v3_2")
                  ), 
                  
                  box(
                    width = 6, 
                    plotOutput("selected_sig_full_v3_2", height = 180)
                  ), 
                  
                  box(
                    width = 2, 
                    plotOutput("selected_sig_2_v3_1", height = 180)
                  )
                  
                ),
                
                
                fluidRow(
                  box(
                    width = 4,             
                    status = "success", solidHeader = TRUE,
                    title = "Most similar COSMIC signature:",
                    uiOutput(paste0('selected_sig_text_2_v3_', 1))
                  ), 
                  
                  box(
                    width = 6, 
                    plotOutput(paste0('selected_sig_full_2_v3_', 1), height = 180)
                  ),
                  
                  box(
                    width = 2,
                    plotOutput(paste0('selected_sig_full_pm_2_v3_', 1), height = 180)
                  )
                ), 
                
                fluidRow(
                  box(
                    width = 4, 
                    title = "Selected COSMIC signature:",
                    status = "warning", solidHeader = TRUE,
                    uiOutput(paste0('selected_sig_text_2_v3_', 2))
                  ), 
                  
                  box(
                    width = 6,
                    plotOutput(paste0('selected_sig_full_2_v3_', 2), height = 180)
                  ), 
                  
                  box(
                    width = 2,
                    plotOutput(paste0('selected_sig_full_pm_2_v3_', 2), height = 180)
                  )
                )
        ),

        tabItem(tabName = "page2-v2",
                fluidRow(
                  valueBox(width = 3, color = "black",
                           "Choosing", "a pmsignature"),
                  
                  box(width = 1, height = "102px",
                      selectInput(inputId = "N_D_v2", label="",
                                  choices = paste0("P",1:27),
                                  selected = 1)
                      
                  ),
                  
                  box(width = 2, height = "102px",
                      selectInput(inputId = "method_2_v2", label="Method",
                                  choices = c("Expand", "Collapse"),
                                  selected = "Expand")
                  )
                  
                ),
                
                # First tab content
                fluidRow(
                  box(
                    width = 4,
                    title = paste("Chosen pmsignature"),
                    plotOutput("selected_sig_2_v2", height = 200)
                  ),
                  
                  box(
                    width = 8, 
                    title = "Its membership among 30 cancer types",
                    plotOutput("corrplot2_v2_1", height = 200)
                  )
                  
                ),
                
                # 2nd Row 
                
                fluidRow(
                  box(width = 4,
                      style="font-size:120%",
                      dataTableOutput("mytable2_v2")
                  ),
                  
                  box(width = 8, 
                      title = "Its cosine similarity to the COSMIC signatures",
                      plotOutput("corrplot2_v2_2", height = 200))
                ),
                
                # 3rd Row
                
                fluidRow(
                  valueBox(width = 2, color = "yellow",
                           "Choose", "a 2nd pmsignature"),
                  
                  box(width = 2, height = "102px",
                      selectInput(inputId = "N_D_S_v2", label="COSMIC",
                                  choices = paste0("C",1:30),
                                  selected = 3)),
                  
                  valueBoxOutput(width = 2,
                                 "selected2_v2_1"),
                
                  valueBoxOutput(width = 2,
                                 "highest2_v2"),
                  
                  valueBoxOutput(width = 2,
                                 "selected2_v2_2")

                ),
              
                fluidRow(
                  box(
                    width = 4,
                    title = "pmsignature in COSMIC signature style:",
                    status = "primary", solidHeader = TRUE,
                    uiOutput("selected_sig_text_v2_2")
                  ), 
                  
                  box(
                    width = 6, 
                    plotOutput("selected_sig_full_v2_2", height = 180)
                  ), 
                  
                  box(
                    width = 2, 
                    plotOutput("selected_sig_2_v2_1", height = 180)
                  )
                  
                ),
                
                
                fluidRow(
                  box(
                    width = 4,             
                    status = "success", solidHeader = TRUE,
                    title = "Most similar COSMIC signature:",
                    uiOutput(paste0('selected_sig_text_2_v2_', 1))
                  ), 
                  
                  box(
                    width = 6, 
                    plotOutput(paste0('selected_sig_full_2_v2_', 1), height = 180)
                  ),
                  
                  box(
                    width = 2,
                    plotOutput(paste0('selected_sig_full_pm_2_v2_', 1), height = 180)
                  )
                ), 
                
                fluidRow(
                  box(
                    width = 4, 
                    title = "Selected COSMIC signature:",
                    status = "warning", solidHeader = TRUE,
                    uiOutput(paste0('selected_sig_text_2_v2_', 2))
                  ), 
                  
                  box(
                    width = 6,
                    plotOutput(paste0('selected_sig_full_2_v2_', 2), height = 180)
                  ), 
                  
                  box(
                    width = 2,
                    plotOutput(paste0('selected_sig_full_pm_2_v2_', 2), height = 180)
                  )
                )
        ),
        
        tabItem(tabName = "page3",
                fluidRow( 
                  box(title = "About input:", status = "warning", solidHeader = TRUE,
                      width = 4, 
                      "Please prepare a CSV file by putting your pmsignature into a matrix with no quotes.",
                      downloadButton('downloadData', label = "Download a sample CSV file", class = NULL),
                      br(), br(),
                      fileInput("file1", "Choose CSV File",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")),
                      checkboxInput("header1", "Header", TRUE)
                      
                  ), 
                  
                  box(width = 4, 
                      height = "270px",
                      title = "Input COSMIC signature", status = "warning", solidHeader = TRUE,
                      plotOutput("similar_full", height = 170)
                  )
                ),
                
                fluidRow(
                  column(width = 4, 
                         box(width = NULL, height = "280px",
                             title = "COSMIC v2 signature",
                             status = "primary", solidHeader = TRUE,
                             style="font-size:110%",
                             dataTableOutput("fu_table_v2")
                         )
                  ),
                  
                  column(width = 4, 
                         box(width = NULL, height = "280px",
                             title = "COSMIC v3.1 signature",
                             status = "info", solidHeader = TRUE,
                             style="font-size:110%",
                             dataTableOutput("fu_table_v3")
                         )
                  ),
                  
                  column(width = 4, 
                         box(width = NULL, height = "280px",
                             title = "pmsignature", 
                             status = "success", solidHeader = TRUE,
                             style="font-size:110%",
                             dataTableOutput("fu_table_pm")
                         )
                  )
                ),
                
                fluidRow(
                  box(width = 4, offset = 4, 
                      title = textOutput("fu_text_v2"), 
                      status = "primary", solidHeader = TRUE,
                      plotOutput("fu_plot_v2", height = 170)
                  ), 
                  
                  box(width = 4,
                      title = textOutput("fu_text_v3"), 
                      status = "info", solidHeader = TRUE,
                      plotOutput("fu_plot_v3", height = 170)
                  ),
                  
                  box(width = 4, offset = 4, 
                      title = textOutput("fu_text_pm"), 
                      status = "success", solidHeader = TRUE,
                      plotOutput("fu_plot_pm", height = 170)
                  )
                ), 
                
                fluidRow(
                  column(width = 4, 
                         valueBoxOutput(width = NULL, "fu_box_v2")
                  ),
                  column(width = 4,  
                         valueBoxOutput(width = NULL, "fu_box_v3")
                  ),
                  column(width = 4,  
                         valueBoxOutput(width = NULL, "fu_box_pm")
                  )
                ), 
                
                fluidRow(
                  box(width = 4, title="Cancer membership in COSMIC v2", status = "primary", 
                      solidHeader = TRUE,
                      uiOutput("selected_sig_text_fu_v2")
                  ),
                  
                  box(width = 4, title="Cancer membership in COSMIC v3", status = "info", 
                      solidHeader = TRUE,
                      uiOutput("selected_sig_text_fu_v3")
                  ),
                  
                  box(width = 4, title="Cancer membership in pmsignature", status = "success", 
                      solidHeader = TRUE,
                      uiOutput("selected_sig_text_fu_pm")
                  )
                )
        ), 
        
        tabItem(tabName = "page4",
              fluidRow( 
                 box(title = "About input:", status = "warning", solidHeader = TRUE,
                   width = 4, 
                   "Please prepare a CSV file by putting your COSMIC signatures into a column with no quotes.",
                   downloadButton('downloadData2', label = "Download a sample CSV file", class = NULL),
                   br(), br(),
                   fileInput("file2", "Choose CSV File",
                             multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   checkboxInput("header2", "Header", TRUE)
                   
                 ), 
                 
                 box(width = 4, 
                     height = "270px",
                     title = "Input a pmsignature", status = "warning", solidHeader = TRUE,
                     plotOutput("similar_pm", height = 200)
                 )
    
              ),
              
                
              fluidRow(
                box(width = 4, height = "280px",
                    title = "COSMIC v2 signature", 
                    status = "primary", solidHeader = TRUE,
                    style="font-size:110%",
                    dataTableOutput("pm_table_v2")
                ),
                
                box(width = 4, height = "280px",
                    title = "COSMIC v3.1 signature", 
                    status = "info", solidHeader = TRUE,
                    style="font-size:110%",
                    dataTableOutput("pm_table_v3")
                ),
                
                box(width = 4, height = "280px",
                    title = "pmsignature",
                    status = "success", solidHeader = TRUE,
                    style="font-size:110%",
                    dataTableOutput("pm_table_pm")
                )
              ), 
              
              fluidRow(
                box(width = 4,
                    title = textOutput("pm_text_v2"), 
                    status = "primary", solidHeader = TRUE,
                    plotOutput("pm_plot_v2", height = 200)
                ), 
                
                box(width = 4,
                    title = textOutput("pm_text_v3"), 
                    status = "info", solidHeader = TRUE,
                    plotOutput("pm_plot_v3", height = 200)
                ),
                
                box(width = 4,
                    title = textOutput("pm_text_pm"), 
                    status = "success", solidHeader = TRUE,
                    plotOutput("pm_plot_pm", height = 200)
                )
              ),
              
              fluidRow(
                column(width = 4, 
                       valueBoxOutput(width = NULL, "pm_box_v2")
                ),
                
                column(width = 4, 
                       valueBoxOutput(width = NULL, "pm_box_v3")
                ),
                
                column(width = 4, 
                       valueBoxOutput(width = NULL, "pm_box_pm")
                )
              ), 
              
              fluidRow(
                box(width = 4, title="Cancer membership in COSMIC v2", status = "primary", 
                    solidHeader = TRUE,
                    uiOutput("selected_sig_text_pm_v2")
                ),
                
                box(width = 4, title="Cancer membership in COSMIC v3.1", status = "info", 
                    solidHeader = TRUE,
                    uiOutput("selected_sig_text_pm_v3")
                ),
                
                box(width = 4, title="Cancer membership in pmsignature", status = "success", 
                    solidHeader = TRUE,
                    uiOutput("selected_sig_text_pm_pm")
                )
              )
            ),
        
        tabItem(tabName = "page5",
                fluidRow(
                  box(width = 2, height = "102px",
                      selectInput(inputId = "heatmap", label="COSMIC version",
                                  choices = c("v2", "v3.1"),
                                  selected = "v2")
                  ),
                  box(width = 2, height = "102px",
                      selectInput(inputId = "method_5", label="Method",
                                  choices = c("Expand", "Collapse"),
                                  selected = "Expand")
                  )
                ),
                fluidRow(
                  box(width = 12,
                      uiOutput("heatmap_dynamic")
                  )
                )
            ),
        
        tabItem(tabName = "page6",
                
                fluidRow(
                box(width = 4, title = "About iMutSig", status = "danger", solidHeader = TRUE,
                    tags$h3("This Shiny app is dependent on:"),
                    h4("- COSMIC v2, March 2015"),
                    actionButton("about1",
                                 label = "Paper",
                                 icon = icon("book"),
                                 onclick = sprintf("window.open('%s')", "https://www.nature.com/articles/nature12477")),
                    actionButton("about2",
                                 label = "View signatures",
                                 icon = icon("laptop"),
                                 onclick = sprintf("window.open('%s')", "https://cancer.sanger.ac.uk/cosmic/signatures_v2")),
                    actionButton("about3",
                                 label = "Download",
                                 icon = icon("download"),
                                 onclick = sprintf("window.open('%s')", "https://cancer.sanger.ac.uk/cancergenome/assets/signatures_probabilities.txt")),
                    h4("-COSMIC v3.1, June 2020"),
                    actionButton("about4",
                                 label = "Paper",
                                 icon = icon("book"),
                                 onclick = sprintf("window.open('%s')", "https://www.nature.com/articles/s41586-020-1943-3")),
                    actionButton("about5",
                                 label = "View signatures",
                                 icon = icon("laptop"),
                                 onclick = sprintf("window.open('%s')", "https://cancer.sanger.ac.uk/cosmic/signatures/SBS/index.tt")),
                    actionButton("about6",
                                 label = "Download",
                                 icon = icon("download"),
                                 onclick = sprintf("window.open('%s')", "https://cancer.sanger.ac.uk/signatures/COSMIC_Mutational_Signatures_v3.1.xlsx")),
                    
                    h4("- pmsignature developed by Shiraishi et al."),
                    actionButton("about7",
                                 label = "Paper",
                                 icon = icon("book"),
                                 onclick = sprintf("window.open('%s')", "https://journals.plos.org/plosgenetics/article?id=10.1371/journal.pgen.1005657")),
                    actionButton("about8",
                                 label = "View signatures",
                                 icon = icon("laptop"),
                                 onclick = sprintf("window.open('%s')", "https://github.com/friend1ws/pmsignature_paper")),
                    actionButton("about9",
                                 label = "Download",
                                 icon = icon("github"),
                                 onclick = sprintf("window.open('%s')", "https://github.com/friend1ws/pmsignature"))
                ),                
                
                box(width = 4, title = "R Package", status = "primary", solidHeader = TRUE,
                    tags$h3("On the Github page, you can: "),
                    h4("- install the iMutSig R pacakge and run it locally."),
                    actionButton("R1",
                                 label = "Download",
                                 icon = icon("github"),
                                 onclick = sprintf("window.open('%s')", "http://www.github.com/USCbiostats/iMutSig")),
                    h4("- report any issues and leave suggestions."),
                    actionButton("R2",
                                 label = "Issues",
                                 icon = icon("bug"),
                                 onclick = sprintf("window.open('%s')", "http://www.github.com/USCbiostats/iMutSig/issues")),
                    h4("- email the author."),
                    actionButton("R2",
                                 label = "Issues",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", "https://github.com/zhiiiyang"))                   
                    ),
                
                box(width = 4, title = "Share", status = "success", solidHeader = TRUE,
                    tags$h3("We're glad that you found iMutSig helpful."),
                    h4("- please cite our paper."),
                    actionButton("share1",
                                 label = "Cite",
                                 icon = icon("edit"),
                                 onclick = sprintf("window.open('%s')", url_cite)),
                    h4("- please tweet about us."),
                    actionButton("share2",
                                 label = "Share",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url_share)),
                    h4("- know more about us."),
                    actionButton("share3",
                                 label = "Info",
                                 icon = icon("info-circle"),
                                 onclick = sprintf("window.open('%s')", "https://zhiyang.netlify.app/project/cancer/"))                    
                    ) # finish box
               ) # finish flowrow
            ) # finish tabItem        
        ) 
      ),
    
    dashboardPage(header, sidebar, body)
)
