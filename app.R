#app.R

library(shiny)
library(shinydashboard)
library(rsconnect)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(UpSetR)
library(shinyjs)
source("Cauti_fakedata_backend1.R")
source("org_plot_backend1.R")
source("resist_plot_backend.R")
source('sensit_plot_backend.R')

CSF_a <- read.csv('CSF_analytic_FAKE.csv', stringsAsFactors = FALSE)
locations <- unique(CSF_a$Loc_Code)

#lists for specific dropdowns per specific page
risk_factors <- c("Transferred", "Antibio", "Chronic_Dis", "Bedridden",
                  "Incontinent", "Immunocomp", "Infection", "Uro_Surgery",
                  "Neonate")
org_factors <- c("KLEB","PAER","ESCH","ENTC","GNRS","CAND","ACIN","ENTB","ENTC-F",
                 "ENTB-SERR","MORG","CITRO","PROT","SAL-PB","PROV","ENTB-A","KLEB-OXY" )

resist_factors <- c("AMIK","AMOX_CLAV", "AMP", "AMP_SUL", "AZTREON", "CEFEP", "CEFIX","CEFTOX", "CEFTAZ", "CEFTRI",
                    "CEFUR","CEFUR_AXE", "CEFUR_SOD", "CEPH", "CIP", "COLISTIN", "ERTA", "GEN" ,"IMI", "LEVO", "MERO", "NIT",
                    "PIP_TAZ", "TRIM_SULFA", "TIG", "VANC")

sensit_factors <- c("AMIK","AMOX_CLAV", "AMP","CEFEP","CEFTAZ","CEFTRI","CEFUR", "CEFUR_AXE","CEPH", "CIP",
                    "COLISTIN","ERTA", "GEN","LEVO", "LNZ","MERO","NIT","PIP_TAZ" ,"TRIM_SULFA", "TIG")

#for dropdowns to work across 4 plots pages

location_picker <- pickerInput("location", label = 'Locations', choices = locations, width = NULL,
                              options = list( `actions-box` = TRUE, size = 10,
                                              `selected-text-format` = "count > 3"),
                              multiple = TRUE,selected = locations)

date_picker <- dateRangeInput('date', label = 'Date range', width = NULL,
                              start = start_date_window, end = end_date_window)

# pickers for the specific pages
risk_picker <- pickerInput("risk", "Risk Factors:", choices = risk_factors,
                           options = list( `actions-box` = TRUE, size = 10,
                                           `selected-text-format` = "count > 3"),  
                           multiple = TRUE,selected = risk_factors)
org_picker <- pickerInput("org", "Organisms:", choices = org_factors, 
                          options = list( `actions-box` = TRUE, size = 10,
                                          `selected-text-format` = "count > 3"),  
                          multiple = TRUE,selected = org_factors)
resist_picker <- pickerInput("resist", "Resistance rates to antibiotics:", choices = resist_factors, 
                             options = list( `actions-box` = TRUE, size = 10,
                                             `selected-text-format` = "count > 3"),  
                             multiple = TRUE,selected = resist_factors)
sensit_picker <- pickerInput("sensit", "Sensitivity for antibiotics:",
                             choices = sensit_factors, 
                             options = list(`actions-box` = TRUE, size = 10,
                                            `selected-text-format` = "count > 3"),
                             multiple = TRUE, selected = sensit_factors)

# list for compare plots dropdowns
plot_choices <- c('Risk Factors', 'Organisms', 'Antibiotic Resistance', 'Antibiotic Sensitivity')

# compare 2 plots on compare page
comp_plot1_picker <- pickerInput("plot6", "Plot 1:", choices = plot_choices, 
                                 options = list( `actions-box` = TRUE, size = 10,
                                                 `selected-text-format` = "count > 3"),  
                                 multiple = FALSE,selected = 'Risk Factors')
comp_plot2_picker <- pickerInput("plot7", "Plot 2:", choices = plot_choices, 
                                 options = list( `actions-box` = TRUE, size = 10,
                                                 `selected-text-format` = "count > 3"),  
                                 multiple = FALSE,selected = 'Organisms')

# to manipulate and hide the top dropdowns
top_drdwns <- fluidRow(box(width = NULL, 
                           column(width = 6, status = "primary", location_picker),
                           column(width = 6, offset =0, date_picker)
                           # column(p(strong("")), br(), width = 2, action_btn))
))

# pages layout, page_1 - loading page LP
page_2 <- fluidRow(box(width = NULL, column(width = 8, 
                  fluidPage(fluidRow(width=NULL, plotOutput('plot2')))),
                  column(width = 4, risk_picker))
)

page_3 <- fluidRow(
  box(width = NULL, footer = actionLink("pdf", "Organisms abbreviations", target = '_blank', class = 'my_a', onclick = "window.open('Pathogen List.pdf')"), 
      column(width = 8,
           fluidPage(fluidRow(width=NULL, plotOutput('plot3')
                       ))),
                       column(width = 4,org_picker 
                       # downloadLink('abbr', "Abbreviations")
                       )
                       )
)

page_4 <- fluidRow(box(width = NULL, 
                       footer = actionLink("pdf", "Antibiotics abbreviations", target = '_blank', 
                                class = 'my_a', onclick = "window.open('Drugs List.pdf')"),
                       column(width = 8, 
                              fluidPage(fluidRow(width=NULL, plotOutput('plot4')))),
                       column(width = 4, resist_picker))
)

page_5 <- fluidRow(box(width = NULL,
                       footer = actionLink("pdf", "Antibiotics abbreviations", target = '_blank', 
                                class = 'my_a', onclick = "window.open('Drugs List.pdf')"),
                       column(width = 8, 
                              fluidPage(fluidRow(width=NULL, plotOutput('plot5')))),
                       column(width = 4, sensit_picker))
)

#page for compare plots
page_6 <- fluidRow(box(width = NULL,
                       column(width = 6,
                              box(width=NULL, comp_plot1_picker),
                              fluidRow(plotOutput('plot6'))),
                       column(width = 6,
                              box(width=NULL,comp_plot2_picker),
                              fluidRow(plotOutput('plot7')))))
#LP styling
LP <- modalDialog(
  title = "CAUTI Surveillance",
  h4(strong(p('This dashboard represents a prototype for visualizing data resulting from a catheter-acquired urinary tract infection (CAUTI) surveillance program at a hospital system.'))),
  tags$div(tags$ul(tags$li('This prototype is based on a', strong('fictitious'), 'hospital system in the midwestern United States in the imaginary city of Holiday. 
                           It uses fabricated data. No patient data is included.'),
                   tags$li('The dashboard visualizes data from four locations: Holiday General Hospital (HGH), Anderson-Walker Hospital (AWK), Holiday Heart Hospital (HH), and Holiday Cancer Center (CC). 
                           HGH and AWK are general hospitals, and HH and CC are specialty hospitals.'),
                   
                   tags$li('Data are assumed to be collected on patients who suffer from CAUTI. 
                           Data about risk factors, infecting organisms, and whether or not the organisms 
                           are sensitive or resistant to a variety of antibiotic drugs are included in the system.'),
                   
                   tags$li('For more information on CAUTI surveillance', a(href='https://www.cdc.gov/hai/ca_uti/uti.html', 'see here.' ),'
                           '))))
#instructions - pop up window
info_modal <- modalDialog(title = "CAUTI Dashboard Instructions",
                          br(),
                        tags$div(tags$ul(tags$li('Choose the location(s) and date range from the dropdown boxes at the top of the dashboard to limit the data being visualized.'),
                        tags$li('The first tab has the "Risk Factor Plot". You may limit which risk factors are visualized using the dropdown box on the right.'), 
                        tags$li('Click on the "Organism Plot" tab to visualize bacterial cells identified in patients. You can use the dropdown on the right to limit the organisms visualized.'),
                        tags$li('Click on the "Resistance Plot" and "Sensitivity Plot" tabs to visualize drug resistance and sensitivities. The dropdowns on the right can be changed to limit the drugs visualized.'),
                        tags$li('On the "Compare Plots" tab, you can choose two of the plots you created to compare. If you adjust the locations and date ranges, the plots will adjust automatically.'
))))

#contact - pop up window
contact_modal <- modalDialog(title = "Contact Information",
h4(strong(p('This dashboard is a project by DethWench Professional Services (DPS). Visit us at', a(href = 'http://www.dethwench.com/', 'www.dethwench.com'))),
br(), 
tags$div(tags$ul(tags$li('For information regarding the design of the 
surveillance system and epidemiologic aspects of the project, 
please contact DPS President Monika Wahi at', a(href = 'mailto: dethwench@gmail.com','dethwench@gmail.com' )),
br(),
 tags$li('For information regarding the design and execution of the dashboard, 
please contact Natasha Dukach, DPS Reports/Dashboard Developer, at', a(href ='mailto:dukach.nat@gmail.com ', 
               'dukach.nat@gmail.com '))))))

# add UI styling sheet CSS
ui <- dashboardPage(
  header <- dashboardHeader(title='CAUTI Surveillance',  
                            tags$li(class = "dropdown", actionButton('about', "About", class = 'my_btn')),
                            tags$li(class = "dropdown", actionButton('help', "Instructions", class = 'my_btn')), 
                            tags$li(class = "dropdown", actionButton('contact', "Contact", class = 'my_btn'))),
  dashboardSidebar(
    sidebarMenu(id='sidemenu',
                #plot 1
                menuItem("Patients Risk Factors",tabName = 'risk', icon = icon (NULL)),
                #plot 2
                menuItem("Detected Microorganisms", tabName = 'org', icon = icon(NULL)),
                #plot 3
                menuItem("Antibiotic Resistance", tabName = 'resist', icon = icon(NULL)),
                #plot 4
                menuItem("Antibiotic Sensitivity", tabName = 'sensit', icon = icon(NULL)),
                # Compare plot
                menuItem("Compare Plots", tabName = 'compare', icon = icon(NULL))
    )
  ), 
  dashboardBody(includeCSS('www/style.css'),
                top_drdwns,
                tabItems(
                  tabItem(tabName = 'risk', page_2),
                  tabItem(tabName = 'org', page_3),
                  tabItem(tabName = 'resist', page_4),
                  tabItem(tabName = 'sensit', page_5),
                  tabItem(tabName = 'compare', page_6)
                )
  )
)

server <- function(input, output,session){
  #loading page-no top dropdowns
  observeEvent(input$sidemeu =='risk',eventExpr = page_2) 
  showModal(LP)
  observeEvent(input$help, {
    showModal(info_modal)
  }, ignoreInit = TRUE)
  observeEvent(input$about, {
    showModal(LP)
  }, ignoreInit = TRUE)
  observeEvent(input$contact, {
    showModal(contact_modal)
  }, ignoreInit = TRUE)
  
  # output$abbr <- downloadHandler(
  #   filename = "abbrev_pathogens.csv",
  #   content = function(file) {
  #     file.copy("abbrev_pathogens.csv", file, overwrite = TRUE)
  #   })  
  
  #tab risks: regular filterering by location
  output$plot2 <- renderPlot(get_data(input$location, input$risk, input$date[1], input$date[2]))
  
  #tab orgs: observe a number of available organisms filtered by location and dates
  observeEvent(input$location,{
    current_org <- available_organism(input$location)
    updatePickerInput(session = session, inputId = "org", 
                      choices = current_org, selected = current_org)
    output$plot3 <- renderPlot(plot_organism(input$location, current_org, input$date[1], input$date[2]))})
  observeEvent(input$org, {
    output$plot3 <- renderPlot(plot_organism(input$location, input$org, input$date[1], input$date[2]))})
  
  #tab resistance: observe a number of available drugs to which organisms are resistant filtered by location and date
  observeEvent(input$location,{
    current_resist <- available_drugs(input$location)
    updatePickerInput(session = session, inputId = "resist", 
                      choices = current_resist, selected = current_resist)
    output$plot4 <- renderPlot(resist_plot(input$location, current_resist, input$date[1], input$date[2]))})
  observeEvent(input$resist, {
    output$plot4 <- renderPlot(resist_plot(input$location, input$resist, input$date[1], input$date[2]))})
  
  #tab suseptibility: observe a number of available drugs to which the organisms are susptible filtered by location and date
  observeEvent(input$location,{
    current_sensit <- available_drugs_S(input$location)
    updatePickerInput(session = session, inputId = "sensit", 
                      choices = current_sensit, selected = current_sensit)
    output$plot5 <- renderPlot(sensit_plot(input$location, current_sensit, input$date[1], input$date[2]))})
  observeEvent(input$sensit, {
    output$plot5 <- renderPlot(sensit_plot(input$location, input$sensit, input$date[1], input$date[2]))})
  
  #tab compare plots: Plot1 
  output$plot6 <- renderPlot(
    if (input$plot6 == 'Risk Factors'){get_data(input$location, input$risk, input$date[1], input$date[2])}
    else if (input$plot6 == 'Organisms'){plot_organism(input$location, input$org, input$date[1], input$date[2])}
    else if (input$plot6 == 'Antibiotic Resistance'){resist_plot(input$location, input$resist, input$date[1], input$date[2])}
    else if (input$plot6 == 'Antibiotic Sensitivity'){sensit_plot(input$location, input$sensit, input$date[1], input$date[2])}
  )
  #tab compare plots: Plot2
  output$plot7 <- renderPlot(
    if (input$plot7 == 'Organisms'){plot_organism(input$location, input$org, input$date[1], input$date[2])}
    else if (input$plot7 == 'Risk Factors'){get_data(input$location, input$risk, input$date[1], input$date[2])} 
    else if (input$plot7 == 'Antibiotic Resistance'){resist_plot(input$location, input$resist, input$date[1], input$date[2])}
    else if (input$plot7 == 'Antibiotic Sensitivity'){sensit_plot(input$location, input$sensit, input$date[1], input$date[2])}
  )}

shinyApp(ui, server)