library(shiny)
library(RNifti)
library(shinythemes)
library(shinyWidgets)
library(markdown)
#library(abr2)
library(knitr)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
#library('autoabr')
require("plotly")
require("tidyverse")
require("svDialogs")
require("XML")
require("plyr")
require("ggplot2")
require("gridExtra")
require("pracma")
require("quantmod")
require("tcltk")
require('Bolstad2')
#require('abr2')
#require('autoabr')
require('xlsx')
require('markdown')
require('knitr')
library('shinyjs')



# Define UI
ui <- fluidPage(theme = shinytheme("darkly"),
                list(
                    tags$head(
                        HTML('<link rel="icon" href="sima.gif" 
                type="image/gif" />'))),
                navbarPage(
                    title= "Ωto_abR",
                    tabPanel("Home",
                             sidebarPanel(
                                 tags$img(src="sima.gif",height=100,width=100),
                                 style = "overflow-y:scroll; max-height: 600px; position:relative;",
                                 shinyjs::useShinyjs(),
                                 #actionButton("refresh", "New Patient Analysis",icon("refresh")),
                                 tags$h3("Patient XML/XML's:"),
                                 fileInput("file1", "Choose Patient XML File/Files",
                                           multiple = FALSE,
                                           accept = c("text/xml",
                                                      ".xml")),
                                 
                                 checkboxInput("select", "Would you like to change the Jewett Labels?", FALSE),
                                 
                                 shinyjs::hidden(
                                     div(
                                         id = "cp1",
                                         conditionalPanel(condition = "input.select", 
                                                          pickerInput(
                                                              inputId = "peak_picker", 
                                                              label = "Jewett Peaks", 
                                                              choices = as.numeric(c(1:5)), 
                                                              options = list(
                                                                  `actions-box` = TRUE, 
                                                                  size = 5,
                                                                  `selected-text-format` = "count > 3"
                                                              ), 
                                                              multiple = TRUE
                                                          )
                                         ))),
                                 
                                 shinyjs::hidden(
                                     div(
                                         id = "p1",
                                         conditionalPanel(condition = "input.peak_picker", 
                                                          sliderInput(inputId = "J1",
                                                                      label = "Position of Peak I",  
                                                                      min = 0, max = 7.19486, 
                                                                      step =0.03211991,value = 1.5))
                                     )
                                 ),
                                 shinyjs::hidden(
                                     div(
                                         id = "p2",
                                         conditionalPanel(condition = "input.peak_picker", 
                                                          sliderInput(inputId = "J2",
                                                                      label = "Position of Peak II",  
                                                                      min = 0, max = 7.19486, 
                                                                      step =0.03211991,value = 2.5))
                                     )
                                 ),
                                 
                                 shinyjs::hidden(
                                     div(
                                         id = "p3",
                                         conditionalPanel(condition = "input.peak_picker", 
                                                          sliderInput(inputId = "J3",
                                                                      label = "Position of Peak III",  
                                                                      min = 0, max = 7.19486, 
                                                                      step =0.03211991,value = 3.5))
                                     )
                                 ),
                                 shinyjs::hidden(
                                     div(
                                         id = "p4",
                                         conditionalPanel(condition = "input.peak_picker", 
                                                          sliderInput(inputId = "J4",
                                                                      label = "Position of Peak IV",  
                                                                      min = 0, max = 7.19486, 
                                                                      step =0.03211991,value = 4))
                                     )
                                 ),
                                 shinyjs::hidden(
                                     div(
                                         id = "p5",
                                         conditionalPanel(condition = "input.peak_picker", 
                                                          sliderInput(inputId = "J5",
                                                                      label = "Position of Peak V",  
                                                                      min = 0, max = 7.19486, 
                                                                      step =0.03211991,value = 5))
                                     )
                                 ),
                                 
                                 checkboxInput("select2", "Would you like to change the Jewett Valleys?", FALSE),
                                 
                                 shinyjs::hidden(
                                     div(
                                         id = "cp2",
                                         conditionalPanel(condition = "input.select2", 
                                                          pickerInput(
                                                              inputId = "valley_picker", 
                                                              label = "Jewett Valleys", 
                                                              choices = as.numeric(c(1:5)), 
                                                              options = list(
                                                                  `actions-box` = TRUE, 
                                                                  size = 5,
                                                                  `selected-text-format` = "count > 3"
                                                              ), 
                                                              multiple = TRUE
                                                          )
                                         ))),
                                 shinyjs::hidden(
                                     div(
                                         id = "v1",
                                         conditionalPanel(condition = "input.valley_picker", 
                                                          sliderInput(inputId = "V1",
                                                                      label = "Position of Peak I Valleys ",  
                                                                      min = 0, max = 7.19486, 
                                                                      step =0.03211991,value = c(1,1.7))
                                         )
                                     )),
                                 shinyjs::hidden(
                                     div(
                                         id = "v2",
                                         conditionalPanel(condition = "input.valley_picker", 
                                                          sliderInput(inputId = "V2",
                                                                      label = "Position of Peak II Valleys ",  
                                                                      min = 0, max = 7.19486, 
                                                                      step =0.03211991,value = c(2,2.7))
                                         )
                                     )),
                                 shinyjs::hidden(
                                     div(
                                         id = "v3",
                                         conditionalPanel(condition = "input.valley_picker", 
                                                          sliderInput(inputId = "V3",
                                                                      label = "Position of Peak III Valleys",  
                                                                      min = 0, max = 7.19486, 
                                                                      step =0.03211991,value = c(3.3,3.7))
                                         )
                                     )
                                 ),
                                 shinyjs::hidden(
                                     div(
                                         id = "v4",
                                         conditionalPanel(condition = "input.valley_picker", 
                                                          sliderInput(inputId = "V4",
                                                                      label = "Position of Peak IV Valleys ",  
                                                                      min = 0, max = 7.19486, 
                                                                      step =0.03211991,value = c(4,4.7))
                                         )
                                     )),
                                 shinyjs::hidden(
                                     div(
                                         id = "v5",
                                         conditionalPanel(condition = "input.valley_picker", 
                                                          sliderInput(inputId = "V5",
                                                                      label = "Position of Peak V Valleys",  
                                                                      min = 0, max = 7.19486, 
                                                                      step =0.03211991,value = c(5,5.7))
                                         )
                                     )
                                 )
                                 #actionButton("refresh", "New Patient Analysis",icon("refresh"))
                             ), # sidebarPanel
                             
                             mainPanel(
                                 tabsetPanel(
                                     
                                     tabPanel("ABR Plot",
                                              
                                              plotOutput("plot_valleys")),
                                     tabPanel("ABR Plot - Extended",
                                              
                                              plotOutput("plot_extended"))
                                     
                                     
                                 )), # mainPanel
                             
                             #tags$h4("To analyze a new patient press:"),
                             actionButton("refresh", "New Patient Analysis",icon("refresh"),
                                          style="color: #fff; background-color: #2F2F30; border-color: #2F2F30"),
                             
                    ), # Navbar 1, tabPanel
                    tabPanel("ABR Analysis",sidebarPanel(
                        
                        tags$h4("To download a pdf with all the plots press:"),
                        downloadLink("report", "Download plots",icon("download")),
                        tags$h4("To download a csv with the amplitude & latency analysis press:"),
                        downloadLink("latency_report", "Download Latency Analysis"),                     
                        tags$h4("To download a csv with the area analysis press:"),
                        downloadLink("area_report", "Download Area Analysis"),
                        tags$h4("To analyze a new patient press:"),
                        actionButton("refresh2", "New Patient Analysis", icon("refresh")),
                        
                    ), # sidebarPanel
                    mainPanel(
                        h1("ABR Analysis Plots"),
                        tabsetPanel(
                            tabPanel("ABR Area Analysis",
                                     plotOutput("plot2")),
                            tabPanel("Latency/Intensity Function - Peak I",
                                     plotOutput("plot3")),
                            tabPanel("Latency/Intensity Function - Peak III",
                                     plotOutput("plot4")),
                            tabPanel("Latency/Intensity Function - Peak V",
                                     plotOutput("plot5"))
                            
                        )
                        
                    )
                    ),
                    tabPanel("About", includeMarkdown("about_us.Rmd"))
                    
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output, session) {
    
    # observer for selection of Jewett Peaks change by user
    observe({
        if (input$select == FALSE){
            shinyjs::hide("cp1")
        }else {
            shinyjs::show("cp1")
        }
    })
    
    # observer for selection of Jewett Valleys change by user
    observe({
        if (input$select2 == FALSE){
            shinyjs::hide("cp2")
        }else {
            shinyjs::show("cp2")
            
        }
    })
    
    # observe for slider of J1
    observe({
        if (length(grep(1,input$peak_picker))>0 & input$select == TRUE){
            shinyjs::show("p1")
            
            
        }else {
            shinyjs::hide("p1")
            
        }
        
    })
    
    # observe for slider of J2
    observe({
        if (length(grep(2,input$peak_picker))>0 & input$select == TRUE){
            shinyjs::show("p2")
        }else {
            shinyjs::hide("p2")
        }
        
    })
    
    # observe for slider of J3
    observe({
        if (length(grep(3,input$peak_picker)) >0 & input$select == TRUE){
            shinyjs::show("p3")
        }else {
            shinyjs::hide("p3")
        }
    })
    
    # observe for slider of J4
    observe({
        if (length(grep(4,input$peak_picker)) >0 & input$select == TRUE){
            shinyjs::show("p4")
        }else {
            shinyjs::hide("p4")
        }
        
    })
    
    # observe for slider of J5
    observe({
        if (length(grep(5,input$peak_picker)) >0 & input$select == TRUE){
            shinyjs::show("p5")
        }else {
            shinyjs::hide("p5")
        }
    })
    
    # observe for slider of v1
    observe({
        if (length(grep(1,input$valley_picker)) >0 & input$select2 == TRUE){
            shinyjs::show("v1")
        }else {
            shinyjs::hide("v1")
        }
    })
    
    # observe for slider of v2
    observe({
        if (length(grep(2,input$valley_picker)) >0 & input$select2 == TRUE){
            shinyjs::show("v2")
        }else {
            shinyjs::hide("v2")
        }
    })
    
    # observe for slider of v3
    observe({
        if (length(grep(3,input$valley_picker)) >0 & input$select2 == TRUE){
            shinyjs::show("v3")
        }else {
            shinyjs::hide("v3")
        }
    })
    
    # observe for slider of v4
    observe({
        if (length(grep(4,input$valley_picker)) >0 & input$select2 == TRUE){
            shinyjs::show("v4")
        }else {
            shinyjs::hide("v4")
        }
    })
    
    # observe for slider of v5
    observe({
        if (length(grep(5,input$valley_picker)) >0 & input$select2 == TRUE){
            shinyjs::show("v5")
        }else {
            shinyjs::hide("v5")
        }
    })
    
    # observer for App Refresh
    observeEvent(input$refresh, {
        session$reload()
    })
    # observer for App Refresh
    observeEvent(input$refresh2, {
        session$reload()
    })
    
    
    # ABR plot with Valleys and Peaks
    output$plot_valleys <- renderPlot({
        inFile <- input$file1$datapath
        valleys_changed <-c()
        if (is.null(inFile)){
            def<-plot(1, type="n", xlim=c(60,110), ylim=c(0,10),
                      main = "This is where the Patient's ABR will be plotted",
                      panel.first = grid(),
                      ylab="Amplitude(μV)", xlab="Time (ms)",xaxt ='n',yaxt='n')
            
            return(def)
        }
        
        abr_test(inFile)
        file<-as.character(input$file1$name)
        pat_id<-strsplit(file[1],".xml")
        
        assign("pat_id",pat_id,envir = .GlobalEnv)
        if (input$select == FALSE & input$select2 == FALSE) {
            
            valleys_list[[1]]<-c(1,valleys_list[[1]],225)
            plot
            points(samples[valleys_list[[1]]],avg_list[[1]][valleys_list[[1]]],pch = 4)
            
        }
        
        else if (input$select == FALSE & input$select2 == TRUE){
            #plot_without
            plot
            if (length(grep(1,input$valley_picker)) >0){
                points(input$V1,avg_list[[1]][(input$V1/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V1)
            }
            if (length(grep(2,input$valley_picker)) >0){
                points(input$V2,avg_list[[1]][(input$V2/0.03211991)+1],pch = 4)
                valleys_changed<-c(valleys_changed,input$V2)
            }
            if (length(grep(3,input$valley_picker)) >0){
                points(input$V3,avg_list[[1]][(input$V3/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V3)
            }
            if (length(grep(4,input$valley_picker)) >0){
                points(input$V4,avg_list[[1]][(input$V4/0.03211991)+1],pch = 4)
                valleys_changed<-c(valleys_changed,input$V4)
            }
            if (length(grep(5,input$valley_picker)) >0){
                points(input$V5,avg_list[[1]][(input$V5/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V5)
            }
            
            
            #valleys_changed <-c(input$V1,input$V2,input$V3,input$V4,input$V5)
            
        }
        else if (input$select == TRUE & input$select2 == FALSE){
            valleys_list[[1]]<-c(1,valleys_list[[1]],225)
            par(xpd=TRUE)
            plot(samples[1:225],avg_list[[1]][1:225],type='l',yaxt='n',main = paste(volumes[1], 'nHL'), 
                 col='darkgreen',panel.first = grid(),
                 xlab="Time (ms)", ylab="Amplitude (μV)")
            
            if (length(grep(1,input$peak_picker)) >0){
                points(input$J1,avg_list[[1]][(input$J1/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J1,avg_list[[1]][(input$J1/0.03211991)+1],labels = as.roman(1), pos = 3)
            }
            if (length(grep(2,input$peak_picker)) >0){
                points(input$J2,avg_list[[1]][(input$J2/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J2,avg_list[[1]][(input$J2/0.03211991)+1],labels = as.roman(2), pos = 3)
            }
            if (length(grep(3,input$peak_picker)) >0){
                points(input$J3,avg_list[[1]][(input$J3/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J3,avg_list[[1]][(input$J3/0.03211991)+1],labels = as.roman(3), pos = 3)
            }
            if (length(grep(4,input$peak_picker)) >0){
                points(input$J4,avg_list[[1]][(input$J4/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J4,avg_list[[1]][(input$J4/0.03211991)+1],labels = as.roman(4), pos = 3)
            }
            if (length(grep(5,input$peak_picker)) >0){
                points(input$J5,avg_list[[1]][(input$J5/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J5,avg_list[[1]][(input$J5/0.03211991)+1],labels = as.roman(5), pos = 3)
            }
            
            points(samples[valleys_list[[1]]],avg_list[[1]][valleys_list[[1]]],pch = 4)
            
            
        }
        else {
            par(xpd=TRUE)
            plot(samples[1:225],avg_list[[1]][1:225],type='l',yaxt='n',main = paste(volumes[1], 'nHL'), 
                 col='darkgreen',panel.first = grid(),
                 xlab="Time (ms)", ylab="Amplitude (μV)")
            if (length(grep(1,input$peak_picker)) >0){
                points(input$J1,avg_list[[1]][(input$J1/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J1,avg_list[[1]][(input$J1/0.03211991)+1],labels = as.roman(1), pos = 3)
            }
            if (length(grep(2,input$peak_picker)) >0){
                points(input$J2,avg_list[[1]][(input$J2/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J2,avg_list[[1]][(input$J2/0.03211991)+1],labels = as.roman(2), pos = 3)
            }
            if (length(grep(3,input$peak_picker)) >0){
                points(input$J3,avg_list[[1]][(input$J3/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J3,avg_list[[1]][(input$J3/0.03211991)+1],labels = as.roman(3), pos = 3)
            }
            if (length(grep(4,input$peak_picker)) >0){
                points(input$J4,avg_list[[1]][(input$J4/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J4,avg_list[[1]][(input$J4/0.03211991)+1],labels = as.roman(4), pos = 3)
            }
            if (length(grep(5,input$peak_picker)) >0){
                points(input$J5,avg_list[[1]][(input$J5/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J5,avg_list[[1]][(input$J5/0.03211991)+1],labels = as.roman(5), pos = 3)
            }
            
            if (length(grep(1,input$valley_picker)) >0){
                points(input$V1,avg_list[[1]][(input$V1/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V1)
            }
            if (length(grep(2,input$valley_picker)) >0){
                points(input$V2,avg_list[[1]][(input$V2/0.03211991)+1],pch = 4)
                valleys_changed<-c(valleys_changed,input$V2)
            }
            if (length(grep(3,input$valley_picker)) >0){
                points(input$V3,avg_list[[1]][(input$V3/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V3)
            }
            if (length(grep(4,input$valley_picker)) >0){
                points(input$V4,avg_list[[1]][(input$V4/0.03211991)+1],pch = 4)
                valleys_changed<-c(valleys_changed,input$V4)
            }
            if (length(grep(5,input$valley_picker)) >0){
                points(input$V5,avg_list[[1]][(input$V5/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V5)
            }
            
            #valleys_changed <-c(input$V1,input$V3,input$V5)
        }
        
        valley_plot = recordPlot()
        assign("test_valleys",valley_plot,envir = .GlobalEnv)
        assign("valleys_changed",valleys_changed,envir = .GlobalEnv)
    })
    
    # Extended ABR plot with Valleys and Peaks
    output$plot_extended <- renderPlot({
        inFile <- input$file1$datapath
        valleys_changed <-c()
        if (is.null(inFile)){
            def<-plot(1, type="n", xlim=c(60,110), ylim=c(0,10),
                      main = "This is where the Patient's ABR will be plotted",
                      panel.first = grid(),
                      ylab="Amplitude(μV)", xlab="Time (ms)",xaxt ='n',yaxt='n')
            
            return(def)
        }
        
        
        if (input$select == FALSE & input$select2 == FALSE) {
            
            valleys_list[[1]]<-c(1,valleys_list[[1]],225)
            par(xpd=TRUE)
            plot(samples,avg_list[[1]],type='l',yaxt='n',main = paste(volumes[1], 'nHL','- Extended'), 
                 col='darkgreen',panel.first = grid(),
                 xlab="Time (ms)", ylab="Amplitude (μV)")
            points(samples[valleys_list[[1]]],avg_list[[1]][valleys_list[[1]]],pch = 4)
            text(samples[Jewett[[1]]],avg_list[[1]][Jewett[[1]]],labels = Jewett[[2]], pos = 3)
        }
        
        else if (input$select == FALSE & input$select2 == TRUE){
            #plot_without
            replayPlot(test_valleys)
            if (length(grep(1,input$valley_picker)) >0){
                points(input$V1,avg_list[[1]][(input$V1/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V1)
            }
            if (length(grep(2,input$valley_picker)) >0){
                points(input$V2,avg_list[[1]][(input$V2/0.03211991)+1],pch = 4)
                valleys_changed<-c(valleys_changed,input$V2)
            }
            if (length(grep(3,input$valley_picker)) >0){
                points(input$V3,avg_list[[1]][(input$V3/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V3)
            }
            if (length(grep(4,input$valley_picker)) >0){
                points(input$V4,avg_list[[1]][(input$V4/0.03211991)+1],pch = 4)
                valleys_changed<-c(valleys_changed,input$V4)
            }
            if (length(grep(5,input$valley_picker)) >0){
                points(input$V5,avg_list[[1]][(input$V5/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V5)
            }
            
            
            #valleys_changed <-c(input$V1,input$V2,input$V3,input$V4,input$V5)
            
        }
        else if (input$select == TRUE & input$select2 == FALSE){
            valleys_list[[1]]<-c(1,valleys_list[[1]],225)
            par(xpd=TRUE)
            plot(samples,avg_list[[1]],type='l',yaxt='n',main = paste(volumes[1], 'nHL','- Extended'), 
                 col='darkgreen',panel.first = grid(),
                 xlab="Time (ms)", ylab="Amplitude (μV)")
            
            if (length(grep(1,input$peak_picker)) >0){
                points(input$J1,avg_list[[1]][(input$J1/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J1,avg_list[[1]][(input$J1/0.03211991)+1],labels = as.roman(1), pos = 3)
            }
            if (length(grep(2,input$peak_picker)) >0){
                points(input$J2,avg_list[[1]][(input$J2/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J2,avg_list[[1]][(input$J2/0.03211991)+1],labels = as.roman(2), pos = 3)
            }
            if (length(grep(3,input$peak_picker)) >0){
                points(input$J3,avg_list[[1]][(input$J3/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J3,avg_list[[1]][(input$J3/0.03211991)+1],labels = as.roman(3), pos = 3)
            }
            if (length(grep(4,input$peak_picker)) >0){
                points(input$J4,avg_list[[1]][(input$J4/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J4,avg_list[[1]][(input$J4/0.03211991)+1],labels = as.roman(4), pos = 3)
            }
            if (length(grep(5,input$peak_picker)) >0){
                points(input$J5,avg_list[[1]][(input$J5/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J5,avg_list[[1]][(input$J5/0.03211991)+1],labels = as.roman(5), pos = 3)
            }
            
            points(samples[valleys_list[[1]]],avg_list[[1]][valleys_list[[1]]],pch = 4)
            
            
        }
        else {
            par(xpd=TRUE)
            plot(samples,avg_list[[1]],type='l',yaxt='n',main = paste(volumes[1], 'nHL','- Extended'), 
                 col='darkgreen',panel.first = grid(),
                 xlab="Time (ms)", ylab="Amplitude (μV)")
            if (length(grep(1,input$peak_picker)) >0){
                points(input$J1,avg_list[[1]][(input$J1/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J1,avg_list[[1]][(input$J1/0.03211991)+1],labels = as.roman(1), pos = 3)
            }
            if (length(grep(2,input$peak_picker)) >0){
                points(input$J2,avg_list[[1]][(input$J2/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J2,avg_list[[1]][(input$J2/0.03211991)+1],labels = as.roman(2), pos = 3)
            }
            if (length(grep(3,input$peak_picker)) >0){
                points(input$J3,avg_list[[1]][(input$J3/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J3,avg_list[[1]][(input$J3/0.03211991)+1],labels = as.roman(3), pos = 3)
            }
            if (length(grep(4,input$peak_picker)) >0){
                points(input$J4,avg_list[[1]][(input$J4/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J4,avg_list[[1]][(input$J4/0.03211991)+1],labels = as.roman(4), pos = 3)
            }
            if (length(grep(5,input$peak_picker)) >0){
                points(input$J5,avg_list[[1]][(input$J5/0.03211991)+1],pch = 23, col='red',bg='red')
                text(input$J5,avg_list[[1]][(input$J5/0.03211991)+1],labels = as.roman(5), pos = 3)
            }
            
            if (length(grep(1,input$valley_picker)) >0){
                points(input$V1,avg_list[[1]][(input$V1/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V1)
            }
            if (length(grep(2,input$valley_picker)) >0){
                points(input$V2,avg_list[[1]][(input$V2/0.03211991)+1],pch = 4)
                valleys_changed<-c(valleys_changed,input$V2)
            }
            if (length(grep(3,input$valley_picker)) >0){
                points(input$V3,avg_list[[1]][(input$V3/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V3)
            }
            if (length(grep(4,input$valley_picker)) >0){
                points(input$V4,avg_list[[1]][(input$V4/0.03211991)+1],pch = 4)
                valleys_changed<-c(valleys_changed,input$V4)
            }
            if (length(grep(5,input$valley_picker)) >0){
                points(input$V5,avg_list[[1]][(input$V5/0.03211991)+1],pch = 19)
                valleys_changed<-c(valleys_changed,input$V5)
            }
            
            
        }
        
        extended_plot = recordPlot()
        assign("extended_plot",extended_plot,envir = .GlobalEnv)
    })
    
    # ABR Area Under Curve Plot
    output$plot2 <- renderPlot({
        inFile <- input$file1$datapath
        
        if (is.null(inFile)){
            def<-plot(1, type="n", xlim=c(60,110), ylim=c(0,10),
                      main = "This is where the Patient's ABR will be plotted",
                      panel.first = grid(),
                      ylab="Amplitude(μV)", xlab="Time (ms)",xaxt ='n',yaxt='n')
            return(def)
        }else(valleys_list[[1]]<-c(1,valleys_list[[1]],225))
        
        
        
        if(input$select == TRUE){
            if (length(grep(1,input$peak_picker)) >0){
                Jewett[[1]][1]<-(input$J1/0.03211991)+1
            }else {
                Jewett[[1]][1]<-NA
            }
            if (length(grep(2,input$peak_picker)) >0){
                Jewett[[1]][2]<-(input$J2/0.03211991)+1
            }else {
                Jewett[[1]][2]<-NA
            }
            if (length(grep(3,input$peak_picker)) >0){
                Jewett[[1]][3]<-(input$J3/0.03211991)+1
            }else {
                Jewett[[1]][3]<-NA
            }
            if (length(grep(4,input$peak_picker)) >0){
                Jewett[[1]][4]<-(input$J4/0.03211991)+1
            }else {
                Jewett[[1]][4]<-NA
            }
            if (length(grep(5,input$peak_picker)) >0){
                Jewett[[1]][5]<-(input$J5/0.03211991)+1
            }else {
                Jewett[[1]][5]<-NA
            }
        }
        
        
        if(input$select2 == TRUE){
            for (i in 1:length(valleys_changed)){
                if(is.na(valleys_changed[i])){
                    valleys_list[[1]][i]<-NA
                }else{valleys_list[[1]][i]<-(valleys_changed[i]/0.03211991)+1}
                
            }
        }
        
        amp_shifted <-avg_list[[1]] + abs(min(avg_list[[1]]))
        
        c.names <- c('c1','c2','c3','c4','c5')
        curves <- data.frame(matrix(ncol=length(c.names)),nrow=0)
        colnames(curves)<-c.names
        curves <- curves[-1,]
        curves <- curves[-6]
        r1<-c()
        r2<-c()
        
        for (j in Jewett[[1]]) {
            if (is.na(j)) {
                r1 <-c(r1,NA)
                r2<-c(r2,NA)
            }else {
                for (v in 1:(length(valleys_list[[1]])-1)) {  
                    if ((valleys_list[[1]][v])<j & j<(valleys_list[[1]][v+1]) ) {
                        r1<-c(r1,valleys_list[[1]][v])
                        r2<-c(r2,valleys_list[[1]][v+1])
                    }else {
                        next
                    }
                }
            }
        }
        
        curves<-rbind(curves,r1)
        curves<-rbind(curves,r2)
        colnames(curves)<-c.names
        
        par(xpd=TRUE)
        plot(samples[1:225],amp_shifted[1:225],type='l',yaxt='n',main = paste(volumes[1], 'nHL'), 
             col='red',panel.first = grid(),
             xlab="Time (ms)", ylab="Amplitude (μV)")
        
        points(samples[r1],amp_shifted[r1],pch=4)
        points(samples[r2],amp_shifted[r2],pch=4)
        points(samples[Jewett[[1]]],amp_shifted[Jewett[[1]]])
        text(samples[Jewett[[1]]],amp_shifted[Jewett[[1]]],labels = Jewett[[2]], pos = 3)
        
        areas<-c()
        for (i in 1:ncol(curves)) {
            if (is.na(curves[1,i])) {
                a<-NA
                areas<-c(areas,a)
                next
            }else {
                x1<-curves[1,i]
                x2<-curves[2,i]
                
                x<-samples[x1:x2]
                y<-amp_shifted[x1:x2]
                
                polygon(c(min(x),x,max(x)),c(min(amp_shifted[r1],amp_shifted[r2],na.rm = TRUE),y,min(amp_shifted[r1],amp_shifted[r2],na.rm = TRUE)),col='cyan',border = 'red',density=c(10, 20), angle=c(0,0))
                polygon(c(min(x),x,max(x)),c(min(amp_shifted[r1],amp_shifted[r2],na.rm = TRUE),y,min(amp_shifted[r1],amp_shifted[r2],na.rm = TRUE)),col='cyan',border = 'red',density=c(10, 20), angle=c(90,90))
                areas<-c(areas,sintegral(x,y)$int)
            }
        }
        
        
        analysis_data<-create_analysis_df()
        area_analysis<-create_area_analysis_df()
        
        area_plot = recordPlot()
        replayPlot(area_plot)
        assign("test2",area_plot,envir = .GlobalEnv)
        
        latency_analysis <- analyze(Jewett,avg_list,pat_id)
        analysis_data[nrow(analysis_data) + 1,] <- latency_analysis
    
        area_analysis[nrow(area_analysis) + 1,] <- c(pat_id,areas)
        assign("area_analysis",area_analysis,envir = .GlobalEnv)
        assign("analysis_data",analysis_data,envir = .GlobalEnv)
        
    })
    
    # ABR LIF I
    output$plot3 <- renderPlot({
        inFile <- input$file1$datapath
        
        if (is.null(inFile)){
            y1_min<-c(0.67,0.19,0.45,1.09)
            y1_max<-c(2.05,1.96,1.927,2)
            x<-c(70,80,90,100)
            
            
            plot(1, type="n", xlim=c(60,110), ylim=c(0,10),
                 main = 'Wave I Latency / Intensity Function',
                 panel.first = grid(),
                 xlab="Intensity (dB nHL)", ylab="Time (ms)")
            polygon(c(x,x[4:1]),c(y1_min,y1_max[4:1]),col='grey')
            
            legend(x='topright',legend='Wave I peak is not present')
            def<-recordPlot()
            return(def)
        }
        
        if (input$select == TRUE) {
            LIF_I(volumes[1],input$J1)
        }else {
            
            area_analysis<-create_area_analysis_df()
            analysis_data<-create_analysis_df()
            #plot<-abr_test(inFile)
            
            
            # Fills 5 peaks with NA 
            
            Jewett<-fill_peaks(Jewett)
            
            
            # analyze latencies of changed labels
            pat_id <- strsplit(inFile," ")[[1]][3]
            name <- paste(pat_id,"_",volumes[1],"_dB",sep="")
            latency_analysis <- analyze(Jewett,avg_list,name)
            analysis_data[nrow(analysis_data) + 1,] <- latency_analysis
            
            
            LIF_I(volumes[1],as.numeric(analysis_data$latency_I))
            
        }
        lat_I_plot = recordPlot()
        assign("test3",lat_I_plot,envir = .GlobalEnv)
    })
    
    # ABR LIF III
    output$plot4 <- renderPlot({
        inFile <- input$file1$datapath
        
        if (is.null(inFile)){
            y3_min<-c(3.24,2.38,2.6,3.24)
            y3_max<-c(3.8,3.95,3.9,3.8)
            x<-c(70,80,90,100)
            
            
            plot(1, type="n", xlim=c(60,110), ylim=c(0,10),
                 main = 'Wave III Latency / Intensity Function',
                 panel.first = grid(),
                 xlab="Intensity (dB nHL)", ylab="Time (ms)")
            
            polygon(c(x,x[4:1]),c(y3_min,y3_max[4:1]),col='grey')
            
            legend(x='topright',legend='Wave III peak is not present')
            def<-recordPlot()
            return(def)
        }
        
        if (input$select == TRUE) {
            LIF_III(volumes[1],input$J3)
        }else {
            
            area_analysis<-create_area_analysis_df()
            analysis_data<-create_analysis_df()
            #plot<-abr_test(inFile)
            
            
            # Fills 5 peaks with NA 
            
            Jewett<-fill_peaks(Jewett)
            
            
            # analyze latencies of changed labels
            pat_id <- strsplit(inFile," ")[[1]][3]
            name <- paste(pat_id,"_",volumes[1],"_dB",sep="")
            latency_analysis <- analyze(Jewett,avg_list,name)
            analysis_data[nrow(analysis_data) + 1,] <- latency_analysis
            
            LIF_III(volumes[1],as.numeric(analysis_data$latency_III))
        }
        lat_III_plot = recordPlot()
        assign("test4",lat_III_plot,envir = .GlobalEnv)
    })
    
    # ABR LIF V
    output$plot5 <- renderPlot({
        inFile <- input$file1$datapath
        
        if (is.null(inFile)){
            y5_min<-c(4.3,4.14,4.5,4.6)
            y5_max<-c(6.2,6.8,6.97,7)
            x<-c(70,80,90,100)
            
            plot(1, type="n", xlim=c(60,110), ylim=c(0,10),
                 main = 'Wave V Latency / Intensity Function',
                 panel.first = grid(),
                 xlab="Intensity (dB nHL)", ylab="Time (ms)")
            
            polygon(c(x,x[4:1]),c(y5_min,y5_max[4:1]),col='grey')
            
            legend(x='topright',legend='Wave V peak is not present')
            def<-recordPlot()
            return(def)
        }
        
        if (input$select == TRUE) {
            LIF_V(volumes[1],input$J5)
        }else {
            
            area_analysis<-create_area_analysis_df()
            analysis_data<-create_analysis_df()
            #plot<-abr_test(inFile)
            
            
            # Fills 5 peaks with NA 
            
            Jewett<-fill_peaks(Jewett)
            
            
            # analyze latencies of changed labels
            
            latency_analysis <- analyze(Jewett,avg_list,pat_id)
            analysis_data[nrow(analysis_data) + 1,] <- latency_analysis
            
            LIF_V(volumes[1],as.numeric(analysis_data$latency_V))
        }
        lat_V_plot = recordPlot()
        assign("test5",lat_V_plot,envir = .GlobalEnv)
    })
    
    # # # Generates Report for All Plots # # #
    output$report <- downloadHandler(
        
        filename = function() {paste(pat_id,".pdf",sep="")},
        content = function(file) {
            pdf(file,onefile = TRUE) #,   # The directory you want to save the file in
            #width = 4, # The width of the plot in inches
            #height = 4 # The height of the plot in inches  
            #replayPlot(test1)
            replayPlot(test_valleys)
            replayPlot(extended_plot)
            replayPlot(test2)
            replayPlot(test3)
            replayPlot(test4)
            replayPlot(test5)
            dev.off()
        }
    )
    
    output$latency_report <- downloadHandler(
        filename = function() {
            paste("Latency Data ",pat_id, ".csv", sep="")
        },
        content = function(file) {
            write.csv(analysis_data, file,row.names = FALSE)
        }
    )
    
    
    output$area_report <- downloadHandler(
        filename = function() {
            paste("Area Data_", pat_id, ".csv", sep="")
        },
        content = function(file) {
            write.csv(area_analysis, file,row.names = FALSE)
        }
    )
    
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)