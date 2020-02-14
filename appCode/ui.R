#===================================================================================#
#-----------------------------------------------------------------------------------#
# This is the ui for an event picking application developed for picking out events 
# on a hydrograph and adjusting the events interactively
#-----------------------------------------------------------------------------------#
# GG
# 2/14/2020
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
#install.packages('shiny')
library(shiny)
#install.packages('shinythemes')
library(shinythemes)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('shinyWidgets')
library(shinyWidgets)
#install.packages('tidyverse')
library(tidyverse)
#install.packages('plyr')
library(plyr)
#Run this script of functions
source('event_picker_functions.R')

#####
#===================================================================================#





fluidPage(
  #select theme
  theme = shinytheme("cerulean"),
  # Give the page a title
  titlePanel("Event picker"),
  tabsetPanel(type = "tabs",
              tabPanel("Upload hydrograph and pick events",
                       # Generate a row with a sidebar
                       sidebarLayout(      
                         # Define the sidebar with one input
                         sidebarPanel(
                           h5('Upload a time series with a daily frequency with a column labled "Date" whose format is mm/dd/yy'),
                           #Upload Time Series
                           fileInput("file1", "Upload Time Series",
                                     accept = c(
                                       "text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")
                           ),
                           uiOutput('plotting_x'),
                           uiOutput('plotting_y'),
                           uiOutput('sb_pk_thresh'),
                           uiOutput('sf_pk_thresh'),
                           uiOutput('event_rise'),
                           uiOutput('peak_top_thresh'),
                           uiOutput('event_length_thresh'),
                           uiOutput('event_scan_btn')
                         ),
                         mainPanel(
                           plotOutput('plot1', height = '600px')
                         ))
              ),
              tabPanel("Adjust Events",
                       sidebarPanel(
                         uiOutput('event_counter_tab2'),
                         
                         numericInput('adjust_event_beginning', 'Adjust Event Beginning', min = -20, max = 20, value = 0, step = 1),
                         numericInput('adjust_event_end', 'Adjust Event End', min = -20, max = 20, value = 0, step = 1),
                         actionButton('remove_event', 'REMOVE'),
                         actionButton('save_adj', 'SAVE ADJUSTMENTS'),
                         selectInput('range', 'Select Range', choices = c('Full Range', 'Annual', 'Month'), selected = 'Full Range'),
                         downloadButton('downloadData')
                         
                       ),
                       mainPanel(
                         plotOutput('plot3', height = '300px'),
                         plotOutput('plot4', height = '325px'),
                         textOutput('path')
                       )
              )
  )
)

