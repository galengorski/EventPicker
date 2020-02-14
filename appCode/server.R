#===================================================================================#
#-----------------------------------------------------------------------------------#
# This is the server for an event picking application developed for picking out events 
# on a hydrograph and adjusting the events interactively
#-----------------------------------------------------------------------------------#
# GG
# 2/14/2020
#-----------------------------------------------------------------------------------#
#===================================================================================#

function(input,output,session){
  #===============================================================#
  #####TAB 1 PICK EVENTS####
  
  #---------------------------------------------------------------#
  #####INITIATE REACTIVE VALUES#####
  event_scan_counter <- reactiveValues(c = NA)
  events_original <- reactiveValues(event.df = NA, record.peaks.df = NA, event.ls = NA, adjusted.events = NA)
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####UPLOAD TIME SERIES#####
  myTimeSeries <- reactive({
    validate(
      need(input$file1, "Select a time series to load")
    )
    inF <- input$file1
    raw <- read.csv(inF$datapath, header = T, stringsAsFactors = F)
    raw$Date <- as.POSIXlt(raw$Date, format = '%m/%d/%y', tz = 'UTC')
    raw
    
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####INPUTS FOR EVENT INITIAL EVENT PICKING#####
  #Independent variable choice bar 
  output$plotting_x <- renderUI({
    validate(
      need(input$file1, '')
    )
    selectInput("plot_x", "Choose x Variable:", 
                choices=colnames(myTimeSeries()), selected = colnames(myTimeSeries())[1])
  })
  
  #Dependent variable choice bar
  output$plotting_y <- renderUI({
    validate(
      need(input$file1, '')
    )
    selectInput("plot_y", "Choose x Variable:", 
                choices=colnames(myTimeSeries()), selected = colnames(myTimeSeries())[2])
  })
  
  #Backward slope threshold input
  output$sb_pk_thresh <- renderUI({
    validate(
      need(input$file1, '')
    )
    numericInput("sb_pk_thresh_val", label = 'Slope Back Threshold (default = 1e-06)', value = 0.000001)
  })
  
  #Forward slope threshold input
  output$sf_pk_thresh <- renderUI({
    validate(
      need(input$file1, '')
    )
    numericInput("sf_pk_thresh_val", label = 'Slope Forward Threshold (default = 0)', value = 0)
  })
  
  #Event rate of rise threshold input
  output$event_rise <- renderUI({
    validate(
      need(input$file1, '')
    )
    numericInput("event_rise_val", label = 'Event Rise (default = 0.001)', value = 0.001)
  })
  
  #Top of event peak threshold input
  output$peak_top_thresh <- renderUI({
    validate(
      need(input$file1, '')
    )
    numericInput("peak_top_thresh_val", label = 'Peak Top (default = 0.1)', value = 0.1)
  })
  
  #Event length threshold input
  output$event_length_thresh <- renderUI({
    validate(
      need(input$file1, '')
    )
    numericInput("event_length_thresh_val", label = 'Event Length (default = 1)', value = 1)
  })
  
  #Event scan button
  output$event_scan_btn <- renderUI({
    validate(
      need(input$file1, '')
    )
    actionButton("event_scan", "SCAN FOR EVENTS")
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####SCAN FOR EVENTS WHEN EVEN SCAN BUTTON IS CLICKED#####
  observeEvent(input$event_scan,{
    df <- myTimeSeries()
    #find.peaks tags the peaks in the data frame and returns a data frame with additional columns that 
    #identify the peaks and their limbs
    df.peaks <- find.peaks(df, input$plot_x, input$plot_y, input$sb_pk_thresh_val, input$sf_pk_thresh_val)
    #events_original$record.peaks.df will be used when the event is adjusted. The event will be redefined 
    #from this reactive dataframe
    events_original$record.peaks.df <- df.peaks
    
    #event scanner returns the events that pass muster in a list
    r.events <- event.scanner(df.peaks, input$plot_x, input$plot_y, input$event_rise_val, input$peak_top_thresh_val, input$event_length_thresh_val)
    
    events_original$event.ls <- r.events
    events_original$adjusted.events <- r.events
    c <- lapply(r.events, function(x) is.null(x)) %>% 
      unlist() %>%
      which(!is.na(.)) %>% 
      as.vector(.)
    first_event <- c[1]
    #use that as a template for a data frame
    events.orig.df <- data.frame(r.events[[first_event]][1,])
    events.orig.df <- events.orig.df[-1,] 
    for(i in 1:length(r.events)){
      if(is.null(nrow(r.events[[i]]))){
        next
      }else{
        if(nrow(r.events[[i]]) == 1){
          next
        }else{
          events.orig.df <- rbind(events.orig.df, r.events[[i]])
        }
      }
    }
    event_scan_counter$c <- 1
    events_original$event.df <- events.orig.df
    
    #add event.index column 
    events_original$event.df[,'event.index'] <<- as.numeric(as.factor(abs(events_original$event.df[,'event.flag'])))
    #don't know if you need to add event.index to the full record
    #record$event.index <- as.numeric(as.factor(abs(record$event.flag)))
    
    df <- myTimeSeries()
    df[,input$plot_x] <- as.POSIXct(strptime(as.character(df[,input$plot_x]), format = '%Y-%m-%d'), format = '%Y-%m-%d', tz = 'UTC')     #make the xlimits for plot 3
    range_xlim$rx_min <- min(df$Date)
    range_xlim$rx_max <- max(df$Date)
    
    events.removed$rm.num <- 0
    
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A PLOT OF THE HYDROGRAPH#####
  output$plot1 <- renderPlot({
    
    #check to make sure that there is an x_plot that has been selected
    validate(
      need(input$plot_x, '')
    )
    
    p.ts <- myTimeSeries()
    #p.ts$Date <- as.POSIXlt(p.ts$Date, format = '%m/%d/%y', tz = 'UTC')
    p.ts[,input$plot_x] <- as.POSIXct(strptime(as.character(p.ts[,input$plot_x]), format = '%Y-%m-%d'), format = '%Y-%m-%d', tz = 'UTC')
    
    plot(p.ts[,input$plot_x], p.ts[,input$plot_y], typ = 'l', ylab = input$plot_y, xlab = input$plot_x, las = 1)
    
    #if you have scanned for events then plot them up
    if(is.na(event_scan_counter$c)){
    }else{
      orig.events <- events_original$event.df
      orig.events$Date <- as.POSIXlt(orig.events$Date, format = '%m/%d/%y', tz = 'UTC')
      points(orig.events$Date, orig.events[,input$plot_y], col = 'dodgerblue')
      legend('topleft', pch = c(NA,1), col = c('black', 'dodgerblue'), lty = c(1,NA), legend = c('Record','Events'))
      
    }
  })
  #####
  #---------------------------------------------------------------#
  
  #####
  #===============================================================#
  
  #===============================================================#
  #####TAB 2 ADJUST EVENTS####
  #---------------------------------------------------------------#
  #####INITIATE SOME REACTIVE VALUES#####
  range_xlim <- reactiveValues(rx_min = NA, rx_max = NA)
  
  events.removed <- reactiveValues(rm.num = 0)
  
  
  selectedEventNumber <- reactive({
    #check to make sure that there is an event_number that has been selected
    #otherwise it will throw an error
    validate(
      need(input$event_number, '')
    )
    events_original$adjusted.events[[input$event_number]]
  })
  
  #this is for paging through the events and highlighting them in red in the hydrograph
  selectedEventNumberStable <- reactive({
    #check to make sure that there is an event_number that has been selected
    #otherwise it will throw an error
    validate(
      need(input$event_number, '')
    )
    events_original$event.ls[[input$event_number]]
  })
  
  
  adjustedEvent <- reactive({
    origEvent <- selectedEventNumber()
    p.ts <- myTimeSeries()
    p.ts <- events_original$record.peaks.df
    p.ts[p.ts[,input$plot_x] >= min(origEvent[,input$plot_x]) - input$adjust_event_beginning*86400 & p.ts[,input$plot_x] <= max(origEvent[,input$plot_x]) + input$adjust_event_end*86400,]
  })
  
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE THE EVENT TICKER#####
  output$event_counter_tab2 <- renderUI({
    validate(
      need(input$file1, '')
    )
    numericInput('event_number', 'Event Number', min = 1, max = length(events_original$event.ls), value = 1)
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####REMOVE EVENT WHEN THE BUTTON IS CLICKED#####
  observeEvent(input$remove_event,{
    events.removed$rm.num <- c(events.removed$rm.num,input$event_number)
    to_be_removed <- events_original$adjusted.events
    events_original$adjusted.events[[input$event_number]] <- 'Event Removed'
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####SAVE PEAK ADJUSTMENTS#####
  observeEvent(input$save_adj,{
    current.event.adj <- adjustedEvent()
    events_original$adjusted.events[[input$event_number]] <- current.event.adj
  })
  
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####CHANGE THE RANGE OF THE HYDROGRAPH#####
  
  observeEvent(input$range,{
    p.ts <- myTimeSeries()
    p.ts[,input$plot_x] <- as.POSIXct(strptime(as.character(p.ts[,input$plot_x]), format = '%Y-%m-%d'), format = '%Y-%m-%d', tz = 'UTC')
    
    if(input$range == 'Full Range'){
      range_xlim$rx_min <- min(p.ts[,input$plot_x])
      range_xlim$rx_max <- max(p.ts[,input$plot_x])
    }else if(input$range == 'Annual'){
      #make is an annual scale
      current.event <- selectedEventNumber()
      
      yr <-strptime(current.event[,input$plot_x], format = '%Y-%m-%d', tz = 'UTC') %>%
        format(., format = '%Y') %>%
        min() %>%
        as.numeric()
      
      nxyr <- yr + 1
      
      range_xlim$rx_min = paste0(yr,'-01-01') %>%
        as.POSIXct(., format = '%Y-%m-%d')
      
      
      range_xlim$rx_max = paste0(nxyr,'-01-01') %>%
        as.POSIXct(., format = '%Y-%m-%d')
      
    }else{
      current.event <- selectedEventNumber()
      
      yr <-strptime(current.event[,input$plot_x], format = '%Y-%m-%d', tz = 'UTC') %>%
        format(., format = '%Y') %>%
        min() %>%
        as.numeric()
      
      nxyr <- yr + 1
      
      mo <-strptime(current.event[,input$plot_x], format = '%Y-%m-%d', tz = 'UTC') %>%
        format(., format = '%m') %>%
        min() %>%
        as.numeric()
      
      nxmo <- mo + 1
      
      range_xlim$rx_min = paste(yr,mo,'01', sep = '-') %>%
        as.POSIXct(., format = '%Y-%m-%d', tz = 'UTC')
      
      
      range_xlim$rx_max = paste(yr,nxmo,'01', sep = '-') %>%
        as.POSIXct(., format = '%Y-%m-%d', tz = 'UTC')
    }
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A PLOT OF THE HYDROGRAPH WITH THE EVENTS PICKED OUT#####
  output$plot3 <- renderPlot({
    #check to make sure that there is an x_plot that has been selected
    validate(
      need(input$plot_x, '')
    )
    p.ts <- myTimeSeries()
    current.event <- selectedEventNumberStable()
    p.ts[,input$plot_x] <- as.POSIXct(strptime(as.character(p.ts[,input$plot_x]), format = '%Y-%m-%d'), format = '%Y-%m-%d', tz = 'UTC')
    x.toplot <- p.ts[,input$plot_x]
    y.toplot <- p.ts[,input$plot_y]
    
    
    plot(x.toplot, y.toplot, typ = 'l', ylab = input$plot_y, xlab = input$plot_x, las = 1,
         xlim = c(range_xlim$rx_min, range_xlim$rx_max))
    
    #if you have scanned for events then plot them up
    if(is.na(event_scan_counter$c)){
    }else{
      orig.events <- events_original$event.df
      orig.events$Date <- as.POSIXlt(orig.events$Date, format = '%m/%d/%y', tz = 'UTC')
      points(orig.events$Date, orig.events[,input$plot_y], col = 'dodgerblue')
      legend('topleft', pch = c(NA,1,16), col = c('black', 'dodgerblue','green'), lty = c(1,NA,NA), legend = c('Record','Events','Removed'))
      
    }
    
    if(length(events.removed$rm.num) == 1){
      
    }else{
      events.rmved <- events_original$event.df[events_original$event.df[,'event.index'] %in% c(events.removed$rm.num),]
      points(events.rmved[,input$plot_x], events.rmved[,input$plot_y], col = 'green', pch = 16)
    }
    points(current.event[,input$plot_x], current.event[,input$plot_y], col = 'red', typ = 'l', lwd = 3)
    
    
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A ZOOMED IN PLOT OF THE EVENT#####
  output$plot4 <- renderPlot({
    #check to make sure that there is an x_plot that has been selected
    validate(
      need(input$plot_x, '')
    )
    p.ts <- myTimeSeries()
    
    current.event <- selectedEventNumber()
    validate(
      need(!is.null(nrow(current.event)), "This event has been removed")
    )
    p.ts$Date <- as.POSIXlt(p.ts$Date, format = '%m/%d/%y', tz = 'UTC')
    x.toplot <- p.ts[,input$plot_x]
    y.toplot <- p.ts[,input$plot_y]
    
    par(mfrow = c(1,2))
    plot(x.toplot,y.toplot, typ = 'l', lwd = 2,
         ylab = input$plot_y, xlab = '', las = 1,
         xlim = c(min(current.event[,input$plot_x])-864000, max(current.event[,input$plot_x])+864000),
         ylim = c(0,max(current.event[,input$plot_y], na.rm = T)*1.25))
    legend('topleft',col = c('red','blue'), pch = c(16,0), legend = c('Original Peak','Revised Peak'), bty = 'n')
    
    if(input$adjust_event_beginning == 0 & input$adjust_event_end == 0){
      
    }else{
      current.event.adj <- adjustedEvent()
      points(current.event.adj[,input$plot_x], current.event.adj[,input$plot_y], col = 'blue', cex = 2.5, pch = 0)
    }
    
    points(current.event[,input$plot_x], current.event[,input$plot_y], col = 'red', cex = 1.5, pch = 16)
    #peak of event
    abline(h = max(current.event[,input$plot_y], na.rm = T), col = 'firebrick', lty = 2, lwd = 2)
    #80% of the difference between max and min
    abline(h = (max(current.event[,input$plot_y], na.rm = T)-(max(current.event[,input$plot_y], na.rm = T)-min(current.event[current.event[,'slp.f']>0,input$plot_y], na.rm = T))*0.8), col = 'darkorange2', lty = 2, lwd = 2)
    #lowest point
    abline(h = min(current.event[current.event[,'slp.f']>0,input$plot_y], na.rm = T), col = 'goldenrod1', lty = 2, lwd = 2)
    
  })
  
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A TABLE WITH A TALLY OF EVENTS#####
  
  # output$event_table <- renderTable({
  #   events.compact <- compact(events_original$adjusted.events)
  #   t <- lapply(events.compact, function(x) strptime(x[,input$plot_x][1], format = '%Y-%m-%d') %>% format(., format = '%Y')) %>%
  #     unlist()%>%
  #     table() %>%
  #     as.matrix()
  #   colnames(t) <- 'Number of Events'
  #   t
  #   },
  #   spacing = 'xs',
  #   rownames = TRUE, colnames = TRUE)
  
  # output$path <- renderText({
  #   print(input$file1$name)
  # })
  
  
  #---------------------------------------------------------------#
  #####DOWNLOAD THE ADJUSTED EVENTS#####
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(strsplit(input$file1$name, '.csv')[[1]][1],'_adjustedevents_',Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(events_original$adjusted.events, file)
    }
  )
  #####
  #---------------------------------------------------------------#
}