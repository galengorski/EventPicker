#===================================================================================#
#-----------------------------------------------------------------------------------#
# This is a script for querying retrieving stream gauge data from USGS, and 
# transforming it into a format that usable for the EventPickerApp
#-----------------------------------------------------------------------------------#
# GG
# 2/11/2020
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
#install.packages('dataRetrieval')
library(dataRetrieval)
#install.packages('tidyverse')
library(tidyverse)
#install.packages('magrittr')
library(magrittr)
#install.packages('Hmisc')
library(Hmisc)
#####
#===================================================================================#
#===================================================================================#
#####INSTALL PACKAGES#####
#right now this loop is set up to query the NWIS database and return daily values ('dv') for
#average ('00003') discharge in cfs ('00060') and [NO3+NO2] in mg-N/L using the site 8 digit id
#codes

# HUC: Kankakee -- 07120001                                                                                  #
# Station                             Station ID                                                             #
# KANKAKEE RIVER AT DAVIS, IN         05515500                                                               #
# KANKAKEE RIVER AT DUNNS BRIDGE, IN  05517500                                                               #
# KANKAKEE RIVER AT SHELBY, IN        05518000                                                               #


rr <- list()
sites <- list()
j = 1
v <- c('05515500','05517500','05518000')
for(i in 1:length(v)){
  site.data <- whatNWISdata(site = v[i], service = 'dv')
  if('00060' %in% site.data$parm_cd & '99133' %in% site.data$parm_cd){
    #print('Correct Parameters') 
    if('00003' %in% site.data[site.data$parm_cd == '00060','stat_cd'] & '00003' %in% site.data[site.data$parm_cd == '99133','stat_cd']){
      print('Correct Parameters/Correct Code')
      sites[[j]] <- whatNWISdata(site = v[i], service = 'dv')[1,]
      rr[[j]] <- readNWISdata(site = v[i], parameterCd = c('00060','99133'),startDate="2012-03-01",endDate=Sys.Date(), statCd = c('00003'))
      j <- j + 1
    }else{
      print('Correct Parameters/Incorrect Code')
      sites[[j]] <- whatNWISdata(site = v[i], service = 'dv')[1,]
      rr[[j]] <- readNWISdata(site = v[i], parameterCd = c('00060','99133'),startDate="2012-03-01",endDate=Sys.Date(), statCd = c('00003','00008'))
      j <- j + 1
    }
  }else{
    print('Incorrect Parameters')
    next
  }
}

#####
#===================================================================================#

#===================================================================================#
#####TRANSFORM AND SAVE THE QUERY RESULTS AS .CSVS#####
direc <- '02_Data/USGS_queries/raw/'

for(i in 1:length(rr)){
temp.record <- rr[[i]]
temp.record$Date <- format(strptime(temp.record$dateTime, format = '%Y-%m-%d', tz = 'UTC'),format = '%m/%d/%y')
temp.record <- temp.record[,c('Date','X_00060_00003','X_99133_00003')]

temp.record <- temp.record[!is.na(temp.record$X_00060_00003)&!is.na(temp.record$X_99133_00003),]

write.csv(temp.record, paste0(direc,sites[[i]]$site_no,'.csv'), row.names = F)

}
#####
#===================================================================================#

