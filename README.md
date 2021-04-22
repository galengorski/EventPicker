# EventPicker
 Tools for identifying storm events in hydrographs
 
Galen Gorski

ggorski@ucsc.edu

2/14/2020
 
EventPicker is a free open-source tool for identifying storm events in hydrographs. As of 02/2020 it is limited to time series with daily frequencies and works best with continuous records. Time series must have a column named 'Date' with timestamps of the form mm/dd/yy, see the format of 'demo_data.csv'. The R script 'query_prepare_data.R' can be used to query data from the [USGS NWIS](https://waterdata.usgs.gov/nwis) and prepare it for use with the event picker app. If you would like to use the code, please contact me and I can provide you with an updated version as improvements are ongoing.

The code for the tool can be accessed in the folder appCode

This tool was developed in collaboration with [Margaret Zimmer](http://mzimmer.weebly.com/)
