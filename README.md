# EventPicker
 Tools for identifying storm events in hydrographs
 
	Galen Gorski
	ggorski@ucsc.edu
	2/14/2020
 
EventPicker is a free open-source tool for identifying storm events in hydrographs. As of 2/2020 it is limited to time series with daily frequencies. Time series must have a column named 'Date' with timestamps of the form mm/dd/yy, see the format of 'demo_data.csv'. The R script 'query_prepare_data' can be used to query data from the [USGS NWIS](https://waterdata.usgs.gov/nwis) and prepare it for use with the event picker app

The tool can be accessed at https://ggorski.shinyapps.io/eventpickerapp/

This tool was developed in collaboration with [Margaret Zimmer](http://mzimmer.weebly.com/)
