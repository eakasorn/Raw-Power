# Information
This application makes use of the electrical power generation obtained from the U.S Environmental Protection Agency website. The link can be accessed here https://www.epa.gov/egrid/download-data. The entire collection consists of data dating back from 1996 in the form of .xls and .xlsx files.
In particular, the focus will be on the data from the year 2000, 2010 and 2018 using the files eGRID2000_plant.xls, eGRID2010_Data.xls and egrid2018_data_v2.xlsx.
The following tabs from the files are used.
Year 2000: EGRDPLNT00 tab
Year 2010: PLNT10 tab
Year 2018: PLNT18 tab
As the data format varies by the year, some data manipulation is done to try and remove irrelevant information as well as reduce the size of data. The main attributes that are kept are plant sequence number, state abbreviation, plant name, ORIS facility code, latitude, longitude, net generation from 10 different energy sources in MWh and their respective percentages. The final number of attributes remaining is 31 or 32 depending on whether the source Other is split or combined. For ease of usage, new tabs have been created with the name 'Mini' following the original tab used (e.g: EGRDPLNT00_Mini). They have also been modified to aid in the reading in of the data. 

# Instruction
To run this application, R language and RStudio version 1.4.1103 is recommended. In this application, the library packages required are shiny, shinydashboard, readxl, ggplot2, DT, and leaflet.
To install R, click the link https://www.r-project.org/ and choose the version 4.0.4.
To install RStudio, click the link https://rstudio.com/products/rstudio/download/ and choose the RStudio Desktop version.
After downloading and installing R and RStudio, to install the required packages to run the code, in RStudio application on the bottom left section named "Console", run the 'code install.packages()' where inside the brackets, put in the name of the library to install. Note that double quotation marks are required. (For example, to install shiny package use the code 'install.packages("shiny")')
Set the working directory to the folder path where the csv file is located. This can be done using the code 'setwd()' where the path to the folder is inside the bracket.
To run the code open the app.R file in RStudio and select Run App in the top middle section of the application.
