# Generates the path for the download
filePath <- function(...) { paste(..., sep = "/") }

# Downloads the file
downloadData <- function() {
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  dir.create("data",showWarnings = FALSE)
  zipFile <- filePath("data", "exdata-data-NEI_data")
  
  if(!file.exists(zipFile)) { 
    download.file(url, zipFile, method = "curl") 
    unzip(zipFile, exdir = "data") 
  }
}

setwd("~/Documents/Projects/datasciencecoursera/ExData_Plotting2")
downloadData()

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

NEI$Emissions <- as.number(NEI$Emissions)
print unique(NEI$year)

#Generates plot 1
png(filename = "plot1.png", width = 480, height = 480, units = "px", bg = "white")
par(mar = c(6, 6, 6, 6))
data_plot <- aggregate(NEI["Emissions"], by = NEI["year"], FUN = "sum")
plot(data_plot$year,data_plot$Emissions, type="o", ylab = "PM2.5 emitted (in tons)", xlab = "Year", main="Plot 1")
print(data_plot)
dev.off()

#Generates plot 2
png(filename = "plot2.png", width = 480, height = 480, units = "px", bg = "white")
par(mar = c(6, 6, 6, 6))
data_plot <- subset(NEI, NEI$fips == "24510")
data_plot <- aggregate(data_plot["Emissions"], by=data_plot["year"], FUN = "sum")
plot(data_plot$year,data_plot$Emissions, type="o", ylab = "PM2.5 emitted (in tons) in Baltimore City", xlab = "Year", main="Plot 2")
print(data_plot)
dev.off()


