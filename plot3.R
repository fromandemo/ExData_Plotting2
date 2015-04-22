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
# setwd("~/Documents/Courses/Coursera/ExData_Plotting2")
setwd("~/Documents/Projects/datasciencecoursera/ExData_Plotting2")
downloadData()

# Read data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

NEI$Emissions <- as.numeric(NEI$Emissions)

# Generates plot 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
png(filename = "plot3.png", width = 480, height = 480, units = "px", bg = "white")
data_plot <- subset(NEI, NEI$fips == "24510")
data_plot <- aggregate(data_plot["Emissions"], by=data_plot[c("type","year")], FUN = "sum")
qplot(year, Emissions, data = data_plot,geom = c("line", "point"), colour = type) + ylab("PM2.5 emitted (in tons) in Baltimore City") + labs(title = "Plot 3") 
print(data_plot)
dev.off()


