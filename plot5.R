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

# Generates plot 5
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
png(filename = "plot5.png", width = 480, height = 480, units = "px", bg = "white")
scc_lookup <- SCC[SCC["Data.Category"]=="Onroad",]
data_plot <- subset(NEI, NEI$SCC %in% scc_lookup$SCC)
data_plot <- subset(data_plot, data_plot$fips == "24510")
data_plot <- aggregate(data_plot["Emissions"], by=data_plot["year"], FUN = "sum")
plot(data_plot$year,data_plot$Emissions, type="o", ylab = "PM2.5 emitted by vehicles in Baltimore City (in tons)", xlab = "Year", main="Plot 5")
print(data_plot)
dev.off()
