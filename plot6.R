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

# Generates plot 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
# in Los Angeles County, California (fips == "06037").
png(filename = "plot6.png", width = 480, height = 480, units = "px", bg = "white")
scc_lookup <- SCC[SCC["Data.Category"]=="Onroad",]
data_plot <- subset(NEI, NEI$SCC %in% scc_lookup$SCC)
data_plot <- subset(data_plot, data_plot$fips %in% c("24510","06037"))
data_plot <- aggregate(data_plot["Emissions"], by=data_plot[c("fips","year")], FUN = "sum")
p1 <- qplot(year, Emissions, data = data_plot,geom = c("line","point"), group = fips, colour=fips)  + xlab("year") + ylab("PM2.5 emitted by vehicles (in tons)")  + labs(title = "Plot 6")
p2 <- p1 + scale_colour_discrete(name="City", labels = c("Los Angeles County","Baltimore City"))
p2
print(data_plot)
dev.off()
