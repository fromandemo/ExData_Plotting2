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
setwd("~/Documents/Courses/Coursera/ExData_Plotting2")
# setwd("~/Documents/Projects/datasciencecoursera/ExData_Plotting2")
downloadData()

# Read data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

NEI$Emissions <- as.number(NEI$Emissions)
print unique(NEI$year)

# Generates plot 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
png(filename = "plot1.png", width = 480, height = 480, units = "px", bg = "white")
par(mar = c(6, 6, 6, 6))
data_plot <- aggregate(NEI["Emissions"], by = NEI["year"], FUN = "sum")
plot(data_plot$year,data_plot$Emissions, type="o", ylab = "PM2.5 emitted (in tons)", xlab = "Year", main="Plot 1")
print(data_plot)
dev.off()

# Generates plot 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
png(filename = "plot2.png", width = 480, height = 480, units = "px", bg = "white")
par(mar = c(6, 6, 6, 6))
data_plot <- subset(NEI, NEI$fips == "24510")
data_plot <- aggregate(data_plot["Emissions"], by=data_plot["year"], FUN = "sum")
plot(data_plot$year,data_plot$Emissions, type="o", ylab = "PM2.5 emitted (in tons) in Baltimore City", xlab = "Year", main="Plot 2")
print(data_plot)
dev.off()

# Generates plot 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
png(filename = "plot3.png", width = 480, height = 480, units = "px", bg = "white")
data_plot <- subset(NEI, NEI$fips == "24510")
data_plot <- aggregate(data_plot["Emissions"], by=data_plot[c("type","year")], FUN = "sum")
qplot(year, Emissions, data = data_plot,geom = c("line", "point"), colour = type) + ylab("PM2.5 emitted (in tons) in Baltimore City") 
print(data_plot)fip
dev.off()

# Generates plot 4
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
png(filename = "plot4.png", width = 480, height = 480, units = "px", bg = "white")
scc_lookup <-subset(SCC[,"SCC"] , grepl("(Coal)",SCC[ ,"Short.Name"],ignore.case = TRUE))
data_plot <- subset(NEI, NEI$SCC %in% scc_lookup)
data_plot <- aggregate(data_plot["Emissions"], by=data_plot["year"], FUN = "sum")
plot(data_plot$year,data_plot$Emissions, type="o", ylab = "PM2.5 emitted by coal combustion sources (in tons)", xlab = "Year", main="Plot 4")
print(data_plot)
dev.off()

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

