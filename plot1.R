## Reading the data
downloadZip <- function(url, subdirectory){
        temp <- tempfile()
        if(!dir.exists(subdirectory)){
                dir.create(subdirectory)
        }
        download.file(url, destfile = temp, mode = "wb")
        unzip(temp, exdir = subdirectory)
        unlink(temp)
}
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
downloadZip (url, "data")
pm25 <- readRDS("./data/summarySCC_PM25.rds")
scc <- readRDS("./data/Source_Classification_Code.rds")

# Constructing the plot
png(filename = "plot1.png")
sum.emission.per.year <- with(pm25,tapply(Emissions, year, sum))
plot (y = sum.emission.per.year, x = names(sum.emission.per.year),
      ylab = "Total Emission", xlab = "Year", pch = 19)
title(main = "Total PM25 emission per year")
dev.off()