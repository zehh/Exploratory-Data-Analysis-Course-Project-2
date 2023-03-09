## Reading the data
library(tidyverse)
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

## Preparing the data
pm25 %>% as_tibble() %>% filter(fips == "24510") -> tibble25
tapply(tibble25$Emissions, list(tibble25$year, tibble25$type), sum) %>%
        as_tibble() %>% mutate(year = c(1999, 2002, 2005, 2008)) -> tibble25
tibble.baltimore <- pivot_longer(tibble25, 1:4)

## Constructing the plot
png(filename = "plot3.png")
tibble.baltimore %>% rename (total.emissions = value) %>%
        mutate(type.of.observation = as_factor(name)) %>%
        ggplot(mapping = aes(x = year, y = total.emissions, color = name)) +
        geom_path() + labs(title = "PM25 emissions in baltimore over the years")
dev.off()