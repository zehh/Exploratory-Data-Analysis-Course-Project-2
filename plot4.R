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
scc %>% as_tibble() %>%
        select (Short.Name, SCC, SCC.Level.One,SCC.Level.Two,SCC.Level.Three,
                SCC.Level.Four) %>%
        right_join(as_tibble(pm25),by = join_by(SCC)) -> joined
joined %>% filter(str_detect(SCC.Level.One, "(c|C)ombustion")) %>%
        filter(str_detect(SCC.Level.Three, "(c|C)oal")) -> joinedcoal
joinedcoal %>% group_by(year) %>% summarise(total.emission = sum(Emissions)) ->
        finaltibble
## Constructing the plot
png(filename = "plot4.png")
finaltibble %>% ggplot(aes(x = year, y = total.emission)) + geom_point() +
        labs(title = "Total emissions from coal combustion-related sources per year") +
        ylab("Ammount of PM2.5 emitted (ton)") + xlab("Year")
dev.off()
