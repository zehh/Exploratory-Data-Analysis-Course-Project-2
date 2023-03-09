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
joined %>% 
        filter(fips == "24510") %>%
        filter(str_detect(SCC.Level.One,"(M|m)obile")) %>%
        filter (str_detect(Short.Name,
                           "(Off-highway)|(Maintenance)", negate = T)) ->
        joinedMVBaltimore
joinedMVBaltimore %>% group_by(year) %>% 
        summarise(total.emissions = sum(Emissions)) %>% 
        mutate (year = as.factor(year)) -> finalTibble
## Constructing the plot
png(filename = "plot5.png")
finalTibble %>%
        ggplot(aes(x = year , y = total.emissions)) +
        geom_col() + ylab ("PM25 Emissions (ton)") + xlab ("Year") +
        labs(title = "Motor Vehicle Emissions in Baltimore")
dev.off()