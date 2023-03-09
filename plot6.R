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
        filter(str_detect(fips,"(24510)|(06037)")) %>%
        filter(str_detect(SCC.Level.One,"(M|m)obile")) %>%
        filter (str_detect(Short.Name,
                           "(Off-highway)|(Maintenance)", negate = T)) ->
        joinedMVBaltLA
joinedMVBaltLA$city <- case_when(
        joinedMVBaltLA$fips == "24510" ~ "Baltimore",
        joinedMVBaltLA$fips == "06037" ~ "Los Angeles",
        TRUE ~ NA_character_)
joinedMVBaltLA %>% group_by(year,city) %>% 
        summarise(total.emissions = sum(Emissions)) %>% 
        mutate (year = as.factor(year)) -> finalTibble
## Calculating spread measure, coefficient of variation
finalTibble %>% filter(city == "Baltimore") -> emissions.balt
finalTibble %>% filter(city == "Los Angeles") -> emissions.la
sd(emissions.balt$total.emissions)/mean(emissions.balt$total.emissions) ->
        coefvarbalt
sd(emissions.la$total.emissions)/mean(emissions.la$total.emissions) ->
        coefvarla
## Constructing the plot
png(filename = "plot6.png")
tibble(city = as.factor(c("Baltimore","Los Angeles")),
       coefficient.variation = c(coefvarbalt,coefvarla)) %>%
        ggplot (aes(x = city,
                    y = coefficient.variation,
                    fill = city)) +
        geom_col() +
        labs(title = "Coefficient of variation of the motor vehicle PM2.5 emission per year",
             subtitle =  "Baltimore versus Los Angeles",
             caption = "The city which saw greater proportional changes over time was Baltimore") +
        xlab("City") + ylab("Coefficient of Variation")
dev.off()