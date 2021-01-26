library(tidyverse)
library(RCurl)
library(doParallel)

stations = getURL("https://mesonet.climate.umt.edu/api/stations?type=csv&clean=true") %>%
  read_csv() %>%
  arrange(`Station name`)

cl = makeCluster(20)
registerDoParallel(cl)

foreach(i = 1:length(stations$`Station ID`), .packages = c('RCurl', 'tidyverse')) %dopar% {
  
  temp = getURL(paste0('https://mesonet.climate.umt.edu/api/observations?stations=',stations$`Station ID`[i],'&latest=false&start_time=2010-01-01&tz=US%2FMountain&wide=false&simple_datetime=false&public=true&type=csv')) %>%
    read_csv()
  
  write_csv(temp, paste0('/home/zhoylman/mesonet-download-data/', stations$`Station ID`[i],'.csv'))
  
}

stopCluster(cl)

