library(dplyr)
library(readr)
library(RCurl)
library(doParallel)

stations = getURL("https://mesonet.climate.umt.edu/api/stations?type=csv&clean=true") %>%
  read_csv() %>%
  arrange(`Station name`)

cl = makeCluster(4)
registerDoParallel(cl)

foreach(i = 1:length(stations$`Station ID`), .packages = c('RCurl', 'dplyr', 'readr')) %dopar% {
  
  temp_name = stations$`Station ID`[i]
  
  current_data = read_csv(paste0('/home/zhoylman/mesonet-download-data/',temp_name,'.csv'))
  
  last_time = max(current_data$datetime) %>% as.Date()
  
  new_data = getURL(paste0('https://mesonet.climate.umt.edu/api/observations?stations=',stations$`Station ID`[i],'&latest=false&start_time=',last_time,'&tz=US%2FMountain&wide=false&simple_datetime=false&public=true&type=csv')) %>%
    read_csv()
  
  new_full = rbind(current_data, new_data) %>% distinct()
  
  write_csv(new_full, paste0('/home/zhoylman/mesonet-download-data/',temp_name,'.csv'))
  
}

stopCluster(cl)
print('Done!')
