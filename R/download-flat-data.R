library(tidyverse)
library(RCurl)

tictoc::tic()
test = getURL('https://mesonet.climate.umt.edu/api/observations?stations=lololowr&latest=false&start_time=2010-01-01&tz=US%2FMountain&wide=false&simple_datetime=false&public=true&type=csv') %>%
  read_csv()
tictoc::toc()

write_csv(test, '/home/zhoylman/mesonet-download-data/lololowr.csv')

