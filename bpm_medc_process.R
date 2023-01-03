REFRESH <- FALSE
SQL_FILE <- 'bpm_medc.sql'
DATA_FILE <- 'bpm_medc.RData'
starttime <- '2021-01-01'
endtime <- '2021-12-01'

source('bpm_medc_core.r')

if (!REFRESH & file.exists(DATA_FILE)) load(DATA_FILE)

if (!exists('events')) {events <- get.events(starttime, endtime, filename=SQL_FILE)}

events %<>% 
  mutate(activity_instance=1:nrow(.))

event_log <- events %>% 
  as.event.log() %>% 
  throughput_time(level = "case", append = TRUE) %>% 
  processing_time('case', append=TRUE)

case.count <- events$stec_id %>% 
  unique() %>% 
  length()

event.counts <- events %>% 
  group_by(activity) %>% 
  summarise(Event.count = length(activity),
            Cases.count = length(unique(stec_id))) 

save.image(DATA_FILE)



