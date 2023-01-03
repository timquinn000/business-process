library(odbc)
library(RODBC)
library(tidyverse)
library(processanimateR)
library(bupaR)
library(htmlwidgets)
library(glue)
library(DiagrammeR)
library(magrittr)
library(kableExtra)
library(flextable)
library(stringr)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

TEST_LEVELS <- c("Production","Test","Confirmed")

toproper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))) %>% 
  str_replace('_id$', '_ID') %>% 
  str_replace('_sk$', '_SK') %>% 
  str_replace_all('\\.|_', ' ') 

tor <- function(x) gsub(' ', '_', x) %>% tolower()

#Can't change dir with saveWidget???
save.widget <- function(x, dir, filename=paste0(deparse(substitute(x)), '.html')){
  plotsfile <- file.path(dir, filename)
  saveWidget(x, filename)
  if (file.exists(plotsfile)) file.remove(plotsfile)
  file.copy(filename, plotsfile)
  invisible(file.remove(filename))
}

get.events <- function(starttime='2021-01-01', endtime=starttime, maxcases=10, filename){
  
  db_handle <- odbcDriverConnect("driver={SQL Server};
                                   server=ACT001VBSRPTP01;
                                   database=T2_EXPORTS;
                                   trusted_connection=true",
                                 believeNRows=FALSE, rows_at_time = 1)
  
  sql <- readChar(filename, file.info(filename)$size) %>% 
    gsub('--##', '', .)
  
  
  events <- RODBC::sqlQuery(db_handle, str_glue(sql)) %>%
    tibble() %>% 
    rename_with(tor)
  
  odbcClose(db_handle)
  
  events
}

as.event.log <- function(events) {
  events %>% 
    mutate(start_datetime = as.POSIXct(start_datetime)) %>% 
    eventlog(
      case_id = "stec_id",
      activity_id = "activity",
      activity_instance_id = "activity_instance",
      lifecycle_id = "status",
      timestamp = "start_datetime",
      resource_id = "destination"
    ) %>% 
    mutate(activity = factor(activity, levels = TEST_LEVELS))
}

short.trace <- function(x, len=1) {
  sht.trc <- function(z) {
    stringr::str_split(z, pattern = ',') %>%
      unlist() %>% 
      substr(1, len) %>% 
      paste(collapse=',')
  }
  sapply(x, sht.trc)
}

# Take an event log and return a datamodel with entities: case, trace, case_trace 
# (an association table)
pbi.datamodel <- function(x){
  
  case_col <- attr(x, 'case_id')
  #res_col <- attr(x, 'resource_id')
  
  cases <- tibble(case = unique(x[[case_col]])) 
  names(cases) <- case_col
  
  traces <- x %>% 
    trace_length(level = 'trace') %>% 
    arrange(desc(absolute)) %>% 
    rename(trace_length=absolute) %>% 
    mutate(trace_id = short.trace(trace)) %>% 
    select(trace_id, trace_length, relative_to_median, trace)
  
  throughput <- x %>% 
    group_by(!! sym(case_col)) %>% 
    summarise(throughput_time = min(throughput_time_case))
  
  tl <- x %>% 
    trace_length(level = "case") %>% 
    rename(trace_length=absolute)
  
  case_trace <- x %>% 
    trace_coverage(level = "case") %>% 
    select(!! sym(case_col), 
           trace) %>% 
    left_join(traces %>% select(trace, trace_id), by='trace') %>% 
    select(-trace) 
  
  cases %<>% 
    left_join(throughput, by=case_col) %>% 
    left_join(tl, by=case_col) 
  
  traces %<>% select(-trace)
  
  return(list(
    cases = cases,
    case_trace = case_trace,
    traces = traces,
    event_log = x
  ))
  
}


