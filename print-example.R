
library(tidyverse)
df= read.csv('data.csv')
df= df %>% 
  mutate(InvoiceDateTime= as.POSIXct(strptime(InvoiceDate, format = "%m/%d/%Y %H:%M")),
         Date= as.Date(InvoiceDate, format = "%m/%d/%Y %H:%M"),
         WeekDay= weekdays(as.Date(InvoiceDate, format = "%m/%d/%Y %H:%M")),
         Hour= as.integer(strftime(InvoiceDateTime, format="%H")),
         Day= as.integer(strftime(InvoiceDateTime, format="%d")),
         Month= as.integer(strftime(InvoiceDateTime, format="%m")),
         Year= as.integer(strftime(InvoiceDateTime, format="%Y"))
  )

df$WeekDay= df$WeekDay %>% 
  substr(1,3) %>% 
  factor(levels= c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))





