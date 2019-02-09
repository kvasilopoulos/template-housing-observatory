
if(download_primary) {

  # Download file -----------------------------------------------------------

  library(lubridate)  
  library(readxl)

  temp <- "data/hp1704.xlsx"

  dataURL <- "https://www.dallasfed.org/~/media/documents/institute/houseprice/hp1704.xlsx"

  download.file(dataURL, destfile = temp, mode = 'wb')

  rhpi <- readxl::read_excel(temp, sheet = 3)

  rpdi <- readxl::read_excel(temp, sheet = 5)

  # Manipulation ------------------------------------------------------------

library(dplyr)

# Real House Price Index
price <- rhpi %>%
  mutate(Date = X__1 %>% 
           zoo::as.yearqtr(format = "%Y:Q%q") %>%
           zoo::as.Date(),
         X__1 = NULL,
         X__2 = NULL
  ) %>% 
  select(Date, everything()) %>%
  na.omit()
  

# Real Personal Disposable Index
income <- rpdi %>% 
  mutate(Date = X__1 %>% 
           zoo::as.yearqtr(format = "%Y:Q%q") %>%
           zoo::as.Date(),
         X__1 = NULL,
         X__2 = NULL
  ) %>% 
  
  select(Date, everything()) %>% 
  na.omit()

pi_frac <- ((price %>% select(-Date)) / (income %>% select(-Date))) %>% 
  as_tibble()

price_income <- bind_cols(Date = price$Date, pi_frac)

# Country Names Ordered
cnames <- price %>%
  select(-Date, -Aggregate) %>%
  names() %>%
  sort() %>%
  c("Aggregate")

}

