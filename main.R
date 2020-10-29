

#### Analysera aktier med hjälp av börsdata API ### 

# Libraries
library(tidyverse)
library(rjson)
library(lubridate)

# API Key
key <-  Sys.getenv("BÖRSDATA_API")



### Plocka fram KPIer/information och aktiekurser för att göra djupare analyser. 


### 1. Hämta information om aktier
fetch_stockInfo <- function(api_key){
  url <- 'https://apiservice.borsdata.se/v1/instruments'
  content <- paste0(url, '?authKey=', api_key, sep = "")
  getdata <- httr::GET(url= content)
  data_json <- httr::content(getdata, type = "text", encoding = "utf-8")
  info <- jsonlite::fromJSON(data_json)
  data_stock_info <- data.frame(id = info$instruments$insId,
                                name = info$instruments$name,
                                urlname = info$instruments$urlName,
                                ticker = info$instruments$ticker, 
                                yahoo = info$instruments$yahoo, 
                                sector = info$instruments$sectorId, 
                                market = info$instruments$marketId, 
                                branch = info$instruments$branchId,
                                country = info$instruments$countryId, 
                                listingDate = info$instruments$listingDate)
  
  data_stock_info <- tibble(data_stock_info)
  data_stock_info
  
}

### 2. Hämta aktiepriser
fetch_stockprices <- function(ids, key, start_date = NULL) {
  
  stock_tbl <- tibble(date = lubridate::ymd(), 
                      price = numeric(), 
                      id = numeric())
  
  urls <- vector(mode = "character", length = length(ids))
  root <- "https://apiservice.borsdata.se" # Root
  auth <- "?authKey="
  for (i in seq_along(ids)) {
    endpoint <- paste("/v1/instruments/", ids[i],"/stockprices", sep = "")
    url <- paste(root, endpoint, auth, key, "&maxcount=20", sep = "")
    urls[i] <- url
  }
  
  
  
  for (j in seq_along(urls)){
    getdata <- httr::GET(url = urls[j])
    data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
    df <- jsonlite::fromJSON(data_json,)
    kurs<-df$stockPricesList
    kurs <- kurs[, c(1,4)]
    colnames(kurs) <- c("date", "price")
    kurs <- tibble(kurs)
    kurs <- kurs %>% 
      mutate(date = ymd(date))
    
    if (!is.null(start_date)) {
      start_date <- ymd(start_date)
      kurs <- kurs %>% 
        filter(date >= start_date)
    }
    kurs <- kurs %>% 
      mutate(id = ids[j])
    
    stock_tbl <- stock_tbl %>% 
      bind_rows(kurs)
    
    Sys.sleep(1)
  }
  return(stock_tbl)
}

### 3. Hämta nyckeltal för aktier

fetch_nyckeltal <- function(kpi, api_key, period = "latest",  func = NULL){
  
  nyckeltal <- c("omsättningstillväxt", "vinsttillväxt", "pe", "ps", "pb",  "eb/ebit", "peg", "bv", "ev", "vinstmarginal",
                 "ebitmarginal", "bruttomarginal", "ebitdamarginal", "avk_eget_kapital", "ROIC", "omsättningshastighet", 
                 "soliditet", "kursutveckling")
  nyckeltalsID <- c("94", "97", "2", "3", "4", "10", "19","50", "49", "30","29" , "28","32", "33", "37", "38", "39", "151")
  nyckeltal_hash <- setNames(as.list(nyckeltalsID), nyckeltal)
  
  
  if (period == "quarter"){
    if (is.null(func)) {
      namn_kolumn <- paste0(kpi, "_latest_", period, sep = "")
    } else {
      namn_kolumn <- paste0(kpi, "_latest_", period,func, sep = "")
    }
  }
  
  else {
    if (is.null(func)){
      namn_kolumn <- paste0(kpi, "_", period, sep = "")
    } else {
      namn_kolumn <- paste0(kpi, "_", period, "_", func, sep = "")
    } 
    
  }
  
  
  url_first_part <- paste0("https://apiservice.borsdata.se/v1/instruments/kpis/", nyckeltal_hash[[kpi]], sep = "")
  
  
  if (period == "latest"){
    url_second_part <- "/last/latest"
  } else if (period == "quarter") {
    url_second_part <- "/last/quarter"
  }
  else {
    url_second_part <- paste0("/", period, "/", func, sep = "")
  }
  url_third_part <- paste0("?authKey=", api_key, sep = "")
  url <- paste0(url_first_part, url_second_part, url_third_part, sep = "")
  
  ### Hämta data
  getdata <- httr::GET(url = url)
  data_json <- httr::content(getdata, type = "text", encoding = "utf-8")
  info <- jsonlite::fromJSON(data_json)
  kpi_df <-  data.frame(id = info$values$i,
                        kpi = info$values$n)
  kpi_tibble <- tibble(kpi_df)
  colnames(kpi_tibble) <- c("id", namn_kolumn)
  
  return(kpi_tibble)
}

# Få in denna funktion i 3 vid senare tillfälle
# 4.  Kursutveckling
kursutveckling <- function(api_key, period) {
  namn_kolumn <- paste0("kursutveckling_",period, sep = "")
  url_part_one <- 'https://apiservice.borsdata.se/v1/instruments/kpis/151/'
  url_part_two <- paste0(period, "/return", sep = "")
  url_part_three <- paste0("?authKey=", key, sep = "")
  
  URL <- paste0(url_part_one, url_part_two, url_part_three, sep = "")
  getdata <- httr::GET(url = URL)
  data_json <- httr::content(getdata, type = "text", encoding = "utf-8")
  info <- jsonlite::fromJSON(data_json)
  data_kursutveckling <- data.frame(id = info$values$i,
                                    kpi = info$values$n)
  kpi_tibble <- tibble(data_kursutveckling)
  colnames(kpi_tibble) <- c("id", namn_kolumn)
  kpi_tibble
}




### Exempel 

stock_information <- fetch_stockInfo(key)
data_vinsttillväxt <- fetch_nyckeltal("vinsttillväxt", key, "3year","mean")
data_soliditet <- fetch_nyckeltal("soliditet", key)
data_omsättningstillväxt <- fetch_nyckeltal("omsättningstillväxt", key, "1year", "mean")
data_peg <- fetch_nyckeltal("peg", key)


stock_data <- stock_information %>% 
  left_join(data_vinsttillväxt, by = "id")

stock_data <- stock_data %>% 
  left_join(data_vinsttillväxt, by = "id")

stock_data <- stock_data %>% 
  left_join(data_soliditet, by = "id")

stock_data <- stock_data %>% 
  left_join(data_omsättningstillväxt, by = "id")

stock_data <- stock_data %>% 
  left_join(data_peg, by = "id")

# Ta bort vissa kolumner
stock_data <- stock_data %>% 
  select(-c(urlname, ticker, yahoo, listingDate))

stock_data %>% 
  head()


### Ändra namnen på sektorer
stock_data <- stock_data %>% 
  mutate(sector = case_when(sector == 1 ~ "Financials", 
                            sector == 2 ~ "Food and Beverege", 
                            sector == 3 ~ "Energy", 
                            sector == 4 ~ "Healthcare", 
                            sector == 5 ~ "Industrials", 
                            sector == 6 ~ "Technology", 
                            sector == 7 ~ "Materials", 
                            sector == 8 ~ "Consumer Durables", 
                            sector == 9 ~ "Telecom", 
                            sector == 10 ~ "Utilities"))


# Ta bort NA rader

stock_data <- stock_data %>% 
  drop_na()


## Hämta kursutveckling
kursutveckling_6m <- kursutveckling(key, "6month")
kursutveckling_1year <- kursutveckling(key, "1year")



stock_data <- stock_data  %>% 
  left_join(kursutveckling_6m, by = "id")

stock_data <- stock_data %>% 
  left_join(kursutveckling_1year, by = "id")


#######################

stock_data <- stock_data %>% 
  select(-vinsttillväxt_3year_mean.x)


stock_data %>% 
  head()


###### Create some plots
stock_data %>% 
  select(sector, kursutveckling_6month) %>%
  ggplot(aes(kursutveckling_6month, sector)) + 
  geom_boxplot(aes(color = factor(sector)))


ggplot(mpg, aes(hwy, class)) + 
  geom_boxplot(aes(color = factor(class)))


#### Hitta Top Performers per sector
stock_data %>% 
  select(name,market, sector, kursutveckling_6month) %>% 
  group_by(sector) %>% 
  top_n(3) %>% 
  arrange(sector, desc(kursutveckling_6month)) %>% 
  ggplot(aes(x = reorder(name, factor(sector)), y = kursutveckling_6month, fill = factor(sector))) + 
  geom_bar(stat = "identity") + 
  coord_flip()


### Bäst performers i Tech & Healtchare

stock_data_health_tech <- stock_data %>% 
  filter(sector %in% c("Healthcare", "Technology"))

library(forcats)

stock_data_health_tech %>% 
  select(name, market, sector, omsättningstillväxt_1year_mean, kursutveckling_6month) %>% 
  group_by(sector) %>% 
  slice_max(order_by = kursutveckling_6month, n = 10) %>% 
  arrange(sector, desc(kursutveckling_6month)) %>% 
  ggplot(aes(reorder(x = name, kursutveckling_6month), y = kursutveckling_6month, fill = factor(sector), order = factor(sector))) +
  geom_bar(stat = "identity") + 
  coord_flip()


### Gör lite mer för att få igång Git
library(tidyverse)
library(ggplot2)
stock_data_health_tech %>% 
  select(name, market, sector, omsättningstillväxt_1year_mean, kursutveckling_6month) %>% 
  group_by(sector) %>% 
  arrange(desc(kursutveckling_6month)) %>% 
  filter(row_number() > max(row_number()) - 5 | row_number() <= 5)




## Hur ser soliditet ut per branch
stock_data %>% 
  group_by(sector) %>% 
  summarize(average_soliditet = mean(soliditet_latest, na.rm = TRUE)) %>% 
  arrange(desc(average_soliditet)) %>% 
  ggplot(aes(reorder(x = sector, average_soliditet), y = average_soliditet, fill = factor(sector))) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  scale_y_continuous(breaks = seq(0, 70, 5)) + 
  ggtitle("Medel soliditet per sektor") + 
  xlab("") + 
  ylab(("Medel soliditet")) + 
  guides(fill  = guide_legend(""))



stock_data %>% 
  filter(name == "Nobia") %>% 
  select(name, sector, soliditet_latest, omsättningstillväxt_1year_mean, peg_latest, 
         kursutveckling_1year, kursutveckling_6month)


# Testing out the dev branch
# Commenting some stuff
