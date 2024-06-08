# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TIME-BASED MATH ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

# Differences between dates ands caracters

# 1.0 Date & Lubridate Basics ----
# 1.1 Character vs Date/Datetime
order_date_tbl <- bike_orderlines_tbl %>% 
    select(order_date)

order_date_tbl %>% 
    pull(order_date) %>% 
    class()
# 1.2 Date Classes
order_date_tbl %>% 
    mutate(order_date_ch = as.character(order_date)) %>% 
    mutate(order_date_ch2 = order_date_ch %>% str_c(" 00:00:00")) %>% 
    mutate(order_date_date = order_date_ch %>% ymd()) %>% 
    mutate(order_date_dttm = order_date_ch2 %>% ymd_hms())
# 1.3 Lubridate Functions

# Conversion a character string to date o date time class
"06/01/18" %>% mdy() %>% class()

"06/01/18 12:30:15" %>% mdy_hms()

"January,1,1995" %>% mdy()

# Extractor extract a part from the date to day time

"2011-01-01" %>% ymd() %>% year()

"2011-01-01" %>% ymd() %>% month(label = TRUE, abbr = TRUE)

"2011-01-01" %>% ymd() %>% wday(label = TRUE, abbr = FALSE)

"2011-01-01" %>% ymd() %>% day()
# Helpers useful when automating things
now()

today() %>% class()


# Periods & Durations - Add/subract time to/from a date
# Period: takes into account leap year and say light savings
# Durations: time spans that are physical time spans without time irregularity

today() + days(12)
today() + years(6) # Period

today() + ddays(12)
today() + dyears(6) # Duration


# Intervals - Calculate time-based distance 
# Calculating the intervals between two periods
i <- interval(today(), today() +ddays()) %>% ddays()

i / ddays(1) # intervals /days = how many days in interval 

i / dminutes(1)

i / dyears(1)

order_date_tbl %>% 
    mutate(today = today()) %>% 
    mutate(diff_days = interval(order_date, today) / ddays(1))

# 2.0 Time-Based Data Grouping ----
bike_sales_y_tbl <- bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    # lubridate
    mutate(order_date = ymd(order_date)) %>% 
    mutate(year = year(order_date)) %>% 
    group_by(year) %>% 
    summarise(
        sales = sum(total_price)) %>% 
    ungroup()

bike_sales_y_tbl  

bike_sales_m_tbl <- bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(order_date = ymd(order_date)) %>% 
    mutate(
        year  = year(order_date),
        month = month(order_date, label = T,abbr = F)
    ) %>% 
    group_by(year, month) %>% 
    summarise(
        sales = sum(total_price)
    ) %>% 
    ungroup()



# Combining month_year floor date
bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(order_date= ymd(order_date)) %>% 
    mutate(year_month = floor_date(order_date, unit = "month")) %>% 
    group_by(year_month) %>% 
    summarise(
        sales = sum(total_price)
    )

# 3.0 Measuring Change ----

# 3.1 Difference from most recent observation ----

bike_sales_y_tbl %>% 
    mutate(sales_lag_1 = lag(sales, n=1)) %>% 
    # Handle NA
    mutate(sales_lag_1 = case_when(
        is.na(sales_lag_1)~ sales,
        TRUE ~ sales_lag_1
    )) %>% 

    # Diff's & Pct Diffs
    mutate(diff_1 = sales - sales_lag_1) %>% 
    mutate(pct_diff_1 = diff_1 / sales_lag_1) %>% 
    mutate(pct_diff_1_Chr = scales::percent(pct_diff_1)) 

# Creating functions
# Step 1: create a name o bind the function
# Step 2: create arguments for the inputs
# step 3: create a body to process the inputs and return results.
# To make functions flexible use Tidy Eval framework
calculate_pct_diff <- function(data){
    data %>% 
    mutate(sales_lag_1 = lag(sales, n=1)) %>% 
        # Handle NA
        mutate(sales_lag_1 = case_when(
            is.na(sales_lag_1)~ sales,
            TRUE ~ sales_lag_1
        )) %>% 
        
        # Diff's & Pct Diffs
        mutate(diff_1 = sales - sales_lag_1) %>% 
        mutate(pct_diff_1 = diff_1 / sales_lag_1) %>% 
        mutate(pct_diff_1_Chr = scales::percent(pct_diff_1))
}

bike_sales_m_tbl %>% 
    calculate_pct_diff
    
# 3.2 Difference from first observation ----
# first get the first observations in a vector
bike_sales_y_tbl %>% 
    mutate(sales_2011 = first(sales)) %>% 
    mutate(diff_2011 = sales - sales_2011) %>% 
    mutate(pct_diff_2011 = diff_2011 / sales_2011) %>% 
    mutate(pct_diff_2011_chr = scales::percent(pct_diff_2011))

# to make sure that we compare to the first month of every year 
# the following is done
bike_sales_m_tbl %>% 
    group_by(year) %>% 
    mutate(sales_jan = first(sales)) %>% 
    mutate(
        diff_jan = sales -sales_jan,
        pct_diff_jan = diff_jan / sales_jan,
        pct_diff_jan_chr = scales::percent(pct_diff_jan)
    ) %>% 
    ungroup()


# 4.0 Cumulative Calculations ----
# As something increases we are adding it to the previous calculations

bike_sales_y_tbl %>% 
    mutate(cumulative_sales = cumsum(sales)) %>% 
    mutate(cumulative_pct = cumulative_sales / sum(sales))
    

bike_sales_m_tbl %>% 
    group_by(year) %>% 
    mutate(cumulative_sales =cumsum(sales)) %>% 
    mutate(cumulative_sales_pct = cumulative_sales / sum(sales)) %>% 
    mutate(cumulative_sales_pct_chr = scales::percent(cumulative_sales_pct)) %>% View(
    )

# 5.0 Rolling Calculations ----
# moving averages, rolling medians
# package zoo comes from tidyquant
# Rolling means expose the trend in a time series
# na.pad TRUE to added despite the non 3 multiplyer
# align is centered but you want right

bike_sales_m_tbl %>% 
    mutate(roll_mean_3 = rollmean(sales,k = 3,
                                  na.pad = TRUE,
                                  align = "right", 
                                  fill = 0)) %>% 
    mutate(roll_mean_6 = rollmean(sales, k = 6,
                                  na.pad = TRUE,
                                  align = "right",
                                  fill = NA))


# 6.0 Filtering Date Ranges ---- 
bike_orderlines_tbl %>% 
    mutate(order_date = ymd(order_date)) %>% 
    filter(order_date %>% between(ymd("2012-01-01"), ymd("2012-12-31")))

bike_orderlines_tbl %>% 
    mutate(order_date = ymd(order_date)) %>%
    filter(year(order_date) == 2012)

bike_orderlines_tbl %>% 
    mutate(order_date = ymd(order_date)) %>%
    filter(year(order_date) %in% c(2012,2013))

    
