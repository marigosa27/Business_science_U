# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# ITERATION WITH PURRR ----

library(readxl)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(broom)

bike_orderlines_tbl <- read_rds("DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 PRIMER ON PURRR ----
# Programmatically getting Excel files into R
# dir info returns information on all files in a directory
excel_path_tbl <- fs::dir_info("DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/")

# this is a character vector of this paths 
paths_chr <- excel_path_tbl %>% 
    pull(path)


# What Not To Do: Don't use for loops
excel_list <- list()

for (path in paths_chr){
    excel_list[[path]] <- read_excel(path)   
}

View(excel_list)

# What to Do: Use map()
?map
# function name
excel_lists_2 <- paths_chr %>% 
    map(read_excel) %>% 
    set_names(paths_chr)

# Anonimous function
# this one seems to be the easiest 
paths_chr %>% 
    map(~ read_excel(.))

# Function specified with function()
paths_chr %>% 
    map(function(x) read_excel(path = x))

# I personatelly thing this is easier
files_2017 <- list.files("Data/JSLC2017/", pattern = ".sav")
files_2017 <- stringr::str_c("Data/JSLC2017/",files_2017)
files_2017
res3 <- sapply(files_2017, read.spss, to.data.frame = TRUE)
list2env(res3,envir=.GlobalEnv)


# Reading Excel Sheets

excel_sheets("DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/bikes.xlsx") %>% 
    map(~ read_excel(path = "DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/bikes.xlsx",
                     sheet = .))


# 2.0 MAPPING DATA FRAMES ----
# Remember that a data frame is a list
bike_orderlines_tbl %>% is.list()

# 2.1 Column-wise Map ----
bike_orderlines_tbl%>% 
    map(~ class(.))

# 2.2 Map Variants ----
# Character map
bike_orderlines_tbl %>% 
    map_chr(~class(.)[1])

# Data Frame Map
bike_orderlines_tbl %>% 
    map_df(~class(.)[1]) %>% 
    gather()

bike_orderlines_tbl %>% 
    map_df(~ length(.))

bike_orderlines_tbl %>% 
    map_df(~ sum(is.na(.)) / length(.)) %>% 
    gather()

# 2.3 Row-wise Map ----

excel_tbl <- excel_path_tbl %>% 
    select(path) %>% 
    mutate(data = path %>% map(read_excel))

# In case the one above does not work 
# excel_path_tbl %>%
#     as_tibble() %>% 
#     select(path) %>% 
#     mutate(data = path %>% map(read_excel))

excel_list

excel_tbl

# 3.0 NESTED DATA ----
# Unnest
# Gets all three data frames nested within it
excel_tbl$data

# if we only want the first one 
excel_tbl$data[[1]]

excel_tbl$data[[3]]

# This combines all data frames into it 
# because the number does not match this creates a bunch of NAs values
excel_tbl %>% 
    tidyr::unnest(data) %>% 
    View()

# the ID will add an ID colum so each of the dataframes grouped will have
# their own ID
excel_tbl_unnested <- excel_tbl %>% 
    tidyr::unnest_legacy(data, .id = "ID")

excel_tbl_unnested
# Nest

excel_tbl_nested <- excel_tbl_unnested %>% 
    group_by(ID, path) %>% 
    nest()
 # unfortunatelly this does not drop the NA values so we have to drop them

excel_tbl_nested$data

# Mapping Nested List Columns

excel_tbl_nested$data[[1]] %>% 
    select_if(~ !is.na(.) %>% 
    # all drops the entire column if there are all NAs
    all())

# Method 1: Creating a function outside of purrr::map()

select_non_na_columns <- function(data){
    data %>% 
    select_if(~ !is.na(.) %>% all())
}

#  Part 2 Testing the fuction
excel_tbl$data[[2]] %>% 
    select_non_na_columns()

excel_tbl_nested$data[[2]] %>% 
    select_non_na_columns()

# Applying the function to the nested datasets all at once
excel_tbl_nested_fixed <- excel_tbl_nested %>% 
    mutate(data = data %>% map(select_non_na_columns))

excel_tbl_nested$data[[3]]
excel_tbl_nested_fixed$data

# 4.0 MODELING WITH PURRR ----

# 4.1 Time Series Plot ----
#  - What if we wanted to approximate the 3 month rolling average with a line?
#  - We can use a smoother

# Code comes from 04_functions_iteration/01_functional_programming
rolling_avg_3_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_1, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(month_end = ceiling_date(order_date, unit = "month") - period(1, unit = "days")) %>%
    
    group_by(category_1, category_2, month_end) %>%
    summarise(
        total_price = sum(total_price)
    ) %>%
    mutate(rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = "right")) %>%
    ungroup() %>%
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price)) 

rolling_avg_3_tbl %>%
    
    ggplot(aes(month_end, total_price, color = category_2)) +
    
    # Geometries
    geom_point() +
    geom_line(aes(y = rolling_avg_3), color = "blue", linetype = 1) +
    facet_wrap(~ category_2, scales = "free_y") +
    
    # Add Loess Smoother
    geom_smooth(method = "loess", se = FALSE, span = 0.2, color = "black") +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))


# 4.2 Modeling Primer ----
# Data Preparation
sales_by_month_end_cross_country_tbl <- rolling_avg_3_tbl %>% 
    filter(category_2 == "Cross Country Race") %>% 
    
    select(month_end, total_price) %>% 
    mutate(month_end_num = as.numeric(month_end)) 

sales_by_month_end_cross_country_tbl %>% 
    ggplot(aes(x = month_end_num, y = total_price)) +
    geom_point() + 
    geom_smooth(method = "loess", span = 0.2, se = FALSE)

# Making a loess model
fit_loess_cross_country <- sales_by_month_end_cross_country_tbl %>% 
    loess(total_price ~ month_end_num, data = ., span = 0.2)

fit_loess_cross_country 

# Working With Broom

fit_loess_cross_country %>% 
    broom::augment() %>%
    
# Visualizing results    
    ggplot(aes(x = month_end_num, y = total_price)) +
    geom_point() + 
    geom_line(aes(y = .fitted), color = "blue")
    
# 4.3 step 1: Function To Return Fitted Results ----

rolling_avg_3_tbl_nested <- rolling_avg_3_tbl %>% 
    group_by(category_1, category_2) %>% 
    nest()

data <- rolling_avg_3_tbl_nested$data[[1]] 

tidy_loees <- function(data){
    
    data_formatted <- data %>% 
        select(month_end, total_price) %>% 
        mutate(month_end_num = as.numeric(month_end))
    
    fit_loess <- loess(formula = total_price ~ month_end_num,
                          data = data_formatted,
                          span = 0.2)
    
    output_tbl <- fit_loess %>% 
        broom::augment() %>% 
        select(.fitted)
    
    return(output_tbl)
} 

# 4.4 Test Function on Single Element ----
rolling_avg_3_tbl_nested$data[[3]] %>% 
    tidy_loees()

# 4.5 Map Function to All Categories ----
# Map Functions

loees_tbl_nested <- rolling_avg_3_tbl_nested %>% 
    mutate(fitted = data %>% map(tidy_loees))

loees_tbl_nested %>% 
    unnest()



# Visualize Results

loees_tbl_nested %>% 
    unnest() %>% 
    ggplot(aes(x = month_end, y = total_price, color = category_2)) +
    
    geom_point() +
    geom_line(aes(y = .fitted, color = "blue")) +
    geom_smooth(method = "loess", span = 0.2) +
    
    facet_wrap(~ category_2, scales = "free_y")



