# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# DATA WRANGLING OVERVIEW ----


library(tidyverse)
library(readxl)

bikes_tbl           <- read_excel("00_data/bike_sales/data_raw/bikes.xlsx")
orderlines_tbl      <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")


bikes_tbl

orderlines_tbl

bike_orderlines_tbl %>% glimpse()

# 1.0 Selecting Columns with select() ----

# Basic select
bike_orderlines_tbl %>%
    select(order_date, order_id, order_line)

bike_orderlines_tbl %>%
    select(1:3)

bike_orderlines_tbl %>%
    select(starts_with("order_"))

# Reduce columns

bike_orderlines_tbl %>%
    select(order_date, total_price, category_1, category_2)

# Rearange columns

bike_orderlines_tbl %>%
    select(bikeshop_name:state, everything())

# Select helpers

?starts_with

bike_orderlines_tbike_orderlines_tbl %>% 
    pull(model)bl %>%
    select(starts_with("price"))

# pull()

bike_orderlines_tbl %>%
    # select(total_price) %>%
    pull(total_price) %>%
    mean()



# select_if

?select_if

bike_orderlines_tbl %>%
    select_if(is.character) 

bike_orderlines_tbl %>%
    select_if(~ is.numeric(.))

bike_orderlines_tbl %>%
    select_if(~ !is.numeric(.))


# 2.0 Arranging with arrange() and desc() ----

bikes_tbl %>%
    select(model, price) %>%
    arrange(desc(price)) %>%
    View()





# 3.0 Filtering Rows with filter() ----

# 3.1 filter(): formula filtering ----

bikes_tbl %>%
    select(model, price) %>%
    filter(price > mean(price))

bikes_tbl %>%
    select(model, price) %>%
    filter((price > 5000) | (price < 1000)) %>%
    arrange(desc(price)) %>%
    View()

bikes_tbl %>%
    select(model, price) %>%
    filter(price > 6000,
           model %>% str_detect("Supersix"))

# Filtering One or More Conditions Exactly Using == and %in%
bike_orderlines_tbl %>%
    filter(category_2 %in% c("Over Mountain", "Trail", "Endurance Road"))

bike_orderlines_tbl %>%
    filter(category_2 == "Over Mountain")

bike_orderlines_tbl %>%
    filter(category_2 != "Over Mountain")

bike_orderlines_tbl %>%
    filter(!(category_2 %in% c("Over Mountain", "Trail", "Endurance Road")))


# 3.2 slice(): filtering with row number(s) ----

bikes_tbl %>%
    arrange(desc(price)) %>%
    slice(1:5)

bikes_tbl %>%
    arrange(price) %>%
    slice(1:5)

bikes_tbl %>%
    arrange(desc(price)) %>%
    slice((nrow(.)-4):nrow(.))

bikes_tbl %>%
    arrange(desc(price)) %>%
    slice((93):97)

# 3.3 distinct(): Unique values

bike_orderlines_tbl %>%
    distinct(category_1)

bike_orderlines_tbl %>%
    distinct(category_1, category_2)

bike_orderlines_tbl %>%
    distinct(bikeshop_name, city, state)


# 4.0 Adding Columns with mutate() ----
bike_orderlines_prices <- bike_orderlines_tbl %>% 
    select(order_date, model, quantity, price) %>% 
    mutate(total_price = quantity * price)

# transformations
bike_orderlines_prices %>% 
    mutate(total_price_log = log(total_price)) %>% 
    mutate(total_price_sqrt = total_price^0.5)

# Adding a flag or Feature engineering
bike_orderlines_prices %>% 
mutate(is_supersix = model %>% str_to_lower() %>% str_detect("supersix")) %>% 
filter(is_supersix)

# Binning with ntile()
# ntile makes it into bins basically allowing the creation of high low or medium
# binning useful for grouping into cohorts and detecting relationships within continuous variables
bike_orderlines_prices %>% 
    mutate(total_price_binned = ntile(total_price, 3))

# if then statements inside a mutate
# Numeric to categorical 
bike_orderlines_prices %>% 
    mutate(total_price_binned = ntile(total_price, 3)) %>% 
    mutate(total_price_binned2 = case_when(
        total_price > quantile(total_price, 0.75) ~ "High",
        total_price > quantile(total_price, 0.23) ~ "Medium",
        TRUE ~ "Low"
    ))

# equal ranges 0.33 0.66
# Internationale ranges 0.76 and 0.25

# Text to categorical
bike_orderlines_prices %>% 
    mutate(bike_type = case_when(
        model %>% str_to_lower() %>% str_detect("supersix") ~ "supersix",
        model %>% str_to_lower() %>% str_detect("jekyll") ~ "Jekyll",
        TRUE ~ "not supersix or Jekyll"
    ))

# 5.0 Grouping & Summarizing with group_by() and summarize() ----
bike_orderlines_tbl %>% 
    summarise(
        revenue = sum(total_price)
    )

bike_orderlines_tbl %>% 
    group_by(category_1,category_2, frame_material) %>% 
    summarise(
        revenue = sum(total_price)) %>% 
    ungroup() %>% 
    arrange(desc(revenue))

# Summary functions
bike_orderlines_tbl %>% 
    group_by(category_1, category_2) %>% 
    summarize(
        count = n(),
        avg = mean(total_price),
        med = median(total_price),
        sd = sd(total_price),
        min = min(total_price),
        max = max(total_price)
    ) %>%  ungroup() %>% 
arrange(desc(count))

# summarize all (applies to all the columns) detect missing values

bike_orderlines_missing <- bike_orderlines_tbl %>% 
    mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))
# adds values to the first four rowsmaking sure it keeps the other values in the column

# detecting missing values
bike_orderlines_missing %>% 
    summarise_all(~sum(is.na(.)))

# Percentage of missing values
bike_orderlines_missing %>% 
    summarise_all(~sum(is.na(.)) / length(.))

# Removing the missing values

bike_orderlines_missing %>% 
    filter(!is.na(total_price))


# 6.1 rename: One column at a time ----
bike_shop_revenue_tbl<- bike_orderlines_tbl %>% 
    select(bikeshop_name, category_1, total_price) %>% 
    group_by(bikeshop_name, category_1) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() %>%
    arrange(desc(sales)) %>% 
    rename(`Bikeshop Name` = bikeshop_name
           ,`Primary Category` = category_1
           , Sales = sales)

# 6.2 set_names: All columns at once ---
bike_shop_revenue_tbl %>% 
    set_names(c("Bikeshop Name","Primary Category","Sales"))

bike_shop_revenue_tbl %>% 
    set_names(names(.) %>%  str_replace("_", "_") %>%  str_to_title())


# 7.0 Reshaping (Pivoting) Data with spread() and gather() ----
# 7.1 spread(): Long to Wide ----
bikeshop_revenue_formated_tbl<- bike_shop_revenue_tbl %>% 
    spread(key = `Primary Category`, value = Sales) %>% 
    arrange(desc(Mountain)) %>% 
    mutate(Mountain = scales::dollar(Mountain),
           Road = scales::dollar(Road)
           )

# 7.2 gather(): Wide to Long ----
bikeshop_revenue_formated_tbl %>% 
    gather(key = "category_1", value = "sales", Mountain, Road) %>% 
    mutate(sales = sales %>% str_remove_all("\\$|,") %>% as.double()) %>% 
    arrange(desc(sales))

# 8.0 Joining Data by Key(s) with left_join() (e.g. VLOOKUP in Excel) ----
orderlines_tbl  %>% 
    left_join(y = bikes_tbl, by = c("product.id" = "bike.id"))

# 9.0 Binding Data by Row or by Column with bind_rows() and bind_col() ----

# 9.1 bind_cols() ----
bike_orderlines_tbl %>% 
    select(-contains("order")) %>% 
    
    bind_cols(
        bike_orderlines_tbl %>% select(order_id)
    )

# 9.2 bind_rows() ----

train_tbl <- bike_orderlines_tbl %>% 
    slice(1:(nrow(.)/2))


test_tbl <- bike_orderlines_tbl %>% 
    slice(nrow(.)/2 + 1:nrow(.))

together <- train_tbl %>% 
    bind_rows(test_tbl)

# Separate and unite function
bike_orderlines_tbl %>% 
    select(order_date) %>% 
    mutate(order_date = as.character(order_date)) %>% 
    separate(order_date,sep = "-",into = c("year", "month","day"),remove = F) %>% 
    mutate(
        year  = as.numeric(year),
        month = as.numeric(month),
        day   = as.numeric(day)
    ) %>%
    
    # unite
    unite(order_date_united, year, month, day, sep = "-", remove = F)






