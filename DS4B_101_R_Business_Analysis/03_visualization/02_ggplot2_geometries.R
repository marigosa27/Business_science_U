# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# Types of Graphs: ggplot2 Geometries ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Point / Scatter Plots ----
# - Great for Continuous vs Continuous
# - Also good for Lollipop Charts (more on this in advanced plots)

# Goal: Explain relationship between order value and quantity of bikes sold

# Data Manipulation
order_value_tbl <- bike_orderlines_tbl %>% 
    select(order_id, order_line, total_price, quantity) %>% 
    group_by(order_id) %>% 
    summarize(
        total_quantity = sum(quantity),
        total_price    = sum(total_price)
    ) %>% 
    ungroup()


# Scatter Plot

order_value_tbl %>% 
    
    ggplot(aes(x = total_quantity, y = total_price)) +
    
    geom_point(alpha = 0.5, size = 2) +
    geom_smooth(method = "lm", se = FALSE)

# 2.0 Line Plots ----
# - Great for time series
# Goal: Describe revenue by Month, expose cyclic nature

# Data Manipulation
revenue_by_month_tbl <- bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(
        year_month = floor_date(order_date, "months") %>% ymd() 
    ) %>% 
    group_by(year_month) %>% 
    summarize( revenue = sum(total_price)) %>% 
    ungroup()
    
# Line Plot

revenue_by_month_tbl %>% 
    ggplot(aes(x = year_month, y = revenue)) +
    geom_line(size = 1, linetype = 1) +
    geom_smooth(method = "loess", span = 0.3)


# 3.0 Bar / Column Plots ----
# - Great for categories
# Goal: Sales by Descriptive Category

# Data Manipulation

revenue_by_category_2_tbl <- bike_orderlines_tbl %>%
    select(category_2, total_price) %>% 
    group_by(category_2) %>% 
    summarize(revenue = sum(total_price))

# Bar Plot
revenue_by_category_2_tbl %>% 
    mutate(category_2 = category_2 %>% as.factor() %>% 
               fct_reorder((revenue))) %>% 
    ggplot(aes(x = category_2, y = revenue)) +
    geom_col(fill = "blue") +
    coord_flip()

# Use geom bar when it is univariate instead of multivariate


# 4.0 Histogram / Density Plots ----
# - Great for inspecting the distribution of a variable
# Goal: Unit price of bicycles
# Histogram

bike_orderlines_tbl %>% 
    select(model, price) %>% 
    distinct(model, price) %>% 
    ggplot(aes(price)) +
    geom_histogram(bins = 25, fill = "blue", color = "white")


# Goal: Unit price of bicylce, segmenting by frame material
# Histogram

bike_orderlines_tbl %>% 
    distinct(price, model, frame_material) %>% 
    ggplot(aes(price, fill = frame_material)) + 
    
    geom_histogram() +
    facet_wrap(~frame_material,ncol = 1)  +
    
    scale_fill_tq() +
    theme_tq()

# Density

bike_orderlines_tbl %>% 
    distinct(price, model, frame_material) %>% 
    ggplot(aes(price, fill = frame_material)) +
    
    geom_density(alpha = 0.5) +
    scale_fill_tq() +
    theme_tq()


# 5.0 Box Plot / Violin Plot ----
# - Great for comparing distributions

price_category_2_tbl <- bike_orderlines_tbl %>% 
    select(category_2, model, price) %>% 
    distinct() %>% 
    mutate(category_2 = as.factor(category_2) %>% 
    fct_reorder(price))

price_category_2_tbl %>% 
    ggplot(aes(x = category_2, y = price)) +
    geom_boxplot() +
    coord_flip() +
    theme_tq()
# Violin Plot & Jitter Plot
price_category_2_tbl %>% 
    ggplot(aes(x = category_2, y = price)) +
    geom_jitter(width = 0.15, color = "#2c3e50") +
    geom_violin(alpha = 0.5) +
    coord_flip() +
    theme_tq()
    


# 6.0 Adding Text & Labels ----
# Goal: Exposing sales over time, highlighting outlier
# Data Manipulation

revenue_by_year_tbl <- bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(year = year(order_date)) %>% 
    group_by(year) %>% 
    summarize(revenue = sum(total_price)) %>% 
    ungroup()

# Adding text to bar chart
# Filtering labels to highlight a point
revenue_by_year_tbl %>% 
    
    ggplot(aes(x = year, y = revenue)) + 
    
    geom_col(fill = "#2c3e50") +
    
    geom_smooth(method = "lm", se = FALSE) +
    
    geom_text(aes(label = scales::dollar(revenue, scale = 1e-6, suffix = "M")),
              vjust = 1.5, color = "white") +
    
    # geom_label(aes(label = scales::dollar(revenue, scale = 1e-6, suffix = "M")),
    #            vjust = -0.5) +
    # geom_label(label = "Major Demand This Year",
    #                vjust = -0.5,
    #                data = revenue_by_year_tbl %>% filter(year == 2013)) +
    
    geom_label(label = "Major Demand This Year",
               vjust = -0.5,
               size =  5, 
               fill = "#1F78B4",
               color = "white",
               data = revenue_by_year_tbl %>% filter(year %in% c(2013, 2015))) +
    
    expand_limits(y = 2e7) +
    theme_tq() 
    
    








