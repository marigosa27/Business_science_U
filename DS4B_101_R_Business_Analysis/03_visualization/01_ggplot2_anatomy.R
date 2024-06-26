# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# ANATOMY OF A GGPLOT2 OBJECT ----


library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Anatomy of a ggplot ----
# 1.1 How ggplot works ----

# Step 1: Format data ----
revenue_by_year_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    
    group_by(year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup()

revenue_by_year_tbl

# Step 2: Plot ----

g <- revenue_by_year_tbl %>%
    
    # Canvas
    ggplot(aes(x = year, y = revenue, color = revenue)) +
    
    # Geometries 
    geom_line(size = 1) +
    geom_point(size = 5) +  # if we want the points to be the size of the revenue    geom_point(aes(size = (revenue))
    geom_smooth(method = "lm", se = FALSE) +
    
    # Formatting
    expand_limits(y = 0) + # 
    scale_color_continuous(low = "red", high = "black",
                           labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
    labs(
        title = "Revenue",
        subtitle = "Sales are trending up and to the right!",
        x = "",
        y = "Sales (Millions)",
        color = "Rev ($M)",
        caption = "What's happening?\nSales numbers showing year-over-year growth."
    ) +
    theme_bw() +
    theme(legend.position = "right", legend.direction = "vertical")

# 1.2 What is a ggplot? ----

g

View(g)
