# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CATEGORICAL DATA MANIPULATION ----

library(tidyverse)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl



# 1.0 Factor Basics ----
# Why factors: 
# The problem with text is
# Makes it easier to work with Categories
# Factor contains both character features and numeric features
# Factors allow you to group numeric values into ordered levels 
# low (1), medium(2), high(3)
# Makes its much easier to create categories from other categories
# Machine learning deals better with factors and numerical data. 



# What is a Factor?
# A way of managing categorical data

# Why do we want factors? 
# 1. Can group numeric values into bin (think price = low, medium, high)
# 2. Can reorder categories for visualization (fct_reorder)
# 3. Can manipulate categories much eaiser (fct_lump)
# 4. Machine learning and modeling algorithms may require factor data type for categorical data. 

# 2.0 Motivating Example -----

# Manipulation
sales_by_cat_2_tbl <- bike_orderlines_tbl %>% 
    select(category_2, total_price) %>% 
    group_by(category_2) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() %>% 
    arrange(desc(sales)) %>% 
    mutate(
        category_2 = category_2 %>% as_factor() %>% fct_rev()
    )

# Plotting

sales_by_cat_2_tbl %>% 
    ggplot(aes(x =  sales, y = category_2)) + 
    geom_point(size = 5, color = "#2c3e50") +
    labs(title = "Sales By Category 2") +
    scale_x_continuous(labels = scales::dollar_format())+
    theme_tq() +
    expand_limits(x = 0)


plot_sales <- function(data){
    data %>% 
        ggplot(aes(x =  sales, y = category_2)) + 
        geom_point(size = 5, color = "#2c3e50") +
        labs(title = "Sales By Category 2") +
        scale_x_continuous(labels = scales::dollar_format())+
        theme_tq() +
        expand_limits(x = 0)
}

sales_by_cat_2_tbl %>% 
    plot_sales()

# 3.0 Forcats Basics ----
# 3.1 Inspecting Factors ----

sales_by_cat_2_tbl %>% pull(category_2) %>% levels()

sales_by_cat_2_tbl %>% pull(category_2) %>% as.numeric()


# Vector

# Tibble

sales_by_cat_2_tbl %>% 
    mmutate(category_2 = category_2 %>% fct_rev() )
    mutate(
        label = category_2 %>% as.character(),
        value = category_2 %>% as.numeric()
    )



# 3.2 Creating Factors: as_factor() vs as.factor() ----
# forcasts _as_factor assigns facto values based on order in the vector.
# base::as.factor assigns factor values based on alphabetical order in the vector
sales_by_cat_2_tbl %>% 
        mutate(
            category_2 = as.character(category_2),
            category_2_as_factor = as_factor(category_2) %>% as.numeric(),
            category_2_as.factor = as.factor(category_2) %>% as.numeric()
        )

# 3.3 Reording Factors: fct_reorder() and fct_rev() ----
    
sales_by_cat_2_tbl %>% 
    arrange(desc(sales)) %>% 
        mutate(sales_negative = -sales) %>% 
        mutate(category_2 = category_2 %>% fct_reorder(sales_negative),
               values     = category_2 %>% as.numeric()) %>% 
        plot_sales()

# 3.4 Time-Based Reordering: fct_reorder2() ----

sales_by_cat_2_q_tbl <- bike_orderlines_tbl %>% 
        
        mutate(order_date = order_date %>% floor_date("quarter") %>% ymd()) %>% 
        group_by(category_2, order_date) %>% 
        summarise(sales = sum(total_price)) %>% 
        ungroup()
    
sales_by_cat_2_q_tbl %>% 
    
    mutate(category_2 = category_2 %>%  fct_reorder2(order_date, sales)) %>% 
    
    ggplot(aes(x = order_date, y = sales, color = category_2)) + 
    geom_point() +
    geom_line() +
    facet_wrap(~ category_2) +
    
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"))
        


# 3.5 Creating "Other" Category - fct_lump() & fct_relevel() ----
# fct_lump combines least common factors together into an other category

sales_by_cat_2_tbl %>% 
    mutate(category_2 = category_2 %>% fct_lump(n = 6,
                                                w = sales,
                                                other_level = "All Other Bike Categories")) %>% 
group_by(category_2) %>% 
    summarise(sales = sum(sales)) %>% 
    
    mutate(category_2 = category_2 %>% fct_relevel("All Other Bike Categories",
                                                   after = 1)) %>% 

plot_sales() 





