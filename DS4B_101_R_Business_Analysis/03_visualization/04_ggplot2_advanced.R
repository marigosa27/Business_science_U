# DS4B 101-R: R FOR BUSINESS ANALYSIS ---- 
# ADVANCED BUSINESS PLOTS ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Lollipop Chart: Top N Customers ----
# - Great for showing order

# Question: How much purchasing power is in top 5 customers?
# Goal: Visualize top N customers in terms of Revenue, include cumulative percentage

n <- 10

# Data Manipulation

top_customers_tbl <- bike_orderlines_tbl %>% 
    select(bikeshop_name, total_price) %>% 
    mutate(bikeshop_name = as.factor(bikeshop_name) %>% fct_lump(n = n, w = total_price)) %>% 
    group_by(bikeshop_name) %>% 
    summarise(revenue = sum(total_price)) %>% 
    ungroup() %>% 
    
    mutate(bikeshop_name = bikeshop_name %>% fct_reorder(revenue)) %>% 
    
    # Thiis is really useful when we need to put compile categories at the bottom
    mutate(bikeshop_name = bikeshop_name %>% fct_relevel("Other", after = 0)) %>% 
    arrange(desc(bikeshop_name)) %>% 
    
    # Revenue text as dollars
    mutate(revenue_text = scales::dollar(revenue)) %>% 
    
    # Cummulative pct
    mutate(cum_pct = cumsum(revenue) / sum(revenue)) %>% 
    mutate(cum_pct_text = scales::percent(cum_pct)) %>% 

    # Make a rank
    mutate(rank = row_number()) %>% 
    mutate(rank = case_when(
        rank == max(rank) ~ NA_integer_, # this s telling the top to save as NA works with characters or doubles
        TRUE ~ rank
    )) %>% 
    
    # label text
    mutate(label_text = str_glue("Rank: {rank}\nRev: {revenue_text}\nCumPct: {cum_pct_text}"))

# Data Visualization

top_customers_tbl %>% 
    ggplot(aes(x= revenue, y = bikeshop_name)) +
    
    geom_segment(aes(xend = 0, yend = bikeshop_name),color = palette_light()[1]) +
    geom_point(color = palette_light()[1],
               aes(size = revenue)) +
    
    geom_label(aes(label = label_text),
               hjust = "inward",
               size = 4, 
               color = palette_light()[1]) +

    # Formatting 
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_fill_brewer(scales::dollar_format()) +
    labs(
        title =  str_glue("Top {n} Customers"),
        subtitle = str_glue("Start: {min(bike_orderlines_tbl$order_date)}
                            End {max(bike_orderlines_tbl$order_date)}"),
        x = "Revenue",
        y = "Customer",
        fill = "Revenue",
        caption = str_glue("Top 6 customers contribute 51% of purchasing power.  ")
        
            ) + 
    theme_tq()+
        theme(
            legend.position = "none",
            plot.title = element_text(face = "bold",size = 20),
            plot.subtitle = element_text(size = 16),
            axis.text = element_text(size = 16),
            axis.title = element_text(face= "bold", size = 16),
            plot.caption = element_text(face = "bold.italic", size = 14)
            
            )
        

# 2.0 Heatmaps ----
# - Great for showing details in 3 dimensions
# Question: Do specific customers have a purchasing preference?
# Goal: Visualize heatmap of proportion of sales by Secondary Product Category

# Data Manipulation

pct_sales_by_costumer_tbl <- bike_orderlines_tbl %>% 
    select(bikeshop_name, category_1, category_2, quantity) %>% 
    group_by(bikeshop_name, category_1, category_2) %>% 
    summarize(total_quantity = sum(quantity)) %>% +++
    ungroup() %>% 
    
    group_by(bikeshop_name) %>% 
    mutate(pct = total_quantity / sum(total_quantity)) %>% 
    ungroup() %>% 
    
    mutate(bikeshop_name = as.factor(bikeshop_name) %>% fct_rev()) %>%
    mutate(bikeshop_name_num = as.numeric(bikeshop_name))

pct_sales_by_costumer_tbl


# Data Visualization

pct_sales_by_costumer_tbl %>% 
    ggplot(aes(x = category_2, y = bikeshop_name)) +
    
    # Geometries
    geom_tile(aes(fill = pct)) +
    geom_text(aes(label = scales::percent(pct,accuracy = 0.1)),
              size = 3) +
    
    # separating mountain and road
    facet_wrap(~ category_1,scales = "free_x") +


    # formating section
    scale_fill_gradient(low = "white", high = palette_light()[1]) +
    
    labs(
        title = "Heat map of purchasing habits",
        x = "Bike Type (Category 2)",
        y = "Customer",
        caption = str_glue("Customers that prefer Road: Arbor Speed, 
                           Austin Cruiser, & Indianapolis Velocipedes
                           
                           Customers that prefer Mountain: Ithcaca Mountain
                           Climbers, Pittsborgh Moutian machiens an Whichita Speed.")) +
    theme_tq() +
    
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "bold.italic")
    )



























