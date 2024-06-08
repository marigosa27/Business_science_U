# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# FORMATTING GGPLOTS ----

# Libraries & Data ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

# Data Manipulation

sales_by_year_category_2_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(year = year(order_date)) %>%
    
    group_by(category_2, year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup() %>%
    
    mutate(category_2 = fct_reorder2(category_2, year, revenue))


# 1.0 Working with Colors ----
colors()
# 1.1 Color Conversion ----
# Named Colors
sales_by_year_category_2_tbl %>% 
    ggplot(aes(x = year, y = revenue)) +
    # geom_col(fill = "turquoise3")
    geom_col(fill = RColorBrewer::brewer.pal(n= 100, name = "Blues")[4])
    #geom_col(fill =viridisLite::viridis(n=20)[15])

# To RGB
col2rgb("turquoise3")

col2rgb("#2C3E50")

# To HEX
rgb(44,62,80, maxColorValue = 255)

# 1.2 Color Palettes ----
# tidyquant
tidyquant::palette_light()

palette_light()[1]

palette_light() %>% col2rgb()

# Brewer
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info
RColorBrewer::brewer.pal(n= 9, name = "Blues")[9]

# Viridis
viridisLite::viridis(n=20)[1]


# 2.0 Aesthetic Mappings ----
# 2.1 Color  -----
# - Used with line and points, Outlines of rectangular objects
# it shows ugl becasue its not grouped
# Usine colors as aesthetics
sales_by_year_category_2_tbl %>% 
    
    ggplot(aes(x = year, y =  revenue))+
    
    geom_line() +
    geom_point()

sales_by_year_category_2_tbl %>% 
    # globally specificed grouping with the color
    ggplot(aes(x = year, y =  revenue, color = category_2))+
    # aes its only needed when mapping to a column
    geom_line(aes(color = category_2, size = 1)) +
    geom_point(color = "dodgerblue", aes(size = revenue))


# 2.2 Fill  -----
# - Used with fill of rectangular objects 
# fill will create a stack bar chart
# if you use color instead of fill it will only do the outline
sales_by_year_category_2_tbl %>% 
    ggplot(aes(x = year, y = revenue, fill = category_2)) +
    geom_col()


# 2.3 Size ----
# - Used with points

sales_by_year_category_2_tbl %>% 
    ggplot(aes(x = year, y = revenue)) +
    
    geom_line(aes(color = category_2)) +
    geom_point(aes(size = revenue))



# 3.0 Faceting ----
# - Great way to tease out variation by category
# Goal: Sales annual sales by category 2
sales_by_year_category_2_tbl %>% 
    select(year, category_2, revenue) %>% 
    
    ggplot(aes(x = year, y = revenue,color = category_2)) +
    
    geom_line(color = "black") +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ category_2, ncol = 3, scales = "free_y") +
    expand_limits(y = 0)
    

# 4.0 Position Adjustments (Stack & Dodge) ----
# Stacked Bars & Side-By-Side Bars
sales_by_year_category_2_tbl %>% 
    ggplot(aes(x = year, y = revenue, fill = category_2)) +
    
    # geom_col(position = "stack")
    # geom_col(position = "dodge")
    geom_col(position = position_dodge(width = 0.9), color = "white") +
    theme_tq()

# Stacked Area

sales_by_year_category_2_tbl %>% 
    ggplot(aes(x = year, y = revenue, fill = category_2)) +
    
    geom_area(color = "black")

# 5.0 Scales (Colors, Fills, Axis) ----

sales_by_year_category_2_tbl %>% 
    ggplot(aes(x = year, y = revenue, fill = category_2)) +
    
    geom_area(color = "black")



# 5.1 Plot Starting Points ----
# - Continuous (e.g. Revenue): Changes color via gradient palette
# - Categorical (e.g. ): Changes color via discrete palette

# Plot 1: Faceted Plot, Color = Continuous Scale
g_graph_continuous <- sales_by_year_category_2_tbl %>% 
    ggplot(aes(x = year, y = revenue, color = revenue)) +
    
    geom_line(size = 1) +
    geom_point(size = 3) +
    
    facet_wrap(~ category_2) +
    expand_limits(y = 0) +
    theme_minimal()

g_graph_continuous

# Plot 2: Faceted Plot, Color = Discrete Scale
g_graph_descrete <- sales_by_year_category_2_tbl %>% 
    ggplot(aes(x = year, y = revenue, color = category_2)) +
    
    geom_line(size = 1) +
    geom_point(size = 3) +
    
    facet_wrap(~ category_2) +
    expand_limits(y = 0) +
    theme_minimal()

g_graph_descrete


# Plot 3: Stacked Area Plot

g_area_descrete <- sales_by_year_category_2_tbl %>% 
    ggplot(aes(x = year, y = revenue, fill = category_2)) +
    
    geom_area(color = "black") +
    theme_minimal()

g_area_descrete


# 5.2 Scale Colors & Fills ----
# - Awesome way to show variation by groups (discrete) and by values (continuous)

# Color by Revenue (Continuous Scale)
g_graph_continuous +
    # scale_color_continuous(
    #     low = "cornflowerblue",
    #     high = "black"
    # ) 
    scale_color_viridis_c(option = "H", direction = -1)

# Color by Category 2 (Discrete Scale)

RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info
RColorBrewer::brewer.pal(n= 100, name = "Blues")[7]

g_graph_descrete +
    scale_color_brewer( palette = "BrBG") +
    theme_tq()

g_graph_descrete +
    scale_color_tq() +
    theme_tq()

g_graph_descrete +
    scale_color_viridis_d(option = "C") +
    theme_tq()


# Fill by Category 2

g_area_descrete +
    scale_fill_brewer(palette = "Set1")

g_area_descrete +
    scale_fill_tq()

g_area_descrete +
    scale_fill_viridis_d(option = "H")
    

# 5.3 Axis Scales ----

g_graph_continuous +
    scale_x_continuous(breaks = seq(2011, 2015, by = 2)) +
    scale_y_continuous(labels = scales::dollar_format())
    #scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"))


# 6.0 Labels ----
# scale_color_viridis_c(label = scales::comma_format(big.mark = ","))


g_graph_continuousV2 <- g_graph_continuous +
    scale_x_continuous(breaks = seq(2011, 2015, by = 2)) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    
    geom_smooth(method = "lm", se = FALSE) +
    
    scale_color_viridis_c(option = "H", direction = -1, label = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    theme_minimal() +
    labs(
        title = "Bike Sales",
        subtitle = "Sales are trending up",
        caption = "5-year sales trends income form our ERP Database",
        x = "Year",
        y = "Revenue ($M)",
        color = "Revenue"
    )

View(g_graph_continuousV2)
# 7.0 Themes  ----

g_graph_continuous +
    theme_light() +
    # Make changes in the theme
    theme(
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        strip.background = element_rect(colour = "black",
                                        fill = "cornflowerblue",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  color = "white")
        
    )


# 8.0 Putting It All Together ----

sales_by_year_category_2_tbl %>% 
    ggplot(aes(x= year, y =  revenue, fill = category_2)) +
    
    geom_area(color = "black") +
    scale_fill_brewer(palette = "Blues",direction = -1) +
    
    labs(
        title = "Sales Over Year By Category 2",
        subtitle = 'Sales trending up',
        caption = "Bike Sales trends stron heading into 2016",
        x = " ",
        y =  "Revenue",
        fill = "2nd Category"
    ) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_light() +
        theme(
            title = element_text(face = "bold", colour = "#08306B") 
        )
    







