# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TEXT MANIPULATION ----

library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl

bikes_tbl <- readxl::read_excel("00_data/bike_sales/data_raw/bikes.xlsx")

bikes_tbl


# 1.0 Basics ----

# 1.1 Detection: Used with filter() ----

# Vector

c("Supersix Evo Black Inc","Supersix Evo Hi-Mod Team") %>% 
    str_detect(pattern = "Supersix")


# Tibble
bikes_tbl %>% 
    select(model) %>% 
    mutate(supersix = model %>% str_detect("Supersix") %>% as.numeric()) %>% 
    mutate(black    = model %>% str_detect("Black") %>% as.numeric())


# 1.2 Case & Concatenation ----


# Case
bikeshop_name <- "Ithaca Mountain Climbers"
str_to_upper(bikeshop_name)
str_to_lower(bikeshop_name)
str_to_title(bikeshop_name)


# Concatenation

order_id <- 1
order_line <- 1

str_c("Order Line: ", order_id, ".", order_line,
    " sent to Customer: ", bikeshop_name    )

str_glue("Order Line: {order_id}.{order_line} sent to Customer:{str_to_upper(bikeshop_name)}")

# Tibble
bike_orderlines_tbl %>% 
    select(bikeshop_name, order_id, order_line) %>% 
    mutate(purchase_statement = str_glue("Order Line: {order_id}.{order_line} sent to Customer:{str_to_upper(bikeshop_name)}"
    )) %>% as.character()

# 1.3 Separating Text: See tidyr::separate() ----
# Vector
c("Road - Elite Road - Carbon", "Road - Elite Road") %>%  str_split(" - ",
                                                                    simplify = TRUE)


# Tibble
bikes_tbl %>% 
    select(description) %>% 
    separate(col = description, into = c("Category_1", "Category_2",
                                         "frame_material"),
             sep = " - ",
             remove = FALSE)



# 1.4 Trimming Text ----

"Text with spaces  " %>% str_trim(side = "right")


# 1.5 Replacement: Used with mutate() [and optionally case_when()] ----

# Vector
c("CAAD12","CAAD","CAAD8") %>% 
    str_replace(pattern = "[0-9]", replacement = "")

c("CAAD12","CAAD","CAAD8") %>% 
    str_replace_all(pattern = "[0-9]", replacement = "")

# Tibble

bikes_tbl %>% 
    select(model) %>% 
    mutate(
        model_num_removed = model %>% str_replace_all(pattern = "[0-9]",
                                                      replacement = " ") %>% str_trim()
    )


# 1.6 Formatting Numbers ----
value <- 1e6 
(1e6/1e6) %>% scales::number(prefix = "$", suffix = "M")

value %>%  scales::number(big.mark = ",")

value %>%  scales::dollar()

pct <- 0.15
pct %>%  scales::percent()

pct %>% scales::number(scale = 100, suffix = "%")


# 1.7 Formatting Column Names ----

# Replacing text in column names
bike_orderlines_tbl %>% 
    set_names(names(.) %>% str_replace("_", ".") %>% str_to_upper()) 
    
# Appending text to column names

bike_orderlines_tbl %>% 
    set_names(str_glue("{names(.)}_bike"))


# Appending text to specific column names
bike_orderlines_colnames_tbl <- bike_orderlines_tbl %>% 
    rename_at(.vars = vars(model:frame_material),
              .funs = ~str_c("prod_",.)) %>% 
    rename_at(vars(bikeshop_name:state),
               ~str_c("cust_",.)) %>% 
    glimpse()

bike_orderlines_colnames_tbl %>% 
    select(contains("cust_"), total_price)

# 2.0 Feature Engineering with Text -----
# Investigating "model" and extracting well-formatted features


bikes_tbl %>% 
    select(model) %>%
    
    # Fixing typos in rows
    mutate(model = case_when(
        model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
        model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
        model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod ultegra",
        TRUE ~ model
        )) %>%  
    
    # separate using the space
    separate(col   = model,
             into  = str_c("model_",1:7),
             sep   = " ", remove = FALSE,
             fill  = "right",
             extra = "drop") %>% 
    
    # Creating a "base" feature
    mutate(model_base = case_when(
        
        # Fix Supersix Evo
        str_detect(str_to_lower(model_1),"supersix") ~ str_c(model_1,
                                                            model_2,
                                                            sep = " "),
        # Fix Fat CAAD bikes
        str_detect(str_to_lower(model_1),"fat") ~ str_c(model_1,
                                                             model_2,
                                                             sep = " "),
        str_detect(str_to_lower(model_1),"beast") ~ str_c(model_1,
                                                          model_2,
                                                          model_3,
                                                          model_4,
                                                          sep = " "),
        str_detect(str_to_lower(model_1),"bad") ~ str_c(model_1,
                                                        model_2,
                                                        sep = " "),
        # Scalpel
        str_detect(str_to_lower(model_1), "scalpel") & str_detect(str_to_lower(model_2),
                                                        "29") ~ str_c(model_1,
                                                        model_2,
                                                        sep = " "),
        TRUE ~ model_1)
        ) %>% 
    
    # Get "tier" feature
    # subtracting the categorical content from a column from another
    mutate(model_tier = model %>% str_replace(model_base, replacement = "")
           %>% str_trim()) %>% 
    
    # remove unnecessary columns 
    select(-matches("[0-9]")) %>% 
    
    mutate(
        black    = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
        Hi_Mod   = model_tier %>% str_to_lower() %>% str_detect("Hi-Mod") %>% as.numeric(),
        team     = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
        Red      = model_tier %>% str_to_lower() %>% str_detect("Red") %>% as.numeric(),
        ultegra  = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
        dura_ace = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
            disc     = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
    ) %>% 
    
    View() 
    
    


    
    
    
    