# WEBSCRAPING ----
# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(httr)      #GET() function

# 1.1 COLLECT PRODUCT FAMILIES ----

url_home          <- "https://www.rosebikes.com/"
#xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Extract the urls from the href attribute
bike_category_tbl <- html_home %>%
  
  # Select nodes by the ids
  html_nodes(css = ".main-navigation-category-with-tiles__item > a") %>%
  html_attr('href') %>%

  discard(.p = ~stringr::str_detect(.x,"sale")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%

  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.rosebikes.com{subdirectory}")) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)

# 2.0 COLLECT BIKE DATA ----

# 2.1 Get URL for each bike of the Product categories

# select first bike category url  
bike_category_url <- bike_category_tbl$url[1]

# Get the URLs for the bikes of the first category
html_bike_category  <- read_html(bike_category_url)

bike_url_tbl        <- html_bike_category %>%
  
  # Select nodes by the ids
  html_nodes(css = ".row.align-middle > a") %>%
  
  html_attr('href') %>%
  
  enframe(name = "position1", value = "sub") %>%

# Add the domain, because we will get only the subdirectories
mutate(
  url = glue("https://www.rosebikes.com{sub}")) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)


# 2.1.2 Extract the descriptions (since we have retrieved the data already)
bike_desc_tbl <- html_bike_category %>%
  
  # Get the nodes in the meta tag where the attribute itemprop equals description
  html_nodes(".catalog-category-bikes__subtitle") %>%
  
  # Extract the content of the attribute content
  html_text() %>%
  
  # Convert vector to tibble
  enframe(name = "position2", value = "description")



# 2.1.2 Extract the name (since we have retrieved the data already)
bike_name_tbl <- html_bike_category %>%
  
  # Get the nodes in the meta tag where the attribute itemprop equals description
  html_nodes(".catalog-category-bikes__title > span") %>%
  
  # Extract the content of the attribute content
  html_text() %>%
  
  # Convert vector to tibble
  enframe(name = "position3", value = "name")
 
  
# 2.1.2 Extract the name (since we have retrieved the data already)
bike_price_tbl <- html_bike_category %>%
  
  # Get the nodes in the meta tag where the attribute itemprop equals description
  html_nodes(".catalog-category-bikes__price") %>%
  
  # Extract the content of the attribute content
  html_text() %>%
  
  stringr::str_extract("(?=)[:alpha:]+.*" ) 

  # Convert vector to tibble
  enframe(name = "position4", value = "price")

bike_new <- tibble(bike_url_tbl , bike_price_tbl, bike_name_tbl , bike_desc_tbl) %>%
  select(name,description,price,url)
