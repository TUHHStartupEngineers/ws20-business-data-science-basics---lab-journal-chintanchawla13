cat("\014")
library(vroom)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(ggrepel)
library(maps)
library(readr)
library(scales)
library(lubridate)

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "00_data/wrangling data/patent.tsv",
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)


col_types <- list(
  id = col_character(),
  type = col_double(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)


assignee_tbl <- vroom(
  file       = "00_data/wrangling data/assignee.tsv",
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)



col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "00_data/wrangling data/patent_assignee.tsv",
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_double()
)

uspc_tbl <- vroom(
  file       = "00_data/wrangling data/uspc.tsv",
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

Patent_Dominance_top_10_tbl <- left_join(x=assignee_tbl, y=patent_assignee_tbl, by = c("id" = "assignee_id")) %>%
   filter(type==2) %>%
   mutate(count=1L) %>%
   group_by(id, organization,name_first,name_last)%>%
   summarise(number_of_patents=sum(count))%>%
   arrange(desc(number_of_patents))%>%
   head(n=10)

combined_tbl_1 <- left_join(x=assignee_tbl, y=patent_assignee_tbl, by = c("id" = "assignee_id"))
 
new_patent_tbl  <- patent_tbl %>% 
  select(date,number)
  
recent_patent_acitivity <- left_join(x=combined_tbl_1, y=new_patent_tbl, by = c("patent_id" = "number"))  %>%  
  filter(type==2) %>%
  mutate(year=year(date)) %>%  
  filter(year==2019) %>%
  mutate(count=1L) %>%
  group_by(organization,year)%>%
  summarise(new_granted_patents =sum(count))%>%
  arrange(desc(new_granted_patents))%>%
  head(n=10)

new_uspc_tbl <- left_join(x=combined_tbl_1, y=uspc_tbl) 

innovative_tech_sector<-  new_uspc_tbl %>%  filter(type==2 | type==3) %>%
                          mutate(count=1L) %>%
                          group_by(organization,type)%>%
                          summarise( Number_patents=sum(count))%>%
                          arrange(desc(Number_patents))%>%
                          head(n=10)

tech_main_classes<-  new_uspc_tbl %>%  filter(type==2 | type==3) %>%
                          mutate(count=1L) %>%
                          group_by(patent_id, mainclass_id)%>%
                          summarise(tech_main_class =sum(count))%>%
                          arrange(desc(tech_main_class))%>%
                          na.omit(mainclass_id)%>%
                          head(n=5)
