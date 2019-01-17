# SO_config.R
# StackOverflow Configurations


# Just add any package and it will install/load it#######

packages <- c("dplyr","XML","tm","textmineR","gsubfn","SnowballC","textstem","tidytext",
              "sentimentr","wordcloud","gender","corrplot","stringr","ggplot2","lubridate",
              "rvest","magrittr","shiny","leaflet","readr","caret","quanteda","ngram","topicmodels",
              "tidyr","kableExtra","knitr","ggrepel","gridExtra","formattable","tm","circlize","plotly")
ipak(packages)

# Do not Change this #################
# xml_loc <- "data/"
# csv_loc <- "data/"

### For Other OS ############
xml_loc <- "F://xml_locations//"
csv_loc <- "F://csv_location//"


#### Used extension #####
ext <- ".csv"

# This will help with automatically processing only those files not processed
files <- c("posts",
           "comments",
           "tags",
           "users")
