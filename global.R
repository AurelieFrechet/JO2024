
# Packages ----------------------------------------------------------------
library(shiny)
library(lubridate) 
library(plotly)
library(wordcloud)

# Chargement des données --------------------------------------------------

trace_points <- readRDS("data/trace_points.RDS") 
trace_points$segment2 <- as.factor(rep(1:2, length = nrow(trace_points)))
