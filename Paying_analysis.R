library(afcharts)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
use_afcharts()

# SETUP ------------------------------------------------------------------------

# Load in survey data
data <- read.csv("FinalSurveyData.csv")