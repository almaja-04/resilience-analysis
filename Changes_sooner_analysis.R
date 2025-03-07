library(afcharts)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(scales)
library(forcats)
library(rstatix)
library(pheatmap)
use_afcharts()

# SETUP ------------------------------------------------------------------------

# Load in survey data
data <- read.csv("FinalSurveyData.csv")