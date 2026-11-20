#Analysis for the Mconlearning paper
#2026-11-sb

# Load necessary library
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrr)
library(moments)
library(lme4)
library(lmerTest)
library(tibble)
library(patchwork)
library(flextable)
library(Hmisc)
library(readr)
library(descr)
library(psych)
library(lavaan)
library(blavaan)
library(semTools)
library(semPlot)
library(rstatix)
library(car)
library(parameters)
library(haven)

#Path
setwd("/safe/data/Buratti/Mconlearning/data")

#Reading
survey <- read.csv("surveycleaned-260401-n5363.csv")
metal <- read_csv("MFT_260105.csv")
number <- read_csv("NST_260105.csv")
opposites <- read_csv("OT_260105.csv")
ugu1948 <- read_csv("ugu1948.csv")
ugu1953 <- read_csv("ugu1953.csv")
foodordering <- read.csv("FOT-260105.csv")
wordlearning <- read.csv("WLT-260105.csv")
fob <- read.csv("FoB_1990.csv")
charlson <- read.csv("charlsondata260818.csv")


