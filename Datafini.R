---
title: "Proyecto Grupo 5"
author: "Briones Coello Doménica"
date: "13/01/2023"
output: pdf_document
---
#
#
# Análisis de datos de Datafini
#
Se procede a cargar la base de datos y ver sus categorías.
#
#
library(nortest)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(boot)
library(car)
library(QuantPsyc)
library(psych)
library(fdth)
library(date)
library(lubridate)
library(corrplot)

electronic <- read.csv("C:\\Users\\ACER\\Documents\\Estadística\\DatafinitiElectronicsProductsPricingData(1).csv",header=FALSE)

