## ----setup, include = FALSE---------------------------------------------------
options(htmltools.dir.version = FALSE)
library(ggplot2)
knitr::opts_chunk$set(collapse = TRUE, fig.height = 5, fig.width = 6)
library(lattice)
library(tidyverse)
library(gridExtra)
library(forecast)
theme_set(theme_minimal() +
          theme(text = element_text(size = 16)) +
          theme(panel.border = element_rect(color = "grey30", fill = NA)))
set.seed(12345)

## ----xaringanExtra-clipboard, echo=FALSE--------------------------------------
xaringanExtra::use_clipboard()

