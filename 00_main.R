# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)

source("01_datasus.R")

load("data/datasus-2020.rda")

# datasus -----------------------------------------------------------------

datasus_faixa_etaria <- calc_faixa_etaria()

estados <- unique(
    datasus_faixa_etaria$nome_uf[!is.na(datasus_faixa_etaria$nome_uf)]
)

map(estados, arrange_faixa_etaria)
