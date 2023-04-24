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

obitos_idade_ufs <- map(estados, arrange_faixa_etaria)
obitos_idade_br <- arrange_faixa_etaria(br = TRUE)

obitos_sexo_ufs <- arrange_obitos_sexo()
