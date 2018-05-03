
library(dplyr)
library(stringr)
library(stringi)
library(lubridate)
# install.packages("DT")
# install.packages("plotly")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("stringi")
# install.packages("anomalize")
# install.packages("lubridate")
#### datos ####
path_archivo <- paste0(getwd(),"/indicadores_algodonera.csv")
data_read_raw <- read.csv(path_archivo,
                      sep = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE) 

glimpse(data_read_raw)
data_read_raw$frec_temporal %>% as.factor() %>% levels()
data_read_raw$alcance_tipo %>% as.factor() %>% levels()

data_read <- data_read_raw %>%
  mutate(indice_tiempo = ymd(indice_tiempo)) %>% 
  filter(alcance_tipo=="PROVINCIA") %>%
  select(indice_tiempo,alcance_nombre,alcance_tipo,sup_sembrada_algodon,sup_cosechada_algodon,prod_algodon)
 
# existen provincias que no producen, tambien puede ser que no tengan mediciones.
# se estima que es por ello que tienen NA. 
data_read %>% filter(!complete.cases(data_read)) %>% glimpse()

# como NA o 0 para nuestro caso es lo mismo , imputamos 0
data_read[is.na(data_read)] <- 0

glimpse(data_read)

write.csv(paste0(getwd(),"/indicadores_algodonera_simple.csv"),x = data_read,row.names = FALSE,fileEncoding = "UTF-8",quote = FALSE)