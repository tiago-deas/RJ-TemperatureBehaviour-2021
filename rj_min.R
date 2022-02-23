library(tidyverse)
library(tidyr)
library(ggplot2)
library(readxl)
library(tmap)
library(rgdal)
library(raster)
library(maptools)
library(broom)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(dplyr)

seropedica <- read.csv2("seropedica.CSV", skip = 8, header = T)
seropedica <- seropedica[ ,c(1:2, 11)] 

seropedica_min <- seropedica %>% 
  summarise(temp_min = min(seropedica$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330555"),
         municipio = paste("Serop?dica")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)

caxias <- read.csv2("INMET_SE_RJ_A603_DUQUE DE CAXIAS - XEREM_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
caxias <- caxias[ ,c(1:2, 11)]

caxias_min <- caxias %>% 
  summarise(temp_min = min(caxias$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330170"),
         municipio = paste("Duque de Caxias")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)

cambuci <- read.csv2("INMET_SE_RJ_A604_CAMBUCI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
cambuci <-  cambuci[ ,c(1:2, 11)]

cambuci_min <- cambuci %>% 
  summarise(temp_min = min(cambuci$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330090"),
         municipio = paste("Cambuci")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


arraial <- read.csv2("INMET_SE_RJ_A606_ARRAIAL DO CABO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
arraial <- arraial[ ,c(1:2, 11)]

arraial_min <- arraial %>% 
  summarise(temp_min = min(arraial$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330025"),
         municipio = paste("Arraial do Cabo")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)

campos <- read.csv2("INMET_SE_RJ_A607_CAMPOS DOS GOYTACAZES_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
campos <- campos[ ,c(1:2, 11)]

campos_min <- campos %>% 
  summarise(temp_min = min(campos$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330100"),
         municipio = paste("Campos dos Goytacazes ")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


macae <- read.csv2("INMET_SE_RJ_A608_MACAE_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
macae <- macae[ ,c(1:2, 11)]

macae_min <- macae %>% 
  summarise(temp_min = min(macae$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330240"),
         municipio = paste("Maca?")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


resende <- read.csv2("INMET_SE_RJ_A609_RESENDE_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
resende <- resende[ ,c(1:2, 11)]

resende_min <- resende %>% 
  summarise(temp_min = min(resende$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330420"),
         municipio = paste("Resende")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


petropolis <- read.csv2("INMET_SE_RJ_A610_PICO DO COUTO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
petropolis <- petropolis[ ,c(1:2, 11)]

petropolis_min <- petropolis %>% 
  summarise(temp_min = min(petropolis$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330390"),
         municipio = paste("Petr?polis")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


valenca <- read.csv2("INMET_SE_RJ_A611_VALENCA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
valenca <- valenca[ ,c(1:2, 11)]

valenca_min <- valenca %>% 
  summarise(temp_min = min(valenca$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330610"),
         municipio = paste("Valen?a")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


teresopolis <- read.csv2("INMET_SE_RJ_A618_TERESOPOLIS-PARQUE NACIONAL_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
teresopolis <- teresopolis[ ,c(1:2, 11)]

teresopolis_min <- teresopolis %>% 
  summarise(temp_min = min(teresopolis$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330580"),
         municipio = paste("Teres?polis")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


parati <- read.csv2("INMET_SE_RJ_A619_PARATI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
parati <- parati[ ,c(1:2, 11)]

parati_min<- parati %>% 
  summarise(temp_min = min(parati$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330380"),
         municipio = paste("Paraty")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


rio <- read.csv2("INMET_SE_RJ_A621_RIO DE JANEIRO - VILA MILITAR_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
rio <- rio[ ,c(1:2, 11)]

rio_min <- rio %>% 
  summarise(temp_min = min(rio$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330455"),
         municipio = paste("Rio de Janeiro")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)

friburgo <- read.csv2("INMET_SE_RJ_A624_NOVA FRIBURGO - SALINAS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
friburgo <- friburgo[ ,c(1:2, 11)]

friburgo_min <- friburgo %>% 
  summarise(temp_min = min(friburgo$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330340"),
         municipio = paste("Nova Friburgo")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


tres_rios <- read.csv2("INMET_SE_RJ_A625_TRES RIOS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
tres_rios <- tres_rios[ ,c(1:2, 11)]

tres_rios_min <- tres_rios %>% 
  summarise(temp_min = min(tres_rios$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330600"),
         municipio = paste("Tr?s Rios")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


rio_claro <- read.csv2("INMET_SE_RJ_A626_RIO CLARO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
rio_claro <- rio_claro[ ,c(1:2, 11)]

rio_claro_min <- rio_claro %>% 
  summarise(temp_min = min(rio_claro$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330440"),
         municipio = paste("Rio Claro")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


niteroi <- read.csv2("INMET_SE_RJ_A627_NITEROI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
niteroi <- niteroi[ ,c(1:2, 11)]

niteroi_min <- niteroi %>% 
  summarise(temp_min = min(niteroi$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330330"),
         municipio = paste("Niter?i")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


angra <- read.csv2("INMET_SE_RJ_A628_ANGRA DOS REIS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
angra <- angra[ ,c(1:2, 11)]

angra_min<- angra %>% 
  summarise(temp_min = min(angra$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330010"),
         municipio = paste("Angra dos Reis")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)

carmo <- read.csv2("INMET_SE_RJ_A629_CARMO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
carmo <- carmo[ ,c(1:2, 11)]

carmo_min <- carmo %>% 
  summarise(temp_min = min(carmo$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330120"),
         municipio = paste("Carmo")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


madalena <- read.csv2("INMET_SE_RJ_A630_SANTA MARIA MADALENA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
madalena <- madalena[ ,c(1:2, 11)]

madalena_min <- madalena %>% 
  summarise(temp_min = min(madalena$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330460"),
         municipio = paste("Santa Maria Madalena")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)


itatiaia <- read.csv2("INMET_SE_RJ_A635_ITATIAIA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
itatiaia <- itatiaia[ ,c(1:2, 11)]

itatiaia_min <- itatiaia %>% 
  summarise(temp_min = min(itatiaia$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330225"),
         municipio = paste("Itatiaia")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)

silva_jardim <- read.csv2("INMET_SE_RJ_A659_SILVA JARDIM_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
silva_jardim <- silva_jardim[ ,c(1:2, 11)]

silva_jardim_min <- silva_jardim %>% 
  summarise(temp_min = min(silva_jardim$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330560"),
         municipio = paste("Silva Jardim")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)

saquarema <- read.csv2("INMET_SE_RJ_A667_SAQUAREMA - SAMPAIO CORREIA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
saquarema <- saquarema[ ,c(1:2, 11)]

saquarema_min <- saquarema %>% 
  summarise(temp_min = min(saquarema$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330550"),
         municipio = paste("Saquarema")) %>% 
  dplyr::select(CODMUN, municipio, temp_min)

#--------------------------------------------------------------------------------------------------------------------------
  tabela_min <- rbind(angra_min, arraial_min, cambuci_min, campos_min, carmo_min, caxias_min,friburgo_min,
                      itatiaia_min, macae_min, madalena_min, niteroi_min, parati_min, petropolis_min,
                      resende_min, rio_claro_min, rio_min, saquarema_min, seropedica_min, silva_jardim_min,
                      teresopolis_min, tres_rios_min, valenca_min)

shp_rio_min_anual <- readOGR(dsn = "RJ_shp", layer = "Base2018abril2019_equiv_albers", encoding = "UTF-8", use_iconv = TRUE)
view(shp_rio_min_anual@data)

shp_rio_min <- merge(x = shp_rio_min_anual,
                     y = tabela_min,
                     by = "CODMUN")

view(shp_rio_min@data)

shp_rio_min_completo <- shp_rio_min[ ,c(1,4,12)]

writeOGR(obj = shp_rio_min_completo,
         layer = "rj_min",
         driver = "ESRI Shapefile",
         dsn = "shp_inv_min")

#---------------------------------------------------------------------------------------------------------------
  
  tm_shape(shp = shp_rio_min_completo) +
  tm_polygons("temp_min",
              title = "Lowest Temperature (degree Celsius)",
              style = "fixed",
              palette = c("blue4", "blue3", "royalblue1", "steelblue1", "lightblue2", "cadetblue1", "khaki","darkorange1", "red1" ),
              breaks = c(-Inf,-9, 0, 2, 4, 6, 8, 10, 12, Inf),
              colorNA = "white",
              textNA = "No INMET Station") +
  tm_layout(main.title = "Lowest Temperature in the State of Rio de Janeiro in 2021",
            title.size = 1.2) +
  tm_compass(type = "8star",
             show.labels = 3) +
  tm_credits("Source: INMET") 


