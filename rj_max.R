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

seropedica_max <- seropedica %>% 
  summarise(temp_max = max(seropedica$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330555"),
         municipio = paste("Serop?dica")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)

caxias <- read.csv2("INMET_SE_RJ_A603_DUQUE DE CAXIAS - XEREM_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
caxias <- caxias[ ,c(1:2, 11)]

caxias_max <- caxias %>% 
  summarise(temp_max = max(caxias$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330170"),
         municipio = paste("Duque de Caxias")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)

cambuci <- read.csv2("INMET_SE_RJ_A604_CAMBUCI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
cambuci <-  cambuci[ ,c(1:2, 11)]

cambuci_max <- cambuci %>% 
  summarise(temp_max = max(cambuci$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330090"),
         municipio = paste("Cambuci")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


arraial <- read.csv2("INMET_SE_RJ_A606_ARRAIAL DO CABO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
arraial <- arraial[ ,c(1:2, 11)]

arraial_max <- arraial %>% 
 summarise(temp_max = max(arraial$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330025"),
         municipio = paste("Arraial do Cabo")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)

campos <- read.csv2("INMET_SE_RJ_A607_CAMPOS DOS GOYTACAZES_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
campos <- campos[ ,c(1:2, 11)]

campos_max <- campos %>% 
  summarise(temp_max = max(campos$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330100"),
         municipio = paste("Campos dos Goytacazes ")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


macae <- read.csv2("INMET_SE_RJ_A608_MACAE_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
macae <- macae[ ,c(1:2, 11)]

macae_max <- macae %>% 
  summarise(temp_max = max(macae$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330240"),
         municipio = paste("Maca?")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


resende <- read.csv2("INMET_SE_RJ_A609_RESENDE_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
resende <- resende[ ,c(1:2, 11)]

resende_max <- resende %>% 
 summarise(temp_max = max(resende$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330420"),
         municipio = paste("Resende")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


petropolis <- read.csv2("INMET_SE_RJ_A610_PICO DO COUTO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
petropolis <- petropolis[ ,c(1:2, 11)]

petropolis_max <- petropolis %>% 
 summarise(temp_max = max(petropolis$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330390"),
         municipio = paste("Petr?polis")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


valenca <- read.csv2("INMET_SE_RJ_A611_VALENCA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
valenca <- valenca[ ,c(1:2, 11)]

valenca_max <- valenca %>% 
 summarise(temp_max = max(valenca$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330610"),
         municipio = paste("Valen?a")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


teresopolis <- read.csv2("INMET_SE_RJ_A618_TERESOPOLIS-PARQUE NACIONAL_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
teresopolis <- teresopolis[ ,c(1:2, 11)]

teresopolis_max <- teresopolis %>% 
  summarise(temp_max = max(teresopolis$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330580"),
         municipio = paste("Teres?polis")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


parati <- read.csv2("INMET_SE_RJ_A619_PARATI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
parati <- parati[ ,c(1:2, 11)]

parati_max<- parati %>% 
  summarise(temp_max = max(parati$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330380"),
         municipio = paste("Paraty")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


rio <- read.csv2("INMET_SE_RJ_A621_RIO DE JANEIRO - VILA MILITAR_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
rio <- rio[ ,c(1:2, 11)]

rio_max <- rio %>% 
  summarise(temp_max = max(rio$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330455"),
         municipio = paste("Rio de Janeiro")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)

friburgo <- read.csv2("INMET_SE_RJ_A624_NOVA FRIBURGO - SALINAS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
friburgo <- friburgo[ ,c(1:2, 11)]

friburgo_max <- friburgo %>% 
  summarise(temp_max = max(friburgo$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330340"),
         municipio = paste("Nova Friburgo")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


tres_rios <- read.csv2("INMET_SE_RJ_A625_TRES RIOS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
tres_rios <- tres_rios[ ,c(1:2, 11)]

tres_rios_max <- tres_rios %>% 
  summarise(temp_max = max(tres_rios$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330600"),
         municipio = paste("Tr?s Rios")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


rio_claro <- read.csv2("INMET_SE_RJ_A626_RIO CLARO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
rio_claro <- rio_claro[ ,c(1:2, 11)]

rio_claro_max <- rio_claro %>% 
  summarise(temp_max = max(rio_claro$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330440"),
         municipio = paste("Rio Claro")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


niteroi <- read.csv2("INMET_SE_RJ_A627_NITEROI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
niteroi <- niteroi[ ,c(1:2, 11)]

niteroi_max <- niteroi %>% 
 summarise(temp_max = max(niteroi$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330330"),
         municipio = paste("Niter?i")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


angra <- read.csv2("INMET_SE_RJ_A628_ANGRA DOS REIS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
angra <- angra[ ,c(1:2, 11)]

angra_max<- angra %>% 
 summarise(temp_max = max(angra$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330010"),
         municipio = paste("Angra dos Reis")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)

carmo <- read.csv2("INMET_SE_RJ_A629_CARMO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
carmo <- carmo[ ,c(1:2, 11)]

carmo_max <- carmo %>% 
 summarise(temp_max = max(carmo$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330120"),
         municipio = paste("Carmo")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


madalena <- read.csv2("INMET_SE_RJ_A630_SANTA MARIA MADALENA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
madalena <- madalena[ ,c(1:2, 11)]

madalena_max <- madalena %>% 
  summarise(temp_max = max(madalena$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330460"),
         municipio = paste("Santa Maria Madalena")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)


itatiaia <- read.csv2("INMET_SE_RJ_A635_ITATIAIA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
itatiaia <- itatiaia[ ,c(1:2, 11)]

itatiaia_max <- itatiaia %>% 
 summarise(temp_max = max(itatiaia$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330225"),
         municipio = paste("Itatiaia")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)

silva_jardim <- read.csv2("INMET_SE_RJ_A659_SILVA JARDIM_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
silva_jardim <- silva_jardim[ ,c(1:2, 11)]

silva_jardim_max <- silva_jardim %>% 
  summarise(temp_max = max(silva_jardim$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330560"),
         municipio = paste("Silva Jardim")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)

saquarema <- read.csv2("INMET_SE_RJ_A667_SAQUAREMA - SAMPAIO CORREIA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
saquarema <- saquarema[ ,c(1:2, 11)]

saquarema_max <- saquarema %>% 
 summarise(temp_max = max(saquarema$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330550"),
         municipio = paste("Saquarema")) %>% 
  dplyr::select(CODMUN, municipio, temp_max)

#-------------------------------------------------------------------------------------------------------------------------
tabela_max <- rbind(angra_max, arraial_max, cambuci_max, campos_max, carmo_max, caxias_max,friburgo_max,
                  itatiaia_max, macae_max, madalena_max, niteroi_max, parati_max, petropolis_max,
                  resende_max, rio_claro_max, rio_max, saquarema_max, seropedica_max, silva_jardim_max,
                  teresopolis_max, tres_rios_max, valenca_max)

shp_rio_max_aual <- readOGR(dsn = "RJ_shp", layer = "Base2018abril2019_equiv_albers", encoding = "UTF-8", use_iconv = TRUE)
view(shp_rio_max_anual@data)

shp_rio_max <- merge(x = shp_rio_max_aual,
                          y = tabela_max,
                          by = "CODMUN")

view(shp_rio_max@data)

shp_rio_max_completo <- shp_rio_max[ ,c(1,4,12)]

writeOGR(obj = shp_rio_max_completo,
         layer = "rj_inv_max",
         driver = "ESRI Shapefile",
         dsn = "shp_max_anual")

#-------------------------------------------------------------------------------------------------------------
  
  tm_shape(shp = shp_rio_max_completo) +
  tm_polygons("temp_max",
              title = "Highest Temperature (degree Celsius)",
              style = "fixed",
              palette = c("YlOrRd"),
              breaks = c(-Inf,22, 24, 28, 30, 32, 34, 36, 38, Inf),
              colorNA = "white",
              textNA = "No INMET Station") +
  tm_layout(main.title = "Highest Temperature in the State of Rio de Janeiro in 2021",
              title.size = 1.2) +
  tm_compass(type = "8star",
             show.labels = 3) +
  tm_credits("Source: INMET") 


