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

seropedica_avg <- seropedica %>% 
  summarise(temp_avg = mean(seropedica$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330555"),
         municipio = paste("Serop?dica")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)

caxias <- read.csv2("INMET_SE_RJ_A603_DUQUE DE CAXIAS - XEREM_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
caxias <- caxias[ ,c(1:2, 11)]

caxias_avg <- caxias %>% 
  summarise(temp_avg = mean(caxias$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330170"),
         municipio = paste("Duque de Caxias")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)

cambuci <- read.csv2("INMET_SE_RJ_A604_CAMBUCI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
cambuci <-  cambuci[ ,c(1:2, 11)]

cambuci_avg <- cambuci %>% 
  summarise(temp_avg = mean(cambuci$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330090"),
         municipio = paste("Cambuci")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


arraial <- read.csv2("INMET_SE_RJ_A606_ARRAIAL DO CABO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
arraial <- arraial[ ,c(1:2, 11)]

arraial_avg <- arraial %>% 
  summarise(temp_avg = mean(arraial$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330025"),
         municipio = paste("Arraial do Cabo")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)

campos <- read.csv2("INMET_SE_RJ_A607_CAMPOS DOS GOYTACAZES_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
campos <- campos[ ,c(1:2, 11)]

campos_avg <- campos %>% 
  summarise(temp_avg = mean(campos$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330100"),
         municipio = paste("Campos dos Goytacazes ")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


macae <- read.csv2("INMET_SE_RJ_A608_MACAE_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
macae <- macae[ ,c(1:2, 11)]

macae_avg <- macae %>% 
  summarise(temp_avg = mean(macae$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330240"),
         municipio = paste("Maca?")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


resende <- read.csv2("INMET_SE_RJ_A609_RESENDE_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
resende <- resende[ ,c(1:2, 11)]

resende_avg <- resende %>% 
  summarise(temp_avg = mean(resende$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330420"),
         municipio = paste("Resende")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


petropolis <- read.csv2("INMET_SE_RJ_A610_PICO DO COUTO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
petropolis <- petropolis[ ,c(1:2, 11)]

petropolis_avg <- petropolis %>% 
  summarise(temp_avg = mean(petropolis$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330390"),
         municipio = paste("Petr?polis")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


valenca <- read.csv2("INMET_SE_RJ_A611_VALENCA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
valenca <- valenca[ ,c(1:2, 11)]

valenca_avg <- valenca %>% 
  summarise(temp_avg = mean(valenca$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330610"),
         municipio = paste("Valen?a")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


teresopolis <- read.csv2("INMET_SE_RJ_A618_TERESOPOLIS-PARQUE NACIONAL_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
teresopolis <- teresopolis[ ,c(1:2, 11)]

teresopolis_avg <- teresopolis %>% 
  summarise(temp_avg = mean(teresopolis$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330580"),
         municipio = paste("Teres?polis")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


parati <- read.csv2("INMET_SE_RJ_A619_PARATI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
parati <- parati[ ,c(1:2, 11)]

parati_avg<- parati %>% 
  summarise(temp_avg = mean(parati$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330380"),
         municipio = paste("Paraty")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


rio <- read.csv2("INMET_SE_RJ_A621_RIO DE JANEIRO - VILA MILITAR_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
rio <- rio[ ,c(1:2, 11)]

rio_avg <- rio %>% 
  summarise(temp_avg = mean(rio$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330455"),
         municipio = paste("Rio de Janeiro")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)

friburgo <- read.csv2("INMET_SE_RJ_A624_NOVA FRIBURGO - SALINAS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
friburgo <- friburgo[ ,c(1:2, 11)]

friburgo_avg <- friburgo %>% 
  summarise(temp_avg = mean(friburgo$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330340"),
         municipio = paste("Nova Friburgo")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


tres_rios <- read.csv2("INMET_SE_RJ_A625_TRES RIOS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
tres_rios <- tres_rios[ ,c(1:2, 11)]

tres_rios_avg <- tres_rios %>% 
  summarise(temp_avg = mean(tres_rios$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330600"),
         municipio = paste("Tr?s Rios")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


rio_claro <- read.csv2("INMET_SE_RJ_A626_RIO CLARO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
rio_claro <- rio_claro[ ,c(1:2, 11)]

rio_claro_avg <- rio_claro %>% 
  summarise(temp_avg = mean(rio_claro$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330440"),
         municipio = paste("Rio Claro")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


niteroi <- read.csv2("INMET_SE_RJ_A627_NITEROI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
niteroi <- niteroi[ ,c(1:2, 11)]

niteroi_avg <- niteroi %>% 
  summarise(temp_avg = mean(niteroi$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330330"),
         municipio = paste("Niter?i")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


angra <- read.csv2("INMET_SE_RJ_A628_ANGRA DOS REIS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
angra <- angra[ ,c(1:2, 11)]

angra_avg<- angra %>% 
  summarise(temp_avg = mean(angra$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330010"),
         municipio = paste("Angra dos Reis")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)

carmo <- read.csv2("INMET_SE_RJ_A629_CARMO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
carmo <- carmo[ ,c(1:2, 11)]

carmo_avg <- carmo %>% 
  summarise(temp_avg = mean(carmo$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330120"),
         municipio = paste("Carmo")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


madalena <- read.csv2("INMET_SE_RJ_A630_SANTA MARIA MADALENA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
madalena <- madalena[ ,c(1:2, 11)]

madalena_avg <- madalena %>% 
  summarise(temp_avg = mean(madalena$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330460"),
         municipio = paste("Santa Maria Madalena")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)


itatiaia <- read.csv2("INMET_SE_RJ_A635_ITATIAIA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
itatiaia <- itatiaia[ ,c(1:2, 11)]

itatiaia_avg <- itatiaia %>% 
  summarise(temp_avg = mean(itatiaia$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330225"),
         municipio = paste("Itatiaia")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)

silva_jardim <- read.csv2("INMET_SE_RJ_A659_SILVA JARDIM_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
silva_jardim <- silva_jardim[ ,c(1:2, 11)]

silva_jardim_avg <- silva_jardim %>% 
  summarise(temp_avg = mean(silva_jardim$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330560"),
         municipio = paste("Silva Jardim")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)

saquarema <- read.csv2("INMET_SE_RJ_A667_SAQUAREMA - SAMPAIO CORREIA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
saquarema <- saquarema[ ,c(1:2, 11)]

saquarema_avg <- saquarema %>% 
  summarise(temp_avg = mean(saquarema$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330550"),
         municipio = paste("Saquarema")) %>% 
  dplyr::select(CODMUN, municipio, temp_avg)

#--------------------------------------------------------------------------------------------------------------------------
  tabela_avg <- rbind(angra_avg, arraial_avg, cambuci_avg, campos_avg, carmo_avg, caxias_avg,friburgo_avg,
                  itatiaia_avg, macae_avg, madalena_avg, niteroi_avg, parati_avg, petropolis_avg,
                  resende_avg, rio_claro_avg, rio_avg, saquarema_avg, seropedica_avg, silva_jardim_avg,
                  teresopolis_avg, tres_rios_avg, valenca_avg)

shp_rio_avg_aual <- readOGR(dsn = "RJ_shp", layer = "Base2018abril2019_equiv_albers", encoding = "UTF-8", use_iconv = TRUE)
view(shp_rio_avg_anual@data)

shp_rio_avg <- merge(x = shp_rio_avg_aual,
                     y = tabela_avg,
                     by = "CODMUN")

view(shp_rio_avg@data)

shp_rio_avg_completo <- shp_rio_avg[ ,c(1,4,12)]

writeOGR(obj = shp_rio_avg_completo,
         layer = "rj_inv_avg",
         driver = "ESRI Shapefile",
         dsn = "shp_avg_anual")

#---------------------------------------------------------------------------------------------------------------
  
  tm_shape(shp = shp_rio_avg_completo) +
  tm_polygons("temp_avg",
              title = "Average Annual Temperature (degree Celsius)",
              style = "fixed",
              palette = c("blue4", "blue2", "steelblue1", "lightblue2", "cadetblue1", "khaki","gold1"),
              breaks = c(-Inf,10, 14, 16, 18, 20, 22, Inf),
              colorNA = "white",
              textNA = "No INMET Station") +
  tm_layout(main.title = "Average Annual Temperature in the State of Rio de Janeiro in 2021",
            title.size = 1.2) +
  tm_compass(type = "8star",
             show.labels = 3) +
  tm_credits("Source: INMET") 


