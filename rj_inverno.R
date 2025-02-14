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
 
seropedica_inv <- seropedica %>% 
  filter(seropedica$Data >= "2021/06/21" & seropedica$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(seropedica$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330555"),
         municipio = paste("Serop?dica")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)

caxias <- read.csv2("INMET_SE_RJ_A603_DUQUE DE CAXIAS - XEREM_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8, encoding = " UTF-8")
caxias <- caxias[ ,c(1:2, 11)]

caxias_inv <- caxias %>% 
  filter(caxias$Data >= "2021/06/21" & caxias$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(caxias$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330170"),
         municipio = paste("Duque de Caxias")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)

cambuci <- read.csv2("INMET_SE_RJ_A604_CAMBUCI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
cambuci <-  cambuci[ ,c(1:2, 11)]

cambuci_inv <- cambuci %>% 
  filter(cambuci$Data >= "2021/06/21" & cambuci$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(cambuci$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330090"),
         municipio = paste("Cambuci")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


arraial <- read.csv2("INMET_SE_RJ_A606_ARRAIAL DO CABO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
arraial <- arraial[ ,c(1:2, 11)]

arraial_inv <- arraial %>% 
  filter(arraial$Data >= "2021/06/21" & arraial$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(arraial$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330025"),
         municipio = paste("Arraial do Cabo")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)

campos <- read.csv2("INMET_SE_RJ_A607_CAMPOS DOS GOYTACAZES_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
campos <- campos[ ,c(1:2, 11)]

campos_inv <- campos %>% 
  filter(campos$Data >= "2021/06/21" & campos$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(campos$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330100"),
         municipio = paste("Campos dos Goytacazes ")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


macae <- read.csv2("INMET_SE_RJ_A608_MACAE_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
macae <- macae[ ,c(1:2, 11)]

macae_inv <- macae %>% 
  filter(macae$Data >= "2021/06/21" & macae$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(macae$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330240"),
         municipio = paste("Maca?")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


resende <- read.csv2("INMET_SE_RJ_A609_RESENDE_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
resende <- resende[ ,c(1:2, 11)]

resende_inv <- resende %>% 
  filter(resende$Data >= "2021/06/21" & resende$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(resende$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330420"),
         municipio = paste("Resende")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


petropolis <- read.csv2("INMET_SE_RJ_A610_PICO DO COUTO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
petropolis <- petropolis[ ,c(1:2, 11)]

petropolis_inv <- petropolis %>% 
  filter(petropolis$Data >= "2021/06/21" & petropolis$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(petropolis$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330390"),
         municipio = paste("Petr?polis")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)
 

valenca <- read.csv2("INMET_SE_RJ_A611_VALENCA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
valenca <- valenca[ ,c(1:2, 11)]

valenca_inv <- valenca %>% 
  filter(valenca$Data >= "2021/06/21" & valenca$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(valenca$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330610"),
         municipio = paste("Valen?a")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


teresopolis <- read.csv2("INMET_SE_RJ_A618_TERESOPOLIS-PARQUE NACIONAL_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
teresopolis <- teresopolis[ ,c(1:2, 11)]

teresopolis_inv <- teresopolis %>% 
  filter(teresopolis$Data >= "2021/06/21" & teresopolis$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(teresopolis$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330580"),
         municipio = paste("Teres?polis")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


parati <- read.csv2("INMET_SE_RJ_A619_PARATI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
parati <- parati[ ,c(1:2, 11)]

parati_inv <- parati %>% 
  filter(parati$Data >= "2021/06/21" & parati$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(parati$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330380"),
         municipio = paste("Paraty")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


rio <- read.csv2("INMET_SE_RJ_A621_RIO DE JANEIRO - VILA MILITAR_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
rio <- rio[ ,c(1:2, 11)]

rio_inv <- rio %>% 
  filter(rio$Data >= "2021/06/21" & rio$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(rio$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330455"),
         municipio = paste("Rio de Janeiro")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)

friburgo <- read.csv2("INMET_SE_RJ_A624_NOVA FRIBURGO - SALINAS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
friburgo <- friburgo[ ,c(1:2, 11)]

friburgo_inv <- friburgo %>% 
  filter(friburgo$Data >= "2021/06/21" & friburgo$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(friburgo$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330340"),
         municipio = paste("Nova Friburgo")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


tres_rios <- read.csv2("INMET_SE_RJ_A625_TRES RIOS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
tres_rios <- tres_rios[ ,c(1:2, 11)]

tres_rios_inv <- tres_rios %>% 
  filter(tres_rios$Data >= "2021/06/21" & tres_rios$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(tres_rios$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330600"),
         municipio = paste("Tr?s Rios")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


rio_claro <- read.csv2("INMET_SE_RJ_A626_RIO CLARO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
rio_claro <- rio_claro[ ,c(1:2, 11)]

rio_claro_inv <- rio_claro %>% 
  filter(rio_claro$Data >= "2021/06/21" & rio_claro$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(rio_claro$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330440"),
         municipio = paste("Rio Claro")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


niteroi <- read.csv2("INMET_SE_RJ_A627_NITEROI_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
niteroi <- niteroi[ ,c(1:2, 11)]

niteroi_inv <- niteroi %>% 
  filter(niteroi$Data >= "2021/06/21" & niteroi$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(niteroi$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330330"),
         municipio = paste("Niter?i")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


angra <- read.csv2("INMET_SE_RJ_A628_ANGRA DOS REIS_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
angra <- angra[ ,c(1:2, 11)]

angra_inv <- angra %>% 
  filter(angra$Data >= "2021/06/21" & angra$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(angra$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330010"),
         municipio = paste("Angra dos Reis")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)

carmo <- read.csv2("INMET_SE_RJ_A629_CARMO_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
carmo <- carmo[ ,c(1:2, 11)]

carmo_inv <- carmo %>% 
  filter(carmo$Data >= "2021/06/21" & carmo$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(carmo$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330120"),
         municipio = paste("Carmo")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


madalena <- read.csv2("INMET_SE_RJ_A630_SANTA MARIA MADALENA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
madalena <- madalena[ ,c(1:2, 11)]

madalena_inv <- madalena %>% 
  filter(madalena$Data >= "2021/06/21" & madalena$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(madalena$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330460"),
         municipio = paste("Santa Maria Madalena")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)


itatiaia <- read.csv2("INMET_SE_RJ_A635_ITATIAIA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
itatiaia <- itatiaia[ ,c(1:2, 11)]

itatiaia_inv <- itatiaia %>% 
  filter(itatiaia$Data >= "2021/06/21" & itatiaia$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(itatiaia$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330225"),
         municipio = paste("Itatiaia")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)

silva_jardim <- read.csv2("INMET_SE_RJ_A659_SILVA JARDIM_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
silva_jardim <- silva_jardim[ ,c(1:2, 11)]

silva_jardim_inv <- silva_jardim %>% 
  filter(silva_jardim$Data >= "2021/06/21" & silva_jardim$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(silva_jardim$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330560"),
         municipio = paste("Silva Jardim")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)

saquarema <- read.csv2("INMET_SE_RJ_A667_SAQUAREMA - SAMPAIO CORREIA_01-01-2021_A_30-11-2021.CSV", header = T, skip = 8)
saquarema <- saquarema[ ,c(1:2, 11)]

saquarema_inv <- saquarema %>% 
  filter(saquarema$Data >= "2021/06/21" & saquarema$Data <= "2021/09/22") %>% 
  summarise(temp_media = mean(saquarema$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm =T)) %>% 
  mutate(CODMUN = paste("330550"),
         municipio = paste("Saquarema")) %>% 
  dplyr::select(CODMUN, municipio, temp_media)

#--------------------------------------------------------------------------------------------------------------------------


tabela_inv <- rbind(angra_inv, arraial_inv, cambuci_inv, campos_inv, carmo_inv, caxias_inv,friburgo_inv,
                   itatiaia_inv, macae_inv, madalena_inv, niteroi_inv, parati_inv, petropolis_inv,
                   resende_inv, rio_claro_inv, rio_inv, saquarema_inv, seropedica_inv, silva_jardim_inv,
                   teresopolis_inv, tres_rios_inv, valenca_inv)

shp_rio_inv <- readOGR(dsn = "RJ_shp", layer = "Base2018abril2019_equiv_albers", encoding = "UTF-8", use_iconv = TRUE)
view(shp_rio_inv@data)

shp_rio_completo <- merge(x = shp_rio_inv,
                          y = tabela_inv,
                          by = "CODMUN")

view(shp_rio_completo@data)

shp_rio_completo <- shp_rio_completo[ ,c(1,4,12)]

writeOGR(obj = shp_rio_completo,
         layer = "rj_inv",
         driver = "ESRI Shapefile",
         dsn = "shp_inv")

#-------------------------------------------------------------------------------------------------------------
  
tm_shape(shp = shp_rio_completo) +
  tm_polygons("temp_media",
              title = "Average Temperature (degree Celsius)",
              style = "fixed",
              palette = c("blue4", "blue3", "blue1", "steelblue1", "lightblue", "khaki", "darkorange"),
              breaks = c(-Inf,10,14,16,18,20,22,Inf),
              colorNA = "white",
              textNA = "No INMET Weather Station") +
  tm_layout(main.title = "Average Minimum Temperature in the State of Rio de Janeiro in 2021",
             title = "Winter Months Only",
            title.size = 1.2) +
  tm_compass(type = "8star",
             show.labels = 3) +
  tm_credits("Source: INMET") 
 
      
             