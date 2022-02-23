
tabela_range <- left_join(tabela_min,
                          tabela_max,
                          by = c("CODMUN", "municipio")) %>% 
  mutate(diferenca = (temp_max - temp_min))


shp_rio_range <- readOGR(dsn = "RJ_shp", layer = "Base2018abril2019_equiv_albers", encoding = "UTF-8", use_iconv = TRUE)
view(shp_rio_range@data)

shp_rio_range_tot <- merge(x = shp_rio_range,
                     y = tabela_range,
                     by = "CODMUN")

view(shp_rio_range_tot@data)

shp_rio_range_completo <- shp_rio_range_tot[ ,c(1,4,12:14)]

view(shp_rio_range_completo)

writeOGR(obj = shp_rio_range_completo,
         layer = "rj_range",
         driver = "ESRI Shapefile",
         dsn = "shp_range")


#------------------------------------------------------------------------------------------
  
  tm_shape(shp = shp_rio_range_completo) +
  tm_polygons("diferenca",
              title = "Temperature Range (degree Celsius)",
              style = "fixed",
              palette = c("darkgreen", "chartreuse4", "yellowgreen", "khaki", "gold2","darkorange1", "red1", "red4"),
              breaks = c(-Inf,21, 23, 25, 27, 29, 31, 32, Inf),
              colorNA = "white",
              textNA = "No INMET Station") +
  tm_layout(main.title = "Temperature Range Between the Highest and Lowest Temperature in the State of Rio de Janeiro in 2021",
            title.size = 0.8,
            legend.text.size = 1) +
  tm_compass(type = "8star",
             show.labels = 3) +
  tm_credits("Source: INMET") 
