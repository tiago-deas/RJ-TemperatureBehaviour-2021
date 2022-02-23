pacotes <- c("rgdal","plotly","tidyverse","knitr","kableExtra","gridExtra",
             "png","grid","magick","rgl","devtools","GISTools","rayshader",
             "tmap","broom")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
  
}

shp_rj_df <- tidy(shp_rio_inv, region = "CODMUN") %>% 
  rename(CODMUN = id)

shp_rj_df <-  shp_rj_df %>% 
  left_join(shp_rio_completo@data,
            by = "CODMUN")

mapa_rj <- shp_rj_df %>% 
  ggplot(aes(x = long,
             y = lat,
             group = group,
             fill = temp_media)) +
           geom_polygon() +
           scale_fill_gradient(limits = range(shp_rj_df$temp_media),
                               low = "darkblue",
                               high = "khaki") +
           layer(geom = "path", 
                 stat = "identity", 
                 position = "identity",
                 mapping = aes(x = long, 
                               y = lat, 
                               group = group, 
                               color = I('#FFFFFF'))) +
           theme(legend.position = "none", 
                 axis.line = element_blank(), 
                 axis.text.x = element_blank(), 
                 axis.title.x = element_blank(),
                 axis.text.y = element_blank(), 
                 axis.title.y = element_blank(),
                 axis.ticks = element_blank(), 
                 panel.background = element_blank())

mapa_rj

xlim <- ggplot_build(mapa_rj)$layout$panel_scales_x[[1]]$range$range
ylim <- ggplot_build(mapa_rj)$layout$panel_scales_y[[1]]$range$range

ggsave(filename = "mapa_rj_dsa.png",
       width = diff(xlim) / 10000, 
       height = diff(ylim) / 10000,
       units = "cm")

background_mapa <- readPNG("mapa_rj_dsa.png")

coord_rj <-  coordinates(shp_rio_inv) %>% 
  data.frame()

coord_rj <- coord_rj %>% 
  rename("longitude" = X1,
         "latitude" = X2) %>% 
  mutate(CODMUN = shp_rio_inv@data$CODMUN)

coord_rj <- coord_rj[ ,c(2,1,3)]

shp_rj_df <- shp_rj_df %>% 
  left_join(coord_rj, by = "CODMUN")

mapa_temp_inv <- shp_rj_df %>%
  ggplot() + 
  annotation_custom(
    rasterGrob(background_mapa, 
               width=unit(1,"npc"),
               height=unit(1,"npc")),-Inf, Inf, -Inf, Inf) + 
  xlim(xlim[1],xlim[2]) + 
  ylim(ylim[1],ylim[2]) +
  geom_point(aes(x = longitude, y = latitude, color = temp_media), size = 1.5, na.rm = TRUE) + 
  scale_colour_gradient(name = "Temperatura Média", 
                        limits = range(shp_rj_df$temp_media), 
                        low = "darkblue", 
                        high = "khaki") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

plot_gg(ggobj = mapa_temp_inv, 
        width = 11, 
        height = 6, 
        scale = 300,
        multicore = TRUE, 
        windowsize = c(1000, 800))

render_camera(fov = 70, 
              zoom = 0.5, 
              theta = 130, 
              phi = 35)

azimute_metade <- 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
azimute_completo <- c(azimute_metade, rev(azimute_metade))

rotacao <- 0 + 45 * sin(seq(0, 359, length.out = 360) * pi/180)

zoom_metade <- 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoom_completo <- c(zoom_metade, rev(zoom_metade))

render_movie(filename = "temp_inv_rj", 
             type = "custom", 
             frames = 360, 
             phi = azimute_completo, 
             zoom = zoom_completo, 
             theta = rotacao)
  
           
           