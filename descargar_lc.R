# instalar paquetes -------------------------------------------------------------------------------------------------------------------

# instalar rgee (es necesario tener instalado el paquete "remotes" antes)
#install.packages("remotes")

#remotes::install_github("r-spatial/rgee")

#library(rgee)

# se hace solo la primera vez
#ee_install()

# se ejecuta cada vez que quiera usar rgee
#ee_Initialize()

#install.packages(c("tidyverse","sf","raster","landscapemetrics","rio","sjPlot"))

# -------------------------------------------------------------------------------------------------------------------------------------

# cargar shape
t <- sf::read_sf("C:\\R\\mis_cursos\\huerquehue.shp")

#funcion LC Chile con shape
lc_chile <- function(ae){
  
  pacman::p_load(tidyverse,rgee,sf,raster,landscapemetrics,rio,sjPlot) 
  
  ee_Initialize()
  
  class <- read_csv("https://raw.githubusercontent.com/andrestyle16/lc_chile_2014/main/classes_lc_chile_2014.csv")
  
  ae <- ae %>% 
    sf_as_ee()
  
  lc <- ee$Image("users/ajsalazar/useful/LC_CHILE_2014")$clip(ae)
  
  lc_rast <- lc %>%
    ee_as_raster(region = ae$geometry(),
                 scale = 30)
  
  lc_rast[lc_rast<1] <- NA
  
  lc_ae <<- lc_rast
  
  writeRaster(lc_rast,"lc_2014_ae.tif", overwrite=TRUE)
  
  plot(lc_rast)
  
  a <- lc_rast %>% values %>% na.omit %>% unique() 
  
  info <- a %>% as_data_frame() %>% rename(code = b1) %>% inner_join(class) %>% arrange(code)
  
  y <- lc_rast %>% 
    lsm_c_ca %>% 
    dplyr::select(class, value) %>% 
    rename(code = class,
           area = value)
  
  data_lc <- y %>% inner_join(info) %>% 
    arrange(desc(area)) %>% 
    mutate(across(c(nivel_2, nivel_1, level_2,level_1), factor))
  
  data_lc <<- data_lc 
  
  rio::export(data_lc,"info_lc_ae.xlsx", overwrite=TRUE)
  
  g <<- ggplot(data_lc, aes(x=nivel_1, y=area, fill = nivel_1)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(y="Area (ha)", x="",  title="Area clases de uso y cobertura de suelo") +
    theme(legend.position = "none",
          plot.title = element_text(face="bold", size=14, hjust = 0.5),
          axis.title.y = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face="bold", size=11, color = "black"),
          axis.text.x = element_text(face="bold", size=9,color = "black", angle = 8))
  
  print(g)
  
  sjPlot::save_plot("area_lc_ae.png",fig = g, width = 28, height = 14, dpi = 300)
  print(data_lc)
}

#ejecutar funcion con el shape como argumento
lc_chile(t)

#funcion LC Chile escribiendo nombre area protegida
lc_chile_ap <- function(ae){
  
  pacman::p_load(tidyverse,rgee,sf,raster,landscapemetrics,rio,sjPlot) 
  
  ee_Initialize()
  
  class <- read_csv("https://raw.githubusercontent.com/andrestyle16/lc_chile_2014/main/classes_lc_chile_2014.csv")
  
  co <- ee$FeatureCollection('WCMC/WDPA/current/polygons')$
    filter(ee$Filter$eq('ORIG_NAME', ae))
  
  lc <- ee$Image("users/ajsalazar/useful/LC_CHILE_2014")$clip(co)
  
  lc_rast <- lc %>%
    ee_as_raster(region = co$geometry(),
                 scale = 30)
  
  lc_rast[lc_rast<1] <- NA
  
  lc_ae <<- lc_rast
  
  writeRaster(lc_rast,"lc_2014_ae.tif", overwrite=TRUE)
  
  plot(lc_rast)
  
  a <- lc_rast %>% values %>% na.omit %>% unique() 
  
  info <- a %>% as_data_frame() %>% rename(code = b1) %>% inner_join(class) %>% arrange(code)
  
  y <- lc_rast %>% 
    lsm_c_ca %>% 
    dplyr::select(class, value) %>% 
    rename(code = class,
           area = value)
  
  data_lc <- y %>% inner_join(info) %>% 
    arrange(desc(area)) %>% 
    mutate(across(c(nivel_2, nivel_1, level_2,level_1), factor))
  
  data_lc <<- data_lc  
  
  rio::export(data_lc,"info_lc_ae.xlsx", overwrite=TRUE)
  
  g <<- ggplot(data_lc, aes(x=nivel_1, y=area, fill = nivel_1)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(y="Area (ha)", x="",  title="Area clases de uso y cobertura de suelo") +
    theme(legend.position = "none",
          plot.title = element_text(face="bold", size=14, hjust = 0.5),
          axis.title.y = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.text.y = element_text(face="bold", size=11, color = "black"),
          axis.text.x = element_text(face="bold", size=9,color = "black", angle = 8))
  
  print(g)
  
  sjPlot::save_plot("area_lc_ae.png",fig = g, width = 28, height = 14, dpi = 300)
  print(data_lc)
}

#ejecutar funcion con el nombre de un AP como argumento
lc_chile_ap("Queulat")

#dibujar poligono
p <- mapview::mapview() %>% mapedit::editMap()
p <- p$ drawn

#ejecutar funcion con el poligono dibujado como argumento
lc_chile(p)
