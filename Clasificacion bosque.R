library(sf)
library(raster)
library(ggplot2)
SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Cacaxtl = st_read("SHP/Cacaxtla.geojson")  %>% st_as_sf()
Cacaxtla <- st_transform(Cacaxtl  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Marco_Cacaxtla= st_as_sfc(st_bbox(Cacaxtla))

Mexico1  <- getData('GADM', country='Mexico', level=1) %>% st_as_sf()
Mexico2  <- getData('GADM', country='Mexico', level=2) %>% st_as_sf()
Sinaloa  =  subset(Mexico2 , NAME_1 == "Sinaloa")


A=ggplot()+
  geom_sf(data=Mexico1, fill="white", color="black", size=0.1)+
  geom_sf(data=Sinaloa, fill="gray", color="black", size=0.1)+
  geom_sf_text(data = Mexico1, aes(label=NAME_1 ),family="serif", color="black", alpha=0.6, size =3 )+
  geom_sf(data = Cacaxtla, fill="black", size=0.01)+
  geom_sf(data = Marco_Cacaxtla, fill=NA, size=1)+
  coord_sf(xlim = c(-110.5, -104), ylim = c(22.47209  , 27.0407)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -109, y = 23, hjust = 0, vjust = 1, 
           label = "Golfo de California",size = 3, family="serif", color = 
             "blue",  fontface="italic")


B=ggplot()+
  geom_sf(data=Mexico1, fill="white", color="black", size=0.1)+
  geom_sf(data=Sinaloa, fill="gray", color="black", size=0.1)+
  geom_sf_text(data = Mexico2, aes(label=NAME_2 ),family="serif", color="black", size =4 
               ,  fontface="italic")+
  geom_sf(data = Cacaxtla, color="black", size=1, fill=NA)+
  coord_sf(xlim = c(-107.2, -106.2), ylim = c(23.2 , 24)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -107.2, y = 23.4, hjust = 0, vjust = 1, 
           label = "Golfo de California",size = 3, family="serif", color = 
             "blue",  fontface="italic")

C=ggplot()+
  geom_sf(data=Mexico1, fill="white", color="black", size=0.1)+
  geom_sf(data=Sinaloa, fill="gray", color="gray", size=0.1)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))

library(ggspatial)

library(elevatr)
elev = get_elev_raster(Marco_Cacaxtla, z=13)
plot(elev)
Poligo_alt    <- crop(elev, Sinaloa)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Sinaloa)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
colores = c( 
  "#8e9aaf",#celeste
  "#dda15e", # maroon 
  "#faedcd")#amarillo pastel


Geo_data       <-  rasterToPoints(elev)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

Cober <- raster("Raster/Cober.tif")
plot(Cober)
Cober.pa        <-  rasterToPoints(Cober)
Cober.pa_a      <-  data.frame(Cober.pa)
colnames(Cober.pa_a) <- c("x","y", "Cober")


col=c(
  "#006400", # Cubierta de árboles
  "#FFBB22", # Matorrales
  "#FFFF4C", # Pastizales
  "#F096FF", # Tierras de cultivo
  "#FA0000", # Construido
  "#B4B4B4", # Vegetación desnuda / escasa
  "#F0F0F0", # Masas de agua permanentes
  "#0064C8", # Humedal herbáceo
  "#0096A0", # Humedal herbáceo
  "#00CF75", # Manglares
  "#FAE6A0" # Musgos y líquenes
)


library(ggnewscale) 
D= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Cacaxtla, color="black", size=1, fill=NA)+
  
  geom_sf(data=Sinaloa, fill=NA, color="black", size=0.01)+
  annotate(geom = "text", x = -106.75, y = 23.55, hjust = 0, vjust = 1, angle=140,
           label = "Golfo de California",size = 4, family="serif", color = 
             "blue",  fontface="italic")+
  geom_raster(data = Cober.pa_a , aes(x,y,fill =Cober), color=NA, alpha=0.6) + 
  scale_fill_gradientn(colours = col, name="Clasificación de \nla cubierta terrestre",
                       labels = c("[Cubierta de árboles] ",
                                  "[Matorrales]", 
                                  "[Pastizales]", 
                                  "[Tierras de cultivo]", 
                                  "[Construido]",
                                  "[Vegetación desnuda / escasa]",
                                  "[Nieve y hielo]",
                                  "[Masas de agua permanentes]",
                                  "[Humedal herbáceo]",
                                  "[Manglares]",
                                  "[Musgos y líquenes]"),
                       breaks = c(10,20,30,40,50,60,70,80,90,100, 110))+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  coord_sf(xlim = c(-106.8106, -106.4991), ylim = c(23.49437 , 23.78556)) +
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_classic()+
  theme(
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=11, family="serif"),
        legend.title = element_text(size=11, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))

legend <- get_legend(D)

D= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Cacaxtla, color="black", size=1, fill=NA)+
  
  geom_sf(data=Sinaloa, fill=NA, color="black", size=0.01)+
  annotate(geom = "text", x = -106.75, y = 23.55, hjust = 0, vjust = 1, angle=140,
           label = "Golfo de California",size = 4, family="serif", color = 
             "blue",  fontface="italic")+
  geom_raster(data = Cober.pa_a , aes(x,y,fill =Cober), show.legend = F, color=NA, alpha=0.6) + 
  scale_fill_gradientn(colours = col, name="Clasificación de \nla cubierta terrestre",
                       labels = c("[Cubierta de árboles] ",
                                  "[Matorrales]", 
                                  "[Pastizales]", 
                                  "[Tierras de cultivo]", 
                                  "[Construido]",
                                  "[Vegetación desnuda / escasa]",
                                  "[Nieve y hielo]",
                                  "[Masas de agua permanentes]",
                                  "[Humedal herbáceo]",
                                  "[Manglares]",
                                  "[Musgos y líquenes]"),
                       breaks = c(10,20,30,40,50,60,70,80,90,100, 110))+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  coord_sf(xlim = c(-106.8106, -106.4991), ylim = c(23.49437 , 23.78556)) +
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=11, family="serif"),
        legend.title = element_text(size=11, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))

library(cowplot)

ggAgru =ggdraw() +
  coord_equal(xlim = c(0, 9), ylim = c(0, 21), expand = FALSE) +
  draw_plot(B , width = 9, height = 9,x = 0, y = -1)+
  draw_plot(A , width = 9, height = 9,x = 0, y = 6.6)+
  draw_plot(legend , width = 9, height = 9,x = 0, y = 13)+
  theme_void()


Mapa=ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(D  , width = 21, height = 21,x = -0.5, y = 0)+
  draw_plot(ggAgru  , width = 20, height = 20,x = 14.5, y = 1)+

  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = -75, y = -17, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)


ggsave(plot=Mapa ,"Mapa/Mapa de clasificacion.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)




















  
