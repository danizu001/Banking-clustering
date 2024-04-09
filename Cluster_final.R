library(readxl)
library("cluster")
library(psych)
library("cluster")
library("fpc")
library(dplyr)
library("corrplot")
library("factoextra")
library(reshape2)
library(RColorBrewer)

cell<-read_excel("Copia de infoclientebanca.xlsx")


# Definir el mapeo de categorías
mapeo_categorias <- tribble(
  ~nombre_original, ~nueva_categoria,
  "CLINICAS - HOSPITALES" , "Salud y Bienestar",
  "MERCADEO DIRECTO - COMERCIANTES DE VENTAS TELEFONICAS  / DIRECTV" , "Comercio",
  "DROGUERIAS, FARMACIAS, TIENDAS NATURISTAS" , "Salud y Bienestar",
  "ALMACEN POR DEPARTAMENTO CON SUPERMERCADO" , "Comercio",
  "MERCADEO DIRECTO - SERVICIOS DE SEGUROS" , "Finanzas",
  "SIN NOMBRE" , "Otra",
  "SUPERMERCADOS / TIENDAS EXPRESS" , "Comercio",
  "ALMACENES DE VESTUARIO Y ACCESORIOS PARA TODA LAFAMILIA" , "Moda",
  "TALLERES DE MECANICA Y LATONERIA Y PINTURA Y SERVICIIOS DE GRUA" , "Automotriz",
  "AEROLINEAS" , "Viajes y Transporte",
  "PRODUCTOS QUIMICOS" , "Industria",
  "FERRETERIAS" , "Hogar y Construcción",
  "RESTAURANTES, PIZZERIAS, FUENTES DE SODA" , "Alimentación y Restaurantes",
  "CLUBES SOCIALES Y DEPORTIVOS" , "Entretenimiento",
  "AGENCIAS DE VIAJE y OPERADORES DE TURISMO" , "Viajes y Transporte",
  "REPUESTOS Y ACCESORIOS AUTOMOTRICES" , "Automotriz",
  "OUTLETS" , "Comercio",
  "ALMACENES DE MISCELANEAS, CACHARRERIA Y ARTICULOS PARA REGALO / TODO TIPO DE ALMACENES" , "Comercio",
  "CONSULTORIOS Y  SERVICIOS MEDICOS" , "Salud y Bienestar",
  "ALMACENES POR DEPARTAMENTO SIN SUPERMERCADO" , "Comercio",
  "COLEGIOS, UNIVERSIDADES , INSTITUTOS DE EDUCACION TECNOLOGICA Y PREESCOLAR" , "Educación",
  "ESTACIONES DE SERVICIO ( Venta de Gasolina y gas vehicular)" , "Servicios",
  "ALMACENES DE ARTICULOS DEPORTIVOS" , "Deporte y Recreación",
  "CONTRATISTAS GENERALES  - RESIDENCIALES Y COMERCIALES" , "Hogar y Construcción",
  "COMPRAS EN INTERNET" , "Tecnología",
  "ALMACENES DE RELOJES, JOYAS Y PLATERIAS" , "Moda",
  "ALMACENES DE CALZADO" , "Moda",
  "DEPOSITO DE MATERIALES DE CONSTRUCCION" , "Hogar y Construcción",
  "SALSAMENTARIAS y VENTAS DE CARNES" , "Alimentación y Restaurantes",
  "VENTA  DE EQUIPOS Y SERVICIOS DE TELECOMUNICACIONES" , "Tecnología",
  "ASOCIACIONES PROFESIONALES Y GREMIOS" , "Servicios",
  "CASINOS Y JUEGOS DE AZAR , LOTERIAS, RIFAS" , "Entretenimiento",
  "MUEBLES PARA EL HOGAR   y OFICINA" , "Hogar y Construcción",
  "ALMACENES DE EQUIPOS, INSUMOS y SERVICIOS AGROPECUARIOS" , "Agropecuario",
  "ALMACENES DE VENTA DE EQUIPOS ELECTRONICOS" , "Tecnología",
  "COMPAÑIAS DE ALQUILER DE VEHICULOS" , "Viajes y Transporte",
  "CONGRESOS Y SEMINARIOS" , "Eventos",
  "COMPUTADORES  - EQUIPOS - ACCESORIOS - SOTFWARE" , "Tecnología",
  "TRANSPORTE DE CARGA EN GENERAL" , "Servicios",
  "SALAS DE CINE" , "Entretenimiento",
  "VENTA DE  COMIDA RAPIDA" , "Alimentación y Restaurantes",
  "PANADERIAS, REPOSTERIAS, SALONES DE TE Y CAFETERIAS" , "Alimentación y Restaurantes",
  "HOTELES -  CENTROS VACACIONALES" , "Viajes y Transporte",
  "LIBRERIAS" , "Educación",
  "ALMACENES DE ELECTRODOMESTICOS Y GASODOMESTICOS" , "Hogar y Construcción",
  "ALMACENES DE JUEGOS, JUGUETES Y HOBBIES" , "Entretenimiento",
  "PAGO DE IMPUESTOS / PAGOS DE TDC" , "Finanzas",
  "SERVICIOS PUBLICOS" , "Servicios",
  "VIVEROS Y ALMACENES PARA JARDINERIA" , "Hogar y Construcción",
  "ALMACENES DE TELAS" , "Moda",
  "ASEGURADORAS" , "Finanzas",
  "GIMNASIOS, SAUNA,  BAÑOS TURCOS  y CENTROS DE ESTETICA" , "Salud y Bienestar",
  "VENTA DE VEHICULOS AUTOMOTORES Y MOTOCICLETAS" , "Automotriz",
  "MERCADEO DIRECTO - COMERCIANTES DE VENTAS TELEFONICAS / AMWAY" , "Comercio",
  "SERVITECAS Y LLANTAS" , "Automotriz",
  "ALMACENES DE ARTICULOS DE CUERO Y MALETAS" , "Moda",
  "ALMACENES DE ARTICULOS PARA EL HOGAR" , "Hogar y Construcción",
  "ALMACENES DE ARTICULOS TIPICOS, ANTIGUEDADES (  Ventas, Reparación y Restauración) / PINTURAS / MUSEOS / ARTE" , "Arte y Antigüedades",
  "ALMACENES DE COMPUTADORES Y SOFTWARE" , "Tecnología",
  "COMPAÑIAS DE AVIACION" , "Viajes y Transporte",
  "MEDICINA PREPAGADA" , "Salud y Bienestar",
  "SERVICIOS FUNERARIOS Y PARQUES CEMENTARIOS" , "Salud y Bienestar",
  "VENTA DE  ARTICULOS MEDICOS, ODONTOLOGICOS, ORTOPEDICOS" , "Salud y Bienestar",
  "AGENCIAS DE BOLETERIA ( Producciones de Teatro ) excepto cine" , "Entretenimiento",
  "SALONES DE BELLEZA - PELUQUERIAS" , "Salud y Bienestar",
  "ALMACENES DE MUSICA - INSTRUMENTOS MUSICALES, PIANOS, PARTITURAS" , "Entretenimiento",
  "SERVICIOS DE MENSAJERIA, CORREO y ENCOMIENDAS" , "Servicios",
  "OPTICAS Y ARTICULOS OPTICOS" , "Salud y Bienestar",
  "PERFUMERIAS y TIENDA DE COSMESTICOS" , "Salud y Bienestar",
  "ALMACENES DE REPARACIONES ELECTRONICAS" , "Tecnología",
  "ALMACENES DE CAMARAS Y EQUIPOS FOTOGRAFICOS" , "Tecnología",
  "TRANSPORTE DE PASAJEROS, TERRESTRE, FLUVIAL y MARITIMO" , "Viajes y Transporte",
  "SERVICIOS DE AMBULANCIA" , "Salud y Bienestar",
  "ALMACENES DE DISCOS" , "Entretenimiento",
  "CLINICAS VETERINARIAS Y SERVICIOS VETERINARIOS" , "Salud y Bienestar",
  "INSUMOS INDUSTRIALES" , "Industria",
  "ENTIDADES FINANCIERAS" , "Finanzas",
  "SERVICIOS DE RECREACION" , "Entretenimiento",
  "SERVICIOS DE PUBLICIDAD, MEDIOS Y ARTES GRAFICAS" , "Servicios",
  "DULCERIAS" , "Alimentación y Restaurantes",
  "SUSCRIPCION Y VENTA DE PERIODICOS Y REVISTAS" , "Entretenimiento",
  "BARES, TABERNAS, DISCOTECAS" , "Entretenimiento",
  "ALMACEN DE TAPETES,  ALFOMBRAS Y PISOS" , "Hogar y Construcción",
  "ESCUELAS DE ENSEÑANZA INFORMAL" , "Educación",
  "PAPELERIAS ( Almacenes de Artículos para Oficina y colegios)" , "Educación",
  "GRILES -WISKERIAS- NIGT CLUB -STRIP TEASE" , "Entretenimiento",
  "TIENDAS DE ALQUILER DE VIDEOS" , "Entretenimiento",
  "MOTELES y  AMOBLADOS" , "Viajes y Transporte",
  "CIGARRERIAS Y LICORERAS" , "Alimentación y Restaurantes",
  "ORGANIZACIONES DE SERVICIO SOCIAL y DE CARIDAD" , "Servicios",
  "VENTA DE BOTES Y ACCESORIOS PARA ACTIVIDADES ACUATICAS" , "Deporte y Recreación",
  "SUSCRIPCION T.V POR CABLE Y OTROS SERVICIOS DE RADIO Y TELEVISION PAGOS" , "Entretenimiento",
  "ALMACENES DE VIDRIOS Y ESPEJOS" , "Hogar y Construcción",
  "GALERIAS DE ARTE - MARQUETERIAS" , "Arte y Antigüedades",
  "GARAJES Y PARQUEADEROS" , "Servicios",
  "ADMINISTRACION DE AREAS COMUNES, SERVICIOS DE LIMPIEZA, MANTENIMIENTO Y CELADURIA" , "Servicios",
  "SERVICIOS Y ASESORIAS PROFESIONALES" , "Servicios",
  "DISTRIBUIDORES DE MATERIALES PARA COMBUSTIBLE, CARBON, ACEITE COMBUSTIBLE PETROLEO LIQUIDO, MADERA" , "Industria",
  "INMOBILIARIAS" , "Servicios",
  "EVENTOS, FIESTAS Y BANQUETES" , "Eventos",
  "ALMACEN DE BICICLETAS VENTA Y SERVICIOS" , "Deporte y Recreación",
  "LABORATORIOS CLINICOS" , "Salud y Bienestar",
  "SERVICIIOS DE REPRODUCCCION Y FOTOCOPIAS" , "Servicios",
  "ALMACENES DE COLGADURAS, TAPICERIA Y CUBIERTAS PARA VENTANAS" , "Hogar y Construcción",
  "ALMACENES DE MASCOTAS - ALIMENTOS Y ACCESORIOS PARA MASCOTAS" , "Mascotas",
  "SASTRES, MODISTAS, CLINICAS DE ROPA" , "Moda",
  "LAVANDERIAS - LAVASECOS" , "Servicios",
  "FLORISTERIAS" , "Hogar y Construcción",
  "AGENCIAS DE PROTECCIÓN Y SERVICIOS DE SEGURIDAD" , "Seguridad"
  # Continúa el mapeo para todas las categorías
)

# Categorizar la columna 'nombre' en el dataframe df
cell <- left_join(cell, mapeo_categorias, by = c("Sitio_consumo_masfrecuente" = "nombre_original")) %>%
  mutate(nueva_categoria = coalesce(nueva_categoria, "Otra"))



#Conversion de porcentajes a números y los días pasan a ser o entre semana o fin de semana
cell$porcentaje_visa_nacional <- round(cell$porcentaje_visa_nacional*cell$Numero_de_transacciones)
cell$porcentaje_visa_internacional <- round(cell$porcentaje_visa_internacional*cell$Numero_de_transacciones)
cell$porcentaje_mastercard_nacional <- round(cell$porcentaje_mastercard_nacional*cell$Numero_de_transacciones)
cell$porcentaje_mastercard_internacional <- round(cell$porcentaje_mastercard_internacional*cell$Numero_de_transacciones)
cell$Porcentaje_otrafranquicia_nacional <- round(cell$Porcentaje_otrafranquicia_nacional*cell$Numero_de_transacciones)
cell$porcentaje_otrafranquicia_internacional <- round(cell$porcentaje_otrafranquicia_internacional*cell$Numero_de_transacciones)
cell$porcentaje_nacional_total <- round(cell$porcentaje_nacional_total*cell$Numero_de_transacciones)
cell$porcentaje_internacional_total <- round(cell$porcentaje_internacional_total*cell$Numero_de_transacciones)
cell$porcentaje_manana <- round(cell$porcentaje_manana*cell$Numero_de_transacciones)
cell$porcentaje_tarde <- round(cell$porcentaje_tarde*cell$Numero_de_transacciones)
cell$porcentaje_noche <- round(cell$porcentaje_noche*cell$Numero_de_transacciones)
cell$porcDOMINGO <- round(cell$porcDOMINGO*cell$Numero_de_transacciones)
cell$porcLUNES <- round(cell$porcLUNES*cell$Numero_de_transacciones)
cell$porcMARTES <- round(cell$porcMARTES*cell$Numero_de_transacciones)
cell$porcMIERCOLES <- round(cell$porcMIERCOLES*cell$Numero_de_transacciones)
cell$porcJUEVES <- round(cell$porcJUEVES*cell$Numero_de_transacciones)
cell$porcVIERNES <- round(cell$porcVIERNES*cell$Numero_de_transacciones)
cell$porcSABADO <- round(cell$porcSABADO*cell$Numero_de_transacciones)
cell$entreSemana <- rowSums(cell[, c("porcLUNES", "porcMARTES", "porcMIERCOLES", "porcJUEVES")])
cell$finSemana <- rowSums(cell[, c("porcVIERNES","porcSABADO", "porcDOMINGO")])


#cell$visa <- rowSums(cell[, c("porcentaje_visa_nacional", "porcentaje_visa_internacional")])
#cell$mc <- rowSums(cell[, c("porcentaje_mastercard_nacional", "porcentaje_mastercard_internacional")])
#cell$of <- rowSums(cell[, c("Porcentaje_otrafranquicia_nacional", "porcentaje_otrafranquicia_internacional")])


#Escoger variables que van a ser clusterizadas
cell_model <- as.data.frame(cbind(cell[4],cell[14:18]))

#Aplicar logaritmo a promedio de transacción para suavizar su distribución
cell_log<-as.data.frame(apply(cell_model[1],2,log1p))
cell_model<-as.data.frame(cbind(cell_log,cell_model[,2:6]))
cell_model<-as.data.frame(scale(cell_model))

####PCA
#Usamos esta parte para definir cuantas variables vamos a aceptar
componentestotal<-prcomp(cell_model, center=TRUE, scale.=FALSE)
summary(componentestotal)
plot(componentestotal, type="l")


componentes2<-principal(cell_model,nfactors=3,rotate="varimax")
# rotaciones disponibles: "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", and "cluster"
componentes2$communality
colnames(componentes2$loadings) <- c("Nacionales","Internacionales","Promedio")
componentes2$loadings


scores <- as.data.frame(componentes2$scores)

#Añadir los resultados de PCA al dataframe principal
bancopca<-cbind(cell_model,scores)

#Escogemos solo el resultado de PCA
bancopca_total<-bancopca[7:9]
#Identificamos el significado de cada componente principal
colnames(bancopca_total) <- c("nacional", "internacional", "promedio trans")

##Identificar número de clusters por gráfico de codo
#utilizo una semilla para replicar resultados
set.seed(8)
#calculo la suma de cuadrados total
wss <- (nrow(bancopca_total)-1)*sum(apply(bancopca_total,2,var))
#calculo para cada solución de clustering
for (i in 2:15) wss[i] <- sum(kmeans(bancopca_total,
                                     centers=i, nstart=10)$withinss)
plot(1:15, wss, type="b", xlab="Número de Clústeres",
     ylab="Suma de cuadrados within")

#Muestrear datos para poder realizar asw
datos_muestra <- bancopca_total %>%  sample_frac(0.2)

# Evaluar usando el criterio ASW (average sillouethe width)
set.seed(8) #Para evitar aleatoriedad en los resultados
clustering.asw <- kmeansruns(datos_muestra,krange=2:15,criterion="asw",iter.max=100, runs= 100,critout=TRUE)
clustering.asw$bestk

#ejecución de k-means
set.seed(8)
cellcluster<-kmeans(bancopca_total,centers=5,nstart=10,iter.max=20)
#tamaño de grupos
cellcluster$size

#centros de grupos
cellcluster$centers



#validar resultados- consistencia
kclusters <- clusterboot(bancopca_total,B=10,clustermethod=kmeansCBI,k=5,seed=8)
#la validacion del resultado. >0.75 o .85 muy bueno; <.6 malo
kclusters$bootmean

grupos <- cellcluster$cluster
cell <- as.data.frame(cbind(cell,grupos))


write.csv(cell, "resultado_masTrans.csv", row.names = FALSE)





library(ggplot2)
library(reshape2)
centrosg<-as.data.frame(cellcluster$centers)
centrosg$grupo<-as.factor(rownames(centrosg))
centrosheat<-reshape2::melt(centrosg)
colnames(centrosheat)<-c("grupo","variable","centroide")
ggplot(centrosheat,aes(x=grupo,y=variable,fill=centroide, label=sprintf("%0.2f", round(centroide, digits=2))))+geom_tile()+scale_fill_distiller(palette="RdBu")+geom_text()



# Calcula el coeficiente de silueta
coeficiente_silueta <- silhouette(cellcluster$cluster, dist(bancopca_total))
print(coeficiente_silueta)

# Para obtener el coeficiente de silueta promedio
silhouette_avg <- mean(coeficiente_silueta[, "sil_width"])
print(silhouette_avg)


#summary pca
summary(componentestotal)


# Graficar los clusters
# Graficar los clusters en 3D
plot_ly(bancopca_total, x = ~nacional, y = ~internacional, z = ~`promedio trans`, color = ~as.factor(cellcluster), type = "scatter3d", mode = "markers")
