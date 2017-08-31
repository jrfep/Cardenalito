##R --vanilla
require(raster)
require(compositions)

archivo <- "~/Dropbox/ceba"
archivo.rda <- sprintf("%s/Rdata/20170821_EVI_Cardenalito.rda",archivo)
(load(archivo.rda))

##Los datos son derivados del producto https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13q1 (versión 5).
## el objeto 'vlr' contiene los datos del índice de vegetación medido quincenalmente en 191 localidades de muestreo.
## el objeto 'qctB' contiene los datos del control de calidad del índice de vegetación medido quincenalmente en 191 localidades de muestreo en formato binario.

##extraemos
CalidadPixel <- substr(qctB,15,16)
Utilidad <- substr(qctB,11,14)
Tierra_Agua <- substring(qctB, 3, 5)

## porcentaje en cada categoría de control de calidad
table(CalidadPixel)
table(CalidadPixel)/length(CalidadPixel)
##00 es buena calidad (peso = 1), 01 es calidad decreciente (pesos variables), y 10 y 11 son de calidad insuficiente (peso=0)

##Para cada fila calculamos:
for (k in 1) {
    pesos <- as.numeric(CalidadPixel[k,])*0
    pesos[CalidadPixel[k,]=="00"] <- 1
    ##Para calcular los pesos según el valor de Utilidad, consideramos los primeros 10 valores como válidos para los análisis
    pesos[CalidadPixel[k,] %in% "01"] <- (10-unbinary(Utilidad[k,][CalidadPixel[k,] %in% "01"]))/10
    plot(vlr[k,],col=grey(pesos),pch=19)
}




##fuentes consultadas:
##https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13q1
##https://lpdaac.usgs.gov/sites/default/files/public/modis/docs/MODIS_LP_QA_Tutorial-1b.pdf
