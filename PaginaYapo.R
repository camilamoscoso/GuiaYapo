setwd("~/GitHub/Bigdata2021")

#Librerias
library("xml2")
library("rvest")
install.packages("dplyr")
library("dplyr")

  # Datos de un subsitio

ObtenerTipo <- function(linkSubPagina){
  resultadoTipo <- NA


  # Abrir subsitio

  subPaginaYapo <- read_html(linkSubPagina)

  boxProductoYapo <- html_nodes(subPaginaYapo, css=".info")

  # Tipo de transacción

  detailProductoYapo <- html_nodes(boxProductoYapo, css=".details")
  trDetailsProductos <- html_nodes(detailProductoYapo,css="tr")

  for (detail in trDetailsProductos) {
    thDetail <- html_nodes(detail, css="th")
    if(html_text(thDetail)=="Tipo"){
      tdDetail <- html_nodes(detail,css="td")
      print(html_text(tdDetail))
      resultadoTipo <- html_text(tdDetail)
    }
  }
  return(resultadoTipo)
}


# Cargar archivos antiguos

if(file.exists("DatosYapo.csv")){
  DatosEnArchivosCsv <- read.csv("DatosYapo.csv")
# Borrar columna X
  DatosEnArchivosCsv["X"] <- NULL
}
seCargoAntiguo=FALSE
if(file.exists("DatosYapo2.csv")){
  DatosEnArchivosCsv2 <- read.csv2("DatosYapo2.csv")
# Borrar columna X
  DatosEnArchivosCsv2["X"] <- NULL
  seCargoAntiguo=TRUE
}

#Definiendo variables para luego ordenar y calcular datos

valorUF <- 30887.21
precioNormal <- c()
PrecioCalculado <- c()
vectorValorUF <- c()
tipoMoneda <- c()
tipoTransaccion <- c()

for (pagina in 1:5) {
  #leyendo pagina

  PaginaYapo <- read_html(paste("https://www.yapo.cl/region_metropolitana?ca=15_s&o=",pagina,sep = ""))
  print(html_text(PaginaYapo))

  # Clase listing_thumbs
  listingThumbs <- html_nodes(PaginaYapo, css=".listing_thumbs")
  print(html_text(listingThumbs))

  # Obteniendo los titulos de los productos
  nombresProductos <- html_nodes(listingThumbs, css=".title")
  print(html_text(nombresProductos))

  # Obteniendo los categoria de los productos
  categoriaProductos <- html_nodes(listingThumbs, css=".category")
  print(html_text(categoriaProductos))

  # Obteniendo la region de los productos
  regionProductos <- html_nodes(listingThumbs, css=".region")
  print(html_text(regionProductos))

  # Obteniendo las comunas de los productos
  comunaProductos <- html_nodes(listingThumbs, css=".commune")
  print(html_text(comunaProductos))

  # Obteniendo los precios
  preciosProductos <- html_nodes(listingThumbs, css=".price")
  print(html_text(preciosProductos))

  # Obteniendo el link de los productos

  seccionImagenProductoYapo <- html_nodes(listingThumbs,css=".listing_thumbs_image")
  linkProductoYapo <- html_nodes(seccionImagenProductoYapo,css="a")
  hrefProductoYapo <- html_attr(linkProductoYapo,"href")
  length(hrefProductoYapo)
  print(hrefProductoYapo)
  print(hrefProductoYapo[1])



  for (elemento in 2:length(listingThumbs)) {
    print(paste("################Elemento", elemento-1, "- pagina", pagina, "###############"))
    print(html_text(nombresProductos[elemento-1]))

    # Obteniendo tipo
    tipo <- ObtenerTipo (hrefProductoYapo[elemento-1])
    tipoTransaccion <- c(tipoTransaccion,tipo)
    precio <- html_nodes(listingThumbs[elemento],css=".price")
    if(length(precio)>0){
      precio <- html_text(precio)
      precio <- gsub("\t","",precio)
      precio <- gsub("\n","",precio)
      precio <- gsub("[$]","",precio)
      precio <- gsub("[.]","",precio)
      precio <- gsub(",",".",precio)
      precio <- gsub(" ","",precio)

      if(substr(precio, 1, 2) == 'UF'){
        precio <- gsub("UF","",precio)
        precio <- as.numeric(precio)

        # guardando precio normal
        precioNormal <- c(precioNormal,precio)

        precio <- precio*valorUF
        PrecioCalculado <- c(PrecioCalculado,precio)
        vectorValorUF <- c(vectorValorUF, valorUF)
        tipoMoneda <- c(tipoMoneda,"UF")
      }else{
        precio <- as.numeric(precio)
        precioNormal <- c(precioNormal,precio)
        PrecioCalculado <- c(PrecioCalculado,NA)
        vectorValorUF <- c(vectorValorUF, NA)
        tipoMoneda <- c(tipoMoneda,"peso")
      }
    }else{
      precio <- NA
      precioNormal <- c(precioNormal,precio)
      PrecioCalculado <- c(PrecioCalculado,NA)
      vectorValorUF <- c(vectorValorUF, NA)
      tipoMoneda <- c(tipoMoneda,NA)
    }
    print(precio)
  }

}


# Data.frame

nuestrosDatos <- data.frame(nombre=html_text(nombresProductos),
                            categoria=html_text(categoriaProductos),
                            region=html_text(regionProductos),
                            comuna=html_text(comunaProductos),
                            tipoTransaccion=tipoTransaccion,
                            precioNormal=precioNormal,
                            PrecioCalculado=PrecioCalculado,
                            tipoMoneda=tipoMoneda,
                            valorUF=vectorValorUF,
                            linkProducto=hrefProductoYapo)

#Unir información con archivos antiguos

if(seCargoAntiguo){
  nuestrosDatos <- rbind(DatosEnArchivosCsv,nuestrosDatos)
}


write.csv(nuestrosDatos,"DatosYapo.csv")
write.csv2(nuestrosDatos,"DatosYapo2.csv")

#eliminando duplicados

distinct(nuestrosDatos)

#contando duplicados
nrow(nuestrosDatos[duplicated(nuestrosDatos), ])

#muestra de datos duplicados
nuestrosDatos[duplicated(nuestrosDatos), ]

