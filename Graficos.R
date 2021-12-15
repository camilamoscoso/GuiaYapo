setwd("~/GitHub/Bigdata2021")
install.packages("ggplot2")
library(ggplot2)
library(graphics)


  DatosEnArchivosCsv2 <- read.csv2("DatosYapo2.csv")
  DatosEnArchivosCsv2["X"] <- NULL

  table(DatosEnArchivosCsv2["categoria"])
  table(DatosEnArchivosCsv2["region"])
  table(DatosEnArchivosCsv2["comuna"])
  table(DatosEnArchivosCsv2["tipoMoneda"])
  table(DatosEnArchivosCsv2["tipoTransaccion"])
  
#######DIFERENTES TIPOS DE GRAFICOS############ 
  
  
  #Grafico geometrico de comunas con mas transacciones
  ggplot(DatosEnArchivosCsv2,aes(x=comuna))+
           geom_bar() +
    coord_flip()
  
  #Grafico geometrico de categoria con mas transacciones
  ggplot(DatosEnArchivosCsv2,aes(x=categoria))+
    geom_bar() +
    coord_flip()
  
  #Grafico de cajascategoria y precio normal
  
  ggplot(DatosEnArchivosCsv2,aes(x=categoria, y=precioNormal))+
    geom_boxplot() +
    coord_flip()
  
  #Grafico de barras con colores categorias
  barplot(table(DatosEnArchivosCsv2["categoria"]),main="Categorias", col=rainbow(10),
          las=1,
    horiz = TRUE)
  
  #Grafico Diagrama de Sectores entre tipo de moneda, con pesos y UF
  pie(table(DatosEnArchivosCsv2["tipoMoneda"]))
  
  #grafico tipo de transaccion 
  barplot(table(DatosEnArchivosCsv2["tipoTransaccion"]),main="Tipo de transaccion", col=rainbow(15),
          las=1,
          horiz = TRUE)
  
 #grafico precioNormal- tipoMoneda
  
  barplot(tab_datos,
          main = "PrecioNormal-TipoMoneda",
          xlab = "tipoMoneda", ylab = "precioNormal",
          legend.text = rownames(tabla_variables),
          beside = TRUE)
  
 #Grafico cantidad de comunas que transan productos
  barplot(table(DatosEnArchivosCsv2["comuna"]),main="Comunas", col=rainbow(10),
          las=1,
          horiz = TRUE)
  
  #GRAFICO CAJA Y BIGOTE
  boxplot(precioNormal~tipoMoneda,col="red",xlab="tipoMoneda",ylab="precioNormal",
          main="Grafico caja y bigote")
  
  
  
  