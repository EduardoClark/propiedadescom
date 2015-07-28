#Scrape Propiedades DF Venta
URLs <- paste("http://propiedades.com/alvaro-obregon-df/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep="")
URLs <- c(URLs,paste("http://propiedades.com/azcapotzalco/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/benito-juarez-df/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/coyoacan/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/cuajimalpa-de-morelos/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/cuauhtemoc-df/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/cuauhtemoc-df/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/gustavo-a-madero/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/iztacalco/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/iztapalapa/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/la-magdalena-contreras/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/miguel-hidalgo/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/milpa-alta/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/tlalpan/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/tlahuac/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/venustiano-carranza-df/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/xochimilco/residencial-venta?pagina=",1:100,"#vista=galeria&cat_searcher=3",sep=""))

Extract <- function(URL){
  Doc <- htmlParse(URL)
  Address <- xpathSApply(Doc,"//span[@itemprop='streetAddress']",xmlValue) 
  Latitude <- xpathSApply(Doc,"//meta[@itemprop='latitude']/@content")
  Longitude <- xpathSApply(Doc,"//meta[@itemprop='longitude']/@content")
  Colonia <- xpathSApply(Doc,"//div[@class='colonia']",xmlValue) 
  CP <- xpathSApply(Doc,"//span[@itemprop='postalCode']",xmlValue) 
  Localidad <- xpathSApply(Doc,"//span[@itemprop='addressLocality']",xmlValue) 
  Estado <- xpathSApply(Doc,"//span[@itemprop='addressRegion']",xmlValue) 
  Info <- xpathApply(Doc,"//div[@class='info-list-prop']",xmlValue) 
  Metros <- function(Doc1){
    tmp <- data.frame(
      Metros = as.numeric(substr(Doc1,start = regexpr(Doc1,pattern = "m2")-5,stop = regexpr(Doc1,pattern = "m2")-1)),
      Recamaras = as.numeric(substr(Doc1,start = regexpr(Doc1,pattern = "Rec")-7,stop = regexpr(Doc1,pattern = "Rec")-1)),
      Bath = as.numeric(substr(Doc1,start = regexpr(Doc1,pattern = "Baños")-7,stop = regexpr(Doc1,pattern = "Baños")-1)),
      Precio = as.character(substr(Doc1,start = regexpr(Doc1,pattern = "\\$")+1,stop = regexpr(Doc1,pattern = "\\$")+15)),
      Tipo= substr(Doc1,start = regexpr(Doc1,pattern =  "m2\n                                     ")+30,regexpr(Doc1,pattern = "m2\n                               ")+70)
    )
  }
  Info <- rbind_all(lapply(Info,Metros))
  Info <- data.frame(Address, Colonia, Localidad, Estado, CP, Latitude, Longitude, Info )
  return(Info)
}

venta <- rbind_all(lapply(URLs,Extract))
write.csv(venta,"data-out/VentaDF.csv",row.names=FALSE)
