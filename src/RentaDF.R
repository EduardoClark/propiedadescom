#Scrape Propiedades DF Renta
URLs <- paste("http://propiedades.com/alvaro-obregon-df/residencial-renta?pagina=",1:30,"#vista=galeria&cat_searcher=3",sep="")
URLs <- c(URLs,paste("http://propiedades.com/azcapotzalco/residencial-renta?pagina=",1:4,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/benito-juarez-df/residencial-renta?pagina=",1:22,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/coyoacan/residencial-renta?pagina=",1:11,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/cuajimalpa-de-morelos/residencial-renta?pagina=",1:24,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/cuauhtemoc-df/residencial-renta?pagina=",1:24,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/cuauhtemoc-df/residencial-renta?pagina=",1:24,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/gustavo-a-madero/residencial-renta?pagina=",1:3,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/iztacalco/residencial-renta?pagina=",1:2,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/iztapalapa/residencial-renta?pagina=",1:3,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/la-magdalena-contreras/residencial-renta?pagina=",1:3,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/miguel-hidalgo/residencial-renta?pagina=",1:64,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/milpa-alta/residencial-renta?pagina=",1:1,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/tlalpan/residencial-renta?pagina=",1:13,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/tlahuac/residencial-renta?pagina=",1:1,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/venustiano-carranza-df/residencial-renta?pagina=",1:2,"#vista=galeria&cat_searcher=3",sep=""))
URLs <- c(URLs,paste("http://propiedades.com/xochimilco/residencial-renta?pagina=",1:2,"#vista=galeria&cat_searcher=3",sep=""))

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

Renta <- rbind_all(lapply(URLs,Extract))
write.csv(Renta,"data-out/RentaDF.csv",row.names=FALSE)
