#
#Use the libraries
library(jsonlite) #https://cran.r-project.org/web/packages/jsonlite/
library(httr)
library(dplyr)
library(stringr)
library(curl)

#Busqueda  per projecte https://minka-sdg.org/projects/search.json?q=Biomarato 2025

funcio_taxonomica <- function(nom_a_buscar) {

  #Separem el string del  taxo a buscar en dos i ho assignem a un dataframe per poder accedir per separat.

  esp <- str_split(nom_a_buscar, " ")
  especie <- data.frame(esp)

  #Si el taxo es simple (genere, clase o ordre,..)  construim la part de l URL de l especie de  diferenta manera

  if (is.na(especie[2,1]))

  {urlespecie <- paste(especie[1,1])

  }else{

    #Si el taxo es una especie concreta l url es construeix de la següent manera

    urlespecie <- paste(especie[1,1],"%20",especie[2,1])
  }


  #Construccio de la part final del link amb la part variable de l especie incloent especies marines o no

  urlNamesPart <- ""

  urlNamesPart <- paste(urlespecie,"?like=false&marine_only=false&offset=1")

  #Construim la resta de la cadena del link

  url_esp <- sprintf("https://www.marinespecies.org/rest/AphiaRecordsByName/%s",urlNamesPart)

  #Eliminem espais en blank que donen errors.

  url <- gsub(" ","",url_esp)

  matches <- fromJSON(url)

}


#######################################################################################################################################

#Creem funcio de busqueda fent servir gestio d´escepcions per capturar errors. Gestionem la taula que trobem nomes amb les dades q interesen

# 18/08/22

####################################################################################################################################


get_taxonomy_marine= function(y){
  tryCatch({

    warn= FALSE

    dades<-funcio_taxonomica(y)

    #Resultat (vector) de la busqueda en WORMS

    info<-c(as.character(dades[["AphiaID"]]), dades[["scientificname"]],
            dades[["rank"]], dades[["isMarine"]], dades[["kingdom"]],
            dades[["phylum"]],dades[["class"]], dades[["order"]],
            dades[["family"]],dades[["genus"]])


  },

  error=function(cond) {

    #Si no es troba l´especie s´omple el vector de resultat amb NA i no apareix missatge d´error

    info <-c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

  })
}

d <- get_taxonomy_marine("Larus cachinnans")
d
