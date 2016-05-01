#cat official-cpe-dictionary_v2.3.xml | grep '<title xml:lang="en-US">' | awk -F'>' '{print $2}' | awk -F'<' '{print $1}' | tr '\/' ' ' | tr '\,' ' ' | tr '\[' ' ' | tr '\]' ' ' | tr '\(' ' ' | tr '\)' ' ' | tr '\-' ' ' | tr '\_' ' ' | tr '\:' ' ' | tr ' ' '\n' | tr [:upper:] [:lower:] | sort -u  | grep -v '^$' > titles.txt
#comm -23 titles.txt ourStopWords.txt > titlesWithoutStopWords.txt






#' cleanSentence
#'
#' Sustituye del string s los caracteres especiales por espacio, espacios dobles por espacio, elimina stopwords, y lo convierte en minúsculas.
#'  
#'
#' @param s 
#'
#' @return
#' @export
#'
#' @examples
cleanSentence <- function(s) {
  s <- gsub('[])/,([]', ' ', s)
  s <- gsub('[-_:]', ' ', s)
  s <- tolower(s)
  s <- tm::removeWords(s, tm::stopwords(kind = 'en'))
  s <- gsub('\\s+', ' ', s)
  s <- gsub('^ ', '', s)
  s
}


#' findCPE
#'
#' Recibe un banner, lo normaliza, por cada una de las palabras que contiene el banner, obtiene los títulos en los que están presentes,
#' y, mediante una función de similitud (índice Jaccard), obtiene un factor de concordancia parcial. Con este resultado, agrupa los 
#' resultados por CPE, sumando los factores parciales de cada uno de ellos y obteniendo un resultado final por cada CPE. Éstos se ordenan
#' por su factor y se devuelve los diez CPEs con el mejor factor de concordancia.
#'
#' @param banner 
#'
#' @return
#' @export
#'
#' @examples
findCPE <- function(banner){
  if(exists("dataFrame") == FALSE){
    load(file = 'R/dataFrame.rda')
    load(file = 'R/titlesWordList.rda')
    dataFrame <<- dataFrame
    titlesWordList <<- titlesWordList
  }
  st <- Sys.time()
  print(banner)
  bannerSet <- prepareBanner(banner, titlesWordList)
  print(bannerSet)
  ndf <- NULL
  i <- 0
  lbs <- length(bannerSet)
  rdf <- sapply(bannerSet, function(word){
    print(word)
    factor <- (lbs - i) / lbs
    pattern <- paste('(^| )', word, '( |$)', sep = '')
    dataframeWithWord <- dataFrame[grep(pattern, dataFrame$titles), ]
    tdf <- FindCPEwithWord(bannerSet, dataframeWithWord, factor)
    ndf <<- rbind(ndf, tdf)
    i <<- i + 1
  })
  cutScore <- 0.07
  tmp <- aggregate(ndf$factor, by = list(CPE = ndf$names), FUN = sum)
  tmp <- data.frame(tmp$CPE[tmp$x > cutScore], tmp$x[tmp$x > cutScore])
  colnames(tmp) <- c('cpe', 'factor')
  tmp[order(-tmp$factor), ]
}



#' FindCPEWithWord
#'
#' Devuelve los CPEs que concuerdan mejor entre el banner y un conjunto de títulos dado (un subconjunto formado por títulos que contienen
#' una palabra del banner).
#'
#' @param banner 
#' @param df 
#' @param factor 
#'
#' @return
#' @export
#'
#' @examples
FindCPEwithWord <- function(banner, df, factor){
  print(factor)
  percentages <- similarity(banner, df$titlesSet) * factor
  cutScore <- 0
  ndf <- data.frame(names = unlist(df$names)[percentages > cutScore], 
                    factor = percentages[percentages > cutScore], 
                    titles = unlist(df$titles)[percentages > cutScore]
                    )
  ndf <- head(ndf[order(-ndf$factor, ndf$names),], n = 10)
  ndf
}


# Función genérica que carga un fichero (indicado como parámetro) y devuelve un conjunto de palabras.

getFromFile<- function(filePath){
  st <- Sys.time()
  words <- scan(filePath, what = 'character', sep = '\n')
  et <- Sys.time()
  print(et - st)
  sets::as.set(words)
}

# Carga el fichero xml, lo parsea y devuelve una matriz de caracteres.

loadXML <- function(xmlFile){
  st <- Sys.time()
  doc <- XML::xmlTreeParse(xmlFile, useInternalNodes = TRUE)
  rootNode <- XML::xmlRoot(doc)
  nameSpace <- XML::xmlNamespace(rootNode)
  print('[+] XML loaded')
  et <- Sys.time()
  print(et - st)
  cpeNames <- '/ns:cpe-list/ns:cpe-item'
  names <- XML::xpathApply(rootNode, cpeNames, XML::xmlGetAttr, 'name', namespaces = c(ns = nameSpace))
  print('[+] Names loaded')
  et <- Sys.time()
  print(et - st)
  cpeTitles <- '/ns:cpe-list/ns:cpe-item/ns:title[@xml:lang="en-US"]'
  titles <- XML::xpathSApply(rootNode, cpeTitles, XML::xmlValue, namespaces = c(ns = nameSpace))
  print('[+] Titles loaded')
  et <- Sys.time()
  print(et - st)
  cbind(names, titles)
}

#' Title
#'
#' Normaliza el banner, y elimina todas aquellas palabras que no estén contenidas en la lista de títulos.
#'
#' @param b 
#' @param wl 
#'
#' @return
#' @export
#'
#' @examples
prepareBanner <- function(b, wl) {
  bs <- sentenceToSet(cleanSentence(b))
  r <- bs & wl
  words <- strsplit(cleanSentence(b), ' ')[[1]]
  fs <- NULL
  sapply(words, function(x){
    if(length(sets::as.set(x) & r) == 1){
      fs <<- paste(fs, x)
    }
  })
  sentenceToSet(cleanSentence(fs))
}

# Modificamos la matriz añadiendo una tercera columna que contiene los títulos de los CPEs en conjuntos de palabras, devolviendo un data frame.

prepareDataframe <- function(m) {
  st <- Sys.time()
  t <- sapply(m[, 2], cleanSentence)
  print('[+] Titles ready')
  et <- Sys.time()
  print(et - st)
  ts <- lapply(t, sentenceToSet)
  print('[+] TitlesSet ready')
  et <- Sys.time()
  print(et - st)
  ndf <- as.data.frame(cbind(m[, 1], t, ts))
  colnames(ndf) <- c('names', 'titles', 'titlesSet')
  print('[+] Dataframe ready')
  et <- Sys.time()
  print(et - st)
  ndf
}

#' prepareGlobalVars
#'
#' Carga de distintos ficheros información y lo vuelca en variables globales, utilizadas para el correcto funcionamiento de todas las funciones. 
#'
#' @return
#' @export
#'
#' @examples
prepareGlobalVars <- function(){
  xmlFile <- 'inst/exdata/official-cpe-dictionary_v2.3.xml'
  matrix <- loadXML(xmlFile)
  dataFrame <- prepareDataframe(matrix)
  titlesWordList <- getFromFile('inst/exdata/titlesWithoutStopWords.txt')
  save(dataFrame, file = 'R/dataFrame.rda')
  save(titlesWordList, file = 'R/titlesWordList.rda')
}

# Dado un string lo convierte en un conjunto de palabras

sentenceToSet <- function(sentence) {
  s <- as.character(sentence)
  w <- strsplit(s, ' ')[[1]]
  uw <- unique(w)
  fs <- NULL
  sapply(uw, function(x){
    if(nchar(x) > 1){
      fs <<- paste(fs, x)
    }
  })
  sets::as.cset(strsplit(cleanSentence(fs), ' ')[[1]])
}

# Función que compara dos conjuntos de palabras y devuelve un factor de concordancia, en función de su similitud.

similarity <- function(sa, list){
  percentages <- sapply(list, function(sb){
    exactMatch <- length(sa & sb) / length(sa)
    sets::set_similarity(sa, sb, method = 'Jaccard') #* exactMatch 
  })
  as.vector(percentages)
}


