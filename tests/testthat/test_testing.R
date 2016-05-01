context("Testing ALL")
print("")
library(XML)
library(sets)

# Cargamos los ficheros, este proceso es lento pero solo se tiene que hacer la primera vez.
#prepareGlobalVars()


# Cambiamos de directório para que se pueda leer los ficheros de la raiz sin cambiar el
#  código principal, ya que está hardcodeado.
library(pracma)
path_old <- pwd()
path_new <- paste(path_old, "/../../../banner2cpe/" , sep = "", collapse = NULL)
cd(path_new)
print("Cambiando a directorio raiz para leer los ficheros.")


# Entrada sacada de un título del fichero CPE, tiene que quedar en la primera posicion.
print("[findCPE] Un Titulo sacado del fichero CPE, se encuentra su CPE:")
entrada <- "1800contacts 1800CONTACTS App (aka com.contacts1800.ecomapp) for Android 2.7.0"
salida  <- "cpe:/a:1800contacts:1800contacts_app:2.7.0::~~~android~~"
test_that("Check a good result", {
  out <- as.vector((findCPE(entrada)$cpe))[1]
  expect_equal( out, salida)
})
print("[OK]")


# Comprobamos que la función de limpieza de strings funciona correctamente.
print("[cleanSentence] La función que limpia los strings, sigue funcionando:")
entrada <- '1800contacts 1800CONTACTS App (aka com.contacts1800.ecomapp) for Android 2.7.0'
salida  <- "1800contacts 1800contacts app aka com.contacts1800.ecomapp android 2.7.0"
test_that("Clean a Banner/CPE Title", {
  out <- cleanSentence(entrada)
  expect_equal( out, salida ) 
})
print("[OK]")




# La función que de un banner(cset), un df(data.frame) que contiene name(codigo CPE), 
#  title(titulo del CPE) y titleSet(set del titulo del CPE limpio) y esta función compara los
#  sets, les da un valor, si este es menor a un linde lo elimina y se queda con los 10 sets mejores:
#print("[FindCPEwithWord] La función que mira la similitud entre un banner y un titulo de un CPE")
#entrada_banner    <- as.cset(c("1800contacts", "app", "aka", "com.contacts1800.ecomapp", "android", "2.7.0"))
#entrada_factor    <- as.numeric(1)
#names    <- "cpe:/a:1800contacts:1800contacts_app:2.7.0::~~~android~~"
#titles   <- "1800contacts 1800contacts app aka com.contacts1800.ecomapp android 2.7.0"
#titlesSet <- c("1800contacts", "2.7.0", "aka", "android", "app", "com.contacts1800.ecomapp")
#entrada_df   <- data.frame(names, titles, titlesSet)
#names    <- "cpe:/a:1800contacts:1800contacts_app:2.7.0::~~~android~~"
#factor       <- 1
#titles <- "1800contacts 1800contacts app aka com.contacts1800.ecomapp android 2.7.0"
#salida   <- data.frame(names, factor, titles)
#print(class(salida))
#print(salida)
#test_that("Compare a banner set with a title set", {
#  salida_func <- FindCPEwithWord(entrada_banner, entrada_df, entrada_factor)
#  
#  print("---------------------")
#  print(class(salida))
#  print(salida)
#  
#  expect_equal( salida_func$names, salida$names )
#  expect_equal( salida_func$titles, salida$titles ) 
#  expect_equal( salida_func$factor, salida$factor ) 
#})
#print("[OK]")


# Se compara la primera linea de lo que se saca del fichero de los CPE, esto no debería de cambiar por ser la primera
print("[loadXML] Se lee el fichero de CPEs y se extraen los titulos CPE y el codigo CPE")
entrada   <- 'inst/exdata/official-cpe-dictionary_v2.3.xml'
salida_1  <- as.list("cpe:/a:%240.99_kindle_books_project:%240.99_kindle_books:6::~~~android~~")[[1]]
salida_2  <- as.list("$0.99 Kindle Books project $0.99 Kindle Books (aka com.kindle.books.for99) for android 6.0")[[1]]
test_that("Lee el XML de los CPEs", {
  matrix <- loadXML(entrada)
  expect_equal(matrix[1,1][[1]], salida_1)
  expect_equal(matrix[1,2][[1]], salida_2)
})
print("[OK]")



# El banner que nos llega, lo limpiamos y eliminamos todas las palabras que no existan en los titulos de los CPE
print("[prepareBanner] Limpiamos un banner quitandole las palabras que NO conocemos:")
entrada_banner    <- 'AAA DDD EEE'
entrada_wordlist  <- set("aaa", "bbb", "ccc", "ddd")
salida            <- as.vector(set("aaa", "ddd"))
test_that("Prepare a Banner", {
  out <- as.vector(prepareBanner(entrada_banner,entrada_wordlist))
  expect_equal(out, salida ) 
})
print("[OK]")


# Testeando que se hace bien la conversión de una frase a un set ordenado para eliminar repetidas
library(sets)
print("[sentenceToSet] La función transforma frases en sets ordenados sin palabras repetidas:")
entrada <- "1800contacts 1800contacts app aka com.contacts1800.ecomapp android 2.7.0"
salida  <- as.cset(set("1800contacts", "app", "aka", "com.contacts1800.ecomapp", "android","2.7.0"))
test_that("Frase a Set", {
  expect_equal( sentenceToSet(entrada) , salida ) 
})
print("[OK]")


# Comprobamos la similitud entre dos conjuntos de palabras haciendo la formula de Jaccard
library(sets)
print("[similarity] Vemos la similaridad entre dos conjuntoss:")
entrada_1 <- as.cset(set("aaa", "bbb", "ccc", "ddd"))
entrada_2 <- list("aaa", "bbbb", "ccc")
salida    <- c(0.25, 0.00, 0.25)
test_that("Similaridad entre 2 conjuntos", {
  out <-  similarity(entrada_1, entrada_2)
  expect_equal( out , salida ) 
})
print("[OK]")


print("-----------------------------------------------------------------------")
print("---- Todos los TEST de Banner2CPE pasados! :D -------------------------")
print("-----------------------------------------------------------------------")

print(path_old)
print(path_new)
print(pwd())