library(dplyr)
library(stringr)
library(purrr)
library(rvest)
library(RSelenium)
library(xml2)
library(rvest)
library(selectr)
library(tidyverse)
library(httr)
library(xml2)
library(reticulate)

get_info <- function(link_jugador) {
  pag_jugador = paste0("https://www.whoscored.com", link_jugador) 
  remDr$navigate(pag_jugador)
  
  print(pag_jugador)
  Sys.sleep(5)
  
  historia <-
    remDr$findElement(using = 'xpath', "//*[@id='sub-navigation']/ul/li[4]/a")
  historia$clickElement()
  
  nombre_jugador <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath = "//*[@id='layout-wrapper']/div[3]/div[1]/div[1]/div[2]/div[2]/div[1]") %>%
    html_text() %>% paste(collapse = ",")
  nombre_jugador = gsub("[\r\n]", "", nombre_jugador)
  nombre_jugador = gsub("Name: *", "", nombre_jugador)
  nombre_jugador = str_trim(nombre_jugador)
  
  
  equipo_actual_jugador <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath = "//*[@id='layout-wrapper']/div[3]/div[1]/div[1]/div[2]/div[2]/div[2]/a") %>%
    html_text()
  
  
  numero_camiseta_jugador <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath = "//*[@id='layout-wrapper']/div[3]/div[1]/div[1]/div[2]/div[2]/div[3]") %>%
    html_text()
  numero_camiseta_jugador = gsub("[\r\n]", "", numero_camiseta_jugador)
  numero_camiseta_jugador = gsub("Shirt Number: *", "", numero_camiseta_jugador)
  numero_camiseta_jugador = str_trim(numero_camiseta_jugador)
  
  
  fecha_nacimiento_jugador <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath = "//*[@id='layout-wrapper']/div[3]/div[1]/div[1]/div[2]/div[2]/div[4]/i") %>%
    html_text()
  
  edad_jugador <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath = "//*[@id='layout-wrapper']/div[3]/div[1]/div[1]/div[2]/div[2]/div[4]/text()[2]") %>%
    html_text()
  edad_jugador = gsub("[\r\n]", "", edad_jugador)
  edad_jugador = gsub("\\(", "", edad_jugador)
  edad_jugador = str_trim(edad_jugador)
  edad_jugador = gsub(" years old", "", edad_jugador)
  
  
  
  
  altura_jugador <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath = "//*[@id='layout-wrapper']/div[3]/div[1]/div[1]/div[2]/div[2]/div[5]/text()") %>%
    html_text()
  
  
  
  
  posiciones_jugador <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath = "//*[@id='layout-wrapper']/div[3]/div[1]/div[1]/div[2]/div[2]/div[7]/span[2]/span[1]") %>%
    html_text()
  
  
  
  nacionalidad_jugador <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath = "//*[@id='layout-wrapper']/div[3]/div[1]/div[1]/div[2]/div[2]/div[6]/span[2]") %>%
    html_text()
  
  nacionalidad_jugador = str_trim(nacionalidad_jugador)
  
  
  estadisticas_jugador <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath = " //*[@id='statistics-table-summary']") %>%
    html_table()  %>% as.data.frame()
  
  
  datos_jugador = list(
    link_jugador = link_jugador,
    nombre_jugador = nombre_jugador,
    equipo_actual_jugador = equipo_actual_jugador,
    numero_camiseta_jugador = numero_camiseta_jugador,
    fecha_nacimiento_jugador = fecha_nacimiento_jugador,
    edad_jugador = edad_jugador,
    altura_jugador = altura_jugador,
    posiciones_jugador = posiciones_jugador,
    nacionalidad_jugador = nacionalidad_jugador,
    estadisticas_jugador = estadisticas_jugador
  )
  
  return(datos_jugador)
  
}

df_jugadores =
  data.table::fread("files/dataset/input/webscrapping/jugadores_total.csv")

names(df_jugadores) =
  c("id", "name", "link", "country")

chrome_version = "107.0.5304.18" # Maca: "106.0.5249.21" / Rodo: "107.0.5304.107"

rD <- RSelenium::rsDriver(browser = "chrome",
                          chromever = chrome_version)
remDr <- rD[["client"]]

url_principal <- "https://www.whoscored.com/Statistics"
remDr$navigate(url_principal)

datos_jugadores = 
  df_jugadores |> 
  # head(2) |> 
  select(link) |> 
  unlist() |> 
  unname() |> 
  map(.f = get_info)

# Save dataset ------------------------------------------------------------

file_name = "files/dataset/intermedia/datos_jugadores"

datos_jugadores |>
  saveRDS(paste0(file_name, 
                 ".rds"))

datos_jugadores |> 
  py_save_object(paste0(file_name, 
                        ".pickle"), 
                 pickle = "pickle")
  
