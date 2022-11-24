# remDr$close()
# rm(rD)
# gc()
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
#t <- proc.time()


# rD[["server"]]$stop()

rD <- RSelenium::rsDriver(browser = "chrome",
                          chromever = "106.0.5249.21")
remDr <- rD[["client"]]


webElem <-
  remDr$findElement(using = 'xpath', value = "//*[@id='seasons']")
webElem$clickElement()

año <-
  remDr$findElement(using = 'xpath', "/*[@id='seasons']/option[7]")
año$clickElement()



url_principal <- "https://www.whoscored.com/Statistics"
remDr$navigate(url_principal)


cookies <-
  remDr$findElement(using = 'xpath', "//*[@id='qc-cmp2-ui']/div[2]/div/button[2]")
cookies$clickElement()

Sys.sleep(10)


# players <- remDr$findElement(using = 'xpath', "//*[@id='sub-navigation']/ul/li[4]/a")
# players$clickElement()
#remDr$setImplicitWaitTimeout(15000)


allplayers <-
  remDr$findElement(using = 'xpath', "//*[@id='apps']/dd[2]/a")
allplayers$clickElement()

Sys.sleep(10)

paginas <- remDr$getPageSource()[[1]] |>
  read_html() |>
  html_nodes(xpath =  "//*[@id='totalPages']") |>
  html_attr('value')
paginas = paginas[2]
paginas = as.numeric(paginas)

Sys.sleep(10)
inicio = 1
fin = paginas



df_jugadores = data.frame(matrix(nrow = 0, ncol = 4))

colnames(df_jugadores) = c("ranking", "nombre", "link", "team")
Sys.sleep(10)

for (i in inicio:fin) {
  ranking_jugadores <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath =  "//*[@id='player-table-statistics-body']/tr/td[1]/a[1]/div") %>%
    html_text()
  ranking_jugadores <- gsub("\t", "", ranking_jugadores)
  
  
  nombres_jugadores <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath =  "//*[@id='player-table-statistics-body']/tr/td[1]/a[1]/span/text()") %>%
    html_text()
  
  team_jugadores <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath =  "//*[@id='player-table-statistics-body']/tr[1]/td[1]/a[2]/span") %>%
    html_text()
  team_jugadores <- gsub(", ", "", team_jugadores)
  
  
  links_jugadores <- remDr$getPageSource()[[1]] %>%
    read_html() %>%
    html_nodes(xpath =  "//*[@class='player-link']") %>%
    html_attr("href")
  
  df_sub_jugadores = data.frame(ranking_jugadores,
                                nombres_jugadores,
                                links_jugadores,
                                team_jugadores)
  
  colnames(df_sub_jugadores) = c("ranking", "nombre", "link", "team")
  
  df_jugadores = rbind(df_jugadores, df_sub_jugadores)
  
  
  siguiente <-
    remDr$findElement(using = 'xpath',
                      "/html/body/div[4]/div[3]/div[5]/div[1]/div[4]/div/dl[2]/dd[3]/a")
  siguiente$clickElement()
  
  print(i)
  Sys.sleep(10)
}

folder_path = "files/dataset/input/webscrapping/"
write.table (
  df_jugadores,
  paste0(folder_path,
         "jugadores_total.csv"),
  append = TRUE,
  sep = "|",
  col.names = FALSE,
  row.names = FALSE
)

