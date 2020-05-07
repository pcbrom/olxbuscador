library(stringr)
library(dplyr)
library(doSNOW)
library(rvest)

# url da primeira pagina
URL <- "https://df.olx.com.br/computadores-e-acessorios/notebook-e-netbook"

# total de paginas
N <- URL %>% 
  read_html() %>% 
  html_nodes("div ul li a") %>%
  html_attr("href") %>% 
  str_subset("o=") %>% 
  str_extract("\\d+") %>% 
  as.numeric() %>% 
  max()

# criando urls
URL <- paste0(URL, "?o=", 1:N, "&ssf=1")


# coletando todos os links ----------------------------------------------------

# funcao para coleta
get_links <- function(URL) {
  x <- URL %>% 
    read_html() %>% 
    html_nodes("div ul li a") %>%
    html_attr("href") %>% 
    str_subset("[0-9]{7,12}$")
  return(x)
}

cores = parallel::detectCores()
cl = makeSOCKcluster(cores)
registerDoSNOW(cl)
pb = txtProgressBar(min = 1, max = N, style = 3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress = progress)
links = foreach(
  i = 1:N, 
  .options.snow = opts, 
  .combine = 'c',
  .packages = c("stringr", "dplyr", "rvest")
) %dopar% {
  get_links(URL = URL[i])
}


# montando o data frame -------------------------------------------------------

# funcao para coletar informacao dentro de cada link
get_info <- function(link) {
  tmp <- link %>% 
    read_html()
  titulo <- tmp %>% 
    html_nodes("div div div h1") %>% 
    html_text()
  valor <- tmp %>% 
    html_nodes("div div div h2") %>% 
    html_text() %>% 
    {.[1]} %>% 
    str_remove("\\D+") %>% 
    str_remove("\\.") %>% 
    as.numeric()
  descricao <- tmp %>% 
    html_nodes("div div p span") %>% 
    html_text()
  x <- cbind.data.frame(titulo, valor, descricao, link)
  return(x)
}

# coletando
total <- length(links)
pb = txtProgressBar(min = 1, max = total, style = 3)
db = foreach(
  i = 1:total, 
  .options.snow = opts, 
  .combine = 'rbind.data.frame',
  .packages = c("stringr", "dplyr", "rvest")
) %dopar% {
  res <- tryCatch({
    get_info(link = links[i])
  }, error = function(e) NULL)
}

# fechando barra de progresso e clusters
close(pb)
stopCluster(cl)

# salvando os resultados
write.csv2(db, "resultados.csv", row.names = F)
