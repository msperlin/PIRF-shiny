Get_TD <- function(src = 'local') {

  require(GetTDData)
  require(purrr)
  require(tidyverse)

  my.f <- '../data/TD.rds'

  if (src == 'local') {
    df <- readr::read_rds(my.f)
  } else {
    download.TD.data(asset.codes = NULL)
    df <- read.TD.files()

    # fix names (current TD names)
    df$asset.code2 <- map2_chr(df$asset.code, df$matur.date, fix_TD_names)

    # remove non used assets
    df <- df %>%
      filter(!str_detect(asset.code, 'NTN-C'))

    readr::write_rds(df, my.f)

  }

  return(df)

}

fix_TD_names <- function(str.in, matur.date) {

  require(stringr)

  name.in = str.in
  if (str_detect(str.in, 'LTN')) name.in <- 'Tesouro Prefixado'
  if (str_detect(str.in, 'LFT')) name.in <- 'Tesouro SELIC'
  if (str_detect(str.in, 'NTN-B')) name.in <- 'Tesouro IPCA+ com Juros Semestrais'
  if (str_detect(str.in, 'Principal')) name.in <- 'Tesouro IPCA+'
  if (str_detect(str.in, 'NTN-F')) name.in <- 'Tesouro Prefixado com Juros Semestrais'
  if (str_detect(str.in, 'NTN-C')) name.in <- 'NTN-C'

  str.out <- paste0(name.in, ' ', lubridate::year(matur.date))

  return(str.out)

}


get.poupanca <- function(first.date = '2000-01-01', last.date = Sys.Date()) {
  require(rbcb)
  require(tidyverse)

  cat('\nFetching Poupança')

  my.code <- c(value = 25)
  last.date.1 = as.Date('2012-05-04')

  df.poupanca.1 <- get_series(code = my.code,
                              start_date = first.date,
                              end_date = last.date.1)

  df.poupanca.1 <- df.poupanca.1 %>%
    mutate(date = as.Date(format(date, '%Y-%m-01'))) %>%
    group_by(date) %>%
    summarise(value = first(value))

  my.code <- c(value = 196)
  first.date.2 = last.date.1 + 1
  df.poupanca.2 <- get_series(code = my.code,
                              start_date = first.date.2, end_date = last.date)

  df.poupanca <- bind_rows(df.poupanca.1, df.poupanca.2) %>%
    select(date, value) %>%
    mutate(value = value/100,
           type = 'Poupança')

  return(df.poupanca)
}

get.ipca <- function() {
  require(rbcb)
  require(tidyverse)

  cat('\nFetching IPCA')

  my.code <- c(value = 433)
  first.date = as.Date('2000-01-01')

  df.ipca <- get_series(code = my.code,start_date = first.date  ) %>%
    mutate(IPCA.am = value/100,
           IPCA.aa = (1+IPCA.am)^(12) - 1,
           type = 'IPCA')

  return(df.ipca)
}

get.cdi.mensal <- function(first.date = '2000-01-01', last.date = Sys.Date()) {
  require(rbcb)
  require(tidyverse)

  cat('\nFetching CDI')

  my.code <- c(value = 4391)
  df.cdi <- get_series(my.code, start_date = first.date,
                       end_date = last.date) %>%
    mutate(series = 'CDI')

  return(df.cdi)
}

get.SELIC <- function(first.date = '2000-01-01', last.date = Sys.Date()) {
  require(rbcb)
  require(tidyverse)

  cat('\nFetching SELIC')

  my.code <- c(value = 432)
  my.code <- c(value = 1178)
  df.selic <- get_series(my.code, start_date = first.date,
                         end_date = last.date) %>%
    mutate(series = 'SELIC')

  return(df.selic)
}



get.cdi.anual <- function(first.date = '2000-01-01', last.date = Sys.Date()) {
  require(rbcb)
  require(tidyverse)

  cat('\nFetching CDI Anual')

  my.code <- c(value = 4389)
  df.cdi <- get_series(my.code, start_date = first.date,
                       end_date = last.date) %>%
    mutate(series = 'CDI')

  return(df.cdi)
}
