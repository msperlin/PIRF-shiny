library(shiny)
library(tidyverse)

source('fcts/shinny_fcts_reusable.R')

app.title <- "PIRF ver 1.0"

sapply(list.files('fcts/book_fcts/', full.names = TRUE), FUN = source)

my.f <- list.files('server_parts/', full.names = T)
sapply(my.f, source)

my.f <- list.files('fcts', full.names = T, pattern = '.R')
sapply(my.f, source)

my.f <- list.files('ui_parts/', full.names = T)
sapply(my.f, source)

df.TD <- read_rds('data/main datasets/TD.rds')

tab <- df.TD %>%
  group_by(asset.code, asset.code2) %>%
  summarise(first.date = first(ref.date),
            n = n()) %>%
  filter(n >= 255*3 ,
         !str_detect(asset.code2, 'Juros'))

df.TD <<- df.TD %>%
  filter(asset.code %in% tab$asset.code)

df.ipca <<- read_rds('data/main datasets/IPCA.rds') 
df.funds <<- read_rds('data/funds-shiny.rds')

#unique.tickers <<- unique(df.TD$asset.code2)
my.title <<- 'Poupando e Investindo em Renda Fixa'
site.shiny <- 'https://www.msperlin.com/shiny/PIRF/'

my.sub.caption <<- paste0(book.title, ' \uA9 Marcelo S. Perlin 2019',
                         '\n',
                         'Aplicativo disponível em ', site.shiny)

my.caption <<- paste0('Dados obtidos junto ao Tesouro Nacional e BCB \n',
                     my.sub.caption)


# Server
server <- function(input, output, session) {
  
  callModule(TD_, "TD_nm")
  
  callModule(PB_, "PB_nm")
  
  callModule(FRF_, "FRF_nm")
  
}

# UI
ui <- navbarPage(app.title,
                 tabPanel('Introdução',
                          Intro_UI('intro_nm', 'intro')),
                 tabPanel('Tesouro Direto',
                          TD_UI('TD_nm', 'TD')),
                 tabPanel('Produtos Bancários',
                          PB_UI('PB_nm', 'PB') ),
                 tabPanel('Fundos de Renda Fixa',
                          FRF_UI('FRF_nm', 'RF') ),
                 #tabPanel('FAQ',
                #          FAQ_UI('FAQ_nm','FAQ') ),
                 tabPanel('Sobre Autor',
                          SA_UI('SA_nm','SA') )
)

shinyApp(ui = ui, 
         server = server)