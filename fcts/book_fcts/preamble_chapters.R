require(tidyverse)
library(kableExtra)

book.title <- 'Poupando e Investindo em Renda Fixa'
book.subtitle <- 'Uma Abordagem Baseada em Dados'
book.isbn.print <- '978-85-922435-9-3'
book.isbn.ebook <- '978-85-922435-8-6'
book.edition1.date <- '15/05/2019'

my.sub.caption <- paste0(book.title, ' \uA9 Marcelo S. Perlin 2019')
my.caption <- paste0('Dados obtidos junto ao Tesouro Nacional e BCB \n',
                     my.sub.caption)

# sites
site.shiny <- 'https://www.msperlin.com/shiny/PIRF/'
site.code <- 'https://www.msperlin.com/blog/publication/2019_book-pirf/'

#source('R fcts and scripts/Fcts_runitall.R')
