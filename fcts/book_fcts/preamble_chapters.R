require(tidyverse)
book.title <- 'Investindo na Renda Fixa'

my.sub.caption <- paste0(book.title, ' \uA9 Marcelo S. Perlin 2019')
my.caption <- paste0('Dados obtidos junto ao Tesouro Nacional e BCB \n',
                     my.sub.caption)

site.shiny <- 'https://www.msperlin.com/shiny/IRF/'

my.theme <- function(my.size = 14) {
  library(ggplot2)

  my.sub.caption <- paste0(book.title, ' \uA9 Marcelo S. Perlin 2019')
  my.caption <- paste0('Dados obtidos junto ao Tesouro Nacional e BCB \n',
                       my.sub.caption)

  my.theme <-  theme_bw(base_size = my.size) + #theme_linedraw(base_size = 14)
    theme(legend.position = 'bottom',
          legend.title = element_blank(),
          #plot.caption = element_text(face = 'bold'),
          legend.box.background = element_rect(),
          legend.key.size = unit(2,"line"),
          legend.key = element_rect(size = 3))

  return(my.theme)

}

#source('R fcts and scripts/Fcts_runitall.R')
