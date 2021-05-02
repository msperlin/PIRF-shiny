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

plot.price <- function(df.in) {
  require(ggplot2)

  p <- ggplot(df.in, aes(x = ref.date,
                         y= price.bid)) +
    geom_line(size= 2) +
    labs(title = str_c(df.in$asset.code2[1]),
         subtitle = str_c('- Emissão em ', format.date(min(df.in$ref.date)),
                          ', último preço em ', format.date(max(df.in$ref.date)) ), #, '\n',
           #'- Custos de transação e impostos não estão inclusos no gráfico'),
         x = '',
         y = 'Preços de Compra e Venda',
         caption = my.caption) +
    my.theme() +
    scale_x_date(breaks = scales::pretty_breaks(10)) +
    scale_y_continuous(labels = format.cash)

  return(p)

}

