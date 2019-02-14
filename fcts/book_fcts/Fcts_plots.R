plot.price <- function(df.in) {
  require(ggplot2)

  p <- ggplot(df.in, aes(x = ref.date, y= price.bid)) +
    geom_line(size= 2) +
    labs(title = df.in$asset.code2[1],
         subtitle = paste0('Dados de ', format.date(min(df.in$ref.date)),
                           ' até ', format.date(max(df.in$ref.date)) ),
         x = '',
         y = 'Preços de Compra',
         caption = paste0('Dados obtidos junto ao Tesouro Nacional \n',
                          'Investindo em Renda Fixa por Marcelo S. Perlin', '\uA9 2019')) +
    my.theme() +
    scale_x_date(breaks = scales::pretty_breaks(12))

  return(p)

}

