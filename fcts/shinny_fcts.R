calc.ir.rate <- function(n.days = 1: 1000) {
  
  out <- numeric(length = length(n.days))
  
  out[n.days <= 6*30] <- 0.225
  out[ (n.days > 6*30)&(n.days <= 12*30)] <- 0.20
  out[ (n.days > 12*30)&(n.days < 24*30)] <- 0.175
  out[ (n.days > 24*30)] <- 0.15
  
}


# code for finding equivalent rate in CDB/LCA
obj.fct <- function(w.lca, w.cdb, ret.cdi, n.T) {
  
  ir <- calc.ir.rate(n.T)
  
  ret.lca <- (1+w.lca*ret.cdi)^(n.T/365)
  ret.cdb <- ((1+w.cdb*ret.cdi)^(n.T/365)) - ((1+w.cdb*ret.cdi)^(n.T/365) - 1)*(ir)
  
  out <- abs(ret.lca - ret.cdb)
  #cat('\n', out)
  return(out)
}

solve.lca <- function(w.cdb, ret.cdi, n.T) {
  require(tidyverse)
  
  par.out <- optim(par = 1, fn = obj.fct, method = 'Brent', lower = 0, upper = 1.5,
                   w.cdb = w.cdb, ret.cdi = ret.cdi, n.T = n.T)
  
  return(tibble(w.lca = par.out$par, 
                w.cdb = w.cdb, 
                ret.cdi = ret.cdi, 
                n.T = n.T))
}


#solve.lca(w.cdb = 1.2, ret.cdi = 0.1, n.T = 100 )
do.plot.comp.cdb.lca <- function(n.points = 100, n.days =365*3, 
                                 ret.cdi = 0.0640) { # https://www.infomoney.com.br/mercados/renda-fixa	
  
  require(purrr)
  require(ggplot2)
  
  df.args <- expand.grid(w.cdb = seq(0.90, 1.3, length.out = n.points),
                         ret.cdi = ret.cdi,
                         n.T = n.days)
  

  df.res <- bind_rows(pmap(.l = df.args, .f = solve.lca))
  
  df.res$n.T.fct <- factor(paste0(df.res$n.T, ' anos'), 
                           levels = paste0(min(df.res$n.T):max(df.res$n.T), ' anos'))
  
  book.title <- 'Investindo na Renda Fixa'
  my.caption <- paste0('Dados do Banco Central do Brasil \n',
                       book.title, ' por Marcelo S. Perlin', '\uA9 2019')
  
  p <- ggplot(df.res, aes(x = w.lca, w.cdb) ) + 
    geom_line(size = 2) + 
    labs(x = 'Retorno % do CDI (LCA/LCI)', y = 'Retorno % do CDI (CDB)',
         title = paste0('Taxas de Equivalência de Retornos entre CDBs e LCAs/LCIs'),
         subtitle = paste0('- O gráfico mostra a equivalência de retorno entre CDBs e LCIs considerando \n', 
                           ' uma taxa CDI futura e constante de ', 
                           format.percent(ret.cdi), ' anuais ()'),
         caption = my.caption) + 
    scale_y_continuous(labels=percent) + 
    scale_x_continuous(labels=percent, limits = c(0.7, 1.3))  + #+ facet_wrap(~n.T.fct, nrow = 2) + 
    theme_bw()
  
  return(p)
}

translate.duration <- function(str.in) {
  
  my.choices <- c('3 meses' = '3 months',
                  '6 meses' = '6 months',
                  '1 ano' = '1 year',
                  '2 anos' = '2 years',
                  '3 anos' = '3 years',
                  '5 anos' = '5 years',
                  'Não Vence' = 'lifetime')
  
  idx <- which(str.in == my.choices)
  
  str.out <- names(my.choices)[idx]
  
}