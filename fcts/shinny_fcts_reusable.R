PB_single_UI <- function(id, title.panel = 'Produto Bancário') { 
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  #browser()
  my.choices.time <- c('3 meses' = '3 months',
                  '6 meses' = '6 months',
                  '1 ano' = '1 year',
                  '2 anos' = '2 years',
                  '3 anos' = '3 years',
                  '5 anos' = '5 years',
                  'Não Vence' = 'lifetime')
  
  my.choices.type = c('CDI' = 'CDI',
                      'IPCA' = 'IPCA',
                      'Prefixado' = 'prefixado')
  
  tagList(h3(title.panel),
    selectInput(ns("select_type_pb"), 
                label = h5("Tipo de Produto"), 
                choices = c('CDB', 'LCA'), 
                selected = sample(c('CDB', 'LCA'), 1) ),
    selectInput(ns("select_index_pb"), 
                label = h5("Índice de Retorno"), 
                choices = my.choices.type, 
                selected = sample(my.choices.type,1)),
    uiOutput(ns('slider_indexing_ui')),
    selectInput(ns("select_expiration_pb"), 
                label = h5("Vencimento CDB/LCA"), 
                choices = my.choices.time,
                selected = sample(my.choices.time,1)) )
   
  
  
}

PB_single_ <- function(input, output, session) {
  
  output$slider_indexing_ui <- renderUI({ 
    
    ns <- session$ns
    
    #browser()
    
    if (input$select_index_pb == 'CDI') {
      out <- sliderInput(ns("slider_indexing"), 
                         label = h6(paste0("% Indexação ao ", 
                                           input$select_index_pb)), 
                         min = 80, 
                         max = 120, 
                         step = 2.5,
                         value = sample(seq(80, 120, by = 5),1),  
                         post = '%') 
    } else if (input$select_index_pb == 'IPCA') {
      out <- sliderInput(ns("slider_indexing"), 
                         label = h6(paste0("Indexação ao ", 
                                           input$select_index_pb)), 
                         min = 0, 
                         step = 0.5,
                         max = 10, 
                         value = sample(seq(1, 10, by = 0.5),1), 
                         post = '%')
    } else if (input$select_index_pb == 'prefixado') {
      out <- sliderInput(ns("slider_indexing"), 
                         label = h6('Taxa de Retorno Anual'), 
                         min = 0, 
                         step = 0.5,
                         max = 20, 
                         value = sample(seq(8, 15, by = 0.5),1), 
                         post = '%')
    }
    
    out
    
  })
  
}

get_invest_tbl <- function(df.in) {
  
  require(tidyverse)
  
  first.date <- min(df.in$ref.month)
  last.date <- max(df.in$ref.month)
  
  df.ipca.temp <- df.ipca %>%
    filter(date >= first.date,
           date <= last.date) %>%
    mutate(cum.ret = cumprod(1+value/100))
  
  ret.ipca <- last(df.ipca.temp$cum.ret)/first(df.ipca.temp$cum.ret) - 1
  n.years <- as.numeric(last.date - first.date)/365
  
  #browser()
  
  tab <- df.in %>%
    group_by(`Produto` = asset.code) %>%
        summarise(#`Valor Total Investido` = format.cash(sum(value.purchases)),
              #`Valor Bruto de Resgate` =  format.cash(last(port.nominal.value)),
              #`Custo Total de Custódia` = format.cash(sum(cost.custody)),
              #`Custo Total de IR` = format.cash(last(cost.IR) + sum(cost.IR[which(n.days == 0)-1])),
              `Valor Total de Resgate` =  format.cash(last(port.net.value)),
              `Liquidez` = get_liquidity(`Produto`),
              `Duração (anos)` = format(n.years, digits = 4, decimal.mark = ',') ) #%>% 
              #`Número de Reaplicações` = sum(n.days ==0)- 1,
              #`Retorno Nominal Total` = last(port.net.value)/parse_number(`Valor Total Investido`) - 1,
              #`Retorno Nominal Anual` = (1+`Retorno Nominal Total`)^(1/n.years) -1,
              #`Retorno Real Total` = (1 + `Retorno Nominal Total`)/(1+ret.ipca) - 1,
              #`Retorno Real Anual` = (1 + `Retorno Real Total`)^(1/n.years) - 1 ) %>%
    #mutate(`Retorno Nominal Total` = format.percent(`Retorno Nominal Total`),
    #       `Retorno Nominal Anual` = format.percent(`Retorno Nominal Anual`)) %>%
    #mutate_at(vars(contains('Retorno')), .funs = format.percent)
  
  return(tab)
}

get_liquidity <- function(str.in) {
  str.in <- str.in[1]
  flag <- str_detect(str.in, 'CDB|LCA')
  
  if (flag) {
    str.out <- 'Baixa (vencimento)'
  } else {
    str.out <- 'Alta (D+1)'
  }
  
  if (str_detect(str.in, 'Poupança') ) str.out <- 'Alta (D+0)'
  
  return(str.out)
  
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