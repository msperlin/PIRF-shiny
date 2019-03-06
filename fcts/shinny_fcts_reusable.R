PB_single_UI <- function(id, title.panel = 'Produto Bancário') { 
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  #browser()
  my.choices <- c('3 meses' = '3 months',
                  '6 meses' = '6 months',
                  '1 ano' = '1 year',
                  '2 anos' = '2 years',
                  '3 anos' = '3 years',
                  '5 anos' = '5 years',
                  'Não Vence' = 'lifetime')
  
  tagList(h3(title.panel),
    selectInput(ns("select_type_pb"), 
                label = h5("Tipo de Produto"), 
                choices = c('CDB', 'LCA'), 
                selected = sample(c('CDB', 'LCA'), 1) ),
    selectInput(ns("select_index_pb"), 
                label = h5("Índice de Retorno"), 
                choices = c('CDI', 'IPCA'), 
                selected = 'CDI'),
    uiOutput(ns('slider_indexing_ui')),
    selectInput(ns("select_expiration_pb"), 
                label = h5("Vencimento CDB/LCA"), 
                choices = my.choices,
                selected = sample(my.choices,1)) )
   
  
  
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
    } else {
      out <- sliderInput(ns("slider_indexing"), 
                         label = h6(paste0("Indexação ao ", 
                                           input$select_index_pb)), 
                         min = 0, 
                         step = 0.5,
                         max = 10, 
                         value = sample(seq(1, 10, by = 0.5),1), 
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
  
  tab <- df.in %>%
    group_by(`Produto` = asset.code) %>%
    summarise(`Valor Total Investido` = format.cash(value.purchases[1] + 
                                                      (n()-1)*value.purchases[2]),
              `Valor Bruto de Resgate` =  format.cash(last(port.nominal.value)),
              `Valor Total de Resgate` =  format.cash(last(port.net.value)),
              `Liquidez` = get_liquidity(`Produto`),
              `Duração (anos)` = format(n.years, digits = 4, decimal.mark = ','),
              `Número de Reaplicações` = sum(n.days ==0)- 1,
              `Retorno Nominal Total` = last(port.net.value)/first(value.purchases) - 1,
              `Retorno Nominal Anual` = (1+`Retorno Nominal Total`)^(1/n.years) -1,
              `Retorno Real Total` = (1 + `Retorno Nominal Total`)/(1+ret.ipca) - 1,
              `Retorno Real Anual` = (1 + `Retorno Real Total`)^(1/n.years) - 1 ) %>%
    #mutate(`Retorno Nominal Total` = format.percent(`Retorno Nominal Total`),
    #       `Retorno Nominal Anual` = format.percent(`Retorno Nominal Anual`)) %>%
    mutate_at(vars(contains('Retorno')), .funs = format.percent)
  
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