library(shiny)
library(tidyverse)

source('fcts/shinny_fcts.R')

sapply(list.files('fcts/book_fcts/', full.names = TRUE), FUN = source)

df.TD <- read_rds('data/TD.rds')

tab <- df.TD %>%
  group_by(asset.code, asset.code2) %>%
  summarise(first.date = first(ref.date),
            n = n()) %>%
  filter(n >= 255*3 ,
         !str_detect(asset.code2, 'Juros'))

df.TD <- df.TD %>%
  filter(asset.code %in% tab$asset.code)

write_rds(df.TD, 'data/TD_temp.rds')

unique.tickers <- unique(df.TD$asset.code2)

shinyServer(function(input, output, session) {
  
  output$data_range_ui <- renderUI({
    df.TD.selected <- get_TD_Data()
    
    dateRangeInput("data_range_td", 
                   label = h5("Data de Compra e Venda"), 
                   start = min(df.TD.selected$ref.date), 
                   end = max(df.TD.selected$ref.date), 
                   format = 'dd/mm/yyyy', language = 'pt-BR'
    ) 
  })
  
  output$index_pb_1 <- renderUI({
    
    if (input$select_index_pb_1 == 'CDI') {
      out <- sliderInput("slider_indexing_1", 
                         label = h6(paste0("Indexação ao ", input$select_index_pb_1)), 
                         min = 75, 
                         max = 150, 
                         value = 100,  
                         post = '%') 
    } else {
      out <- sliderInput("slider_indexing_1", 
                         label = h6(paste0("Indexação ao ", input$select_index_pb_1)), 
                         min = 0, 
                         max = 10, 
                         value = 5, 
                         post = '%') 
    }
    
    out
    
  })
  
  output$index_pb_2 <- renderUI({
    
    if (input$select_index_pb_2 == 'CDI') {
      out <- sliderInput("slider_indexing_2", 
                         label = h6(paste0("Indexação ao ", input$select_index_pb_2)), 
                         min = 75, 
                         max = 150, 
                         value = 80,  
                         post = '%') 
    } else {
      out <- sliderInput("slider_indexing_2", 
                         label = h6(paste0("Indexação ao ", input$select_index_pb_2)), 
                         min = 0, 
                         max = 10, 
                         value = 3, 
                         post = '%') 
    }
    
    out
    
  })
  
  get_TD_Data <- reactive({
    #df.TD <- read_rds('data/TD.rds')
    #browser()
    
    if (is.null(input$select_td)) {
      my.asset = unique.tickers[1]
    } else {
      my.asset = input$select_td
    }
    
    df.TD.selected <- df.TD %>%
      filter(asset.code2 == my.asset)
    
  })
  
  get_invest_data <- reactive({ 
    
    df.TD.selected <- get_TD_Data()
    
    #browser()
    
    
    if (is.null(input$data_range_td)) {
      my.date.buy <- min(df.TD.selected$ref.date)
      my.date.sell <- max(df.TD.selected$ref.date)
    } else {
      my.date.buy <- input$data_range_td[1]
      my.date.sell <- input$data_range_td[2]
    }
    
    if (is.null(input$slider_indexing_1)) {
      
      if (input$select_type_pb_1 == 'CDI') {
        my.percent.index.1 <- 1
      } else {
        my.percent.index.1 <- 0.03
      }
      
      if (input$select_type_pb_2 == 'CDI') {
        my.percent.index.2 <- 1
      } else {
        my.percent.index.2 <- 0.03
      }
      
    } else {
      my.percent.index.1 <- input$slider_indexing_1/100
      my.percent.index.2 <- input$slider_indexing_2/100
    }
    
    
    df.invest.TD <- invest_TD(df.in = df.TD.selected ,
                              date.buy = my.date.buy,
                              date.sell = my.date.sell,
                              value.first.buy = input$slider_first_invest_ammount, 
                              value.monthly.buy = input$slider_monthly_invest_ammount)
    
    df.invest.TD <- df.invest.TD %>%
      mutate(ref.month = as.Date(format(ref.date, '%Y-%m-01')),
             asset.code = asset.code2) %>%
      group_by(asset.code2, ref.month) %>%
      summarise_all(first)
    
    #browser()
    
    df.invest.pb.1 <- invest_cdb_lca(type.invest = input$select_type_pb_1, 
                                     type.indexing = input$select_index_pb_1, 
                                     percent.index = my.percent.index.1, 
                                     date.buy = min(df.invest.TD$ref.month),
                                     date.sell = max(df.invest.TD$ref.month), 
                                     contract.duration = input$select_expiration_pb_1,
                                     value.first.buy = input$slider_first_invest_ammount, 
                                     value.monthly.buy = input$slider_monthly_invest_ammount)
    
    df.invest.pb.2 <- invest_cdb_lca(type.invest = input$select_type_pb_2, 
                                     type.indexing = input$select_index_pb_2, 
                                     percent.index = my.percent.index.2, 
                                     date.buy = min(df.invest.TD$ref.month),
                                     date.sell = max(df.invest.TD$ref.month), 
                                     contract.duration = input$select_expiration_pb_2,
                                     value.first.buy = input$slider_first_invest_ammount, 
                                     value.monthly.buy = input$slider_monthly_invest_ammount)
    
    df.invest.poup <- invest_poup(date.buy = min(df.invest.TD$ref.month),
                                  date.sell = max(df.invest.TD$ref.month), 
                                  value.first.buy = input$slider_first_invest_ammount, 
                                  value.monthly.buy = input$slider_monthly_invest_ammount)
    
    df.to.plot <- bind_rows(df.invest.TD, 
                            df.invest.pb.1, 
                            df.invest.pb.2,
                            df.invest.poup)
    
  })
  
  
  
  output$plot_comp <- renderPlot({
    
    df.to.plot <- get_invest_data()
    
    monthly.investment <- 0
    p <- ggplot(df.to.plot, aes(x = ref.month, y = port.net.value, color = asset.code)) + 
      geom_line(size=1.5) + 
      labs(x = '', 
           y = 'Valores Líquidos de Resgate',
           title = paste0('Comparação Tesouro Direto e Produtos Bancários'),
           subtitle = paste0('- Simulação do aporte mensal de ', format.cash(monthly.investment), '\n',
                             '- Todos impostos (IR, IOF) e custos (custódia B3) foram \n',
                             ' inclusos no cálculo'), 
           caption = my.caption) + 
      scale_x_date(breaks = pretty_breaks(10)) + 
      scale_y_continuous(labels = format.cash) + my.theme()  
    
    print(p)
    
  })
  
  output$tbl_td <- renderTable({
    df.to.plot <- get_invest_data()
    
    if (is.null(input$data_range_td)) {
      first.date <- min(df.to.plot$ref.month)
      last.date <- max(df.to.plot$ref.month)
    } else {
      first.date <- input$data_range_td[1]
      last.date <- input$data_range_td[2]
    }
    
    n.years <- as.numeric(last.date - first.date)/365
    
    df.ipca <- read_rds('data/IPCA.rds') %>%
      filter(date >= first.date,
             date <= last.date) %>%
      mutate(cum.ret = cumprod(1+value/100))
    
    ret.ipca <- last(df.ipca$cum.ret)/first(df.ipca$cum.ret) - 1
    
    #browser()
    
    my.fct <- function(str.in) {
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
    
    tab <- df.to.plot %>%
      group_by(`Produto` = asset.code) %>%
      summarise(`Valor Final do Portfolio` =  format.cash(last(port.net.value)),
                `Liquidez` = my.fct(`Produto`),
                `Valor Total Investido` = format.cash(sum(value.purchases)),
                `Duração (anos)` = format(n.years, digits = 4, decimal.mark = ','),
                `Número de Reaplicações` = sum(n.days ==0)- 1,
                `Retorno Nominal Total` = last(port.net.value)/first(value.purchases) - 1,
                `Retorno Nominal Anual` = (1+`Retorno Nominal Total`)^(1/n.years) -1,
                `Retorno Real Total` = (1 + `Retorno Nominal Total`)/(1+ret.ipca) - 1,
                `Retorno Real Anual` = (1 + `Retorno Real Total`)^(1/n.years) - 1 ) %>%
      #mutate(`Retorno Nominal Total` = format.percent(`Retorno Nominal Total`),
      #       `Retorno Nominal Anual` = format.percent(`Retorno Nominal Anual`)) %>%
      mutate_at(vars(contains('Retorno')), .funs = format.percent)
    
    if (input$slider_monthly_invest_ammount !=0) {
      tab <- tab[ , 1:6]
    }
    
    tab
    
  })
  
  
  
})
