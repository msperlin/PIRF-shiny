TD_ <- function(input, output, session) {
  
  
  output$data_range_ui <- renderUI({
    df.TD.selected <- get_TD_Data()
    
    dateRangeInput("data_range_td", 
                   label = h5("Datas de Compra e Venda"), 
                   start = min(df.TD.selected$ref.date), 
                   end = max(df.TD.selected$ref.date),  separator = '',
                   format = 'dd/mm/yyyy', language = 'pt-BR'
    ) 
  })
  
  
  # call all PB modules
  callModule(PB_single_, "TD_PB_1")
  callModule(PB_single_, "TD_PB_2")
  
  
  get_TD_Data <- reactive({
    #df.TD <- read_rds('data/TD.rds')
    
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
    
    if (is.null(input$data_range_td)) {
      my.date.buy <- min(df.TD.selected$ref.date)
      my.date.sell <- max(df.TD.selected$ref.date)
    } else {
      my.date.buy <- input$data_range_td[1]
      my.date.sell <- input$data_range_td[2]
    }
    
    if (is.null(input$`TD_PB_1-slider_indexing`)) {
      
      if (input$`TD_PB_1-select_type_pb` == 'CDI') {
        my.percent.index.1 <- 1
      } else {
        my.percent.index.1 <- 0.03
      }
      
      # if (input$`TD_PB_2-select_type_pb` == 'CDI') {
      #   my.percent.index.2 <- 1
      # } else {
      #   my.percent.index.2 <- 0.03
      # }
      
    } else {
      my.percent.index.1 <- input$`TD_PB_1-slider_indexing`/100
      my.percent.index.2 <- input$`TD_PB_2-slider_indexing`/100
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
    
    df.invest.pb.1 <- invest_cdb_lca(type.invest = input$`TD_PB_1-select_type_pb`, 
                                     type.indexing = input$`TD_PB_1-select_index_pb`, 
                                     percent.index = my.percent.index.1, 
                                     date.buy = min(df.invest.TD$ref.month),
                                     date.sell = max(df.invest.TD$ref.month), 
                                     contract.duration = input$`TD_PB_1-select_expiration_pb`,
                                     value.first.buy = input$slider_first_invest_ammount, 
                                     value.monthly.buy = input$slider_monthly_invest_ammount)
    
    # df.invest.pb.2 <- invest_cdb_lca(type.invest = input$`TD_PB_2-select_type_pb`, 
    #                                  type.indexing = input$`TD_PB_2-select_index_pb`, 
    #                                  percent.index = my.percent.index.2, 
    #                                  date.buy = min(df.invest.TD$ref.month),
    #                                  date.sell = max(df.invest.TD$ref.month), 
    #                                  contract.duration = input$`TD_PB_2-select_expiration_pb`,
    #                                  value.first.buy = input$slider_first_invest_ammount, 
    #                                  value.monthly.buy = input$slider_monthly_invest_ammount)
    
    df.invest.poup <- invest_poup(date.buy = min(df.invest.TD$ref.month),
                                  date.sell = max(df.invest.TD$ref.month), 
                                  value.first.buy = input$slider_first_invest_ammount, 
                                  value.monthly.buy = input$slider_monthly_invest_ammount)
    
    #browser()
    df.to.plot <- bind_rows(bind_rows(df.invest.pb.1) %>%
                              mutate(asset.code = paste0(asset.code, ' (', 
                                                         sapply(contract.duration,
                                                                translate.duration), ')')),
                            df.invest.TD,
                            df.invest.poup)
    
  })
  
  
  
  output$TD_plot_comp <- renderPlot({
    
    df.to.plot <- get_invest_data()
    
    monthly.investment <- 0
    
    p <- ggplot(df.to.plot, aes(x = ref.month, y = port.net.value, color = asset.code)) + 
      geom_line(size=1.5) + 
      labs(x = '', 
           y = 'Valores Líquidos de Resgate',
           title = paste0('Comparação Tesouro Direto e Produtos Bancários'),
           subtitle = paste0('- Simulação do aporte inicial de ', format.cash(input$slider_first_invest_ammount), 
                             ' e mensal de ', format.cash(input$slider_monthly_invest_ammount), '\n',
                             '- A reaplicação dos CDBs depende do prazo de vencimento \n',
                             '- Todos impostos (IR, IOF) e custos (custódia B3) estão inclusos no cálculo'), 
           caption = my.caption) + 
      scale_x_date(breaks = pretty_breaks(10)) + 
      scale_y_continuous(labels = format.cash) + my.theme() +
      guides(color=guide_legend(nrow=1,byrow=TRUE))
    
    print(p)
    
  })
  
  output$TD_tbl_td <- renderTable({
    df.to.plot <- get_invest_data()
    
    if (is.null(input$data_range_td)) {
      first.date <- min(df.to.plot$ref.month)
      last.date <- max(df.to.plot$ref.month)
    } else {
      first.date <- input$data_range_td[1]
      last.date <- input$data_range_td[2]
    }
    
    tab <- get_invest_tbl(df.to.plot)
    
    if (input$slider_monthly_invest_ammount !=0) {
      tab <- tab[ , 1:7]
    }
    
    tab
    
  })
  
}