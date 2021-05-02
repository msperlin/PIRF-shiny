PB_ <- function(input, output, session) {
  
  # call all PB modules
  callModule(PB_single_, "PB_PB_1")
  callModule(PB_single_, "PB_PB_2")
  callModule(PB_single_, "PB_PB_3")
  
  
  PB_get_invest_data <- reactive({ 

    if (is.null(input$`PB_PB_1-slider_indexing`)) {
      
      my.percent.index.1 <- 0
      my.percent.index.2 <- 0
      my.percent.index.3 <- 0
      
    } else {
      my.percent.index.1 <- input$`PB_PB_1-slider_indexing`/100
      my.percent.index.2 <- input$`PB_PB_2-slider_indexing`/100
      my.percent.index.3 <- input$`PB_PB_3-slider_indexing`/100
      
    }
    
    first.date <- input$date_range[1]
    last.date <- input$date_range[2]
    
    #browser()
    
    df.invest.pb.1 <- invest_cdb_lca(type.invest = input$`PB_PB_1-select_type_pb`, 
                                     type.indexing = input$`PB_PB_1-select_index_pb`, 
                                     percent.index = my.percent.index.1, 
                                     date.buy = first.date,
                                     date.sell = last.date, 
                                     contract.duration = input$`PB_PB_1-select_expiration_pb`,
                                     value.first.buy = input$slider_first_invest_ammount, 
                                     value.monthly.buy = input$slider_monthly_invest_ammount)
    
    df.invest.pb.2 <- invest_cdb_lca(type.invest = input$`PB_PB_2-select_type_pb`, 
                                     type.indexing = input$`PB_PB_2-select_index_pb`, 
                                     percent.index = my.percent.index.2, 
                                     date.buy = first.date,
                                     date.sell = last.date, 
                                     contract.duration = input$`PB_PB_2-select_expiration_pb`,
                                     value.first.buy = input$slider_first_invest_ammount, 
                                     value.monthly.buy = input$slider_monthly_invest_ammount)
    
    df.invest.pb.3 <- invest_cdb_lca(type.invest = input$`PB_PB_3-select_type_pb`, 
                                     type.indexing = input$`PB_PB_3-select_index_pb`, 
                                     percent.index = my.percent.index.3, 
                                     date.buy = first.date,
                                     date.sell = last.date, 
                                     contract.duration = input$`PB_PB_3-select_expiration_pb`,
                                     value.first.buy = input$slider_first_invest_ammount, 
                                     value.monthly.buy = input$slider_monthly_invest_ammount)
    
    
    df.invest.poup <- invest_poup(date.buy = first.date,
                                  date.sell = last.date, 
                                  value.first.buy = input$slider_first_invest_ammount, 
                                  value.monthly.buy = input$slider_monthly_invest_ammount)
    
    #browser()
    df.to.plot <- bind_rows(bind_rows(df.invest.pb.1, 
                                      df.invest.pb.2,
                                      df.invest.pb.3) %>%
                              mutate(asset.code = paste0(asset.code2, ' (', 
                                                         sapply(contract.duration, 
                                                                translate.duration), ')')),
                            df.invest.poup)
    
  })
  
  
  
  output$PB_plot_comp <- renderPlot({
    
    df.to.plot <- PB_get_invest_data()

    p <- ggplot(df.to.plot, aes(x = ref.month, y = port.net.value, color = asset.code)) + 
      geom_line(size=1.5) + 
      labs(x = '', 
           y = 'Valores Líquidos de Resgate',
           title = paste0('Comparação de Produtos Bancários'),
           subtitle = paste0('- Simulação do aporte inicial de ', format.cash(input$slider_first_invest_ammount), 
                             ' e mensal de ', format.cash(input$slider_monthly_invest_ammount), '\n',
                             '- Todos impostos (IR e IOF) são levados em conta  \n',
                             '- O eixo vertical representa valores resgatáveis líquidos a cada momento\n',
                             '- A reaplicação dos CDBs é realizada a cada vencimento'), 
           caption = my.caption) + 
      scale_x_date(breaks = pretty_breaks(10)) + 
      scale_y_continuous(labels = format.cash) + my.theme()  + 
      guides(color=guide_legend(nrow=2,byrow=TRUE))
    
    print(p)
    
  })
  
  output$PB_tbl_td <- renderTable({
    df.to.plot <- PB_get_invest_data()
    
    if (is.null(input$date_range)) {
      first.date <- min(df.to.plot$ref.month)
      last.date <- max(df.to.plot$ref.month)
    } else {
      first.date <- input$date_range[1]
      last.date <- input$date_range[2]
    }
    
    tab <- get_invest_tbl(df.to.plot)
    
    if (input$slider_monthly_invest_ammount !=0) {
      #tab <- tab[ , 1:7]
      tab <- tab[ , ]
    }
    
    tab
    
  })
  
}