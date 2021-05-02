FRF_ <- function(input, output, session) {
  
  callModule(PB_single_, "PB_PB_1")
  
  FRF_get_invest_data <- reactive({ 

    if (is.null(input$`PB_PB_1-slider_indexing`)) {
      my.percent.index.1 <- 0
    } else {
      my.percent.index.1 <- input$`PB_PB_1-slider_indexing`/100
    }
    
    first.date <- min(df.funds$ref.date)
    last.date <- max(df.funds$ref.date)
    
    df.invest.pb.1 <- invest_cdb_lca(type.invest = input$`PB_PB_1-select_type_pb`, 
                                     type.indexing = input$`PB_PB_1-select_index_pb`, 
                                     percent.index = my.percent.index.1, 
                                     date.buy = first.date,
                                     date.sell = last.date, 
                                     contract.duration = input$`PB_PB_1-select_expiration_pb`,
                                     value.first.buy = input$slider_first_invest_ammount, 
                                     value.monthly.buy = 0) %>%
      mutate(CNPJ_FUNDO = asset.code2,
             type.asset = asset.code2)
    
    my.asset <-"Tesouro IPCA+ 2024"
    
    
    df.in <- df.TD %>%
      filter(asset.code2 %in% my.asset) %>%
      mutate(ref.month = as.Date(format(ref.date, '%Y-%m-01'))) %>%
      group_by(ref.month, asset.code) %>%
      summarise_all(first)

    
    df.invest.TD <- invest_TD(TD.to.invest = my.asset,
                              date.buy = first.date,
                              date.sell = last.date,
                              value.first.buy = input$slider_first_invest_ammount) %>%
      mutate(CNPJ_FUNDO = asset.code2,
             type.asset = asset.code2)
    
    df.invest.poup <- invest_poup(date.buy = first.date,
                                  date.sell = last.date, 
                                  value.first.buy = input$slider_first_invest_ammount, 
                                  value.monthly.buy = 0) %>%
      mutate(CNPJ_FUNDO = asset.code2,
             type.asset = asset.code2)
    
    #browser()
    df.to.plot <- bind_rows(bind_rows(df.invest.pb.1) %>%
                              mutate(asset.code = paste0(asset.code2, ' (', 
                                                         sapply(contract.duration,
                                                                translate.duration), 
                                                         ')')),
                            df.invest.poup,
                            df.invest.TD)
    
  })
  
  FRF_get_topN_funds <- reactive({
    
    n.funds.by.cot <- input$slider_n_funds
    
    df.funds.temp <-  FRF_get_fund_data()
    
    tab <- df.funds.temp %>%
      group_by(CNPJ_FUNDO, DENOM_SOCIAL) %>%
      summarise(n.cot = last(n.cot))
    
    idx <- which(rank(-tab$n.cot) %in% 1:n.funds.by.cot)
    tab <- tab[idx, ]
    
    top.funds <- df.funds.temp %>%
      filter(CNPJ_FUNDO %in% tab$CNPJ_FUNDO) %>%
      mutate(type.asset = paste0(n.funds.by.cot, ' Fundos com maior nº de cotistas') )
    
    return(top.funds)
    
  })
  
  
  FRF_get_fund_data <- reactive({ 
    
    if (input$check_box_IQ) {
      df.funds.temp <- df.funds %>%
        filter(INVEST_QUALIF %in% c('S', 'N') )
    } else {
      df.funds.temp <- df.funds %>%
        filter(INVEST_QUALIF %in% c('N') )
    }
    
    # if (!input$check_box_privado) {
    #   df.funds.temp <- df.funds.temp  %>%
    #     filter(!str_detect(str_to_lower(DENOM_SOCIAL), 'privado'))
    # } 
    
    df.funds.temp <- df.funds.temp %>%
      mutate(port.net.value = input$slider_first_invest_ammount*
               port.net.value)
    
  })
  
  output$FRF_plot1 <- renderPlot({
    
    df.funds.temp <-  FRF_get_fund_data()
    df.to.plot <- FRF_get_invest_data()
    
    top_n_funds <- FRF_get_topN_funds()
    
    #browser()
    
    my.asset <-"Tesouro IPCA+ 2024"
    df.invest.TD <- df.to.plot %>%
      filter(asset.code2 == my.asset)
    
    p <- ggplot(df.funds.temp, aes(x = ref.date, y = port.net.value, group = CNPJ_FUNDO)) +
      geom_line(size = 0.07)  +
      geom_line(data = df.to.plot,
                mapping = aes(x = ref.month, y = port.net.value, color = asset.code2),
                size = 2) +
      geom_line(data = df.to.plot %>%
                  filter(asset.code == 'Caderneta de Poupança'),
                mapping = aes(x = ref.month, y = port.net.value, color = asset.code2),
                size = 2.5) +
      geom_line(data = df.invest.TD,
                mapping = aes(x = ref.month, y = port.net.value, color = asset.code2),
                size = 2.5) +
      geom_line(data = top_n_funds, aes(x = ref.month,
                                         y = port.net.value,
                                         color = type.asset,
                                         group = CNPJ_FUNDO),
                size = 0.5) +
      labs(x = '',
           y = 'Valores Líquidos de Resgate',
           color = '',
           title = paste0('Desempenho de Longo Prazo de Fundos de Renda Fixa (', year(min(df.funds$ref.date)),
                          ' - ', year(max(df.funds$ref.date)), ')'),
           subtitle = paste0('- A figura simula o investimento inicial em ', year(min(df.funds$ref.date)), 
                             ' e resgate em ', year(max(df.funds$ref.date)), ' para cada classe de ativo\n',
                             '- Valores de resgate consideram tributação (come-cotas/ganho de capital/IOF), custódia \n e taxa de administração de Fundos\n',
                             '- Fundos de crédito privado são excluídos da amostra \n',
                             '- Dados contemplam apenas fundos de renda fixa que existiram durante todo o período analisado \n',
                             '- Todos CDBs possum vencimento a cada 3 anos'),
           caption = paste0('Dados obtidos no Tesouro Nacional, BCB e Portal Brasileiro de Dados Abertos \n',
                            my.sub.caption) ) +
      my.theme(my.size = 14) +
      theme(legend.position="bottom") +
      scale_x_date(breaks = scales::pretty_breaks(10)) +
      scale_y_continuous(labels = format.cash, 
                         limits = c(0.5, 
                                    max(df.invest.TD$port.net.value)+input$slider_first_invest_ammount))  + 
      guides(color=guide_legend(nrow=2,byrow=TRUE)) 
    
    print(p)
    
  })
  
  # Plot 2
  output$FRF_plot2 <- renderPlot({
    
    df.funds.temp <-  FRF_get_fund_data()
    df.to.plot <- FRF_get_invest_data()
    top_n_funds <- FRF_get_topN_funds()

    my.asset <-"Tesouro IPCA+ 2024"
    df.invest.TD <- df.to.plot %>%
      filter(asset.code == my.asset)
    
    
    # Plot 2
    df <- bind_rows(df.funds.temp, df.invest.TD,
                    df.to.plot,top_n_funds ) %>%
      group_by(ref.year = as.Date(format(ref.month,'%Y-01-01')),
               CNPJ_FUNDO,
               type.asset ) %>%
      summarise(cum.ret = last(port.net.value)/first(port.net.value) - 1)
    
    my.assets <- unique(df$type.asset)
    my.assets <- my.assets[my.assets != 'Fundos']
    
    my.fct <- function(df.in) {
      
      my.assets <- unique(df.in$type.asset)
      my.assets <- my.assets[my.assets != 'Fundos']
      year.now <- df.in$ref.year[1]
      
      df.out <- tibble()
      for (i.asset in my.assets) {
        temp.df <- tibble(ref.year = year.now,
                          asset.in =i.asset ,
                          perc.test = mean(df.in$cum.ret[df.in$type.asset == 'Fundos'] <
                                             mean(df.in$cum.ret[df.in$type.asset == asset.in]),
                                           na.rm = TRUE) )
        
        df.out <- bind_rows(df.out, temp.df)
      }
      
      return(df.out)
      
    }
    
    l <- split(df, df$ref.year)
    
    df.res <- bind_rows(map(l, .f = my.fct))
    
    p <- ggplot(df.res, aes(x = ref.year, y = perc.test)) +
      geom_col() + facet_wrap(~asset.in) +
      labs(x = '', y = 'Proporção de Fundos com Retorno Anual Inferior',
           title = 'Desempenho de Curto Prazo (Anual) de Fundos de Renda Fixa',
           subtitle = paste0('- A figura simula o investimento e resgate ano a ano para cada classe de ativo\n',
                             '- O eixo vertical (y) representa a proporção (percentagem) de fundos com retornos inferiores ao ativo do painel para cada ano\n',
                             '- Todos valores de resgate consideram tributação (come-cotas/ganho de capital), emolumentos e taxa de administração de Fundos\n',
                             '- Fundos de crédito privado são excluídos da amostra\n',
                             '- Dados contemplam apenas fundos de renda fixa que existiram durante todo o período analisado\n',
                             '- Para comparar os N maiores fundos contra os demais, utiliza-se uma média de retorno acumulado'),
           caption = paste0('Dados obtidos no Tesouro Nacional, BCB e Portal Brasileiro de Dados Abertos \n',
                            my.sub.caption)) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(breaks = scales::pretty_breaks(12)) + 
      theme_bw()
    
    print(p)
    
  })
  
  
  output$FRF_tbl_td <- renderTable({
    
    df.funds.temp <-  FRF_get_fund_data()
    df.to.plot <- FRF_get_invest_data()
    top_n_funds <- FRF_get_topN_funds()
    
    tab <- get_invest_tbl(df.to.plot)
    
    tab
    
  })
  
}