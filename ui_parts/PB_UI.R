PB_UI <- function(id, label = "PB") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  my.size.sidebar <- 4
  
  fluidPage(fluidRow(column(8, # initial text
                            includeHTML('html_text/html_PB.html'))),
            br(),
            fluidRow(
            column(width = my.size.sidebar, # left side bar
                     wellPanel(
                     fluidRow(
                     h3('Parâmetros de Investimento'),
                     sliderInput(ns("slider_first_invest_ammount"), 
                                 label = h5("Valor Inicial Investido"), 
                                 min = 1000, 
                                 max = 100000, 
                                 step = 500,
                                 value = 1000, pre = 'R$'),
                     
                     sliderInput(ns("slider_monthly_invest_ammount"), 
                                 label = h5("Valor Mensal Investido"), 
                                 min = 0, 
                                 max = 10000, 
                                 step = 500,
                                 value = 1000, pre = 'R$') ,
                     
                     dateRangeInput(ns("date_range"), 
                                    label = h5("Datas de Compra e Venda"), 
                                    start = as.Date('2000-01-01'), 
                                    end = as.Date('2018-12-01'), separator = '',
                                    format = 'dd/mm/yyyy', language = 'pt-BR') ),
                     br(),
                     fluidRow(h3('Produtos Bancários'),
                              column(width = my.size.sidebar,
                                     PB_single_UI(ns('PB_PB_1'), title.panel = '' ) ),
                              column(width = my.size.sidebar,
                                     PB_single_UI(ns('PB_PB_2'),title.panel = '' )),
                              column(width = my.size.sidebar,
                                     PB_single_UI(ns('PB_PB_3'), title.panel = '' ) ) ) ) ),
              column(width = 4, 
                     tabsetPanel(
                       tabPanel('Gráfico',
                                h3('Comparação de Valores de Resgate'),
                                plotOutput(ns('PB_plot_comp'), 
                                           #width = '100%', 
                                           height = '750px') )  ,
                       tabPanel('Tabela',
                                
                                h3('Performance dos Investimentos'),
                                tableOutput(ns('PB_tbl_td') )   )
                     )
              )
              
            )
  )
  
}
