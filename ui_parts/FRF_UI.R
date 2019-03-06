FRF_UI <- function(id, label = "RF") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(width = 2,
                   sliderInput(width = '100%',
                               ns("slider_first_invest_ammount"), 
                               label = h5("Valor Inicial investido"), 
                               min = 1000, 
                               max = 100000, 
                               step = 500,
                               value = 1000, pre = 'R$'),
                   checkboxInput(ns('check_box_IQ'),
                                 label = 'Incluir Fundos para Investidores Qualificados', 
                                 value = FALSE),
                   sliderInput(width = '100%',
                               ns("slider_n_funds"), 
                               label = h5("Quantos fundos com maior número de cotistas a salientar"), 
                               min = 1, 
                               max = 20, 
                               step = 1,
                               value = 10),
                   br(),
                   h4('Produtos Bancários'),
                   PB_single_UI(ns('PB_PB_1'), title.panel = '' ) ), 
      mainPanel(tabsetPanel(type = 'tabs', 
                            tabPanel('Gráfico 1',
                                     column(8,
                                            h3('Comparação de Valores de Resgate'),
                                            plotOutput(ns('FRF_plot1'), 
                                                       width = "110%", 
                                                       height = "600px") ) ),
                            tabPanel('Gráfico 2',
                                     column(8,
                                            h3('Performance Ano a Ano'),
                                            plotOutput(ns('FRF_plot2'), 
                                                       width = "110%", 
                                                       height = "600px") ))
                            #tabPanel('Tabela',
                            #         column(8,
                            #                h3('Comparação de Valores de Resgate'),
                            #                tableOutput(ns('FRF_tbl_td')) )
      )
      )
      
      
    )
  )
  
}
