library(shiny)
library(stringr)
library(writexl)
library(tidyverse)

my.f <- list.files('fcts', full.names = T)
sapply(my.f, source)

df.TD <- read_rds('data/TD_temp.rds')
unique.tickers <- unique(df.TD$asset.code2)

my.title <- 'Investindo na Renda Fixa (provisório)'

shinyUI(navbarPage("IRF ver 0.5",
                   tabPanel('Introdução',
                            fluidPage(titlePanel(title = my.title, 
                                                 windowTitle = my.title),
                                      tags$head(includeScript("GAnalytics/google-analytics.js")),
                                      fluidRow(
                                        column(8,
                                               h4('Seja bem vindo a interface IRF-shiny do livro ', strong(my.title), 
                                                  '\uA9 Marcelo S. Perlin 2019'),
                                               br(),
                                               p("Aqui terás acesso a um modo dinâmico de analisar o mercado de renda fixa.", 
                                                 br(),
                                                 'Todos aplicativos são abertos ao público e se utilizam de dados públicos do Tesouro Nacional e BCB'),
                                               p('Nenhum dos aplicativos, isoladamente ou em conjunto, oferece recomendação de compra ou venda dos títulos públicos.',
                                                 ' Esta deve ser sua própria decisão como investidor'),
                                               h5('Caso tiver interesse na obra que está sendo produzida, indique o seu email abaixo.'),
                                               h5('Sugestões e comentários são bem vindos!'),
                                               br(),
                                               HTML(readLines('www/html_form.html')),
                                               br(),
                                               h4('Autor e Desenvolvedor'),
                                               p(a('Marcelo S. Perlin', href = 'https://www.msperlin.com/blog/'),
                                                 '/ EA - UFRGS (marcelo.perlin@ufrgs.br)'),
                                               img(src='logo_ufrgs.png', align = "left", width="100", height="100"),
                                               img(src='logo_ea.png', align = "left", width="100", height="100")
                                        )
                                      )
                            )
                   ),
                   tabPanel('Comparação Tesouro Direto, CDB/LCA/LCI',
                            pageWithSidebar(
                              headerPanel('Histórico Tesouro Direto e Produtos Bancários'),
                              sidebarPanel(width = 2,
                                           sliderInput("slider_first_invest_ammount", 
                                                       label = h5("Valor Inicial investido"), 
                                                       min = 1000, 
                                                       max = 100000, 
                                                       value = 1000, pre = 'R$'),
                                           
                                           sliderInput("slider_monthly_invest_ammount", 
                                                       label = h5("Valor Mensal investido"), 
                                                       min = 0, 
                                                       max = 10000, 
                                                       value = 1000, pre = 'R$'),
                                           
                                           selectInput("select_td", 
                                                       label = h5("Título do Tesouro Direto"), 
                                                       choices = unique.tickers, 
                                                       selected = sample(unique.tickers,10)),
                                           
                                           uiOutput("data_range_ui"),
                                           br(),
                                           selectInput("select_type_pb_1", 
                                                       label = h5("Produto bancário #1"), 
                                                       choices = c('CDB', 'LCA'), 
                                                       selected = 'CDB'),
                                           selectInput("select_index_pb_1", 
                                                       label = h5("Tipo de indexação #1"), 
                                                       choices = c('CDI', 'IPCA'), 
                                                       selected = 'CDI'),
                                           uiOutput("index_pb_1"),
                                           selectInput("select_expiration_pb_1", 
                                                       label = h5("Vencimento CDB e Reaplicação #1"), 
                                                       choices = c('1 ano' = '1 year',
                                                                   '2 anos' = '2 years',
                                                                   '3 anos' = '3 years',
                                                                   '5 anos' = '5 years',
                                                                   'Infinito' = 'lifetime'),
                                                       selected = '2 anos'),
                                           br(),
                                           selectInput("select_type_pb_2", 
                                                       label = h5("Produto bancário #2"), 
                                                       choices = c('CDB', 'LCA'), 
                                                       selected = 'LCA'),
                                           selectInput("select_index_pb_2", 
                                                       label = h5("Tipo de indexação #2"), 
                                                       choices = c('CDI', 'IPCA'), 
                                                       selected = 'IPCA'),
                                           uiOutput("index_pb_2"),
                                           selectInput("select_expiration_pb_2", 
                                                       label = h5("Vencimento CDB e Reaplicação #1"), 
                                                       choices = c('1 ano' = '1 year',
                                                                   '2 anos' = '2 years',
                                                                   '3 anos' = '3 years',
                                                                   '5 anos' = '5 years',
                                                                   'Infinito' = 'lifetime'),
                                                       selected = '2 anos') ),
                                    mainPanel(width = 5,
                                        h3('Comparação de Valores de Resgate'),
                                        plotOutput('plot_comp'),
                                        br(),
                                        h3('Performance dos Investimentos'),
                                        tableOutput('tbl_td')
                              )
                            ) ) #,
                   # tabPanel('CDBs X LCA/LCI',
                   #          pageWithSidebar(
                   #            headerPanel('Retornos Equivalentes entre CDBs e LCA/LCI'),
                   #            sidebarPanel(width = 2,
                   #                         sliderInput("slider_ndays", label = h5("Horizonte de Investimento (dias)"), 
                   #                                     min = 1, step = 100,
                   #                                     max = 10*365, value = 365*2),
                   #                         sliderInput("slider_cdi", label = h5("Taxa CDI (% ano)"), 
                   #                                     min = 0.00, step = 0.05,sep = ',',post = '%',
                   #                                     max = 15, value = 5.4)
                   #            ),
                   #            mainPanel(width = 7,
                   #                      plotOutput('plot2')
                   #            )
                   #          )
                   #          
                   # )
                   
)
)
