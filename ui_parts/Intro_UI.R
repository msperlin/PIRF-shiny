Intro_UI <-  function(id, label = "intro") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    
    fluidPage(titlePanel(title = '', 
                         windowTitle = my.title),
              tags$head(includeScript("GAnalytics/google-analytics.js")),
              tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.ico"),
                        tags$title("YourTitle")),
              fluidRow(
                column(4,
                       div('Seja bem vindo a interface IRF-shiny do livro ', strong(my.title), 
                          '\uA9 Marcelo S. Perlin 2019'),
                       br(),
                       p("Aqui terás acesso a um modo dinâmico de recriar as principais figuras apresentadas na obra.", 
                         'Todos aplicativos são abertos ao público e se utilizam de dados do Tesouro Nacional e BCB.'),
                       br(),
                       div(paste0('Nenhum dos aplicativos, isoladamente ou em conjunto, oferece recomendação de compra ou venda dos títulos públicos,',
                                 'produtos bancários ou qualquer outro tipo de investimento.'), 
                         code('Esta deve ser sua própria decisão como investidor. Analise os resultados e tire suas próprias conclusões.')),
                       br(),
                       p('Caso tiver interesse na obra que está sendo produzida, indique o seu email abaixo que encaminharei notícias.',
                         'Sugestões e comentários serão muito bem vindas!'),
                       br(),
                       hr(),
                       HTML(readLines('www/html_form.html'))
                )
              )
    )
  )
  
}