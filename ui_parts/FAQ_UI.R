FAQ_UI <- function(id, label = "FAQ") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidPage(h3('Questões frequentes'),
            br(),
            strong('Quando o livro vai ser lançado?'),
            p('Uma previsão otimista é meados de junho 2019.'),
            br(),
            
            strong('Como posso ficar sabendo do lançamento?'),
            p('Veja o formulário disponibilizado na aba "Introdução". Basta colocar seu email que enviarei a notícia do lançamento assim que acontecer.'),
            br(),
            
            strong('Quais os formatos do livro?'),
            p('A previsão é de que o livro seja disponibilizado em formato impresso, Ebook e parcialmente online (apenas primeiros capítulos).'),
            br(),
            
            strong('Qual será o número de páginas e preço do livro?'),
            p('Minha expectativa é de em torno de 170 páginas do livro impresso em formato tradicional (6 X 9 in).'),
            p('O preço será modesto e acessível, perto de 37 R$ (10 USD).'),
            br(),
            
            strong('O livro virá com códigos?'),
            p('Sim. Além do aplicativo web e o livro em si, o código utilizado para montar todas figuras será disponibilizado ao público'),
            br(),
            
            strong('Onde posso obter os dados do livro?'),
            p('Os dados podem ser baixados no seguinte', 
              a('link', href = 'TBA'), '.')
  )
  
}