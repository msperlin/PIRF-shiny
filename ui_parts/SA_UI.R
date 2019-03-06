SA_UI <- function(id, label = "SA") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidPage(h4('Autor e Desenvolvedor'),
            p(a('Marcelo S. Perlin', href = 'https://www.msperlin.com/blog/'),
              '/ EA - UFRGS (marcelo.perlin@ufrgs.br)'),
            img(src='logo_ufrgs.png', align = "left", width="100", height="100"),
            img(src='logo_ea.png', align = "left", width="100", height="100")
  )
  
}