FAQ_UI <- function(id, label = "FAQ") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  #fluidPage(includeHTML('html_text/html_faq.html'))
  
  fluidPage(fluidRow(
    column(5,
           includeHTML('html_text/html_faq.html')
    )
  )
  )
  
}