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
                column(5,
                       includeHTML('html_text/html_intro.html'),
                       br() #,
                       #hr(),
                       #includeHTML('www/html_form.html')
                )
              )
    )
  )
  
}