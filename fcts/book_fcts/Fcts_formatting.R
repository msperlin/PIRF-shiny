format.cash <- function(x) {
  require(scales)

  x.formatted <- dollar(x,
                        prefix = 'R$ ',
                        decimal.mark = ',',
                        big.mark = '.',
                        largest_with_cents = Inf)

  return(x.formatted)
}


format.percent <- function(x) {
  require(scales)

  x.formatted <- percent(x,
                         decimal.mark = ',',
                         big.mark = '.')

  return(x.formatted)
}

format.date <- function(x) {

  x <- as.Date(x)
  x.formatted <- format(x, '%d/%m/%Y')

  return(x.formatted)
}

my.kable <- function(tbl,
                     my.tab.caption = 'MISSING CAPTION',
                     do.scale.down = TRUE) {
  require(knitr)
  require(kableExtra)
  require(tidyverse)

  if (do.scale.down) {
    my.latex.options <- c("scale_down", 'hold_position')
  } else {
    my.latex.options <- c('HOLD_position')
  }

  tab <- kable(tbl,
               align = 'l',
               caption = my.tab.caption,
               booktabs=TRUE,
               digits = 4) %>%
    kable_styling(latex_options = my.latex.options,
                  position = 'center')

  return(tab)
}


replace_str_file <- function(f.in, f.out, str.id, str.replace) {

  require(tidyverse)
  txt.out <- str_c(read_lines(f.in), collapse = '\n')

  for (i.str in seq(str.id)) {
    txt.out <- str_replace(txt.out,
                           pattern = fixed(str.id[i.str]),
                           replacement = str.replace[i.str])

  }

  cat(txt.out, file = f.out)

  return(invisible(TRUE))

}

