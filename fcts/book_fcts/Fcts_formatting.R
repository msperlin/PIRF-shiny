format.cash <- function(x) {
  require(scales)

  x.formatted <- dollar(x, prefix = 'R$ ',
                        decimal.mark = ',',
                        big.mark = '.')

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

