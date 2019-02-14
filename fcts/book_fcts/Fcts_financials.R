calc.ir.rate <- function(n.days = 1:1000) {

  out <- numeric(length = length(n.days))

  out[n.days <= 6*30] <- 0.225
  out[ (n.days > 6*30)&(n.days <= 12*30)] <- 0.20
  out[ (n.days > 12*30)&(n.days < 24*30)] <- 0.175
  out[ (n.days > 24*30)] <- 0.15

  return(out)
}

calc.iof.rate <- function(n.days = 1:100 ) {

  vec.diff <- c(0, rep(c(-4,-3,-3), 10))

  per.out <- (100+cumsum(vec.diff))/100

  iof.out <- rep(0, length(n.days))

  idx.1 <- which(n.days <= 30)
  idx.2 <- na.omit(match(n.days, 0:30))

  iof.out[idx.1] <- per.out[idx.2]

  return(iof.out)
}

my.cum.ret.TD <- function(ret.in,
                          ticker = rep('ticker', length(ret.in)),
                          ref.date,
                          value.invested = 1,
                          do.emoluments = TRUE) {

  require(purrr)

  unique.tickers <- unique(ticker)
  ret.in[is.na(ret.in)] <- 0

  n.obs <- length(ret.in)

  my.fct <- function(ret.in, ref.date, do.emoluments) {

    cum.ret.nominal <- numeric(length = length(ret.in))
    cum.ret.liquid <- numeric(length = length(ret.in))
    for (i in (1:n.obs)) {

      ir.rate <- calc.ir.rate(i)

      if (i == 1) {
        ir.value = ir.rate*(value.invested*ret.in[i])
        cum.ret.nominal[i] <- value.invested*(1+ret.in[i]) - ir.value
      } else {

        month.now <- lubridate::month(ref.date[i])

        if ((do.emoluments)&(month.now %in% c(6,12))) { # pay emoluments
          emolument.perc = (1+0.005)^(1/2) -1
          emolument.value = cum.ret.nominal[i-1]*emolument.perc
        } else {
          emolument.value = 0
        }

        cum.ret.nominal[i] <- cum.ret.nominal[i-1]*(1+ret.in[i]) - emolument.value

        # pay IR
        ir.value = ir.rate*(cum.ret.nominal[i] - value.invested)
        cum.ret.liquid[i] <- cum.ret.nominal[i] - ir.value
      }

    }

    # do IR TD
    #ir.rate <- calc.ir.rate(n.obs)
    #last.value = cum.ret.out[length(cum.ret.out)]
    #cum.ret.out[length(cum.ret.out)] <- last.value - (cum.ret.out[length(cum.ret.out)] - value.invested)*ir.rate

    return(cum.ret.liquid)

  }

  l <- pmap(.l = list(ret.in = split(x = ret.in, f = ticker),
                      ref.date = split(x = ref.date, f = ticker),
                      do.emoluments = rep(do.emoluments, length(unique.tickers)) ),
            .f = my.fct)

  cum.ret <- do.call(what = c, args = l)

  return(cum.ret)

}


my.cum.ret.funds <- function(ret.in, ticker, ref.date, value.invested = 1,
                             do.come.cota = TRUE) {

  require(purrr)

  unique.tickers <- unique(ticker)
  ret.in[is.na(ret.in)] <- 0

  my.fct <- function(...) {

    n.obs <- length(ret.in)

    cum.ret.out <- numeric(length = length(ret.in))
    for (i in (1:n.obs)) {

      if (i == 1) {
        cum.ret.out[i] <- value.invested*(1+ret.in[i])
      } else {

        month.now <- lubridate::month(ref.date[i])

        cum.ret.out[i] <- cum.ret.out[i-1]*(1+ret.in[i])

        if ((do.come.cota)&(month.now %in% c(6,12))) { # pay comecota

          comecota.perc = 0.15

          if (i == 6) {
            cum.ret.comp <- value.invested
          } else {
            cum.ret.comp <- cum.ret.out[i-6]
          }

          comecota.value = (cum.ret.out[i] - cum.ret.comp)*comecota.perc

          cum.ret.out[i] <- cum.ret.out[i-1]*(1+ret.in[i]) - comecota.value
        }
      }

    }

    return(cum.ret.out)

  }


  l <- pmap(.l = list(ret.in = split(x = ret.in, f = ticker),
                      ref.date = split(x = ref.date, f = ticker),
                      do.come.cota = rep(do.come.cota, length(unique.tickers)) ),
            .f = my.fct)

  cum.ret <- do.call(what = c, args = l)

  return(cum.ret)

}

invest_TD <- function(df.in,
                      date.buy = min(df.in$ref.date),
                      date.sell = max(df.in$ref.date),
                      custody.cost.aa = 0.0025,
                      value.first.buy = 1,
                      value.monthly.buy = 0) {

  require(lubridate)
  require(tidyverse)

  custody.cost.as <- (1 + custody.cost.aa)^(1/2) - 1

  df.in <- df.in %>%
    filter(ref.date >= date.buy,
           ref.date <= date.sell)

  df.in <- ungroup(df.in)

  df.firstdates <- df.in %>%
    group_by(ref.month = as.Date(format(ref.date, '%Y-%m-01'))) %>%
    summarise(first.date = min(ref.date)) %>%
    mutate(month = month(ref.month))

  df.refdate <- df.firstdates %>%
    filter(month %in% c(7,1))

  # calculate B3 custody costs
  df.in$custody.cost <- 0
  idx <- df.in$ref.date %in% (df.refdate$first.date)
  df.in$custody.cost[idx] <- custody.cost.as

  df.in <- df.in %>%
    mutate(n.days = as.numeric(df.in$ref.date - min(df.in$ref.date)),
           ir.rate = calc.ir.rate(n.days),
           iof.rate = calc.iof.rate(n.days),
           asset.code = asset.code2)

  # do purchases
  df.in$value.purchases <- 0
  idx <- df.in$ref.date %in% df.firstdates$first.date
  df.in$value.purchases[idx] <- value.monthly.buy

  df.in$value.purchases[1] <-value.first.buy

  df.in <- invest_build_port(df.in)

  return(df.in)

}

invest_single_cdb_lca <- function(type.invest = 'CDB',
                           type.indexing = 'CDI',
                           percent.index = dplyr::if_else(type.indexing == 'CDI', 1, 0.05),
                           date.buy = as.Date('2010-01-01'),
                           date.sell = Sys.Date(),
                           value.first.buy = 1,
                           value.monthly.buy = 0) {

  require(tidyverse)

  if ( !(type.invest %in% c('CDB', 'LCA')) ) {
    stop('type.invest should be "CDB" or "LCA"')
  }

  if ( !(type.indexing %in% c('CDI', 'IPCA')) ) {
    stop('type.indexing should be "CDI" or "IPCA"')
  }

  if (type.indexing == 'CDI') {
    my.subname <- paste0(scales::percent(percent.index), ' CDI')
  } else {
    my.subname <- paste0('IPCA + ', scales::percent(percent.index))
  }

  if (type.invest == 'LCA') {
    do.ir = FALSE
    my.name <- paste0('LCA/LCI/LC ',
                      my.subname)
  } else if (type.invest == 'CDB') {
    do.ir = TRUE
    my.name <- paste0('CDB ',
                      my.subname)
  }

  if (type.indexing == 'CDI') {
    df.invest <- read_rds('data/CDI.rds') %>%
      rename(ref.month = date,
             index.nom.ret = value) %>%
      filter(ref.month >= date.buy,
             ref.month <= date.sell) %>%
      mutate(percent.index = percent.index,
             nom.ret = percent.index*index.nom.ret/100)
  } else {
    df.invest <- read_rds('data/IPCA.rds') %>%
      rename(ref.month = date,
             index.nom.ret = value) %>%
      select(-IPCA.am, - IPCA.aa) %>%
      filter(ref.month >= date.buy,
             ref.month <= date.sell) %>%
      mutate(percent.index = percent.index,
             nom.ret = index.nom.ret/100 + ( (1+percent.index)^(1/12) -1))
  }


  # calculate B3 custody costs (no custody costs for CDB)
  df.invest$custody.cost <- 0

  df.invest <- df.invest %>%
    mutate(asset.code = my.name,
           n.days = as.numeric(df.invest$ref.month - min(df.invest$ref.month)),
           ir.rate = if_else(rep(do.ir, nrow(df.invest)), calc.ir.rate(n.days), 0),
           iof.rate = calc.iof.rate(n.days),
           price.bid = cumprod(1 + nom.ret))

  # do purchases
  df.invest$value.purchases <- 0
  idx <- df.invest$ref.month %in% df.invest$ref.month
  df.invest$value.purchases[idx] <- value.monthly.buy
  df.invest$value.purchases[1] <-value.first.buy


  df.invest <- invest_build_port (df.invest)


  return(df.invest)

}



invest_cdb_lca <- function(type.invest = 'CDB',
                           type.indexing = 'CDI',
                           percent.index = dplyr::if_else(type.indexing == 'CDI', 1, 0.05),
                           date.buy = as.Date('2005-01-01'),
                           date.sell = as.Date('2018-12-31'),
                           contract.duration = 'lifetime',
                           value.first.buy = 1,
                           value.monthly.buy = 0) {

  require(tidyverse)

  # set date vec for purchases
  if (contract.duration == 'lifetime') {
    date.repurchases <- c(date.buy, date.sell)
  } else {
    date.repurchases <- unique(c(seq(date.buy, date.sell,
                                     by = contract.duration),
                                 date.sell))
  }


  temp.value.first.buy <- value.first.buy

  df.invest.out <- tibble()
  for (i.repurchase in seq(length(date.repurchases) -1)) {

    if (i.repurchase == 1) {
      temp.date.buy <- date.repurchases[i.repurchase]
    } else {
      temp.date.buy <- date.repurchases[i.repurchase] + 1
    }

    temp.date.sell <- date.repurchases[i.repurchase+1]

    df.invest.temp <- invest_single_cdb_lca(type.invest = type.invest,
                                            type.indexing = type.indexing,
                                            percent.index = percent.index,
                                            date.buy = temp.date.buy,
                                            date.sell = temp.date.sell,
                                            value.first.buy = temp.value.first.buy,
                                            value.monthly.buy = value.monthly.buy) %>%
      mutate(contract.duration = contract.duration )

    temp.value.first.buy <- last(df.invest.temp$port.net.value)

    df.invest.out <- bind_rows(df.invest.out,
                               df.invest.temp)
  }

  return(df.invest.out)

}


invest_poup <- function(date.buy = as.Date('2010-01-01'),
                        date.sell = Sys.Date(),
                        value.first.buy = 1,
                        value.monthly.buy = 0) {

  require(tidyverse)

  my.name <- 'Caderneta de Poupança'

  df.invest <- read_rds('data/Poupanca.rds') %>%
      rename(ref.month = date,
             index.nom.ret = value) %>%
      filter(ref.month >= date.buy,
             ref.month <= date.sell) %>%
      mutate(percent.index = NA,
             nom.ret = index.nom.ret)

  do.ir <- FALSE

  # calculate B3 custody costs (no custody costs for CDB)
  df.invest$custody.cost <- 0

  df.invest <- df.invest %>%
    mutate(asset.code = my.name,
           n.days = as.numeric(df.invest$ref.month - min(df.invest$ref.month)),
           ir.rate = if_else(rep(do.ir, nrow(df.invest)), calc.ir.rate(n.days), 0),
           iof.rate = calc.iof.rate(n.days),
           price.bid = cumprod(1 + nom.ret))

  # do purchases
  df.invest$value.purchases <- 0
  idx <- df.invest$ref.month %in% df.invest$ref.month
  df.invest$value.purchases[idx] <- value.monthly.buy
  df.invest$value.purchases[1] <-value.first.buy

  df.invest <- invest_build_port (df.invest)

  return(df.invest)

}

invest_build_port <- function(df.invest) {
  df.invest <- df.invest %>%
    mutate(qtd.purchased = value.purchases/price.bid,
           cost.portfolio = cumsum(value.purchases),
           qtd = cumsum(qtd.purchased),
           port.nominal.value = qtd*price.bid,
           pm = cost.portfolio/qtd,
           port.net.value = port.nominal.value -
             port.nominal.value*(custody.cost) -
             (ir.rate+iof.rate)*(price.bid - pm)*qtd)

  return(df.invest)
}
