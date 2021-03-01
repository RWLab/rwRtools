# Factor Analysis Utilities
# =========================

# Perform factor analysis given a data frame of factors & prices
# TODO
  # Document properly
  # Needs some unit tests
  # pass column name as argument

#' Calculate price momentum over some lookback
#'
#' @param prices_df dataframe of prices, optionally with an interest rate column
#' @param formation_period period over which to calculate momentum
#' @param factor_col the column of prices_df over which to calculate momentum factor
#' @param ticker_col the column of prices_df containing the ticker names
#'
#' @return prices_df grouped by ticker with momentum factor column
#' @export
#'
#' @examples
#' prices <- data.frame(
#'   Ticker = rep("EURUSD", 6),
#'   Close = c(1.11567, 1.11609, 1.11823, 1.11990, 1.12056, 1.12161)
#'   )
#' momo_factor(prices, 3)
momo_factor <- function(prices_df, formation_period = 60, factor_col = Close, ticker_col = Ticker) {

  factor_col <- dplyr::enquo(factor_col)
  ticker_col <- dplyr::enquo(ticker_col)

  # Calculate total return momentum factor
  prices_df %>%
    dplyr::group_by(!!ticker_col) %>%
    dplyr::mutate(
      factor = TTR::ROC(!!factor_col, n = formation_period, type = 'discrete', na.pad = TRUE)
    )
}

#' SMA Difference Factor
#'
#' Calculate differential between shorter term and longer term moving average as  a momentum measure.
# A typical setting might be to set the short SMA to 1/10th the length of the longer SMA.
#'
#' @param prices_df dataframe of prices, optionally with an interest rate column
#' @param formation_period period over which to calculate long SMA
#' @param short_period period over which to calculate short SMA
#' @param factor_col the column of prices_df over which to calculate momentum factor
#' @param ticker_col the column of prices_df containing the ticker names
#'
#' @return prices_df grouped by ticker with long SMA, short SMA, and factor columns
#' @export
#'
#' prices <- data.frame(
#'   Ticker = rep("EURUSD", 6),
#'   Close = c(1.11567, 1.11609, 1.11823, 1.11990, 1.12056, 1.12161)
#'   )
#' sma_diff_factor(prices, 3, 2)
sma_diff_factor <- function(prices_df, formation_period = 60, short_period = formation_period * 0.1, factor_col = Close, ticker_col = Ticker) {
  factor_col <- dplyr::enquo(factor_col)
  ticker_col <- dplyr::enquo(ticker_col)

  # calculate sma factor
  prices_df %>%
    group_by(!!ticker_col) %>%
    dplyr::mutate(
      long_sma = TTR::SMA(!!factor_col, n = formation_period, na.pad = FALSE),
      short_sma = TTR::SMA(!!factor_col, n = short_period, na.pad = FALSE),
      factor = (short_sma / long_sma) - 1
    )
}

# Differential between current price and sma
price_to_sma_factor <- function(prices_df, formation_period = 60, total_return = FALSE) {
  use_col <- get_return_col(prices_df, total_return)

  # calculate sma factor
  prices_df %>%
    group_by(Ticker) %>%
    tq_mutate_(use_col, 'SMA', n = formation_period, na.pad = FALSE, col_rename = 'long_sma') %>%
    mutate(price = !!sym(use_col)) %>%
    mutate(factor = (price / long_sma) - 1) %>%
    na.omit() %>%
    select(Ticker, Date, factor)
}

# Slope of a simple moving average as momentum factor
sma_slope_factor <- function(prices_df, formation_period = 60, total_return = FALSE) {
  use_col <- get_return_col(prices_df, total_return)

  # calculate sma factor
  prices_df %>%
    group_by(Ticker) %>%
    tq_mutate_(use_col, 'SMA', n = formation_period, na.pad = FALSE, col_rename = 'long_sma') %>%
    tq_mutate_('long_sma', 'lag.xts', k = 1, na.pad = TRUE, col_rename = 'lag_sma') %>%
    mutate(factor = (long_sma / lag_sma) - 1) %>%
    na.omit() %>%
    select(Ticker, Date, factor)
}

# Current price percentile over a rolling lookback period
percentile_factor <- function(prices_df, formation_period = 60, total_return = FALSE) {
  use_col <- get_return_col(prices_df, total_return)

  # calculate sma factor
  prices_df %>%
    group_by(Ticker) %>%
    tq_transmute_(use_col, 'runPercentRank', n = formation_period, col_rename = 'factor') %>%
    na.omit()
}

# Z-score of current price over rolling lookback period
price_zscore_factor <- function(prices_df, formation_period = 60, total_return = FALSE) {
  use_col <- get_return_col(prices_df, total_return)

  # calculate sma factor
  prices_df %>%
    group_by(Ticker) %>%
    tq_mutate_(use_col, 'runMean', n = formation_period, col_rename = 'running_mean') %>%
    tq_mutate_(use_col, 'runSD', n = formation_period,  col_rename = 'running_sd') %>%
    mutate(price = !!sym(use_col)) %>%
    mutate(factor = (price - running_mean) / (running_sd)) %>%
    na.omit() %>%
    select(Ticker, Date, factor)
}

sign_ratio_factor <- function(prices_df, formation_period = 22, num_formation_periods = 12, total_return = FALSE) {

  use_col <- get_return_col(prices_df,total_return)

  # Calculate return sign ratio factor
  prices_df %>%
    group_by(Ticker) %>%
    tq_transmute_(use_col, 'rollapplyr', width = formation_period*num_formation_periods, FUN = function(x){
      last_idx <- last(index(x))
      c <- coredata(x)

      sequence <- seq(0, formation_period * num_formation_periods, formation_period)
      start <- (sequence + 1)[-(num_formation_periods+1)]
      end <- sequence[-1]

      s <- sign(c[end]-c[start])
      pos <- sum(s[s>0])
      neg <- abs(sum(s[s<0]))

      ratio <- ifelse(neg > 0, pos/neg, pos)

      xts(ratio, order.by=last(last_idx))
    }, col_rename = 'factor') %>%
    na.omit()
}

efficiency_ratio_factor <- function(prices_df, formation_period = 60, total_return = FALSE) {

  use_col <- get_return_col(prices_df,total_return)

  # Calculate efficiency ratio factor
  prices_df %>%
    group_by(Ticker) %>%
    tq_transmute_(use_col, 'rollapplyr', width = formation_period, FUN = function(x){
      change <- last(x)-first(x)
      vola <- sum(abs(diff(x)[-1]))
      er <- change/vola
    }, col_rename = 'factor') %>%
    na.omit()
}

# Carry factor requires a price data frame with appended interest rates to be passed in. See append_interest_rate_differential()
carry_factor <- function(extended_prices_df) {

  # Check for existence of Rate_Diff column
  if (!'Rate_Diff' %in% colnames(extended_prices_df)) {
    stop('Rate_Diff column does not exist in extended prices data frame')
  }

  extended_prices_df %>%
    mutate('factor' = Rate_Diff) %>%
    select(c('Ticker','Date', 'factor')) %>%
    na.omit()
}

#Calculate volatility adjusted momentum factor (Sharpe Ratio)
#Pretty sure there is something wrong with it :)
sr_factor <- function(prices_df, formation_period = 252) {
  # Calculate price change momentum factor
  prices_df %>%
    group_by(Ticker) %>%
    mutate(ReturnClose = c(NA, diff(Close))) %>%
    tq_transmute(ReturnClose, rollapplyr, width = formation_period, FUN =
                   function(x){
                     SharpeRatio(x, FUN = "StdDev")
                   }, col_rename = 'factor') %>%
    na.omit()
}

scale_factors <- function (factors, lower = -1, upper = 1) {
  minFac <- min(factors)
  maxFac <- max(factors)

  if ( maxFac != minFac ) {
    (upper - lower) * (factors - minFac) / (maxFac - minFac) + lower;
  } else {
    if ( minFac == 0 ) {
      factors
    } else {
      factors/minFac
    }
  }
}

ensemble_factors <- function(prices_df, factor_configs, total_return) {

  factor_df <- prices_df %>%
    select(Ticker, Date)

  factor_types <- names(factor_configs)
  for (type in factor_types) {

    config <- factor_configs[[type]]

    factor_df <- factor_df %>% inner_join(
      {switch( type,
              "price" = calc_momo_factor(prices_df, formation_period = config['lookback'], total_return = total_return),
              "smadiff" = calc_sma_diff_factor(prices_df, formation_period = config['lookback'], total_return = total_return),
              "pricesmadiff" = calc_price_to_sma_factor(prices_df, formation_period = config['lookback'], total_return = total_return),
              "smaslope" = calc_sma_slope_factor(prices_df, formation_period = config['lookback'], total_return = total_return),
              "pricepercentile" = calc_price_percentile_factor(prices_df, formation_period = config['lookback'], total_return = total_return),
              "pricezscore" = calc_price_zscore_factor(prices_df, formation_period = config['lookback'], total_return = total_return),
              "carry" = calc_carry_factor(prices_df),
              "sign" = calc_sign_ratio_factor(prices_df, formation_period = config['lookback'], total_return = total_return),
              "efficiencyratio" = calc_efficiency_ratio_factor(prices_df, formation_period = config['lookback'], total_return = total_return),
              stop('Factor "', type, '" is not known, make sure the name is correct.')
      ) %>%
        group_by(Date) %>%
        mutate(factor = scale_factors(factor)) %>%
        mutate(factor = factor - mean(factor)) %>%
        rename(!!paste0('factor_', type) := factor)},
      by = c('Ticker', 'Date')
    )
  }

  factor_df %>%
    group_by(Date, Ticker) %>%
    mutate(factor = mean(c(!!!syms(paste0('factor_', factor_types)))))
}

plot_factor_correlations <- function(factor_df) {

  factor_columns <- grep('^factor_\\w+', colnames(factor_df), value = TRUE)

  factor_matrix <- factor_df %>%
    group_by(Ticker) %>%
    {cbind(.[factor_columns])}

  PerformanceAnalytics::chart.Correlation(factor_matrix)
}


# Calculate factor ranks and quantiles given data frame of factors
"
Explanation of get_quantiles():

First it is checked whether the number of distinct assets `n_assets` in `factor_df`
is smaller than the number of quantiles `n_quantiles` we want to use.
If so, `n_quantiles` is reduced to `n_assets` and a warning is given.

Next it is checked whether the current `q_type` selected for quantization is compatible
with the structure of the `factor_df`. To do that, the funtion `check_quantization`
is called. To skip this step, set `check_quantization = FALSE`.

Last, the quantiles are assigned.
For easier handling of different cases, the process of quantization is moved to a helper function `get_quantiles`.
The function used to group the factors into quantiles `quantile(factor, seq(0,1,1/n))` only works when `factor` has
more than one observation for a given `Date`. So in `get_quantiles` we make a distinction based on the length of the `factor`
vector. For `Date` values that have only a `factor` vector of `length == 1`, `ceiling(n/2)` is assigned as the `quantile` value.
The other `factor` vectors will be processed normally using the `quantile` function.
"
get_factor_quantiles <- function(factor_df, n_quantiles = 5, q_type = 7, check_quantization = TRUE) {


  # If we have less than n_quantiles assets set to number of assets and warn
  n_assets <- factor_df %>% ungroup %>% select(Ticker) %>% n_distinct()
  if (n_assets < n_quantiles) {
    n_quantiles <- n_assets
    if ( check_quantization ) {
      warning(paste('Number of quantiles set to', n_quantiles))
    }
  }

  if ( check_quantization ) {
    check_quantization(factor_df, n_quantiles, q_type)
  }

  factor_df %>%
    group_by(Date) %>%
    mutate(rank = rank(factor, ties.method='first'),
           quantile = get_quantiles(factor, n_quantiles, q_type))
}


get_quantiles <- function(factors, n_quantiles, q_type) {
  switch( as.character(length(factors)),
          '1' = ceiling(n_quantiles/2),
          '2' = c(1, n_quantiles)[row_number(factors)],
          '3' = c(1, ceiling(n_quantiles/2), n_quantiles)[row_number(factors)],
          if (anyNA(factors)) { rep(NA, length(factors))
            } else {
              tryCatch(cut(factors, quantile(factors, seq(0,1,1/n_quantiles), type = q_type), FALSE, TRUE),
                       error = function(err) {
                        # Add jitter to factors and try again
                         factor_jitter <- jitter(factors)
                         cut(factor_jitter, quantile(factor_jitter, seq(0,1,1/n_quantiles), type = q_type), FALSE, TRUE)
                       }
              )
            }
  )
}
"
This method ensures that the structure of `factor_df` produces symmetric quantization results.

First, all distinct cases of asset numbers are filtered into `distinct_n_assets`.
Not all assets have the same history length, so even if we have for example 12 assets in total
in `factor_df`, there might be observations on certain `Date` values where the number is smaller
than that. To make sure quantization works for all cases, we have therefore to test all distinct cases.
For every distinct case we record the number of assets `num_assets` in that case and how many times
`n` that case occured within `factor_df`.

Next for every distinct `n_assets`, the quantization using `n_quantiles` and `q_type` is tested.
There are certain values of `q_type` that require a minimum number of `n_assets` to work. If the
number is smaller, `quantile` throws an error. The exception is caught and a meaningful error message
given.

If no error is thrown, it is checked whether the resulting quantization is symmetrical. That
means it is counted, how many assets `n` are in which bucket `q_values`.
Then, the vector `n` is checked for symmetry.

The following code is not identical with the actual code used in the function. It is simplified
to illustrate the key operations, but it has the same functionality. It can also be copied to the
console and run to inspect intermediate steps.

=======================================================================
q_values <- c(1, 1, 2, 3, 4, 5, 5)
# [1] 1 1 2 3 4 5 5

c <- tibble(q_values) %>% count(q_values)

#  A tibble: 5 x 2
#   q_values     n
#      <dbl> <int>
# 1        1     2
# 2        2     1
# 3        3     1
# 4        4     1
# 5        5     2

c$n
# [1] 2 1 1 1 2

vec <- c$n

# extract the elements 1:3
vec[1:ceiling(NROW(vec)/2)]
# [1] 2 1 1

#extract the elements 5:3
vec[NROW(vec):floor(NROW(vec)/2+1)]
# [1] 2 1 1

# check whether vec[1:3] is equal to vec[5:3], i.e. check for symmetry
all(vec[1:ceiling(NROW(vec)/2)] == vec[NROW(vec):floor(NROW(vec)/2+1)])
# TRUE
=======================================================================

If the resulting `q_values` for any combination of `n_assets`, `n_quantiles` and `q_type`
are not symmetrical, a meaningful warning is given including the values of `n_assets`, `n_quantiles`,
`q_type`, `q_values` and `n_occurences` so it can be judged whether we care about that.
"
check_quantization <- function(factor_df, n_quantiles, q_type) {

  cut_quantiles <- function(n_assets,n_quantiles,q_type) {
    cut(1:n_assets, quantile(1:n_assets, seq(0,1,1/n_quantiles), type = q_type), FALSE, TRUE)
  }

  q_are_symmetric <- function(q_values) {
    tibble(q_values) %>%
      count(q_values) %>%
      select(n) %>%
      {all(.[1:ceiling(nrow(.)/2),] == .[nrow(.):floor(nrow(.)/2+1),])}
  }

  other_q_types <- c(4:9)[!(c(4:9) %in% q_type)]

  n_total_obs <- factor_df %>%
    group_by(Date) %>%
    select(Date) %>%
    n_distinct()

  distinct_n_assets <- factor_df %>%
    group_by(Date) %>%
    summarise(num_assets = n()) %>%
    filter(num_assets > 3) %>%
    count(num_assets) %>%
    split(1:nrow(.))

  n_distinct_combinations <- distinct_n_assets %>%
    sapply(nrow) %>%
    sum

  if ( n_distinct_combinations > 0) {
    for (df_row in distinct_n_assets) {

      n_assets <- df_row$num_assets
      n_occurences <- df_row$n


      q_values <- tryCatch({
        cut_quantiles(n_assets,n_quantiles,q_type)
      }, error = function(e)stop('q_type = ', q_type, ' is not compatible with a combination of ', n_assets,
                                 ' assets and ', n_quantiles, ' quantiles.\nTry choosing a different q_type.',
                                 call. = FALSE))

      if ( !q_are_symmetric(q_values) ) {
        working_q_types <- other_q_types[tryCatch({
          sapply(other_q_types, function(x,y,z) q_are_symmetric(cut_quantiles(x,y,z)),x=n_assets,y=n_quantiles)},
          error=function(e){})]
        warning('Could not divide factor values into symmetric quantiles for following configuration:\n',
                'n_assets = ', n_assets, ', n_quantiles = ', n_quantiles, ', q_type = ', q_type, '\n',
                'Quantile distribution:\n',
                paste(q_values, collapse = ' '),
                '\nNumber of occurences: ', n_occurences, ' of a total of ', n_total_obs, ' observations\n',
                ifelse(length(working_q_types)!=0,
                       paste0('Alternative q_type settings that yield symmetric quantiles are: ',paste(working_q_types,collapse = ","),'.\n'),
                       paste0('No working alternative to q_type ', q_type,' was found.\n')))
      }
    }
  }
}

check_factor_integrity <- function(factor_df, n_quantiles = 5, q_type = 7) {
  result <- factor_df %>%
    group_by(Date) %>%
    filter(n() > 3 && !anyNA(factor)) %>%
    mutate(error = any(duplicated(quantile(factor, seq(0,1,1/n_quantiles), type = q_type)))) %>%
    slice(which(error)) %>%
    select(-error)

  if(nrow(result)) {
    return(result)
  } else {
    return('No incompatibilities found.')
  }
}

# Calculate forward returns from prices and append to quantile data frame
# quantiles are  calculated to foward returns when forward_quantiles is set as: c(n_quantiles,q_type)
# TODO: This needs unit tests. And, ideally, splitting out into more testable components.
append_forward_returns <- function(quantile_df, prices_df, total_return = FALSE, demean_returns = FALSE, forward_quantiles = NULL, forward_periods = c(10,20,60)) {

  use_col <- get_return_col(prices_df,total_return)

  # Calculate forward returns for forward periods
  forward_returns <- prices_df

  for(period in forward_periods ) {
    period_col <- paste0('period_', period)
    lead_col <- paste0('leadperiod_', period)
    fw_q_col <- paste0('forward_quantile_', period)

    forward_returns <- forward_returns %>%
      group_by(Ticker) %>%
      tq_mutate_(use_col, 'ROC', n = period, type = 'discrete', col_rename = period_col) %>%
      mutate(!!lead_col := lead(!!sym(period_col), period)) %>%
      group_by(Date) %>%
      {if (demean_returns) mutate(., !!lead_col := !!sym(lead_col)-mean(!!sym(lead_col))) else .} %>%
      {if (!is.null(forward_quantiles)) mutate(., !!fw_q_col := get_quantiles(!!sym(lead_col),forward_quantiles[1],forward_quantiles[2])) else .}
  }

  # Join to Quantile df
  if(is.null(forward_quantiles))
    period_cols <- paste0('leadperiod_', forward_periods)
  else
    period_cols <- c(paste0('leadperiod_', forward_periods), paste0('forward_quantile_', forward_periods))

  quantile_df %>%
    left_join(forward_returns, by = c('Ticker','Date')) %>%
    select(Ticker, Date, grep('^factor', colnames(quantile_df), value = TRUE), rank, quantile, period_cols)
}


# Plot bar chart of forward returns
plot_forward_mean_returns <- function(analysis_df, plot_yearly = FALSE, chart_title = 'Forward mean returns') {

  forward_period_labels <- gsub("leadperiod_", "", grep("lead", colnames(analysis_df), value = TRUE))
  plot_df <- plot_yearly_df <- NULL

  i <- 1
  for (period in forward_period_labels) {

    lead_col <- paste0('leadperiod_', period)
    daily_lead_col <- paste0('leadperiod_daily_mean_', period)
    mean_lead_col <- paste0('meanlead', i)

    daily_means_df <- analysis_df %>%
      group_by(Date, quantile) %>%
      summarise(!!daily_lead_col := mean(!!rlang::sym(lead_col), na.rm=T))


    plot_df <- plot_df %>% bind_cols(
      daily_means_df %>%
        group_by(quantile) %>%
        summarise(!!mean_lead_col := mean(!!rlang::sym(daily_lead_col), na.rm=T)) %>%
        {if (i > 1) select(., !!mean_lead_col) else .}
    )

    if ( plot_yearly ) {
      plot_yearly_df <- plot_yearly_df %>% bind_cols(
        daily_means_df %>%
          mutate(year = year(Date)) %>%
          group_by(year, quantile) %>%
          summarise(!!mean_lead_col := mean(!!rlang::sym(daily_lead_col), na.rm=T)) %>%
          ungroup() %>%
          {if (i > 1) select(., !!mean_lead_col) else .}
      )
    }

    i <- i + 1
  }

  (plot_df %>%
      gather(key='hold_period', value='mean_returns', paste0('meanlead', seq_along(forward_period_labels))) %>%
      ggplot(aes(y = mean_returns, x = quantile, fill = hold_period)) +
      geom_col(position = position_dodge(preserve = "total")) +
      scale_fill_discrete(name = "Forward returns",
                          breaks = paste0('meanlead', seq_along(forward_period_labels)),
                          labels = paste(forward_period_labels, 'days')) +
      labs(y = "Mean return",
           x = "Quantile",
           title = chart_title)) %>%
    print()

  if ( plot_yearly ) {

    labels <- paste(forward_period_labels, 'day forward returns') %>%
      setNames(paste0('meanlead', seq_along(forward_period_labels)))

    plot_yearly_df <- plot_yearly_df %>%
      na.omit() %>%
      mutate(quantile = as.character(quantile)) %>%
      gather(key='hold_period', value='mean_returns', paste0('meanlead', seq_along(forward_period_labels)))

    for (i in seq_along(forward_period_labels)) {
      (plot_yearly_df %>%
         filter(hold_period ==  paste0('meanlead', i)) %>%
         ggplot(aes(y = mean_returns, x = year, group = interaction(quantile,hold_period), color = quantile)) +
         geom_hline(yintercept = 0) +
         geom_point() +
         geom_smooth(method = 'loess', se = FALSE) +
         facet_grid(.~hold_period, labeller=labeller(hold_period = labels)) +
         labs(y = "Mean return",
              x = "Year",
              title = chart_title)) %>%
        print()
    }
  }
}



# Plot quantile factor/target rolling rank correlation plots
plot_quantile_cor <- function(analysis_df, method = "spearman", ma_per = 200, chart_title = 'forward correlation') {

  forward_period_labels <- gsub("forward_quantile_", "", grep("forward", colnames(analysis_df), value = TRUE))
  plot_df  <- NULL

  for (period in forward_period_labels) {

    fow_col <- paste0('forward_quantile_', period)
    cor_col <- paste0('cor_', period)

    plot_df <- plot_df %>% bind_cols(
      analysis_df %>%
        group_by(Date) %>%
        summarise(!!cor_col := cor(quantile,!!sym(fow_col), method = method)) %>%
        {if (!is.null(plot_df)) select(., !!cor_col) else .}
    )
  }

  print(plot_df %>%
          gather(key='hold_period', value='correlation', paste0('cor_', forward_period_labels)) %>%
          ggplot(aes(y = correlation, x = Date, color = hold_period)) +
          geom_hline(yintercept = c(-0.25,0.25), linetype = "dashed") +
          geom_ma(n=ma_per, linetype = "solid",size=0.8) +
          labs(title = paste0(chart_title, " (",method,")")) +
          scale_color_discrete(name = "Cor Period",
                               breaks = paste0('cor_', forward_period_labels),
                               labels = paste(forward_period_labels, 'days')))

}


#plot a violin plot of the forward returns
plot_returns_violin <- function(analysis_df){

  forward_period_labels <- gsub("leadperiod_", "", grep("^lead", colnames(analysis_df), value = TRUE))
  plot_df <- NULL
  summary_function <- 'median.quartile'

  i <- 1
  for (period in forward_period_labels) {
    plot_df <- plot_df %>% bind_cols(
      analysis_df %>% group_by(Date, quantile) %>%
        summarise(!!paste0('leadperiod_daily_mean_', period) := mean(!!rlang::sym(paste0('leadperiod_', period)), na.rm=T)) %>%
        ungroup() %>%
        {if (i > 1) select(., !!paste0('leadperiod_daily_mean_', period)) else .}
    )
    i <- i + 1
  }

  plot_df %>%
    na.omit() %>%
    mutate(quantile = as.character(quantile)) %>%
    gather(key = 'hold_period', value = 'mean_returns', paste0('leadperiod_daily_mean_', forward_period_labels)) %>%
    ggplot(aes(y = mean_returns, x = quantile)) +
    geom_violin(aes(fill = hold_period)) +
    scale_fill_discrete(name = "Forward returns",
                        breaks = paste0('leadperiod_daily_mean_', forward_period_labels),
                        labels = paste(forward_period_labels, 'days')) +
    stat_summary(aes(group=interaction(quantile, hold_period)), fun.data=summary_function,  position=position_dodge(width = .9), size = 0.3) +
    labs(y = "Returns",
         x = "Quintile")
}

median.quartile <- function(x){
  out <- quantile(x, probs = c(0.25,0.5,0.75))
  names(out) <- c("ymin","y","ymax")
  return(out)
}


# run long-short backtest
# hold_period determines the number of days between rebalancing
# num_quantiles determines the number N of top N quantiles and bottom N quantiles to select for trading
# direction determines which strategy is applied
#     momo longs top quantiles and shorts bottom quantiles, mean-rev does the opposite
long_short_backtest <- function(quantile_df, prices_df, hold_period = 60, num_quantiles = 1, direction = c('momo', 'mean-rev'), total_return = FALSE) {

  if (total_return) {

    # Calcalate returns from Total Return Index
    if (!'Total_Return_Index' %in% colnames(prices_df)) {
      stop('Total_Return_Index column does not exist in prices data frame. Set total_return = FALSE to use spot forward returns, or call append_interest_rate_differential()')
    }

    use_col <- "Total_Return_Index"
  } else {
    use_col <- "Close"
  }

  max_quantile <- max(quantile_df$quantile)

  if ( num_quantiles > floor(max_quantile/2) ) {
    stop(paste('Too many quantiles selected. You have', max_quantile,
               'quantiles available so the max. number to select for trading is', floor(max_quantile/2)))
  }

  dir_sign <- function(quantile, num_quantiles, max_quantile, direction) {

    ifelse( quantile < (num_quantiles+1) | quantile > (max_quantile-num_quantiles), {
      if (direction[1] == 'momo') {
        sign(quantile - max_quantile/2 - .5)
      } else {
        sign(max_quantile/2 - quantile + .5)
      }
    }, {
      0
    })
  }

  weights_table <- quantile_df %>%
    group_by(Date) %>%
    mutate(num_assets = sum(quantile < (num_quantiles+1)) + sum(quantile > (max_quantile-num_quantiles))) %>%
    mutate(weight = dir_sign(quantile, num_quantiles, max_quantile, direction)/num_assets) %>%
    group_by(Ticker) %>%
    mutate(weight = lag.xts(weight, 1)) %>%
    slice(2:n())

  weights_df <- weights_table %>%
    select(Date, Ticker, weight) %>%
    spread(Ticker, weight) %>%
    replace(is.na(.), 0)

  weights_xts <- as.xts(weights_df[,-1], order.by = weights_df$Date)
  rebal_idx <- na.omit(index(weights_xts)[seq(1, length(weights_xts), hold_period)])
  rebal_weights_xts <- abs(weights_xts[rebal_idx])

  return_sign_xts <- weights_xts
  return_sign_xts[!(index(return_sign_xts) %in% rebal_idx),] <-  NA
  return_sign_xts <- sign(na.locf(return_sign_xts))

  returns_table <- prices_df %>%
    right_join(quantile_df, by = c('Ticker','Date')) %>%
    group_by(Ticker) %>%
    tq_mutate_(use_col, 'ROC',type = 'discrete', col_rename = 'returns')

  returns_df <- returns_table %>%
    select(Date, Ticker, returns) %>%
    spread(Ticker, returns) %>%
    replace(is.na(.), 0)

  returns_xts <- xts(returns_df[,-1], order.by = returns_df$Date)
  long_short_returns_xts <- returns_xts * return_sign_xts

  port_returns <- Return.portfolio(long_short_returns_xts, rebal_weights_xts)

  charts.PerformanceSummary(port_returns,
                            main=paste('Long-short',  direction, 'portfolio, ',
                                       if(total_return) 'Total returns' else 'Spot returns',
                                       '\nTop/Bottom', num_quantiles, 'of', max_quantile, 'quantiles, ',
                                       hold_period, 'days holding period'))
}

long_per_quantile_backtest <- function(quantile_df, prices_df, total_return = FALSE) {

  if (total_return) {

    # Calcalate returns from Total Return Index
    if (!'Total_Return_Index' %in% colnames(prices_df)) {
      stop('Total_Return_Index column does not exist in prices data frame. Set total_return = FALSE to use spot forward returns, or call append_interest_rate_differential()')
    }

    use_col <- "Total_Return_Index"
  } else {
    use_col <- "Close"
  }

  q_weight <- function(quantile, n_quantile, max_quantile) {

    ifelse( quantile == n_quantile, {
      1/sum(quantile == n_quantile)
    }, {
      0
    })
  }

  max_quantile <- max(quantile_df$quantile)

  returns_table <- prices_df %>%
    right_join(quantile_df, by = c('Ticker','Date')) %>%
    group_by(Ticker) %>%
    tq_mutate_(use_col, 'ROC',type = 'discrete', col_rename = 'returns')

  returns_df <- returns_table %>%
    select(Date, Ticker, returns) %>%
    spread(Ticker, returns) %>%
    replace(is.na(.), 0)

  returns_xts <- xts(returns_df[,-1], order.by = returns_df$Date)

  quantile_returns <- xts(NULL, order.by=index(returns_xts))

  for ( n_quantile in 1:max_quantile ) {

    weights_table <- quantile_df %>%
      group_by(Date) %>%
      mutate(weight = q_weight(quantile, n_quantile, max_quantile)) %>%
      group_by(Ticker) %>%
      mutate(weight = lag.xts(weight, 1)) %>%
      slice(2:n())

    weights_df <- weights_table %>%
      group_by(Date) %>%
      filter(sum(weight) == 1) %>%
      select(Date, Ticker, weight) %>%
      spread(Ticker, weight) %>%
      replace(is.na(.), 0)

    weights_xts <- as.xts(weights_df[,-1], order.by = weights_df$Date)

    quantile_returns <- quantile_returns %>% merge.xts(
      Return.portfolio(returns_xts, weights_xts) %>% setNames(paste0('q_', n_quantile))
      , join = if (n_quantile == 1) 'right' else 'outer')
  }

  quantile_returns <- quantile_returns %>%
    replace(is.na(.), 0)

  charts.PerformanceSummary( quantile_returns,
                             main = paste('Long-only performance per quantile,',
                                          if(total_return) 'Total returns' else 'Spot returns'))

}


plot_forward_series <- function(prices_df, n_quantiles, factor_list = c("calc_momo_factor","calc_price_zscore_factor","calc_efficiency_ratio_factor"),
                                  lookback = c(10,30,60,90,120,150), forward_periods = seq(1,100,by=5), total_return = FALSE, q_type = 7,cor_method = "pearson") {

  require(foreach)
  require(doParallel)

  return_spread <- function(quant,value)
  {
    return(mean(value[quant == max(quant)])-mean(value[quant == min(quant)]))
  }


  registerDoParallel(detectCores())

  for (factor_function in factor_list) {

    fun <- match.fun(factor_function)

    plot_spread_df <- plot_cor_df <- NULL

    output <- foreach (period = lookback, .combine = "bind_rows", .multicombine = TRUE, .inorder = FALSE, .packages = "tidyquant",
                       .export = c("get_return_col","get_quantiles","get_factor_quantiles","append_forward_returns")) %dopar% {

                         factor_df <- fun(prices_df, period, total_return = total_return)
                         quantile_df <- get_factor_quantiles(factor_df, n_quantiles = n_quantiles, q_type = q_type, check_quantization = FALSE)
                         analysis_df <- append_forward_returns(quantile_df, prices_df, total_return = FALSE, demean_returns = FALSE, forward_periods = forward_periods, forward_quantiles = c(n_quantiles,q_type))

                         spread_df <- analysis_df %>%
                           gather(key="Forward_Period", value="Forward_Return", grep("^lead", colnames(analysis_df), value = TRUE)) %>%
                           mutate(Forward_Period = as.numeric(gsub("leadperiod_", "",Forward_Period))) %>%
                           select(Date,quantile,Forward_Period,Forward_Return)

                         cor_df <- analysis_df %>%
                           gather(key="Forward_Period", value="Forward_Quantile", grep("^forward", colnames(analysis_df), value = TRUE)) %>%
                           mutate(Forward_Period = as.numeric(gsub("forward_quantile_", "",Forward_Period))) %>%
                           select(Date,quantile,Forward_Period,Forward_Quantile)

                         bind_cols(spread_df %>% group_by(Forward_Period,Date) %>%
                                     summarise(Return_spread = return_spread(quantile,Forward_Return)) %>%
                                     group_by(Forward_Period) %>%
                                     summarise(Mean_S = mean(Return_spread,na.rm=TRUE),SD_S = sd(Return_spread,na.rm = TRUE)),

                                   cor_df %>% group_by(Forward_Period,Date) %>%
                                     summarise(Cor = cor(quantile,Forward_Quantile,method = cor_method)) %>%
                                     group_by(Forward_Period) %>%
                                     summarise(Mean_C = mean(Cor,na.rm=TRUE),SD_C = sd(Cor,na.rm = TRUE))) %>%
                           mutate(Lookback = period)

                       }

    print(ggplot(data=output,aes(x=Forward_Period,y=Mean_S,group=Lookback,color=Lookback)) +
            geom_line() + geom_point() +
            geom_errorbar(aes(ymin=Mean_S-0.01*SD_S,ymax=Mean_S+0.01*SD_S)) +
            labs(title = paste("Mean Forward Spread between Quantile 1 and",n_quantiles," factor: ",factor_function)) +
            geom_hline(yintercept = c(-0.01,0.01), linetype = "dashed") +
            scale_color_gradient(low="blue", high="red"))

    print(ggplot(data=output,aes(x=Forward_Period,y=Mean_C,group=Lookback,color=Lookback)) +
            geom_line() + geom_point() +
            geom_errorbar(aes(ymin=Mean_C-0.01*SD_C,ymax=Mean_C+0.01*SD_C)) +
            labs(title = paste0("Mean Forward Correlation between all Quantiles (",n_quantiles,"), factor: ", factor_function)) +
            geom_hline(yintercept = c(-0.1,0.1), linetype = "dashed") +
            scale_color_gradient(low="blue", high="red"))
  }

  stopImplicitCluster()
}


# Plot bar chart of forward sharpe ratios
plot_forward_std_returns <- function(analysis_df, plot_yearly = FALSE, chart_title = 'Forward sharpe ratios') {

  forward_period_labels <- gsub("leadperiod_", "", grep("lead", colnames(analysis_df), value = TRUE))
  plot_df <- plot_yearly_df <- NULL

  i <- 1
  for (period in forward_period_labels) {

    lead_col <- paste0('leadperiod_', period)
    daily_lead_col <- paste0('leadperiod_daily_mean_', period)
    mean_lead_col <- paste0('meanlead', i)

    daily_means_df <- analysis_df %>%
      group_by(Date, quantile) %>%
      summarise(!!daily_lead_col := mean(!!rlang::sym(lead_col), na.rm=T))


    plot_df <- plot_df %>% bind_cols(
      daily_means_df %>%
        group_by(quantile) %>%
        summarise(!!mean_lead_col := mean(!!rlang::sym(daily_lead_col), na.rm=T)*252/as.numeric(period) / sd(!!rlang::sym(daily_lead_col)*sqrt(252/as.numeric(period)),na.rm = T)) %>%
        {if (i > 1) select(., !!mean_lead_col) else .}
    )

    if ( plot_yearly ) {
      plot_yearly_df <- plot_yearly_df %>% bind_cols(
        daily_means_df %>%
          mutate(year = year(Date)) %>%
          group_by(year, quantile) %>%
          summarise(!!mean_lead_col := mean(!!rlang::sym(daily_lead_col), na.rm=T)*252/as.numeric(period) / sd(!!rlang::sym(daily_lead_col)*sqrt(252/as.numeric(period)),na.rm = T)) %>%
          ungroup() %>%
          {if (i > 1) select(., !!mean_lead_col) else .}
      )
    }

    i <- i + 1
  }

  (plot_df %>%
      gather(key='hold_period', value='mean_returns', paste0('meanlead', seq_along(forward_period_labels))) %>%
      ggplot(aes(y = mean_returns, x = quantile, fill = hold_period)) +
      geom_col(position = position_dodge(preserve = "total")) +
      scale_fill_discrete(name = "Forward std returns",
                          breaks = paste0('meanlead', seq_along(forward_period_labels)),
                          labels = paste(forward_period_labels, 'days')) +
      labs(y = "Mean sharpe ratios",
           x = "Quantile",
           title = chart_title)) %>%
    print()

  if ( plot_yearly ) {

    labels <- paste(forward_period_labels, 'day forward returns') %>%
      setNames(paste0('meanlead', seq_along(forward_period_labels)))

    plot_yearly_df <- plot_yearly_df %>%
      na.omit() %>%
      mutate(quantile = as.character(quantile)) %>%
      gather(key='hold_period', value='mean_returns', paste0('meanlead', seq_along(forward_period_labels)))

    for (i in seq_along(forward_period_labels)) {
      (plot_yearly_df %>%
         filter(hold_period ==  paste0('meanlead', i)) %>%
         ggplot(aes(y = mean_returns, x = year, group = interaction(quantile,hold_period), color = quantile)) +
         geom_hline(yintercept = 0) +
         geom_point() +
         geom_smooth(method = 'loess', se = FALSE) +
         facet_grid(.~hold_period, labeller=labeller(hold_period = labels)) +
         labs(y = "Mean sharpe ratios",
              x = "Year",
              title = chart_title)) %>%
        print()
    }
  }
}
