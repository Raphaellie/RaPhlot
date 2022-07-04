#' @title dycoef
#'
#' @description Running one regresison model over multiple subsamples by time periods, get the resuls, and plot coefficients over time.
#' @param data a data set object
#' @param dv  the outcome variable of your interest
#' @param x the explaining variable of your interest, should be available across several time periods
#' @param tid ID for each time point
#' @param years a sequence of perids you want to take into account; all years in your data by default.
#' @param covar control variables to be included
#'
#' @return
#' This functions return the following three objects:
#' A. "lastreg": regression results of you model in the subsample of last time period
#' B. "results": A data frame reporting coefficients and 95% CIs for all regressions.
#' C. "plot": A dynamic plot of x coefficients across periods using ggplot2().
#' @examples
#'
#' # load in your data
#' anes <- read.dta13("data/ANES cleaned.dta")
#'
#' # use the function
#' anes.dynamic <- dycoef(
#'   data = anes, dv = "pid7", x = 'ft_black', tid = "year",
#'   years = seq(1960,2020,4),
#'   covar = ('ft_white','as.factor(race)')
#' )
#'
#' # see the organized table for coefficients and 95% CIs
#' anes.dynamic$results
#'
#' # see the dynamic plot of DiD results
#' anes.dynamic$plot
#'
#' @export
#' @import tidyverse
#' @import lfe
#' @import fastDummies


## FUNCTION for coefficient trends
dycoef <- function(data,dv = 'pid7',x = 'ft_black',covar = NULL,tid,
                   years = seq(min(data[[tid]]),max(data[[tid]]),1),
                   linetype = 6, size = 3){

  # check var availability across time ---------------------------------

  ## function for checking
  tcheck <- function(data,var,tid){
    tcheck <- data %>%
      group_by(get(tid)) %>%
      summarize(dvm = mean(get(var), na.rm = TRUE)) %>%  # Use get(string) in dplyr to refer to the intended var
      filter(is.na(dvm) == FALSE)
    return(tcheck[['get(tid)']]) # Use data[[string]] to refer to the intended column?
  }

  ## check for x & y, then get the years that overlap
  # years <- seq(min(anes[[tid]]),max(anes[[tid]]),1)
  for (var in c(x,dv,covar) ){
    var <- ifelse(startsWith(var,"as.factor("),
                  sub("\\).*", "", sub( ".*as.factor\\(",'',var)),
                  var)

    yearsvar <- tcheck(data = data, var = var, tid = tid)

    years <- intersect(years, yearsvar)
  }


  # electionys <- seq(1952,2020,4) # whether to limit sample to election years
  # years <- intersect(years, electionys)
  print("Years where observations are available:")
  print(years)

  # set model and run regressions ---------------------------------------------------
  covar <- ifelse(is.null(covar), " ", paste("+", paste(covar, collapse = " + ")))
  model <- as.formula( paste(dv,'~',x,covar,'|0|0|0') )
  print(model)

  ## set a extraction function
  get.stats <- function(position, var, reg, time = NA) {
    stats <- data.frame(var = var,
                        coef = reg$coefficients[position],
                        se = reg$se[position], time = time)
    stats <- stats %>% mutate(ciup = coef + se * qnorm(.975), cilow = coef - se * qnorm(.975)) # get 95% CIs
    return(stats)
  }

  stats <- data.frame()

  for (i in years) {
    reg <- felm(model, data = data[data$year == i,])
    stats <- rbind(stats,get.stats(2,x,reg,i))
    # print(paste(i, '-- Regression Done'))
  }


  # plot and return --------------------------------------------------------------------

  dycoef.ribbon <- function(results) {
    plot <- ggplot(data = results, aes(x = time, y = coef, ymax = ciup, ymin = cilow)) + # geom_vline(xintercept = 2000,linetype = 2, size = 0.6, alpha = 0.5) +  # reference for any year
      geom_hline(yintercept = 0,linetype = 2, size = 0.6, alpha = 0.5) +  # reference for effect
      geom_ribbon(linetype = 2, fill = 'deepskyblue4' , alpha = 0.15) + # shaded area for 95% CIs
      geom_line(color = 'navyblue', linetype = linetype, alpha = 0.85) + # line and points for coefficients
      geom_point(color = 'navyblue', size = size) +
      theme_bw() +
      xlab('Year of Survey') +
      ylab('Point Estimate with 95% CIs') +
      scale_x_continuous(breaks = years) +
      theme(plot.title = element_text(face = 'bold',size = 12),
            legend.position = 'none',
            panel.grid.minor.x = element_blank(),
            axis.text = element_text(color = 'black'))

    return(plot)
  }

  # dycoef.lrange <- function(results) {
  #   plot <- ggplot(data = results, aes(x = time, y = coef, ymax = ciup, ymin = cilow)) + # geom_vline(xintercept = 2000,linetype = 2, size = 0.6, alpha = 0.5) +  # reference for any year
  #     geom_hline(yintercept = 0,linetype = 2, size = 0.6, alpha = 0.5) +  # reference for effect
  #     geom_linerange(color = 'slategray4', alpha = 0.50, size = 1.6) + # shaded area for 95% CIs
  #     geom_point(color = 'black', alpha = 0.9 ,size = 3)+ # aes(size = abs(coef)
  #     theme_bw() +
  #     xlab('Year of Survey') +
  #     ylab('Estimated Effect') + # scale_x_continuous(breaks = span[1]:span[2]) +
  #     theme(plot.title = element_text(face = 'bold',size = 12),
  #           legend.position = 'none')
  #   return(plot)
  # }

  return(list('plot' = dycoef.ribbon(stats), 'results' = stats,'lastreg' = summary(reg)))
}
