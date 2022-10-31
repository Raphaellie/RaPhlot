#' @title Sequential Display of Coefficient Estimates
#'
#' @description Running one regresison model over multiple subsamples by time periods, get the resuls, and plot coefficients over time.
#'
#' @param data a data set object
#' @param y  the outcome variable of your interest
#' @param x the explaining variable of your interest, should be available across several time periods
#' @param time_id ID for each time point
#' @param years a sequence of periods you want to take into account; all years in your data by default.
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
#'   data = anes, dv = "pid7", x = 'ft_black', time_id = "year",
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
#' @import estimatr
#' @import fastDummies
#' @import viridisLite

## FUNCTION for coefficient trends

dycoef <- function(data,y,x,covar = NULL,time_id,
                   years = seq(min(data[[time_id]]),max(data[[time_id]]),1),
                   linetype = 6, size = 3){

  # check var availability across time ---------------------------------

  ## function for checking
  tcheck <- function(data,var,time_id){
    tcheck <- data %>%
      group_by(get(time_id)) %>%  # Use get(string) in dplyr to refer to the intended var
      summarize(dvm = mean(get(var), na.rm = TRUE)) %>%
      filter(is.na(dvm) == FALSE)
    return(tcheck[['get(time_id)']]) # Use data[[string]] to refer to the intended column?
  }

  ## check for x & y, then get the years that overlap
  # years <- seq(min(anes[[time_id]]),max(anes[[time_id]]),1)
  for (var in c(x,y,covar) ){

    var <- ifelse(startsWith(var,"as.factor("),
                  sub("\\).*", "", sub( ".*as.factor\\(",'',var)),
                  var)

    yearsvar <- tcheck(data = data, var = var, time_id = time_id)

    years <- intersect(years, yearsvar)
  }


  # electionys <- seq(1952,2020,4) # whether to limit sample to election years
  # years <- intersect(years, electionys)
  print("All Years where observations are available:")
  print(years)

  # set model and run regressions ---------------------------------------------------
  covar <- ifelse(is.null(covar), " ", paste("+", paste(covar, collapse = " + ")))
  model.str <- paste(y,'~',x,covar)
  model <- as.formula(model.str)
  print('Model for Each Wave:') ; print(model.str)

  stats <- data.frame()

  for (i in years) {
    data.reg <-data %>% filter(get(time_id) == i)
    reg <-
      lm_robust(model, data = data.reg) %>%
      tidy %>%
      mutate(time = i) %>%
      filter(term == x)
    stats <- rbind(stats,reg)
    print(paste(i, '-- Regression Done'))
  }


  # plot and return --------------------------------------------------------------------

  # plot.ribbon <- function(results) {
  #   plot <- ggplot(data = results, aes(x = time, y = estimate, ymax = conf.high, ymin = conf.low)) +
  #     # geom_vline(xintercept = 2000,linetype = 2, size = 0.6, alpha = 0.5) +  # reference for any year
  #     geom_hline(yintercept = 0,linetype = 2, size = 0.6, alpha = 0.5) +  # reference for effect
  #     geom_ribbon(linetype = 2, fill = 'deepskyblue4' , alpha = 0.15) + # shaded area for 95% CIs
  #     geom_line(color = 'navyblue', linetype = linetype, alpha = 0.85) + # line and points for coefficients
  #     geom_point(color = 'navyblue', size = size) +
  #     theme_bw() +
  #     xlab('Year of Survey') +
  #     ylab('Point Estimate with 95% CIs') +
  #     scale_x_continuous(breaks = years) +
  #     theme(plot.title = element_text(face = 'bold',size = 12),
  #           legend.position = 'none',
  #           panel.grid.minor.x = element_blank(),
  #           axis.text = element_text(color = 'black'))
  #
  #   return(plot)
  # }

  plot.range <- function(results) {
    plot <- ggplot(data = results, aes(x = time, y = estimate, ymax = conf.high, ymin = conf.low)) +
      # reference for effect
      geom_hline(yintercept = 0,linetype = 2, size = 0.6, alpha = 0.5) +
      # line range for each year
      geom_pointrange(aes(color = p.value < 0.05)) + # shaded area for 95% CIs
      scale_color_viridis_d(end = 0.7) +
      theme_minimal() +
      xlab('Year of Survey') +
      scale_x_continuous(breaks = years) +
      ylab('Point Estimate with 95% CIs') +
      theme(plot.title = element_text(face = 'bold',size = 12),
            legend.position = 'none',
            panel.grid.minor.x = element_blank(),
            axis.text = element_text(color = 'black'))
    return(plot)
  }

  return(list('plot' = plot.range(stats), 'results' = stats,'lat' = reg))
}
