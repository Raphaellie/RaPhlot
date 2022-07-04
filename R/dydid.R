#' @title dydid
#'
#' @description Set specifications for dynamic Difference-in-Differences analysis, report coefficients from regressions, and draw coefficients in a dynamic way.
#'
#' @param data a data set object
#' @param dv  the outcome variable of your interest
#' @param tpoint variable in your data that indicates the time when the treatment takes place
#' @param eid ID for each individual entity
#' @param tid ID for each time point
#' @param step the unit by which the time ID increments, e.g., 2 for every two years
#' @param span the number of periods before and after the treatment you want to take into consideration.
#' @param covar controls variables to be included
#'
#' @return
#' This functions return the following three objects:
#' A. "regs": Original results of DiD regressions using felm().
#' B. "results": A data frame reporting coefficients and 95% CIs for treatment-time indicators.
#' C. "plot": A dynamic plot of DiD coefficients using ggplot2().
#' @examples
#'
#' # load in your data
#' df <- read.dta13("data/big bad banks.dta")
#'
#' # use the function
#' bbb.did <- dydid(
#'   data = df, dv = "y", tpoint = "reform_timepoint", step = 1,
#'   eid = "state_id", tid = "year_id",
#'   span = c(-7, 7), covar = NULL
#' )
#'
#' # see the origigal regression results
#' bbb.did$regs
#'
#' # see the organized table for coefficients and 95% CIs
#' bbb.did$results
#'
#' # see the dynamic plot of DiD results
#' bbb.did$plot
#'
#' @export
#' @import tidyverse
#' @import lfe
#' @import fastDummies

# Prepare -----------------------------------------------------------------


# dydid: Function for Dynamic DiD Regression and Plot-------------------------------------------------------------


dydid <- function(data = df, dv = "y",
                  tpoint = "reform_timepoint", step = 1,
                  eid = "state_id", tid = "year_id", span = c(-5, 5), covar = NULL,
                  linetype = 5,size = 2.7) {

  # Set Treatment-Post -------------------------------------------
  df <- data

  df$relative <- df[, tid] - df[, tpoint]
  df <- df %>%
    mutate(post = relative / step) %>%
    mutate(pre = -relative / step)

  varnum <- span[2] - span[1]

  df$post <- ifelse(df$post >= span[2], span[2], df$post)
  df$post <- ifelse(df$post < 0, -1, df$post)
  df$pre <- ifelse(df$pre >= -span[1], -span[1], df$pre)
  df$pre <- ifelse(df$pre < 0, -1, df$pre)


  df <- fastDummies::dummy_cols(df, select_columns = "post")
  df <- fastDummies::dummy_cols(df, select_columns = "pre")

  pre_list <- paste0("pre_", -span[1]:2)
  post_list <- paste0("post_", 0:span[2])
  d_list <- c(pre_list, post_list)

  ## For observations without treatment, set all Dit indicators as 0
  for (i in d_list) {
    df[is.na(df[, tpoint]) == 1, i] <- 0
  }

  results <- rbind(
    data.frame(var = pre_list, time = span[1]:-2, position = 1:(0 - span[1] - 1)),
    data.frame(var = post_list, time = 0:span[2], position = -span[1]:varnum)
  )


  # DiD Regression ----------------------------------------------------------

  covar <- ifelse(is.null(covar), " ", paste("+", paste(covar, collapse = " + "))) # set formula part for covars

  model <- as.formula(
    paste(
      dv, "~", paste(pre_list, collapse = "+"), # remember to EXCLUDE pre_1
      "+", paste(post_list, collapse = "+"), # remember to INCLUDE post_0
      covar,
      "|", eid, "+", tid,
      "| 0 |", eid
    )
  ) # FE & Cluster SE

  rm(post_list, pre_list)

  did <- felm(model, data = df)


  # Extract Stats -----------------------------------------------------------
  ## set a extraction function

  get.stats <- function(position, var) {
    stats <- data.frame(var = var, coef = did$coefficients[position], se = did$cse[position])
    stats <- stats %>% mutate(ciup = coef + se * qnorm(.975), cilow = coef - se * qnorm(.975)) # get 95% CIs
    return(stats)
  }

  ## use loop to get stats for all indicators
  stats <- data.frame()
  for (i in 1:varnum) {
    stat <- get.stats(i, results[results$position == i, "var"])
    stats <- rbind(stats, stat)
  }

  ## combine variable lists and stats
  results <- left_join(results, stats, by = "var")
  rm(stats)

  # DiD Ploting Function-------------------------------------------------------------

  ## Recommended: CI as shade area
  did.plot <- function(results) {
    did_plot <- ggplot(data = results, aes(x = time, y = coef, ymax = ciup, ymin = cilow)) +
      geom_hline(yintercept = 0, linetype = 2, size = 0.6, alpha = 0.5) + # reference for effect
      geom_vline(xintercept = -1, linetype = 2, size = 0.6, alpha = 0.5) + # reference for post-treatment
      geom_ribbon(fill = "deepskyblue3", alpha = 0.15) + # shaded area for 95% CIs
      geom_line(color = "navyblue", linetype = linetype, alpha = 0.75) + # line and points for coefficients
      geom_point( color = "navyblue",size = size) +
      theme_bw() +
      xlab("Periods relative to Treatment") +
      ylab("Estimate of Treatment Effect") +
      scale_x_continuous(breaks = span[1]:span[2]) +
      theme(
        panel.grid.minor.x = element_blank(),
        legend.position = "none"
      )
    return(did_plot)
  }

  # Plot and Return Results
  return(list("plot" = did.plot(results), "results" = results, "regs" = did))
}
