#' @title Dynamic Display of Variable Means
#'
#' @description Showing the temporal trend of a variable.
#' @param data a data.frame object
#' @param var  the outcome variable of your interest
#' @param time_id ID for each time point
#' @param breaks a sequence of periods you want to take into account; all years in your data by default.
#'
#' @import ggplot2
#' @import dplyr
#' @import viridisLite


dyvar <- function(data,var,time_id,group = NA, breaks = seq(1952,2020,4) ){


results <-
  data %>%
  mutate(time = get(time_id),
         var2 = get(var),
         group = ifelse(is.na(group),'All',get(group))) %>%
  filter(time %in% breaks) %>%
  group_by(time, group) %>%
  summarise(estimate = mean(var2,na.rm = T),
            sd = sd(var2,na.rm = T),
            n = sum(!is.na(var2)),
            std.err = sd/sqrt(n),
            conf.high = estimate + qnorm(0.975)*std.err,
            conf.low  = estimate - qnorm(0.975)*std.err ) %>%
  filter(!is.na(estimate),!is.na(get(group)))

plot <-
  results %>%
  ggplot(aes(x = time, y = estimate, ymin = conf.low, ymax = conf.high, color = get(group))) +
  geom_pointrange(position = position_dodge(width = 2)) +
  scale_x_continuous(breaks = breaks) +
  scale_color_brewer(palette = 'Set1') +
  theme_minimal() +
  theme(legend.position = 'none')

return(list('plot' = plot, 'results' = results))

}
