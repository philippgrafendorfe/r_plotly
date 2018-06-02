## ----message=FALSE, warning=FALSE----------------------------------------
library(dplyr)
suppressPackageStartupMessages({library(plotly)})
library(zoo)
library(htmlwidgets)
library(knitr)
opts_chunk$set(fig.width=9.5)

## ------------------------------------------------------------------------
raw_bank <- read.table("http://www.trutschnig.net/Datensatz.txt", header =TRUE)

## ----fig.width=9.5-------------------------------------------------------
raw <- raw_bank %>% group_by(weekday)
p <- plot_ly(raw, x = ~ymd, y = ~sum_out)

add_lines(
  add_lines(p # put in plotly object
            ,alpha = 0.2
            ,name = "other days"
            ,hoverinfo = "none"
            )
  ,name = "Monday"
  ,data = filter(raw_bank, weekday == "Mon") # put in dataframe
  )

## ------------------------------------------------------------------------
na_index <- !complete.cases(raw_bank$sum_out)
raw_bank[na_index,]

## ------------------------------------------------------------------------
A <- raw_bank %>% 
  mutate(ymd = as.Date(ymd, "%Y-%m-%d")) %>% 
  mutate(year_month=format(as.Date(ymd), "%Y-%m"))

## ------------------------------------------------------------------------
for (day in unique(A$weekday)) {
  
  actual_day <- A %>% 
    filter(weekday==day) %>% 
    filter(year_month %in% c("2007-09", "2007-10", "2007-11"))

  actual_day <- zoo(actual_day$sum_out, actual_day$ymd)
  actual_day <- na.spline(actual_day)

  index <- A$ymd %in% index(actual_day)
  A$sum_out[index] <- coredata(actual_day)
}

## ------------------------------------------------------------------------
A[na_index,]

## ------------------------------------------------------------------------
B <- A %>% group_by(year_month, weekday) %>% summarise(average_sum_out = mean(sum_out), sd = sd(sum_out))
B

## ------------------------------------------------------------------------
B1 <- B %>% group_by(weekday)
p1 <- plot_ly(B1, x = ~year_month, y = ~average_sum_out)
add_lines(
  add_lines(p1
            ,alpha = 0.2
            ,name = "other days"
            ,hoverinfo = "none"
            )
  ,name = "Monday"
  ,data = filter(B1, weekday == "Mon")
  )

## ------------------------------------------------------------------------
axis_style <- list(
  autotick = FALSE
  ,dtick = 3
  ,title = FALSE
  ,tickangle = 45
  ,tickcolor = toRGB("blue")
)

allWeekdays <- B %>%
  group_by(weekday) %>% 
  plot_ly(x = ~year_month, y = ~average_sum_out) %>% 
  add_lines(alpha=0.2
            ,name="all Days"
            ,hoverinfo="none"
            ) 

allWeekdays %>% 
  filter(weekday == "Mon") %>% 
  add_lines(name = "Monday") %>% 
  layout(xaxis = axis_style
         , title = "Average monthly money withdrawals"
         , margin = list(b = 80))

## ------------------------------------------------------------------------
allWeekdays %>%
  add_fun(function(plot) {
    plot %>% filter(weekday == "Mon") %>% 
      add_lines(name = "Monday")
  }) %>%
  add_fun(function(plot) {
    plot %>% filter(weekday == "Fri") %>% 
      add_lines(name = "Friday", error_y = list(value = ~sd))
  }) %>%
  add_fun(function(plot) {
    plot %>% filter(weekday == "Sun") %>% 
      add_lines(name = "Sunday", error_y = list(value = ~sd))
  }) %>%
  add_lines(data = B %>% 
              group_by(year_month) %>% 
              summarise(average_sum_out=sum(average_sum_out))
            , name = "Total Average") %>% 
  layout(xaxis = axis_style
         , title = "Average monthly money withdrawals"
         , margin = list(b = 80))

## ------------------------------------------------------------------------
layer_day <- function(plot, day) {
  plot %>% filter(weekday == day) %>% add_lines(name = day)
}

# unfortunately this does not work properly. I have not been able to spot the bug yet.
# layer_iqr <- function(plot) {
#   plot %>%
#     group_by(year_month) %>% 
#     summarise(
#       q1 = quantile(average_sum_out, 0.25),
#       med = median(average_sum_out),
#       q3 = quantile(average_sum_out, 0.75)
#       ) %>%
#     add_lines(y = ~med, name = "median", color = I("black")) %>%
#     add_ribbons(ymin = ~q1, ymax = ~q3, name = "IQR", color = I("black"))
# }

layer_layout <- function(plot) {
  plot %>% 
    layout(xaxis = axis_style
           , title = "Average monthly money withdrawals"
           , margin = list(b = 80)
    )
  }

## ------------------------------------------------------------------------
allWeekdays %>%
  add_fun(layer_day, "Mon") %>%
  add_fun(layer_day, "Fri") %>% 
  add_fun(layer_day, "Sun") %>%
  add_fun(layer_layout)

## ------------------------------------------------------------------------
p <- allWeekdays %>% 
  group_by(year_month) %>% 
  summarise(q1=quantile(average_sum_out, 0.25), m=median(average_sum_out), q3=quantile(average_sum_out, 0.75)) %>% 
  add_lines(y = ~q1, name = "Q1", color = I("black")) %>% 
  add_lines(y = ~m, name = "median", color = I("black")) %>% 
  add_lines(y = ~q3, name = "Q3", color = I("black")) %>% 
  add_fun(layer_layout)
p

## ------------------------------------------------------------------------
# htmlwidgets::saveWidget(p, file = "time_series_median_cashout.html")
# purl('plotly_time_series.Rmd')
# rmarkdown::render(input = "plotly_time_series.Rmd", output_format = "html_document", output_file = "test.html")

## ------------------------------------------------------------------------
new <- A %>% mutate(year = as.factor(substring(ymd, 1, 4))) %>% select(-year_month)
new$holiday <- as.factor(new$holiday)
new

## ------------------------------------------------------------------------
new1 <- new %>% 
  group_by(year, holiday) %>% 
  summarise(average_sum_out = mean(sum_out), sd = sd(sum_out))
new1

## ------------------------------------------------------------------------
layer_day <- function(plot, day) {
  plot %>% filter(weekday == day) %>% add_lines(name = day)
}

## ------------------------------------------------------------------------
layer_holiday <- function(plot, day) {
    plot %>% filter(holiday == day) %>% add_lines(name = day, error_y = list(value = ~sd))
    }

## ------------------------------------------------------------------------
allWeekdays <- B %>%
  group_by(weekday) %>% 
  plot_ly(x = ~year_month, y = ~average_sum_out) %>% 
  add_lines(alpha=0.2
            ,name="all Days"
            ,hoverinfo="none"
            ) 

## ------------------------------------------------------------------------
allHoliday <- new1 %>%
  group_by(holiday) %>% 
  plot_ly(x = ~year, y = ~average_sum_out) %>% 
  add_lines(alpha=0.2
            ,name="all holidays"
            ,hoverinfo="none"
            )

## ------------------------------------------------------------------------
allHoliday %>% 
  add_fun(layer_holiday , 0) %>% 
  add_fun(layer_holiday , 0.5) %>% 
  add_fun(layer_holiday , 1) %>% 
  add_fun(layer_holiday , 1.5)

