## ----message=FALSE, warning=FALSE----------------------------------------
library(dplyr)
library(plotly)
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
raw_bank[!complete.cases(raw_bank$sum_out),]

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
B <- A %>% dplyr::group_by(year_month, weekday) %>% summarise(average_sum_out = mean(sum_out), sd = sd(sum_out))
B

## ------------------------------------------------------------------------
B1 <- group_by(B, weekday)
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
layer_iqr <- function(plot) {
  plot %>%
    group_by(year_month) %>% 
    summarise(
      q1 = quantile(average_sum_out, 0.25),
      med = median(average_sum_out),
      q3 = quantile(average_sum_out, 0.75)
      ) %>%
    add_lines(y = ~med, name = "median", color = I("black")) %>%
    add_ribbons(ymin = ~q1, ymax = ~q3, name = "IQR", color = I("black"))
}

layer_layout <- function(plot) {
  plot %>% 
    layout(xaxis = axis_style
           , title = "Average monthly money withdrawals"
           , margin = list(b = 80)
    )
  }

## ----fig.width=9---------------------------------------------------------
allWeekdays %>%
  add_fun(layer_day, "Mon") %>%
  add_fun(layer_day, "Fri") %>% 
  add_fun(layer_day, "Sun") %>% 
  add_fun(layer_layout)

## ----fig.width=9---------------------------------------------------------
allWeekdays %>% 
  group_by(year_month) %>% 
  summarise(q1=quantile(average_sum_out, 0.25), m=median(average_sum_out), q3=quantile(average_sum_out, 0.75)) %>% 
  add_lines(y = ~q1, name = "Q1", color = I("black")) %>% 
  add_lines(y = ~m, name = "median", color = I("black")) %>% 
  add_lines(y = ~q3, name = "Q3", color = I("black")) %>% 
  add_fun(layer_layout) 

## ------------------------------------------------------------------------
htmlwidgets::saveWidget(p, file = "time_series_median_cashout.html")

