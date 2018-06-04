## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.align = 'center', fig.width = 9.5, echo = TRUE, warning = FALSE, message = FALSE)

## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(xtable)
library(doBy)
library(scales)
library(gridExtra)
library(plotly)


## ------------------------------------------------------------------------
ATM=read.table("http://www.trutschnig.net/Datensatz.txt",head=TRUE)
ATM$ymd=as.Date(ATM$ymd)
names(ATM)[names(ATM)=="ymd"] = "date"
names(ATM)[names(ATM)=="sum_out"] = "cash"

## ------------------------------------------------------------------------
address = url("http://www.trutschnig.net/RTR2015.RData")
load(address)
RTR2015_sample = RTR2015[sample(nrow(RTR2015), 2500), ]


## ------------------------------------------------------------------------
p = ggplot(ATM, aes(date, cash)) +
  geom_line(aes(group = weekday),alpha = 0.3)
p

## ------------------------------------------------------------------------
subplot(
  p, ggplotly(p), 
  ggplot(ATM, aes(date, cash)) + geom_bin2d(),
  ggplot(ATM, aes(date, cash)) + geom_hex(),
  nrows = 2, shareX = TRUE, shareY = TRUE,
  titleY = T, titleX = T
)

## ------------------------------------------------------------------------
ATMgrouped = group_by(ATM, weekday)
p = plot_ly(ATMgrouped, x = ~date, y = ~cash, mode = 'markers',
            hoverinfo = 'text',
            text = ~paste('</br> weekday: ', weekday))
p


## ------------------------------------------------------------------------
ATMgrouped = group_by(ATM, weekday)
p = plot_ly(ATMgrouped, x = ~date, y = ~cash, mode = 'markers',
            hoverinfo = 'text',
            text = ~paste('</br> weekday: ', weekday,
                          '</br> cash: ', cash,
                          '</br> holiday: ', holiday))
p


## ------------------------------------------------------------------------
add_lines(
  add_lines(p, alpha = 0.2, name = "cash withdrawals"),
  name = "friday", data = filter(ATM, weekday == "Fri")
)

## ------------------------------------------------------------------------
p %>% 
  add_lines(name = ~"cash", alpha = 0.2)

## ------------------------------------------------------------------------
allWeekdays = ATM %>%
  group_by(weekday) %>%
  plot_ly(x = ~date, y = ~cash) %>%
  add_lines(alpha = 0.2, name = "cash withdrawals", hoverinfo = "none")
allWeekdays

## ------------------------------------------------------------------------
allWeekdays %>%
  filter(weekday == "Fri") %>%
  add_lines(name = "Fri") %>% 
  rangeslider()


## ------------------------------------------------------------------------
subplot(
  add_lines(p, color = ~weekday),
  add_lines(p, linetype = ~weekday),
  shareX = TRUE, nrows = 2
)

## ------------------------------------------------------------------------
allWeekdays %>%
  add_fun(function(plot) {
    plot %>% filter(weekday == "Fri") %>% add_lines(name = "Fri")
  }) %>%
  add_fun(function(plot) {
    plot %>% filter(weekday == "Mon") %>% 
      add_lines(name = "Mon")
  }) %>%
  rangeslider()

## ------------------------------------------------------------------------
rangeslider(allWeekdays)


## ------------------------------------------------------------------------
p = ggplot(RTR2015_sample, aes(x = rtr_speed_ul, y = rtr_speed_dl)) +
  geom_point(alpha = 0.05, color = "magenta") + 
  geom_smooth(color = "blue") +
  geom_smooth(method = "lm", se = F, color = "black")

ggplotly(p, hoverinfo = "none")


## ------------------------------------------------------------------------
p %>%
  ggplotly(layerData = 2, originalData = FALSE, hoverinfo = "none") %>%
  plotly_data()

## ------------------------------------------------------------------------
p %>%
  ggplotly(layerData = 2, originalData = F) %>%
  add_fun(function(p) {
    p %>% slice(which.max(se)) %>%
      add_segments(x = ~x, xend = ~x, y = ~ymin, yend = ~ymax) %>%
      add_annotations("Maximum uncertainty", ax = 60)
  }) %>%
  add_fun(function(p) {
    p %>% slice(which.min(se)) %>%
      add_segments(x = ~x, xend = ~x, y = ~ymin, yend = ~ymax) %>%
      add_annotations("Minimum uncertainty")
  })

## ------------------------------------------------------------------------
subplot(
  plot_ly(RTR2015_sample, x = ~rtr_speed_dl, y = ~rtr_speed_ul, name = "default"),
  plot_ly(RTR2015_sample, x = ~rtr_speed_dl, y = ~rtr_speed_ul) %>% 
    add_markers(alpha = 0.2, name = "alpha = 0.2"),
  plot_ly(RTR2015_sample, x = ~rtr_speed_dl, y = ~rtr_speed_ul) %>% 
    add_markers(alpha = 0.02, name = "alpha = 0.02")
)

## ------------------------------------------------------------------------
plot_ly(RTR2015_sample, x = ~rtr_speed_dl, y = ~rtr_speed_ul, z = ~rtr_ping)


## ------------------------------------------------------------------------
plot_ly(RTR2015_sample, x = ~rtr_speed_dl, y = ~rtr_speed_ul, z = ~rtr_ping) %>% 
  add_markers(alpha = 0.2, color = ~op_name)

## ------------------------------------------------------------------------
plot_ly(RTR2015_sample, x = ~rtr_speed_dl, y = ~rtr_speed_ul) %>%
  add_markers(alpha = 0.2, color = ~op_name)

## ------------------------------------------------------------------------
x = RTR2015_sample$rtr_speed_dl
y = RTR2015_sample$rtr_speed_ul

s = subplot(
  plot_ly(RTR2015_sample, x = x, color = I("green")) %>% add_trace(x = x, name = 'download speed'), 
  plotly_empty(), 
  plot_ly(RTR2015_sample, x = x, y = y, color = I("blue")) %>%  add_markers(alpha = 0.2, color = ~op_name), 
  plot_ly(y = y, color = I("blue")) %>% add_trace(y = y, name = 'upload speed'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
)
layout(s, showlegend = TRUE)

## ------------------------------------------------------------------------
RTR2015_4G = RTR2015[ which(RTR2015$nw_cat=='4G'), ]
RTR2015_4G_sample = RTR2015_4G[sample(nrow(RTR2015_4G), 2500), ]



x = RTR2015_4G_sample$rtr_speed_dl
y = RTR2015_4G_sample$rtr_speed_ul

s = subplot(
  plot_ly(RTR2015_4G_sample, x = x, color = I("green")) %>% add_trace(x = x, name = 'download speed'), 
  plotly_empty(), 
  plot_ly(RTR2015_4G_sample, x = x, y = y, color = I("blue")) %>%  add_markers(alpha = 0.2, color = ~op_name), 
  plot_ly(y = y, color = I("blue")) %>% add_trace(y = y, name = 'upload speed'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
)
layout(s, showlegend = TRUE)

## ------------------------------------------------------------------------
library(ggmosaic)

p = ggplot(data = RTR2015_sample) + 
  geom_mosaic(aes(x = product(op_name, device_has_lte), fill = factor(nw_cat)),
  divider = ddecker(), offset = 0.05) + 
  labs(x = "operator and LTE", y = "proportion", title = 'Mosaicplot') + 
  guides(fill = guide_legend(title = "technology"))
ggplotly(p)

