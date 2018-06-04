## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE
)
library(plotly)
library(dplyr)

data(bank)
bank <-read.table("http://www.trutschnig.net/Datensatz.txt",head=TRUE)

data(RTR2015)
address <- url("http://www.trutschnig.net/RTR2015.RData")
load(address)
head(RTR2015)


## ------------------------------------------------------------------------
Galaxy = filter(RTR2015, device == "Galaxy Note 10.1") 

plot_ly(Galaxy, x = ~rtr_speed_ul, y = ~rtr_speed_dl, name = "default")

## ------------------------------------------------------------------------
plot_ly(Galaxy, x = ~op_name, y = ~rtr_speed_dl, name = "default")

## ------------------------------------------------------------------------
plot_ly(Galaxy, x = ~ rtr_speed_ul, y = ~rtr_speed_dl)%>% 
  add_markers(alpha = 0.9, name = "alpha")

## ------------------------------------------------------------------------
plot_ly(Galaxy, x = ~ rtr_speed_ul, y = ~rtr_speed_dl)%>% 
  add_markers(symbol = I(2), name = "hollow")

## ------------------------------------------------------------------------

subplot(
  plot_ly(Galaxy, x = ~rtr_speed_ul, y = ~rtr_speed_dl, name = "default"),
  plot_ly(Galaxy, x = ~rtr_speed_ul, y = ~rtr_speed_dl) %>% 
    add_markers(alpha = 0.2, name = "alpha"),
  plot_ly(Galaxy, x = ~rtr_speed_ul, y = ~rtr_speed_dl) %>% 
    add_markers(symbol = I(1), name = "hollow")
)

## ------------------------------------------------------------------------

subplot(
  plot_ly(Galaxy, x = ~rtr_speed_ul, y = ~rtr_speed_dl, name = "default"),
  plot_ly(Galaxy, x = ~rtr_speed_ul, y = ~rtr_speed_dl) %>% 
    add_markers(alpha = 0.5, name = "alpha"),
  plot_ly(Galaxy, x = ~rtr_speed_ul, y = ~rtr_speed_dl) %>% 
    add_markers(symbol = I(3), name = "hollow")
)

## ------------------------------------------------------------------------
plot_ly(data = Galaxy, x = ~rtr_speed_ul, y = ~rtr_speed_dl, color = ~op_name)

## ------------------------------------------------------------------------
plot_ly(data = Galaxy, x = ~rtr_speed_ul, y = ~rtr_speed_dl, color = ~op_name, colors = c("red","green","blue"))

## ------------------------------------------------------------------------
plot_ly(Galaxy, x = ~longitude, y = ~rtr_speed_dl) %>%
  add_lines(color = ~op_name)

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~rtr_speed_dl, color = ~device_platform) %>% add_histogram(name = "plotly.js")

## ------------------------------------------------------------------------
p1 <- plot_ly(RTR2015, x = ~rtr_speed_dl) %>% add_histogram(name = "plotly.js")
speed_hist <- function(method = "FD") {
  h <- hist(RTR2015$rtr_speed_dl, breaks = method, plot = FALSE)
  plot_ly(x = h$mids, y = h$counts) %>% add_bars(name = method)
}

## ------------------------------------------------------------------------
subplot(
  p1, speed_hist(), speed_hist("Sturges"),  speed_hist("Scott"),
  nrows = 4, shareX = TRUE
)

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~op_name) %>% add_histogram()

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~op_name, color = ~device_platform) %>% add_histogram()

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~op_name, y =~rtr_speed_dl, color = ~device_platform) %>% add_bars()

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~device_has_lte, y= ~device, color = ~op_name) %>%
  add_bars() %>%
  layout(barmode = "stack")

## ------------------------------------------------------------------------
plot_ly(Galaxy, y = ~rtr_speed_ul, type = "box", name ="Upload") %>%
  add_trace(y = ~rtr_speed_dl, name = "Download")

## ------------------------------------------------------------------------
plot_ly(Galaxy, x = ~rtr_speed_ul, type = "box", name ="Upload") %>%
  add_trace(x = ~rtr_speed_dl, name = "Download")

## ------------------------------------------------------------------------
p <- plot_ly(Galaxy, y = ~rtr_speed_dl, color = I("blue"), 
             alpha = 0.1, boxpoints = "suspectedoutliers")
p1 <- p %>% add_boxplot(x = "Overall")
p2 <- p %>% add_boxplot(x = ~op_name)
subplot(
  p1, p2, shareY = TRUE,
  widths = c(0.2, 0.8), margin = 0
) %>% hide_legend()

## ------------------------------------------------------------------------
d = filter(RTR2015, nw_cat %in% c("4G", "3G", "2G"))
plot_ly(d, x = ~rtr_speed_dl, y = ~interaction(nw_cat, device_has_lte)) %>%
  add_boxplot(color = ~nw_cat) %>%
  layout(yaxis = list(title = ""), margin = list(l = 100))

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~op_name, y = ~nw_cat, z = ~rtr_speed_dl, type = "heatmap")

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~mymd, y = ~device_platform, z = ~rtr_speed_dl, type = "heatmap")

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~mymd, y = ~op_name, z = ~rtr_speed_dl, type = "heatmap")

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~mymd, y = ~op_name, z = ~rtr_speed_dl, type = "heatmap", colorscale="Greys")

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~mymd, y = ~op_name, z = ~rtr_speed_dl,colors = colorRamp(c("red", "green")), type = "heatmap")

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~op_name, y = ~nw_cat, z = ~rtr_speed_dl, colors = colorRamp(c("red", "yellow")), type = "heatmap")

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~op_name, y = ~nw_cat) %>%
 add_histogram2d()

## ------------------------------------------------------------------------
plot_ly(RTR2015, x = ~op_name, y = ~rtr_speed_dl) %>%
  add_histogram2d()

