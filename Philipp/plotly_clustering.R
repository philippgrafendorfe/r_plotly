## ------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(MeanShift)
library(plotly)

## ------------------------------------------------------------------------
address <- url("http://www.trutschnig.net/RTR2015.RData")
load(address)
test_df <- RTR2015[sample(nrow(RTR2015), 500), ]

## ------------------------------------------------------------------------
test_df %>% ggplot(mapping = aes(x = longitude, y = latitude)) + 
  geom_point()

## ------------------------------------------------------------------------
spatial_df_t <- t(test_df[,c("longitude", "latitude", "rtr_speed_dl")])

## ------------------------------------------------------------------------
spatial_df_t <- spatial_df_t / apply( spatial_df_t, 1, sd )

h.cand <- quantile( dist( t( spatial_df_t ) ), seq( 0.05, 0.40, by=0.05 ) )

## ------------------------------------------------------------------------
system.time( bms.clustering <- lapply( h.cand,
function( h ){ bmsClustering( spatial_df_t, h=h ) } ) )

## ------------------------------------------------------------------------
plot( spatial_df_t[1,], spatial_df_t[2,], col=bms.clustering[[5]]$labels, 
xlab="longitude", ylab="latitude", main="Mean shift labels",
cex=spatial_df_t[3,], pch=16 )
points( bms.clustering[[5]]$components[1,], bms.clustering[[5]]$components[2,], col=1:ncol( bms.clustering[[5]]$components ),
pch="+", cex=3 )

## ------------------------------------------------------------------------
p <- plot_ly(spatial_df_t %>% 
               t() %>% 
               as.data.frame()
             , x = ~longitude
             , y = ~latitude
             , z = ~rtr_speed_dl) %>%
  add_markers(color = bms.clustering[[5]]$labels)
p

