# ------------------------------------------------------------------------------
# for quantile regression tests

library(modeldata)

data("Sacramento")

cols <- c("price", "beds", "baths", "sqft", "latitude", "longitude")
Sacramento_small <- modeldata::Sacramento
Sacramento_small$price <- log10(Sacramento_small$price)
Sacramento_small <- Sacramento_small[, cols]

sac_train <- Sacramento_small[-(1:5), ]
sac_test  <- Sacramento_small[  1:5 , ]
