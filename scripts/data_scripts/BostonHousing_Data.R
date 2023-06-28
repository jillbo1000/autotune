library(mlbench) 

data(BostonHousing2)
bh <- dplyr::mutate(BostonHousing2, lcrim = log(crim)) |>
    dplyr::select(-town, -medv, -crim)
bh <- bh[, c(4, 1:3, 5:17)]


