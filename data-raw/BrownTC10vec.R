## code to prepare `BrownTC10vec` dataset goes here

BrownTC10vec <- df2v(BrownTC10monyr, col1=2, col2=ncols(BrownTC10monyr))

usethis::use_data(BrownTC10vec, overwrite = TRUE)
