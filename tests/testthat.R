library(testthat)
library(fin.backend)

test_check("fin.backend")

detach("package:fin.backend", unload = TRUE)
