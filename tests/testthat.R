if(F){
  testthat::test_dir("tests/testthat/")
}

library(testthat)
library("ptree")

test_check("ptree")
