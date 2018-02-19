library(relationshipMatrix)
library(yuxiaCharts)
source('tests/aggregates.R')
source('tests/load_dt.R')

library(testthat)
options(warn=2)
context("Dispatching a sample matrix")

test_that("Dispatching a sample matrix", {
  dt<-loaddt()
  dt_structure<-danesurowe::create_df_from_df_structure(dt, flag_include_vartype = TRUE)
  aggrt<-allAggregates()
  m<-read_matrix('tests/testthat/macierze_analiz.xlsx', dt_structure, aggregate_types = aggrt)

  cl<-classify_analyses(m)
  expect_length(cl$tododf, 40)
  expect_equal(nrow(cl$tododf), 692)
  clref<-readRDS('tests/testthat/test-01_reference.rds')
  expect_equal(cl$tododf, clref$tododf)
  #saveRDS(cl, 'tests/testthat/test-01_reference.rds')
})

