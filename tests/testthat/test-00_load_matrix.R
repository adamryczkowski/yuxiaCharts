library(relationshipMatrix)
library(yuxiaCharts)
source('tests/aggregates.R')
source('tests/load_dt.R')

library(testthat)
options(warn=2)
context("Reading in sample matrix")

test_that("Reading the relationship matrix with aggregates", {
  dt<-loaddt()
  dt_structure<-danesurowe::create_df_from_df_structure(dt, flag_include_vartype = TRUE)
  aggrt<-allAggregates()
  m<-read_matrix('tests/testthat/macierze_analiz.xlsx', dt_structure, aggregate_types = aggrt)
  expect_length(m, 37)
  expect_equal(nrow(m), 692)
  mref<-readRDS('tests/testthat/test-00_reference.rds')
  expect_equal(m, mref)
#  saveRDS(m, 'tests/testthat/test-00_reference.rds')
})

