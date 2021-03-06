library(relationshipMatrix)
library(yuxiaCharts)
source('tests/aggregates.R')
source('tests/load_dt.R')

library(testthat)
options(warn=1)
context("Calculating a cell")

test_that("Dispatching a sample matrix", {
  dt<-loaddt()
  dt_structure<-danesurowe::create_df_from_df_structure(dt, flag_include_vartype = TRUE)
  aggrt<-allAggregates()
  debugonce(read_matrix)
  m<-read_matrix('tests/testthat/macierze_analiz.xlsx', dt_structure, aggregate_types = aggrt)
  #debugonce(classify_analyses)
  #debugonce(recognize_sheet_format)
  cl<-classify_analyses(m)

  #debugonce(relationshipMatrix::render_matrix)
#1:53
  doc<-relationshipMatrix::render_matrix(cellsdf=cl$tododf[1:53,], author="Adam", title="analiza",
                                    stats_dispatchers=cl$dispatchers,
                                    report_dispatchers=list(),
                                    report_functions=list(),
                                    aggregates=aggrt, filters=get_filters(), df_task=dt)
  #  debugonce(relationshipMatrix:::do_cell)

  pandoc<-pander::Pandoc$new()
  #  debugonce(doc$render)
  doc$pre_render()
  doc$render(pandoc)
  unlink('/tmp/cos.docx')
  unlink('/tmp/cos.md')
  save_report(pandoc, filename = '/tmp/cos')


  relationshipMatrix:::do_cell(cl$tododf[13,], stats_dispatchers=cl$dispatchers,
                               report_dispatchers=list(),
                               report_functions=list(),
                               aggregates=aggrt, filters=get_filters(), cellnr=1L, df_task=dt)

  relationshipMatrix:::do_cell(cl$tododf, dispatchers=cl$dispatchers, filters=get_filters(), cellnr=1)

  expect_length(cl$tododf, 40)
  expect_equal(nrow(cl$tododf), 692)
  clref<-readRDS('tests/testthat/test-01_reference.rds')
  expect_equal(cl$tododf, clref$tododf)
  #saveRDS(cl, 'tests/testthat/test-01_reference.rds')
})

