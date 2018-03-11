#' yuxiaCharts:  Chart generator for the relationshipMatrix
#'
#'It defines charts, tables and analysis for the various types of pairs of
#'variables.
#'
#'
#' @docType package
#' @name yuxiaCharts
NULL

.datatable.aware=TRUE

# nocov start
.onLoad	<-	function(libname,	pkgname)	{
  op	<-	options()
  op.yuxiaCharts	<-	list(
    my_option=42
  )
  toset	<-	!(names(op.yuxiaCharts)	%in%	names(op))
  if(any(toset))	options(op.yuxiaCharts[toset])
  invisible()
}
# nocov end

