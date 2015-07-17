#/////////////////////////////////////////////////////////////////////////
#
#   Organization  : KEMRI Wellcome Trust
#   Department    : Health Systems Research Group
#   Project       : DQA Audit Reports
#   Task          : Reporting utility helpers
#   Author        : Boniface Makone [bmakone@kemri-wellcome.org]
#   Details       :
#         This script contains functions that aid in proper formatting
#         of the report and and facilitating interactions with
#         external packages that aid in producing the report
#
#///////////////////////////////////////////////////////////////////////////

cat("loading reporting helper functions and objects...\n")

.is.valid.cell.entry = function(x) {
  if (!is_number(x)) {
    warning(paste("x is not a number! (", paste0(x), ")"))
    return(F)
  }
  x = as.numeric(x)
  if (data_missing(x))
    return(T)
  else if (x < 0 || x > 1) {
    warning(paste("x must be between 0 and 1! (", paste0(x), ")"))
    return(F)
  }
  return(T)
}

.get.cell.color.impl = function(x) {
  if (!.is.valid.cell.entry(x))
    stop("invalid cell entry")
  x = as.numeric(str_trim(x))
  if (data_missing(x))
    return("#FFFFFF")
  return(if (x < .7)
    "#FF6666"
    else if (x < .8)
      "#FFB2FF"
    else if (x < .9)
      "#FFFF99"
    else if (x >= .9)
      "#CCFFCC"
    else
      "#FFFFFF")
}

get.cell.colors = function(x) {
  if (!all(sapply(x, .is.valid.cell.entry)))
    stop("some cell entries are invalid")
  sapply(x, .get.cell.color.impl)
}

header.text = function() {
  textProperties(
    color = "#FFFFFF"
    ,font.size = 10
    ,font.weight = "bold"
    ,font.style = "normal"
    ,underlined = FALSE
    ,font.family = "Corbel"
    ,vertical.align = "baseline"
  )
}

header.par = function() {
  parProperties(
    text.align = "center"
    ,padding.top = 1
    ,padding.bottom = 1
    ,padding.left = 0
    ,padding.right = 0
  )
}

header.cell = function() {
  cellProperties(
    border.color = "#FFFFFF"
    ,background.color = "#001F00"
    ,padding.top = 2
    ,padding.bottom = 2
    ,padding.left = 0
    ,padding.right = 0
  )
}

data.text = function() {
  textProperties(
    color = "black"
    ,font.size = 10
    ,font.weight = "normal"
    ,font.style = "normal"
    ,underlined = FALSE
    ,font.family = "Corbel"
    ,vertical.align = "baseline"
  )
}

data.par = function() {
  parProperties(
    text.align = "left"
    ,padding.top = 1
    ,padding.bottom = 1
    ,padding.left = 0
    ,padding.right = 0
  )
}

data.cell  = function() {
  cellProperties(
    border.color = "#391D00"
    ,padding.top = 2
    ,padding.bottom = 2
    ,padding.left = 0
    ,padding.right = 0
    ,border.style = "dotted"
  )
}

cat("reporting helper functions loaded!\n")
