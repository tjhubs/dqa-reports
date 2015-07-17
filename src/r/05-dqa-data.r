#/////////////////////////////////////////////////////////////////////////
#
#   Organization  : KEMRI Wellcome Trust
#   Department    : Health Systems Research Group
#   Project       : DQA Audit Reports
#   Task          : Concordance analysis and data munging
#   Author        : Boniface Makone [bmakone@kemri-wellcome.org]
#   Details       :
#         This script performs the concordance analysis and munges the results
#         to a format that is palatable for report generation. This is the
#         bones of the app.
#
#///////////////////////////////////////////////////////////////////////////

cat("performing the concordance analysis...\n")
dqa.obj = redcap_dqa(
  repo_token = repos.token
  ,repo_api_url = repos.api.url
  ,repo_local = repos.local
  ,dqa_token = audit.token
  ,dqa_api_url = audit.api.url
  ,dqa_local = audit.local
  ,fields_to_exclude = c(
    "^date_today$"
    ,"^id$"
    ,"^ipno$"
    ,"^redcap_data_access_group$"
    ,"^leave_period"
    ,"_complete$"
    ,"^depid$"
    ,"^is_minimum$"
    ,"^random$"
    )
  ,strata = "hosp_id"
)
dqa.results = audit(dqa.obj, stratified = T)
cat("concordance analysis done!\n")

cat("reshaping audit results for reporting...\n")
dqa.results = unclass(dqa.results)
tmp = names(dqa.results)
site.in.db = which(tmp %in% site.db$Code)
idx = tmp[site.in.db]
names(dqa.results)[site.in.db] = site.db$Name[site.db$Code %in% idx]

summaries = lapply(dqa.results, function(x) {
  top.five = sort(x$field.concordance, decreasing = T)[1:5]
  bottom.five = sort(x$field.concordance, decreasing = F)[1:5]
  data.frame(
    Label = c(
      "Total Concordance", "Field Concordance", "Tuple Concordance",
      rep("Top 5 best performing fields", 5), rep("Bottom 5 worst performing fields", 5)
    ),
    Values = c(
      paste0(round(x$concordance * 100, 1), "%")
      ,paste0(paste0(round(
        range(x$field.concordance) * 100, 1
      ), "%"), collapse = " - ")
      ,paste0(paste0(round(
        range(x$record.concordance) * 100, 1
      ), "%"), collapse = " - ")
      ,paste0(names(top.five), " [", paste0(round(top.five * 100, 1), "%", "]"))
      ,paste0(names(bottom.five), " [", paste0(round(
        bottom.five * 100, 1
      ), "%", "]"))
    ),
    Color = c(get.cell.colors(x$concordance)
              ,rep(get.cell.colors(NA), 12))
  )
})

names(summaries) = names(dqa.results)

detailed = sapply(dqa.results, function(x) {
  x$field.concordance
})
detailed.colors = apply(detailed, 1:2, get.cell.colors)
detailed = apply(detailed, 1:2, function(x) {
  as.character(round(x * 100, 0))
})

cat("report data ready!\n")
