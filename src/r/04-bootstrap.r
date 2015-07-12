#/////////////////////////////////////////////////////////////////////////
#
#   Organization  : KEMRI Wellcome Trust
#   Department    : Health Systems Research Group
#   Project       : DQA Audit Reports
#   Task          : Initializing the R environment
#   Author        : Boniface Makone [bmakone@kemri-wellcome.org]
#   Details       :
#         This script loads up the ideal environment for the running of
#         this syndicated app. It loads the required packages and sets
#         global settings and variables
#
#///////////////////////////////////////////////////////////////////////////

cat("initializing reporting environment...")

libs.to.load = c("ReporteRs"
                 ,"RedcapData"
                 ,"RedcapDqa"
                 ,"stringr"
                 ,"data.table")

cat("attaching packages to search path...")
libs.not.available = libs.to.load[!libs.to.load %in% .packages(all.available =
                                                                 T)]
if (0 < length(libs.not.available))
  install.packages(libs.not.available, dependencies = T, clean = T)
invisible(sapply(libs.to.load, function(x) {
  suppressWarnings(suppressPackageStartupMessages(library(x, character.only =
                                                            T)))
}))
cat("packages attached!")

cat("setting global settings and variables...")
project.name = "Clinical Information Network"
project.alias = "CIN"
report.location = file.path(prj.dir, "cache", "doc", paste0("audit_", tolower(project.alias), ".docx"))
if (!file.exists(dirname(report.location)))
  invisible(dir.create(
    dirname(report.location), showWarnings = F, recursive = T
  ))
data.cache.location = file.path(prj.dir, "cache", "data", "audit-cache.Rda")
if (!file.exists(dirname(data.cache.location)))
  invisible(dir.create(
    dirname(data.cache.location), showWarnings = F, recursive = T
  ))
template.location = file.path(prj.dir, "doc", "assets", "template.docx")
if (!file.exists(template.location))
  stop("report template not found!")
site.db.location = file.path(prj.dir, "doc", "assets", "hospdb.csv")
if (!file.exists(site.db.location))
  stop("site database not found!")
site.db = read.csv(site.db.location, as.is = T)
repos.token = ""
repos.api.url = "http://hsu.kemri-wellcome.org/redcap/api/"
repos.local = F
audit.token = ""
audit.api.url = repos.api.url
audit.local = repos.local
project.period = "June - July, 2015"

cat("global settings and variables initialized")

cat("environment initialized!")
