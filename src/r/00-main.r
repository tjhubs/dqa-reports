#/////////////////////////////////////////////////////////////////////////
#
#   Organization  : KEMRI Wellcome Trust
#   Department    : Health Systems Research Group
#   Project       : DQA Audit Reports
#   Task          : Runtime Engine
#   Author        : Boniface Makone [bmakone@kemri-wellcome.org]
#   Details       :
#         This script is used to run the syndicated app and ensures
#         sequential execution of the relevant R scripts hence eliminating
#         spagetti code as no other script should call another unless
#         explicitly metaprogrammed
#
#///////////////////////////////////////////////////////////////////////////

cat(paste0(
  "welcome \"", Sys.getenv("USERNAME"), "\"\naudit began at ", format(Sys.time(), "%I:%M%p"), "\n"
))
cat("generating data quality assurance audit report (concordance analysis)...\n")

prj.dir = gsub("\\\\", "/", if (.Platform$OS.type == "windows")
  Sys.getenv("USERPROFILE")
  else
    Sys.getenv("HOME"))

cat("checking for required files and directories...\n")
prj.dir = file.path(prj.dir, "hsuApps", "dqa-reports")
if (!file.exists(prj.dir))
  stop(paste0("project location \"", prj.dir, "\" not found!"))
src.dir = file.path(prj.dir, "src", "r")
if (!file.exists(src.dir))
  stop(paste0("scripts' location \"", src.dir, "\" not found!"))
r.scripts = sort(list.files(src.dir))
cat("required files and directories available!\n")

cat("running app...\n")
r.scripts = paste(src.dir, r.scripts, sep = "/")[-1]
invisible(sapply(r.scripts, function(x) {
  source(x, max.deparse.length = Inf)
}))
cat("app successfully run!\n")

cat("audit process complete!\n")
cat(paste0("audit ended at ", format(Sys.time(), "%I:%M%p"), "\n"))
