#/////////////////////////////////////////////////////////////////////////
#
#   Organization  : KEMRI Wellcome Trust
#   Department    : Health Systems Research Group
#   Project       : DQA Audit Reports
#   Task          : Report generation
#   Author        : Boniface Makone [bmakone@kemri-wellcome.org]
#   Details       :
#         This script builds the word document that serves as the final
#         output of the audit process. It takes the reshaped data from
#         previous processes and generates a formatted word document for
#         interpretation and presentation
#
#///////////////////////////////////////////////////////////////////////////

cat("generating report document...\n")

site.names = names(summaries)
site.labs = site.names[2:(length(summaries))]
site.names[2:length(site.names)] = 1:(length(site.names) - 1)
site.names[(2:length(site.names))[1:9]] = paste0("0", (1:(length(site.names) - 1))[1:9])
site.names = cbind(c("", rep("Hosp", length(site.names) - 1)), site.names)

key = data.frame(Code = apply(site.names, 1, function(x) paste0(x, collapse = ""))[-1], Label = site.labs)

site.names[1,] = site.names[1,][c(2, 1)]
site.names = rbind(c("", ""), site.names)
detailed.header = FlexRow(
  text.properties = header.text(), cell.properties = header.cell(), par.properties =
    header.par()
)
for (i in seq_along(site.names[, 1])) {
  detailed.header[i] = FlexCell(value = set_of_paragraphs(pot(ifelse(i == 2, "All", site.names[i, 1]), format = header.text()),
                                                  pot(ifelse(i == 2, "Hosps", site.names[i, 2]), format = header.text())), cell.properties=header.cell(), par.properties = header.par())
}
report.doc = docx(title = "Audit", template = template.location)
report.doc = addParagraph(report.doc, project.name, bookmark = "project_name", stylename =
                            "titlesubheader")
report.doc = addParagraph(report.doc, project.period, bookmark = "period", stylename =
                            "titlesubheader")
if (!file.exists(dirname(report.location)))
  invisible(dir.create(
    dirname(report.location), showWarnings = F, recursive = T
  ))
report.doc = addPageBreak(report.doc)
report.doc = addParagraph(report.doc, "Table of Contents", stylename="dqaheadermain")
report.doc = addTOC(report.doc)
report.doc = addPageBreak(report.doc)

report.doc = addParagraph(
  report.doc, "Hospital Key", stylename="dqaheadermain")
tmp = FlexTable(key,
                body.cell.props = data.cell(), body.par.props = data.par(),
                body.text.props = data.text(), header.cell.props= header.cell(),
                header.text.props=header.text(), header.par.props=header.par(),
                add.rownames = F)
tmp = setFlexTableWidths(tmp, widths=c(2, 2))
report.doc = addFlexTable(report.doc, tmp)
report.doc = addPageBreak(report.doc)

report.doc = addParagraph(
  report.doc, "Color Key", stylename="dqaheadermain")

color.key = data.frame(Label = c(">90%", "80-89%", "70-79%", "<70%"),
                       Color = rep("", 4),
                       Code = c("#CCFFCC", "#FFFF99", "#FFB2FF", "#FF6666")
                       )
tmp = FlexTable(color.key[, 1:2],
                body.cell.props = data.cell(), body.par.props = data.par(),
                body.text.props = data.text(), header.cell.props= header.cell(),
                header.text.props=header.text(), header.par.props=header.par(),
                add.rownames = F)
tmp = setFlexTableWidths(tmp, widths=c(2, 2))
tmp = setFlexTableBackgroundColors(tmp, j = 2, colors = as.character(color.key$Code))
report.doc = addFlexTable(report.doc, tmp)
report.doc = addPageBreak(report.doc)

summaries.report = lapply(summaries, function(x) {
  ft = FlexTable(
    x[, c("Label", "Values")], header.columns = F, body.cell.props = data.cell(), body.par.props =
      data.par(), body.text.props = data.text(),add.rownames = F
  )
  ft = spanFlexTableRows(ft, j = 1, runs = as.character(x[, "Label"]))
  ft = setFlexTableBackgroundColors(ft, j = 2, colors = as.character(x[, "Color"]))
  ft = setFlexTableWidths(ft, widths = c(4, 4))
  ft
})

report.doc = addParagraph(report.doc, "Summary", stylename = "dqaheadermain")
invisible({
  for (i in 1:length(summaries.report)) {
    addParagraph(report.doc, names(summaries.report[i]), stylename = "dqasubtitle")
    addFlexTable(report.doc, summaries.report[[i]])
    addPageBreak(report.doc)
  }
})


report.doc = addParagraph(report.doc, "Details", stylename = "dqaheadermain")
detailed.paginated.nos = RedcapData::get_chunks(1:nrow(detailed), 17)
detailed.paginated = lapply(detailed.paginated.nos, function(x) {
  if (1 < length(x)) {
    data.ft = data.frame(detailed[x,])
    cols.ft = as.matrix(detailed.colors[x,])
  } else {
    data.ft = data.frame(matrix(detailed[x,], nrow = 1))
    cols.ft = matrix(detailed.colors[x,], nrow = 1)
    rownames(data.ft) = rownames(detailed)[x]
  }
  ft = FlexTable(
    data.ft, add.rownames = T,body.cell.props = data.cell(), body.par.props =
      data.par(), body.text.props = data.text(), header.columns = F
  )
  for (i in (1:ncol(data.ft))) {
    ft = setFlexTableBackgroundColors(ft, j = i + 1, colors = cols.ft[, i])
  }
  ft = addHeaderRow(ft, detailed.header)
  ft
})
for (i in 1:length(detailed.paginated.nos)) {
  tmp = paste0("Variable ", detailed.paginated.nos[[i]][1], " - Variable " , detailed.paginated.nos[[i]][length(detailed.paginated.nos[[i]])])
  report.doc = addParagraph(report.doc, tmp, stylename = "dqasubtitle")
  report.doc = addFlexTable(report.doc, detailed.paginated[[i]])
  if (i != length(detailed.paginated.nos))
    report.doc = addPageBreak(report.doc)
}
writeDoc(report.doc, file = report.location)
RedcapData::open_using_default_app(report.location)
cat("report document generated\n")
