tbls = readHTMLTable("http://www.webmaster-toolkit.com/mime-types.shtml")
tmp = tbls[[1]][-1,]
mimeTypeExtensions = structure(as.character(tmp[[2]]), names = gsub("^\\.", "", tmp[[1]]))
save(mimeTypeExtensions, file = "data/mimeTypeExtensions.rda")

