library(RWordPress)
options(WordpressLogin = c(duncan = "openscience"), WordpressURL = "http://www.carlboettiger.info/xmlrpc.php")
getCategories()


png("test_rwordpress.png")
plot(rnorm(100), rnorm(100))
dev.off()

library(RWordPress)
options(WordpressLogin = c(duncan = "openscience"), WordpressURL = "http://www.carlboettiger.info/xmlrpc.php")
uploadFile(what="test_rwordpress.png")


