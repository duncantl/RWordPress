findXMLRPCMethod =
function(fun)
{
  foo =
    function(x) {
                  if(is.call(x))
                    if(as.character(x[[1]]) == "xml.rpc")
                     as.character(x[[3]])
                    else if(as.character(x[[1]]) == "generalRequest")
                       as.character(x[[2]])
                  else if(is(x, "="))
                     foo(x[[3]])
               }
  
  b = body(fun)
  if(is(b, "{"))
    unlist(lapply(b, foo))
  else
    foo(b)
}

is.missing.arg <- function(arg) typeof(arg) == "symbol" && 
        deparse(arg) == ""



makeUsage =
function(x)
{
  fun = get(x)
  parms = formals(fun)
  paste(x, "(",
         trim(paste(names(parms),
               ifelse(sapply(parms, is.missing.arg), "", "="),
              sapply(parms, function(x)
                              paste(deparse(x), collapse = "")), collapse = ", ")),
        ")", sep = "")
}

getParamNames =
function(x)
{
 sapply(x, function(x) names(formals(get(x))))
}


trim = function (x) 
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)

getAliasNames =
function(doc)
{
  al = grep("\\alias{", readLines(doc), val = TRUE, fixed = TRUE)  
  gsub("\\\\alias\\{|\\}", "", al)
}

getParamsForRd =
function(doc = "RWordpress/man/functions.Rd")
{
 names = getAliasNames(doc)
 sprintf("\\item{%s}{}\n", unique(unlist(getParamNames(names))))
}

if(FALSE) {
library(RWordpress)
funs = sapply(objects("package:RWordpress"), function(x) get(x, "package:RWordpress"))
funs = funs[sapply(funs, class) == "function"] # skip the standardGeneric


apiMethods = sapply(funs, findXMLRPCMethod)

apiMethods$newPost = "metaWeblog.newPost"
apiMethods$newPage = "wp.newPage"
apiMethods = structure(unlist(apiMethods), names = names(apiMethods))

urls = rep("", length(apiMethods))
i = grep("^wp.", apiMethods)
urls[i] = sprintf("http://codex.wordpress.org/XML-RPC_wp#%s", apiMethods[i])
i = grep("^(mt|metaWeblog)\\.", apiMethods)
urls[i] = sprintf("http://www.sixapart.com/developers/xmlrpc/movable_type_api/%s.html", gsub("\\.", "", tolower(apiMethods[i])))

sprintf("\\item{%s}{%s  \\url{%s}}", names(apiMethods), apiMethods, urls)




# http://codex.wordpress.org/XML-RPC_wp

  # For the usages.
 names = getAliasNames("RWordpress/man/functions.Rd")
 usg = sapply(names, makeUsage)
 cat(usg, sep = "\n")  
}
