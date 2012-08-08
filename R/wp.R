# http://codex.wordpress.org/XML-RPC_wp
# http://www.movabletype.org/documentation/developer/api/

ServerURL = "https://omegahat.wordpress.com/xmlrpc.php"
#ServerURL = "https://www.carlboettiger.info/xmlrpc.php"

getServerURL =
 function() {
   getOption("WordpressURL", "https://omegahat.wordpress.com/xmlrpc.php")
 }

categories = getCategoryList = getCategories =
function(login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
{
  ans = xml.rpc(.server, "mt.getCategoryList", 0L, names(login), as.character(login))
  structure(sapply(ans, `[[`, "categoryId"), names = sapply(ans, `[[`, "categoryName"))
}

getPosts = getRecentPostTitles =
function(num = 100, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ...,
         .server = getServerURL())
{
  ans = xml.rpc(.server, "mt.getRecentPostTitles", as.integer(blogid),
                     names(login), as.character(login), as.integer(num), ...)
  do.call("rbind", lapply(ans, as.data.frame))
}

getTags =
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
  xml.rpc(.server, "wp.getTags", as.character(blogid),
                     names(login), as.character(login), ...)
}

getPostCategories =
function(post = 100, login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
{
  ans = xml.rpc(.server, "mt.getPostCategories", as.integer(post), names(login), as.character(login))
}


getPost =
  #XXX match by name by looking up all the posts and matching.
function(postid, login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
{
  xml.rpc(.server, "metaWeblog.getPost", as.character(postid), names(login), as.character(login))
}

getPage =
  # ll = getPages()
function(pageid, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
  #XXX conversion is not right. 
   xml.rpc(.server, "wp.getPage", blogid, pageid, names(login), as.character(login), ...)
}


getPages =
  # ll = getPages()
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
 generalRequest("wp.getPages", blogid, login, ..., .server = .server)
}

getPageList =
 # ll = getPageList()
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
 generalRequest("wp.getPageList", blogid, login, ..., .server = .server)
}

getOptions =
 # ll = getOptions()
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
 generalRequest("wp.getOptions", blogid, login, ..., .server = .server)
}

getPageStatusList =
 # ll = getOptions()
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
 generalRequest("wp.getPageStatusList", blogid, login, ..., asDataFrame = FALSE, .server = .server)
}

getPostStatusList =
 # ll = getOptions()
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
 generalRequest("wp.getPageStatusList", blogid, login, ..., asDataFrame = FALSE, .server = .server)
}

generalRequest =
function(op, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., asDataFrame = TRUE, .server = getServerURL())
{
  ans = xml.rpc(.server, op, as.integer(blogid),
                     names(login), as.character(login), ...)
  if(asDataFrame) {
     makeIntoDataFrame(ans)
  } else
     ans
}


makeIntoDataFrame =
   # XXX have to make certain that we have the same variables.  
function(els)
{
    # handle scalar variables and non-scalar variables separately.
  vars = unique(unlist(lapply(els, names)))
  isScalar = sapply(vars, function(v) all(sapply(els, function(x) length(x[[v]])) == 1))
  scalarVars = vars[ isScalar ]

  tmp = lapply(els, function(x) {
                       vals = x[scalarVars]
                       vals[sapply(vals, length) == 0] = NA
                       as.data.frame(vals)
                     })
  ans = do.call("rbind", tmp)

      # now go and grab the non-scalar values across the different elements
  if(any(!isScalar)) {
     ids = vars[!isScalar]
     ans[ids] = lapply(ids, function(i) lapply(els, `[[`, i))
  }

  ans
}

newCategory =
  # Problem with this. it only uses the first letter.
  # Also have to get the parent id in, etc.
  #
  # a = newCategory("xmlrpc")
  # cat = newCategory("tmp", "A temporary category that I will then delete")
  # length(categories())
  # deleteCategory(cat)
function(category, description = "???", slug = category, parent = 0L, blogid = 0,
          login = getOption("WordpressLogin", stop("need a login and password")),
           .server = getServerURL())
{
#  catg = list(category, slug, parent, description)
  catg = category
  ans = xml.rpc(.server, "wp.newCategory",
                   as.integer(blogid), names(login), as.character(login), catg)
  structure(ans, names = category, class = "WordPressCategoryId")
}


#
getUsersBlogs =
function(login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
{
  ans = xml.rpc(.server, "wp.getUsersBlogs", names(login), as.character(login))
  ans[[1]]
}


###########
newPage =
  #
  # wp.newPage(list(description = "This is some text that I posted from R", title = "Test from R"))
  #
function(content, publish = TRUE, blogid = 0, login = getOption("WordpressLogin", stop("need a login and password")),
          method = "wp.newPage", .server = getServerURL())
{
  if(is.character(content))
    content = list(description = content)
  
  ans = xml.rpc(.server, method,
                    as.character(blogid), names(login), as.character(login), content, as.logical(publish))
  ans
}


newPost =
  #
  # wp.newPost(list(description = "This is some text that I posted from R", title = "Test from R"), blogId = "1345476")
  #
function(content, publish = TRUE, blogid = 0, login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
{
  ans = newPage(content, publish, blogid, login, "metaWeblog.newPost", .server = .server)
  structure(ans, class = "WordpressPostId")
}


  # The categories should be an unnamed list with the ids of the category
  #  and we should create an XML-RPC array of these with each element a struct
  #  containing a single element named categoryId and the value  being the category identifier.
  # So if the caller gives us strings that are not numbers, lookup the category ids.
setGeneric("setPostCategories",
             function(postid, categories, login = getOption("WordpressLogin", stop("need a login and password")),
                       publish = TRUE, ..., .server = getServerURL())
           standardGeneric("setPostCategories"))

setMethod("setPostCategories", c(categories = "character"),
function(postid, categories, login = getOption("WordpressLogin", stop("need a login and password")),
           publish = TRUE, ..., .server = getServerURL())
{
   cats = matchCategory(categories, login, .server = .server)
   values = lapply(cats, function(x) list(categoryId = x))
   setPostCategories(postid, values, login, publish, ..., .server = .server)
})

setMethod("setPostCategories", c(categories = "list"),
function(postid, categories, login = getOption("WordpressLogin", stop("need a login and password")), publish = TRUE, ..., .server = getServerURL())
{
  ans = xml.rpc(.server, "mt.setPostCategories", as.character(postid),
                       names(login), as.character(login), categories, ...)
  if(ans && publish)
     publishPost(postid, login, .server = .server)
  else
     ans
})

publishPost =
 # 
function(postid, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
  xml.rpc(.server, "mt.publishPost", as.character(postid),
                     names(login), as.character(login),  ...)
}

if(FALSE) {
 setPostCategories("54", "Web services")
 publishPost("54")
}



##############


uploadFile =
  #
  # uploadFile("RWordpress/R/wp.R")
  #
function(what, type = guessMIMEType(what), blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")),
          remoteName = basename(what), overwrite = TRUE, ..., .server = getServerURL())
{
  if(inherits(what, "AsIs")) {
    content = what     
  } else {
    if(!file.exists(what))
      stop("no such file ", what)
    content = readBinaryFile(what)
  }

  info = list(name = remoteName, type = type, bits = content, overwrite = overwrite)
  xml.rpc(.server, "wp.uploadFile", as.character(blogid),
                     names(login), as.character(login),  info, ...)  

}

readBinaryFile = 
function (filename) 
{
    con = file(filename, "rb")
    on.exit(close(con))
    fs = file.info(filename)$size
    readBin(con, raw(fs), fs)
}


######

deletePost =
function(postid, login = getOption("WordpressLogin", stop("need a login and password")),
          publish = TRUE, appId = "RWordPress", ..., .server = getServerURL())
{
  xml.rpc(.server, "metaWeblog.deletePost", appId, as.character(postid),
                     names(login), as.character(login), as.logical(publish), ...)
}

deleteCategory =
function(categoryid, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")),
          ..., .server = getServerURL())
{
  if(!inherits(categoryid, "WordPressCategoryId"))
     categoryid = matchCategory(categoryid, login, .server = .server)
  
  xml.rpc(.server, "wp.deleteCategory", as.character(blogid),
                     names(login), as.character(login), as.character(categoryid), ...)
}

matchCategory =
function(categories, login, catIds = getCategoryList(login, .server = .server), .server = getServerURL())
{

   i = pmatch(tolower(categories), tolower(names(catIds)))
   cats = catIds[ i[!is.na(i)] ]
   if(any(is.na(i))) {
     i = which(is.na(i))
     j = lapply(tolower(categories[i]), function(x) {
                                          tmp = agrep(x, tolower(names(cats)), value = TRUE)
                                          if(length(tmp) > 1)
                                            warning(x, " matches ", paste(tmp, collapse = ", "))
                                          tmp
                                        })
     if(any(sapply(j, length) != 1))
          # should create them possibly, but only after looking at agrep to see possible matches.
       stop("don't recognize category ", paste(categories[i], collapse = ", "))

     cats[i] = unlist(j)
   }

   cats

}


deletePage =
  #
  #  pages = getPages()
  #  page = wp.newPage(list(description = "This is some text that I posted from R", title = "Test from R"))
  #  tmp = getPages()  
  #  deletePage(getUsersBlogs()[[1]]$blogid, page)
  #  newPages = getPages()
  #  nrow(pages) == nrow(newPages)
  #
function(pageid, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
  xml.rpc(.server, "wp.deletePage", as.character(blogid),
                     names(login), as.character(login),  as.character(pageid), ...)
}


editPost =
function(postid, content, login = getOption("WordpressLogin", stop("need a login and password")), publish = TRUE, appId = "RWordpress", ..., .server = getServerURL())
{
  if(!is.list(content))
    content = list(description = as.character(content))
  
  xml.rpc(.server, "metaWeblog.editPost", as.character(postid),
                     names(login), as.character(login), content, as.logical(publish), ...)
}

#################

# http://www.sixapart.com/developers/xmlrpc/movable_type_api/

supportedMethods =
function(.server = getServerURL())  
   xml.rpc(.server, "mt.supportedMethods")

supportedTextFilters =
function(.server = getServerURL())  
   xml.rpc(.server, "mt.supportedTextFilters")  



##################
