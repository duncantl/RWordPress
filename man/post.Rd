\name{modifyBlog}
\alias{newCategory}
\alias{publishPost}
\alias{uploadFile}
\alias{newPage}
\alias{newPost}
\alias{deleteCategory}
\alias{deletePage}
\alias{deletePost}
\alias{setPostCategories}
\alias{setPostCategories,ANY,list-method}
\alias{setPostCategories,ANY,character-method}
\alias{editPost}

\title{Functions for posting, modifying and deleting blog content}
\description{
  These functions are for changing the content of a blog.
  We can create a new page, a new post on a page,
  add a new category, delete a category.
}
\usage{
newCategory(category, description = "???", slug = category, parent = 0L,
             blogid = 0, login = getOption("WordpressLogin", stop("need a login and password")),
              .server = getServerURL())
publishPost(postid, login = getOption("WordpressLogin", stop("need a login and password")),
             ...,  .server = getServerURL())
uploadFile(what, type = guessMIMEType(type), blogid = 0L,
            login = getOption("WordpressLogin", stop("need a login and password")),
             remoteName = basename(what), overwrite = TRUE, ..., .server = getServerURL()  )
newPage(content, publish = TRUE, blogid = 0,
            login = getOption("WordpressLogin", stop("need a login and password")), method = "wp.newPage", .server = getServerURL())
newPost(content, publish = TRUE, blogid = 0,
          login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
deleteCategory(categoryid, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL()  )
deletePage(pageid, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL()  )
deletePost(postid, login = getOption("WordpressLogin", stop("need a login and password")),
            publish = TRUE, appId = "RWordPress", ..., .server = getServerURL() )
}
\arguments{
 \item{category}{the name of the new category to create}
 \item{categoryid}{the identifier for the category of interest.}  
 \item{description}{the content of the blog post}
 \item{slug}{the desired name (i.e. URL) of the resource on the server when we
   create it. This can be omitted and the default is typically fine.}
 \item{parent}{the parent of the }
 \item{login}{the login and password given as
 \code{c(login = "password")}}
 \item{postid}{the identifier for the post. See
   \code{\link{getRecentPostTitles}} for the identifiers.}
 \item{...}{additional arguments passed on to \code{\link[XMLRPC]{xml.rpc}}.}
 \item{what}{the file to upload. This is typically a file name, but can
   also be an R object given as an object of class \code{AsIs} and that
   is serialized. If it is text, this is the content of the file, not
   the file name. If this is a raw vector (with or without the AsIs
   class), this is also taken as the content.
   }
 \item{type}{the MIME type of the file being uploaded.}
 \item{blogid}{the identifier for the blog. Often this can be left as 0
   to identify the default blog identified by the specified login and password.}
 \item{remoteName}{the name to use for the file on the blog server. This
       allows us to use a different name than the one on the local system and
       to specify a name for anonymous object.}
 \item{overwrite}{whether to }
 \item{content}{the }
 \item{publish}{a logical value that controls whether the changes are
   immediately published.}
 \item{pageid}{the identifier for the page on the blog}
 \item{appId}{the name of our program making the request}  
 \item{method}{the name of the actual XMLRPC method to invoke. This
   allows one function to be parameterized to implement multiple methods
   that are very similar.}
  \item{.server}{a string giving the URL of the Wordpress server. This
    can be specified by the caller directly or can be set as a global
    option in R named \code{WordpressURL}, e.g.
       \code{options(WordpressURL = "https://www.mywordpress.org/xmlrpc.php")}.}
}
\value{}
\examples{
\dontrun{
 uploadFile("~/bob.png", "image/png")
}
}

\keyword{IO}
\keyword{programming}
