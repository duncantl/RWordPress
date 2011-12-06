\name{categories}
\alias{categories}
\alias{generalRequest}
\alias{getCategories}
\alias{getCategoryList}
\alias{getOptions}
\alias{getPage}
\alias{getPageList}
\alias{getPageStatusList}
\alias{getPages}
\alias{getPost}
\alias{getPostCategories}
\alias{getPostStatusList}
\alias{getRecentPostTitles}
\alias{getPosts}
\alias{getTags}
\alias{getUsersBlogs}

\title{Wordpress functions}
\description{
   These are functions   that allow us to interact with a
   Wordpress blog. We can query the contents of the blog
   and its meta-data. We can also post to the blog,
   create new pages, delete pages, posts and categories
   and create new categories.
   The functions documented here are for querying the blog
   information. See \code{\link{newPost}} for
   functions for creating, deleting and modifying the content.

   Wherever the documentation for the methods talks about a
   \code{struct}, this corresponds to  a named  list in R.
 }
 \usage{
getPostCategories(post = 100, login = getOption("WordpressLogin", stop("need a login and password")))
getPost(postid, login = getOption("WordpressLogin", stop("need a login and password")))
generalRequest(op, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., asDataFrame = TRUE)
getPage(pageid, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ...)
getRecentPostTitles(num = 100, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ...)
}
 \arguments{
 \item{login}{a character vector giving the login and password as a
   named character vector of the form \code{c(login = "password")}.}
 \item{blogid}{the identifier for the particular blog. Often this can be
   left as 0 to identify the default blog.}
 \item{...}{additional parameters passed to \code{xml.rpc}}
 \item{asDataFrame}{whether to conver the result into a data frame}
 \item{pageid,postid,post}{the id of the page or post of interest}
 \item{num}{the number of posts/titles to retrieve}
 \item{op}{the name of the operation/method} 
 } 
 \value{}
 \examples{}
 \keyword{IO}
 \concept{blogging}
 