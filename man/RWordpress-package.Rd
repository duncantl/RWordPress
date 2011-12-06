\name{RWordPress-package}
\alias{RWordPress-package}
\alias{RWordPress}
\docType{package}
\title{Interface to Wordpress blogs}
\description{
This is a package that provides a programmatic interface to
access Wordpress blogs using XMLRPC.
}
\details{
\tabular{ll}{
Package: \tab RWordPress\cr
Version: \tab 0.1-0\cr
License: \tab BSD\cr
Imports: \tab XMLRPC\cr
Built: \tab R 2.11.0; ; 2010-03-13 23:08:36 UTC; unix\cr
}

Most of the  functions in the package are direct
R implementations of the corresponding methods described
in the Wordpress API. This is described in
\url{http://codex.wordpress.org/XML-RPC_wp}
and
\url{http://www.movabletype.org/documentation/developer/api/}
There are essentially 4 overlapping APIs, i.e. that have similar
facilities in common with perhaps slightly different parameters.
We are not documenting how to use these functions explicitly in this
package.
You are encouraged to read the documentation for each method on the
pages above.

All the functions require a login and password. This is specified
as a named character vector with one element. The value is the password
and the name is the login. So we specify this as
\code{c(duncan = "mypassword")}.
Rather than specifying this in each call, we can set the value of the R option
\code{WordpressLogin} to this vector. By defalt, the functions look
there for the login and password.

For the functions that require a blog identifier, we can usually give
the value 0 to indicate the default blog.  Alternatively, you can get
the blog identifier by calling \code{getUsersBlogs}. The identifier is
in the element \code{blogid}.

The following are a list of the R functions, the corresponding
XMLRPC method and a link to the API reference to that function
(where the documentation exists).
You should consult that documentation for the number and types of
the parameters expected and the return type.

\describe{
\item{categories}{mt.getCategoryList  \url{http://www.sixapart.com/developers/xmlrpc/movable_type_api/mtgetcategorylist.html}}
\item{deleteCategory}{wp.deleteCategory  \url{http://codex.wordpress.org/XML-RPC_wp#wp.deleteCategory}}
\item{deletePage}{wp.deletePage  \url{http://codex.wordpress.org/XML-RPC_wp#wp.deletePage}}
\item{deletePost}{metaWeblog.deletePost  \url{http://www.sixapart.com/developers/xmlrpc/movable_type_api/metaweblogdeletepost.html}}
\item{getCategories}{mt.getCategoryList  \url{http://www.sixapart.com/developers/xmlrpc/movable_type_api/mtgetcategorylist.html}}
\item{getCategoryList}{mt.getCategoryList  \url{http://www.sixapart.com/developers/xmlrpc/movable_type_api/mtgetcategorylist.html}}
\item{getOptions}{wp.getOptions  \url{http://codex.wordpress.org/XML-RPC_wp#wp.getOptions}}
\item{getPage}{wp.getPage  \url{http://codex.wordpress.org/XML-RPC_wp#wp.getPage}}
\item{getPageList}{wp.getPageList  \url{http://codex.wordpress.org/XML-RPC_wp#wp.getPageList}}
\item{getPageStatusList}{wp.getPageStatusList  \url{http://codex.wordpress.org/XML-RPC_wp#wp.getPageStatusList}}
\item{getPages}{wp.getPages  \url{http://codex.wordpress.org/XML-RPC_wp#wp.getPages}}
\item{getPost}{metaWeblog.getPost  \url{http://www.sixapart.com/developers/xmlrpc/movable_type_api/metawebloggetpost.html}}
\item{getPostCategories}{mt.getPostCategories  \url{http://www.sixapart.com/developers/xmlrpc/movable_type_api/mtgetpostcategories.html}}
\item{getPostStatusList}{wp.getPageStatusList  \url{http://codex.wordpress.org/XML-RPC_wp#wp.getPageStatusList}}
\item{getRecentPostTitles}{mt.getRecentPostTitles  \url{http://www.sixapart.com/developers/xmlrpc/movable_type_api/mtgetrecentposttitles.html}}
\item{getTags}{wp.getTags  \url{http://codex.wordpress.org/XML-RPC_wp#wp.getTags}}
\item{getUsersBlogs}{wp.getUsersBlogs  \url{http://codex.wordpress.org/XML-RPC_wp#wp.getUsersBlogs}}
\item{newCategory}{wp.newCategory  \url{http://codex.wordpress.org/XML-RPC_wp#wp.newCategory}}
\item{newPage}{wp.newPage  \url{http://codex.wordpress.org/XML-RPC_wp#wp.newPage}}
\item{newPost}{metaWeblog.newPost  \url{http://www.sixapart.com/developers/xmlrpc/movable_type_api/metaweblognewpost.html}}
\item{publishPost}{mt.publishPost  \url{http://www.sixapart.com/developers/xmlrpc/movable_type_api/mtpublishpost.html}}
\item{supportedMethods}{mt.supportedMethods  \url{http://www.sixapart.com/developers/xmlrpc/movable_type_api/mtsupportedmethods.html}}
\item{supportedTextFilters}{mt.supportedTextFilters  \url{http://www.sixapart.com/developers/xmlrpc/movable_type_api/mtsupportedtextfilters.html}}
\item{uploadFile}{wp.uploadFile  \url{http://codex.wordpress.org/XML-RPC_wp#wp.uploadFile}}
}
}
\author{
Duncan Temple Lang

Maintainer: Duncan Temple Lang <duncan@r-project.org>
}
\references{
}
\keyword{ package }
\seealso{
\code{XMLRPC} package
}
\examples{

}
