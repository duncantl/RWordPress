\name{supportedMethods}
\alias{supportedMethods}
\alias{supportedTextFilters}
\title{Meta-information about the blog server}
\description{
  These functions tell us about the Wordpress or blog server,
  specifically what XML-RPC methods are available for us to invoke,
  and what text filters are supported on the server. This can help
  us to construct content appropriately.
}
\usage{
supportedMethods(.server = getServerURL())
supportedTextFilters(.server = getServerURL())
}
\arguments{
  \item{.server}{a string giving the URL of the Wordpress server. This
    can be specified by the caller directly or can be set as a global
    option in R named \code{WordpressURL}, e.g.
       \code{options(WordpressURL =
	 "https://www.mywordpress.org/xmlrpc.php")}.}
}
\value{
  \code{supportedMethods} returns a character vector of the method
  names. There is no information in the return value about the arguments.
}
\author{Duncan Temple Lang}
\examples{

}
\keyword{IO}
\keyword{programming}
