#' @title set_rest_credentials
#'
#' @description Stores a set of credentials to use when publishing content
#'              using the WordPress REST API.
#'
#' An application password must be created in WordPress for the user who
#' is publishing the content.
#'
#' @param site_url The URL of the site.
#' @param site_user The username to assign the content to.
#' @param site_password The application password created for this user.
#' @param path An optional path if different than the WordPress default.
#'
#' @examples
#' set_rest_credentials( site_url = 'https://example.com', site_user = 'WPUser', site_password = 'ABCD EFGH IJKL MNOP' )
#' @export
#' @importFrom glue glue
#' @importFrom base64enc base64encode
set_rest_credentials <- function(
    site_url,
    site_user,
    site_password,
    path = "/wp-json/wp/v2/posts"
) {
    options(
        RESTPostURL = glue("{site_url}{path}")
    )

    options(
        RESTAuthHeader = base64encode(
              charToRaw(glue("{site_user}:{site_password}"))
        )
    )
}
