#' @title publish_rest
#'
#' @description Renders an R Markdown document into an HTML fragment and
#' publishes the HTML as a post in WordPress using the WordPress REST API.
#'
#' @param file The path and file name of an R Markdown document.
#' @param post_title The title that should be used for the post.
#' @param post_id A post ID to update with the HTML content.
#' @param status The WordPress post status to assign. Default "publish".
#'
#' @return Nothing?
#' @examples
#' publish_rest(file = 'current_file.Rmd', post_title = 'My post title')
#' @export
#' @importFrom rmarkdown render
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr verbose
#' @importFrom glue glue
#' @importFrom xfun file_string
publish_rest <- function(file, post_title, post_id = 0, status = "publish") {
    html_content <- render(
        file,
        output_format = "html_fragment"
    )

    body <- list(
        title = post_title,
        content = file_string(html_content)
    )

    auth_key <- getOption(
        "RESTAuthHeader",
        stop("Set an authentication header with set_REST_credentials")
    )

    POST(
        getOption(
            "RESTPostURL",
            stop("Set a site URL with set_REST_credentials")
        ),
        body = body,
        encode = "json",
        add_headers(
            Authorization = glue("Basic {auth_key}")
        ),
        verbose()
    )
}
