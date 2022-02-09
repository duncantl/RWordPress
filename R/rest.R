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

publish_rest <- function(file, post_title, post_id = 0, publish = FALSE) {
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
