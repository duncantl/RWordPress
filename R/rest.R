setRESTCredentials <- function( siteURL, siteUser, sitePass, path = "/wp-json/wp/v2/posts" ) {
	options(
		RESTPostURL = glue( "{siteURL}{path}" )
	)

	options(
		RESTAuthHeader = base64encode(
	  		charToRaw( glue("{siteUser}:{sitePass}") )
		)
	)
}

publishREST <- function( file, postTitle, currentPostId = 0, publishPost = FALSE ) {
	htmlFile = render(
		file,
		output_format='html_fragment'
	)

	body = list(
		title = postTitle,
		content = file_string( htmlFile )
	)

	authKey = getOption( "RESTAuthHeader", stop( "Set an authentication header with setRESTCredentials" ) )

	POST(
		getOption( "RESTPostURL", stop( "Set a site URL with setRESTCredentials" ) ),
		body = body,
		encode = "json",
		add_headers(
			Authorization = glue("Basic {authKey}")
		),
		verbose()
	)
}
