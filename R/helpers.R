RWPSetCredentials <- function( siteURL, siteUser, sitePass) {
  credentials <- setNames( sitePass, siteUser )
  options(WordpressURL = c( siteURL ) )
  options(WordpressLogin = credentials)
}

RWPSaveManual <- function( file, postTitle, currentPostId = 0, publishPost = FALSE) {
  if ( currentPostId > 0 )
    knit2wp( file, title = postTitle, action=("editPost"), postid = currentPostId, publish = publishPost )
  else
    knit2wp( file, title = postTitle, publish = publishPost )
}

RWPSaveCurrent <- function( fileName ) {
  fileData <- rmarkdown::yaml_front_matter( fileName )

  if ( fileData$postid )
    knit2wp( fileName, title = fileData$title, action=("editPost"), postid = fileData$postid, publish = fileData$publish )
  else
    knit2wp( fileName, title = fileData$title, publish = fileData$publish )
}
