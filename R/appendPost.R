
appendToPost =
function(postid, content, login = getOption("WordpressLogin", stop("need a login and password")))
{
  current = getPost(postid, login)
  new = paste(current, content)
  editPost(postid, new, login, TRUE)
}
