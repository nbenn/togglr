
toggl_route <- function(...) {
	paste("https://api.track.toggl.com", "api", "v8", ..., sep = "/")
}

auth <- function(tok) {

  if (is.null(tok)){
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
  }

  authenticate(tok, "api_token")
}

to_json <- function(...) {
  toJSON(list(...), auto_unbox = TRUE)
}
