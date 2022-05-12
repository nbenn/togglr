
#' Toggle tags
#'
#' Tags (as ids and names) can be retrieved for a workspace (give its ID),
#' created, as well as updated and deleted (given its ID).
#'
#' @inheritParams get_project_id_and_name
#'
#' @examples
#' \dontrun{
#' toggl_get_tags()
#' }
#'
#' @rdname toggl_tags
#' @export
toggl_get_tags <- function(api_token = get_toggl_api_token(),
                           workspace_id = get_workspace_id(api_token)) {

  res <- GET(
    toggl_route("workspaces", workspace_id, "tags"),
    auth(api_token),
    encode = "json"
  )

  tags_to_tbl(res)
}

#' @param tag_name tag name
#' @inheritParams toggl_get_tags
#'
#' @examples
#' \dontrun{
#' toggl_create_tag("new tag")
#' }
#'
#' @rdname toggl_tags
#' @export
toggl_create_tag <- function(tag_name, api_token = get_toggl_api_token(),
                             workspace_id = get_workspace_id(api_token)) {

  tag <- list(
    name = tag_name,
    wid = workspace_id
  )

  res <- POST(
    toggl_route("tags"),
    verbose(),
    auth(api_token),
    encode = "json",
    body = to_json(tag = tag)
  )

  tags_to_tbl(res)
}

#' @param tag_id tag ID
#' @param new_name Updated tag name
#' @inheritParams toggl_get_tags
#'
#' @examples
#' \dontrun{
#' toggl_update_tag(1, "updated tag name")
#' }
#'
#' @rdname toggl_tags
#' @export
toggl_update_tag <- function(tag_id, new_name,
                             api_token = get_toggl_api_token()){

  tag <- list(
    name = new_name
  )

  res <- PUT(
    toggl_route("tags", tag_id),
    verbose(),
    auth(api_token),
    encode = "json",
    body = to_json(tag = tag)
  )

  tags_to_tbl(res)
}

#' @inheritParams toggl_update_tag
#'
#' @examples
#' \dontrun{
#' toggl_delete_tag(1)
#' }
#'
#' @rdname toggl_tags
#' @export
toggl_delete_tag <- function(tag_id, api_token = get_toggl_api_token()){

  DELETE(
    toggl_route("tags", tag_id),
    verbose(),
    auth(api_token)
  )

  invisible(NULL)
}

tags_to_tbl <- function(x) {
  content(x) %>%
    bind_rows()  %>%
    rename(workspace_id = wid, tag_id = id, tag_name = name)
}
