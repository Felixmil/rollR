#' Title
#'
#' @param roll_cmd
#'
#' @return
#' @export
#'
#' @examples
parse_roll_cmd <- function(roll_cmd) {
    pattern = "[[\\p{P}][\\p{S}]]"

    elements <- trimws(stringr::str_split(roll_cmd, pattern)[[1]])

    operators <- stringr::str_extract_all(roll_cmd, pattern)[[1]]

    parsed_cmd <- list(elements=elements,
                       operators=operators)
    return(parsed_cmd)
}
