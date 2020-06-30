#' A roll command parser
#'
#' @description The roll command parser transform a complex roll command into a set of
#' individual rolls and operations.
#'
#' @param roll_cmd a string containing the roll command.
#'
#' @return a list containing dice to roll and mathematical operators.
parse_roll_cmd <- function(roll_cmd) {
    pattern = "[\\+-\\/\\*]"

    dices <- trimws(stringr::str_split(roll_cmd, pattern)[[1]])

    operators <- stringr::str_extract_all(roll_cmd, pattern)[[1]]

    parsed_cmd <- list(dices=dices,
                       operators=operators)
    return(parsed_cmd)
}
