#' A roll command parser
#'
#' @description The roll command parser transform a complex roll command into a set of
#' individual rolls and operations.
#'
#' @param roll_cmd a string containing the roll command.
#'
#' @return a list containing dice to roll and mathematical operators.
parse_roll_cmd <- function(roll_cmd) {
    # Group "Ops":
    # "+", "-", "*", "/", "^", "%%", "%/%"
    # "&", "|", "!"
    # "==", "!=", "<", "<=", ">=", ">"

    # lose any parentheticals, as we are not looking to capture exact mathematical sequence,
    # just dice and operators
    # replace with space to avoid inadvertently jamming together certain symbols
    # roll_cmd <- "(!20d6!>5 + 20) * 3 >= (-2d20h1 + 1d4=4 + 2d4>=3)*2"
    roll_cmd <- tolower(roll_cmd)
    roll_cmd <- stringr::str_replace_all(roll_cmd, pattern = "[()]", replacement = " ")

    # add space at beginning and end to make it easier to capture certain patterns without needing anchors
    roll_cmd <- paste0(" ", roll_cmd, " ")

    pattern = "([|/*+^&-])|( [><=] )|( [!+-])|( [><!]=)|(%%)|(%/%)"
    dices <- trimws(stringr::str_split(roll_cmd, pattern)[[1]])
    dices <- dices[dices != ""]

    operators <- trimws(stringr::str_extract_all(roll_cmd, pattern)[[1]])

    parsed_cmd <- list(dices=dices,
                       operators=operators)
    return(parsed_cmd)
}
