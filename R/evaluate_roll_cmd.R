#' A complex roll resolution
#'
#'  @description The complex roll resolution function evaluates each elements of a complex
#'  roll command and compute their total result
#'
#' @param parsed_cmd a list of dice and operators as returned by parse_roll_cmd.
#'
#' @return roll result.
#'
evaluate_roll_cmd <- function(parsed_cmd){
  final_result <- NA

  for (i in 1:length(parsed_cmd$dice)) {
    result <- roll_one(parsed_cmd$dice[i])
    if (is.na(final_result)) {
      final_result = result
    } else {
      final_result = eval(parse(text = paste(final_result, parsed_cmd$operators[i-1], result)))
    }
  }

  return(final_result)
}
