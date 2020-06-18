#' Title
#'
#' @param parsed_cmd
#'
#' @return
#'
#' @examples
evaluate_roll_cmd <- function(parsed_cmd){
  final_result <- NA

  for (i in 1:length(parsed_cmd$elements)) {
    result <- roll_one(parsed_cmd$elements[i],parsed_cmd$operators[i-1])
    if (is.na(final_result)) {
      final_result = result
    } else {
      final_result = eval(parse(text = paste(final_result, parsed_cmd$operators[i-1], result)))
    }
  }

  return(final_result)
}
