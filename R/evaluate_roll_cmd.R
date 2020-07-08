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





roll_set <- function(parsed_cmd, repetitions) {
  dice_tbls <- lapply(parsed_cmd$dices, construct_dice_table)

  calculated_dice_tbls <- lapply(dice_tbls, function(tbl, n) { do.call(rbind, replicate(n, calculate_dice_table(tbl), simplify = FALSE)) },
                                 n = repetitions)
  names(calculated_dice_tbls) <- parsed_cmd$dices

  result <- parse_result(calculated_dice_tbls, operators = parsed_cmd$operators)

  return(list(calculated_dice_tbls = calculated_dice_tbls,
              operators = parsed_cmd$operators,
              result = result))
}

parse_result <- function(calculated_dice_tbls, operators = NULL, .summary_fn = identity) {
  # result is a list one longer than operators
  # Each result element is a table with 1 or more rows for a single set of dice rolls
  # first choose either the calculated roll or success if available, and sum
  out <- sapply(calculated_dice_tbls, function(tbl) {
    out <- tbl$Calculated.Roll
    out[!is.na(tbl$Success.Outcome)] <- tbl$Success.Outcome
    return(sapply(out, sum, na.rm = TRUE))
  })

  out <- apply(out, 2, .summary_fn)
  if(is.null(dim(out))) dim(out) <- c(1, length(out))

  if(!is.null(operators)) {
    # now apply the operators to each
    text <- paste(out[,1])
    for(i in 2:length(result)) {
      text <- paste(text, operators[i-1], out[, i])
    }
    out <- sapply(text, function(txt) { eval(parse(text = txt)) } )
  }
  return(out)
}

