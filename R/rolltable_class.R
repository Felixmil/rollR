#' Rolltable class
#'
#' Class used to store multiple dice rolls for a roll command.
#' This function is the main helper, which sets up a new rolltable.
#'
#' @param cmd character string describing the dice roll to compute.
#' @param repetitions Number of times to roll the command.
#' @param verbose If TRUE, dice rolls details are visible in the console.
#' @export
#'
#' @examples
#' rolltable("2d20h1 + 20")
#' rolltable("2d20h1 + 20", repetitions = 3)
#' rolltable("2d20h1 + 20", verbose = TRUE)
rolltable <- function(cmd, repetitions = 1, verbose = FALSE) {
  cmd <- tolower(cmd) # avoid needing to check for a bunch of capital letters
  parsed_cmd <- rollr:::parse_roll_cmd(cmd)
  dice_tbls <- lapply(parsed_cmd$dices, construct_dice_table)
  tbl <- new_rolltable(lst = dice_tbls,
                       cmd = cmd,
                       dices = parsed_cmd$dices,
                       operators = parsed_cmd$operators)
  roll(tbl, repetitions = repetitions, verbose = verbose)
}

#' Constructor for rolltable
#'
#' Internal function to build the rolltable class structure.
#' Minimal checks for validity.
new_rolltable <- function(lst, cmd, dices, operators) {
  stopifnot(is.list(lst))
  stopifnot(is.character(cmd),
            length(cmd) == 1)
  stopifnot(is.character(dices),
            length(dices) == length(lst))
  stopifnot(is.character(operators),
            length(operators) == (length(dices) - 1))

  names(lst) <- dices
  structure(
    .Data = lst,
    command = cmd,
    dices = dices,
    operators = operators,
    class = "rolltable"
  )
}

#' Roll Method
#'
#' Takes a rolltable and rolls to create a new set of results, using one or more repetitions.
#'
#' @param tbl A rolltable class.
#' @param repetitions Number of times to roll the command.
#' @param verbose If TRUE, dice roll details are visible in the console.
#' @return An updated rolltable.
#' @export
#'
#' @examples
#'
#' tbl <- rolltable("2d20h1 + 20"); tbl
#' roll(tbl, repetitions = 3)
#' roll(tbl, repetitions = 2, verbose = TRUE)
roll <- function(tbl, ...) { UseMethod("roll") }
roll.rolltable <- function(tbl, repetitions = 1, verbose = FALSE) {
  lapply_fn <- ifelse(verbose, lapply, function(...) suppressMessages(lapply(...)))

  rollhist <- lapply_fn(tbl, function(df, n) { df[rep(1, times = n), , drop = FALSE] }, n = repetitions)
  rollhist <- lapply_fn(rollhist, rollr:::calculate_dice_table)

  # lapply strips class, so add back.
  new_rolltable(rollhist, cmd = attr(tbl, "command"), dices = attr(tbl, "dices"), operators = attr(tbl, "operators"))
}

#' Method to calculate the current value of a rolltable
#'
#' For each repetition in the rolltable, summarizes each dice set and
#' applies the operators in turn.
#'
#' @param tbl A rolltable class.
#' @param .summary_fn Function used to summarize results between repetitions. Default returns each repetition separately.
#' @return A numeric vector whose names are the individual sums for each dice set.
#' @export
#'
#' @examples
#'
#' tbl <- rolltable("2d20h1 + 20")
#' calculate(tbl)
#' calculate(tbl, .summary_fn = "median")
calculate <- function(tbl, ...) { UseMethod("calculate") }
calculate.rolltable <- function(tbl, .summary_fn = "identity") {
  out <- parse_result(tbl, operators = attr(tbl, "operators"), .summary_fn = get(.summary_fn))
  attr(out, "command") <- attr(tbl, "command")
  attr(out, "parse_function") <- .summary_fn
  out
}

#' Mean rolltable
#'
#' Determines the average over repetitions for the rolls.
#' Calculates the total by applying the operators to each average.
#'
#' @param x A rolltable class.
#' @return A numeric vector whose names are the individual means for each dice set.
#' @export
mean.rolltable <- function(x) {
  calculate(x, .summary_fn = "mean")
}

#' Median rolltable
#'
#' Determines the median over repetitions for the rolls.
#' Calculates the total by applying the operators to each median value.
#'
#' @param x A rolltable class.
#' @return A numeric vector whose names are the individual means for each dice set.
#' @export
median.rolltable <- function(x, na.rm = FALSE) {
  calculate(x, .summary_fn = "median")
}



#' Min rolltable
#'
#' Determines the minimum over repetitions for the rolls.
#' Calculates the total by applying the operators to each minimum.
#'
#' @param tbl A rolltable class.
#' @return A numeric vector whose names are the individual minimums for each dice set.
#' @export
min.rolltable <- function(tbl, ..., na.rm = FALSE) {
  calculate(tbl, .summary_fn = "min")
}

#' Max rolltable
#'
#' Determines the maximum over repetitions for the rolls.
#' Calculates the total by applying the operators to each maximum.
#'
#' @param tbl A rolltable class.
#' @return A numeric vector whose names are the individual maximums for each dice set.
#' @export
max.rolltable <- function(tbl, ..., na.rm = FALSE) {
  calculate(tbl, .summary_fn = "max")
}

#' Print a rolltable
#'
#' @param tbl A rolltable class.
#' @param n Number of repetitions to print.
#' @param rolls If TRUE, print the base rolls and modifications to rolls, such as keep highest or exploding.
#' @param digits Round numbers to this number of digits when printing.
#' @export
print.rolltable <- function(tbl, n = 10, rolls = FALSE, digits = 2, ...) {
  res <- mean(tbl)
  repetitions <- nrow(tbl[[1]])

  cat(attr(tbl, "command"), "\n")
  cat("======\n")
  cat(sprintf("Number of repetitions: %d\n", repetitions))

  if(rolls) {
    # basic format:
    # ----------
    # 1. 1d6 rolls: 3
    # 1. 2d10h1 rolls: 10, 4
    # 1. Keep Highest: 10, 4
    # 1. 8d6! rolls: 5, 2, 4, 5, 3, 1, 3, 4
    # 1. Exploding: 5, 2, 4, 5, 3, 1, 3, 4
    # ----------

    total_i <- min(repetitions, n)
    iteration_s <- paste0("%0", ceiling(total_i / 10), "d")
    for(i in seq_len(total_i)) {
      cat("----------\n")
      i_str <- sprintf(iteration_s, i)
      for(d in seq_len(length(tbl))) {

        cat(sprintf("%s. %s base rolls: %s\n", i_str, names(tbl)[[d]], paste(tbl[[d]]$Base.Roll[[i]], collapse = ", ")))

        if(tbl[[d]]$Type[[i]] != "none" & tbl[[d]]$Type[[i]] != "simple") {
          cat(sprintf("%s. %s: %s\n", i_str, stringr::str_to_title(tbl[[d]]$Type[[i]]), paste(tbl[[d]]$Calculated.Roll[[i]], collapse = ", ")))
        }
      }
    }
    if(total_i < repetitions) cat("...\n")

  }

  cat("======\n")
  s <- paste0("Mean result: %s = %.", digits, "f\n")
  cat(sprintf(s, trim_numeric_string(names(res), digits), res))


}

#' Helper function to change the number of digits for a string with numerals
trim_numeric_string <- function(str, digits = 2) {
  p <- sprintf("([[:digit:]]+[.])([[:digit:]]{%d})[[:digit:]]+", digits)
  stringr::str_replace_all(str, pattern = p, replacement = "\\1\\2")
}
