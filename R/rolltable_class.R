#' Rolltable class
#'
#' Class used to store multiple dice rolls for a roll command.
#' This function is the main helper, and sets up a new rolltable.
#'
#' @param cmd character string describing the dice roll to compute.
#'   Can be of any length, but each character element can only represent a single dice set with no operators.
#' @param repetitions Number of times to roll the command.
#' @param verbose If TRUE, dice rolls details are visible in the console.
#' @export
#'
#' @details
#' Users interested in simulation may wish to use the rolltable class directly.
#' For example, the following simulates 1000 different D&D 5e ability rolls:
#' \code{rolltable("4d6h3", repetitions = 1000)}
#'
#' You can apply mathematical operations to one or more rolltables. For example:
#' \code{(rolltable("3d6") * 3) + rolltable("1d4r1")}
#' \code{median(rolltable("4d6h3", repetitions = 1000))}
#'
#' @examples
#' rolltable("2d20h1")
#' rolltable("2d20h1", repetitions = 5) + rolltable("10")
#'
#' # How often would advantage beat disadvantage in D&D 5e?
#' result <- rolltable("2d20h1", repetitions = 1000) > rolltable("2d20l1", repetitions = 1000)
#' summary(result[[1]])
#'
#' # convert to dataframe
#' as.data.frame(rolltable("2d20h1", repetitions = 10))
#' as.data.frame(rolltable(c("1d6", "2d6", "3d6", "4d6"), repetitions = 2))
#'
rolltable <- function(cmd, repetitions = 1, verbose = FALSE) {
  cmd <- tolower(cmd) # avoid needing to check for a bunch of capital letters
  error_txt <- "Rolltable only accepts single dice set commands (e.g., '2d6!', not '2d6! + 20'. To add, subtract, or use other mathematical symbols either apply the symbol to rolltables directly or use roll_dice."

  if(length(cmd) > 1) {
    parsed_cmd <- sapply(cmd, parse_roll_cmd)
    ln <- sapply(parsed_cmd["dices",], length)
    if(any(ln > 1)) stop(error_txt)
  } else {
    parsed_cmd <- rollr:::parse_roll_cmd(cmd)
    if(length(parsed_cmd$dices) > 1) stop(error_txt)
  }

  tbl <- rollr:::new_rolltable(tbl = rollr:::construct_dice_table(cmd),
                       cmd = cmd)
  roll(tbl, repetitions = repetitions, verbose = verbose)
}

#' Constructor for rolltable
#'
#' Internal function to build the rolltable class structure.
#' Minimal checks for validity.
new_rolltable <- function(cmd, tbl) {
  stopifnot(is.data.frame(tbl))
  stopifnot(is.character(cmd))

  structure(
    .Data = tbl,
    command = cmd,
    class = c("rolltable", "rollr", "data.frame")  # rollr virtual class needed for Ops methods.
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

#' @export
roll.rolltable <- function(tbl, repetitions = 1, verbose = FALSE) {
  calc_fn <- ifelse(verbose, rollr:::calculate_dice_table, function(...) suppressMessages(rollr:::calculate_dice_table(...)))

  # add repetitions, if any
  n_orig <- length(attr(tbl, "command"))
  rollhist <- tbl[rep(1:n_orig, each = repetitions), , drop = FALSE]
  rollhist$Repetition <- rep(1:repetitions, times = n_orig)
  calc_fn(rollhist)
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

#' @export
calculate.rolltable <- function(tbl, .summary_fn = "identity") {
  out <- parse_result(tbl, .summary_fn = get(.summary_fn))
  new_rolltable_calculation(out)
}

#' Number of repetitions in a rolltable
#'
#' @param tbl A rolltable class.
#' @return Integer value indicating the number of repetitions.
#' @export
repetitions <- function(tbl, ...) { UseMethod("repetitions") }

#' @export
repetitions.rolltable <- function(tbl, ...) { max(tbl$Repetition) }

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
print.rolltable <- function(tbl, n = 10, rolls = FALSE, digits = 1, ...) {
  res <- mean(tbl)
  repetitions <- max(tbl$Repetition)

  # format so that the dice and means line up.
  res <- round(res, digits = digits)
  field_widths <- pmax(nchar(attr(tbl, "command")),
                        nchar(res))

  field_s <- paste(paste0("%", field_widths, "s"), collapse = " ")
  field_f <- paste(paste0("%", field_widths, ".", digits, "f"), collapse = " ")

  cat("Dice:", do.call(sprintf, args = c(list(fmt = field_s), as.list(attr(tbl, "command")))), "\n")
  cat("Mean:", do.call(sprintf, args = c(list(fmt = field_f), as.list(res))), "\n")

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

    max_repetition <- min(repetitions, n)
    iteration_s <- paste0("%0", ceiling(max_repetition / 10), "d")
    for(i in seq_len(nrow(tbl))) {
      if(tbl$Repetition[[i]] > max_repetition) next;
      cat("----------\n")
      i_str <- sprintf(iteration_s, tbl$Repetition[[i]])
        cat(sprintf("%s. %s base rolls: %s\n", i_str, tbl$Die[[i]], paste(tbl$Base.Roll[[i]], collapse = ", ")))

        if(tbl$Type[[i]] != "none" & tbl$Type[[i]] != "simple") {
          cat(sprintf("%s. %s: %s\n", i_str, stringr::str_to_title(tbl$Type[[i]]), paste(tbl$Calculated.Roll[[i]], collapse = ", ")))
        }

        if(!is.na(tbl$Success[[i]])) {
          cat(sprintf("%s. Number successes: %s\n", i_str, paste(tbl$Success.Outcome[[i]], collapse = ", ")))
        }
    }

    if(max_repetition < repetitions) cat("...\n")

  }

  cat("======\n")
}

#' Helper function to change the number of digits for a string with numerals
trim_numeric_string <- function(str, digits = 2) {
  p <- sprintf("([[:digit:]]+[.])([[:digit:]]{%d})[[:digit:]]+", digits)
  stringr::str_replace_all(str, pattern = p, replacement = "\\1\\2")
}

