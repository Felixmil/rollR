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
    parsed_cmd <- parse_roll_cmd(cmd)
    if(length(parsed_cmd$dices) > 1) stop(error_txt)
  }

  tbl <- new_rolltable(tbl = construct_dice_table(cmd),
                       cmd = cmd)
  roll(tbl, repetitions = repetitions, verbose = verbose)
}

#' Constructor for rolltable
#'
#' Internal function to build the rolltable class structure.
#' Minimal checks for validity.
#'
#' @param cmd Character vetor. Intended to be one or more dice set commands.
#' @param tbl Dataframe to convert to rolltable.
#' @return Rolltable class object.
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
#' Method for rolling dice.
#'
#' @param obj Object to roll
#' @param ... For use by class methods
#' @return An updated rolltable.
#' @export
#'

roll <- function(obj, ...) { UseMethod("roll") }


#' Roll or reroll a rolltable
#'
#' Any existing repetitions will be removed and all rolls re-done.
#' @param obj A rolltable.
#' @param repetitions Number of times to roll the command.
#' @param verbose If TRUE, dice roll details are visible in the console.
#' @param ... Unused.
#' @export
#'
#' @examples
#'
#' tbl <- rolltable("2d20h1"); tbl
#' roll(tbl, repetitions = 3)
#' roll(tbl, repetitions = 2, verbose = TRUE)
#'
roll.rolltable <- function(obj, repetitions = 1, verbose = FALSE, ...) {
  calc_fn <- ifelse(verbose, calculate_dice_table, function(...) suppressMessages(calculate_dice_table(...)))

  # add repetitions, if any
  n_orig <- length(attr(obj, "command"))
  rollhist <- obj[rep(1:n_orig, each = repetitions), , drop = FALSE]
  rollhist$Repetition <- rep(1:repetitions, times = n_orig)
  calc_fn(rollhist)
}

#' Method to calculate the current value of a rolltable
#'
#' @param obj Table to calculate, such as a rolltable.
#' @param ... For use by class methods
#' @return A numeric vector whose names are the individual sums for each dice set.
#' @export
#'
#' @examples
#'
#' tbl <- rolltable("2d20h1")
#' calculate(tbl)
#' calculate(tbl, .summary_fn = "median")
calculate <- function(obj, ...) { UseMethod("calculate") }

#' Calculate a rolltable
#'
#' Summarizes the provided summary function over repetitions, groups by Die if more than one in the table.
#'
#' @param obj A rolltable.
#' @param .summary_fn A character string providing the name of a summary function, such as mean or median, or the function itself.
#' @param ... Unused.
#' @return A rolltable_calculation.
#' @export
calculate.rolltable <- function(obj, .summary_fn = "identity", ...) {
  if(is.character(.summary_fn)) .summary_fn <- get(.summary_fn, envir = parent.frame(), mode = "function")

  out <- parse_result(obj, .summary_fn = .summary_fn)
  new_rolltable_calculation(out)
}

#' Helper function for calculate to apply a summary function to a rolltable.
#' @param tbl Rolltable
#' @param .summary_fn A summary function to apply
#' @return A summarized result generated using the by function.
parse_result <- function(tbl, .summary_fn = identity) {
  stopifnot(inherits(tbl, "rolltable"))
  # result is a list one longer than operators
  # Each result element is a table with 1 or more rows for a single set of dice rolls
  # first choose either the calculated roll or success if available, and sum
  tbl$Result <- tbl$Calculated.Roll
  success_idx <- !sapply(tbl$Success.Outcome, is.na)
  tbl$Result[success_idx] <- tbl$Success.Outcome[success_idx]

  # sum each set of dice rolls or successes
  tbl$Result <- sapply(tbl$Result, sum, na.rm = TRUE)

  # summarize by die type if more than one type
  by(tbl$Result, INDICES = tbl$Die, FUN = .summary_fn)
}


#' Number of repetitions in a rolltable
#'
#' @param tbl A rolltable class.
#' @param ... For use by class methods.
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
#' @param ... Unused.
#' @return A numeric vector whose names are the individual means for each dice set.
#' @export
mean.rolltable <- function(x, ...) {
  calculate(x, .summary_fn = "mean")
}

#' Median rolltable
#'
#' Determines the median over repetitions for the rolls.
#' Calculates the total by applying the operators to each median value.
#'
#' @param x A rolltable class.
#' @param na.rm Unused. For consistency with \code{\link[stats]{median}}.
#' @param ... Unused. For consistency with \code{\link[stats]{median}}.
#' @return A numeric vector whose names are the individual means for each dice set.
#' @export
#' @importFrom stats median
median.rolltable <- function(x, na.rm = FALSE, ...) {
  calculate(x, .summary_fn = "median")
}



#' Min rolltable
#'
#' Determines the minimum over repetitions for the rolls.
#' Calculates the total by applying the operators to each minimum.
#'
#' @param ... A rolltable class.
#' @param na.rm Unused. For consistency with \code{\link[base]{min}}.
#' @return A numeric vector whose names are the individual minimums for each dice set.
#' @export
min.rolltable <- function(..., na.rm = FALSE) {
  args <- list(...)
  if(length(args) > 1) stop("Min for rolltables currently does not handle multiple arguments.")
  calculate(args[[1]], .summary_fn = "min")
}

#' Max rolltable
#'
#' Determines the maximum over repetitions for the rolls.
#' Calculates the total by applying the operators to each maximum.
#'
#' @param ... A rolltable class.
#' @param na.rm Unused. For consistency with \code{\link[base]{min}}.
#' @return A numeric vector whose names are the individual maximums for each dice set.
#' @export
max.rolltable <- function(..., na.rm = FALSE) {
  args <- list(...)
  if(length(args) > 1) stop("Max for rolltables currently does not handle multiple arguments.")
  calculate(args[[1]], .summary_fn = "max")
}

#' Print a rolltable
#'
#' @param x A rolltable class.
#' @param n Number of repetitions to print.
#' @param rolls If TRUE, print the base rolls and modifications to rolls, such as keep highest or exploding.
#' @param digits Round numbers to this number of digits when printing.
#' @param ... Unused.
#' @export
print.rolltable <- function(x, n = 10, rolls = FALSE, digits = 1, ...) {
  res <- mean(x)
  repetitions <- max(x$Repetition)

  # format so that the dice and means line up.
  res <- round(res, digits = digits)
  field_widths <- pmax(nchar(attr(x, "command")),
                        nchar(res))

  field_s <- paste(paste0("%", field_widths, "s"), collapse = " ")
  field_f <- paste(paste0("%", field_widths, ".", digits, "f"), collapse = " ")

  cat("Dice:", do.call(sprintf, args = c(list(fmt = field_s), as.list(attr(x, "command")))), "\n")
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
    for(i in seq_len(nrow(x))) {
      if(x$Repetition[[i]] > max_repetition) next;
      cat("----------\n")
      i_str <- sprintf(iteration_s, x$Repetition[[i]])
        cat(sprintf("%s. %s base rolls: %s\n", i_str, x$Die[[i]], paste(x$Base.Roll[[i]], collapse = ", ")))

        if(x$Type[[i]] != "none" & x$Type[[i]] != "simple") {
          cat(sprintf("%s. %s: %s\n", i_str, stringr::str_to_title(x$Type[[i]]), paste(x$Calculated.Roll[[i]], collapse = ", ")))
        }

        if(!is.na(x$Success[[i]])) {
          cat(sprintf("%s. Number successes: %s\n", i_str, paste(x$Success.Outcome[[i]], collapse = ", ")))
        }
    }

    if(max_repetition < repetitions) cat("...\n")

  }

  cat("======\n")
}


