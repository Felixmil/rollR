#' Rolltable calculation class
#'
#' Class returned when calculating a rolltable.
#' This inherits from the "by" class returned when using the \code{\link[base]{by}} function.
#' Typically, this function is not needed by the user, who instead should rely on \code{\link{calculate}}.
#'
#' @param lst A list object returned using the "by" function.
#' @return A rolltable_calculation classed object.
#'
#' @export
new_rolltable_calculation <- function(lst, n = names(lst)) {
  # ignoring the call attribute as it is not useful for rolltables.
  # these may be already set if lst is a "by" object.
  force(n) # needed to avoid circular promise when reassigning names below
  attr(lst, "dim") <- length(lst)
  attr(lst, "dimnames") <- list("Die" = n)
  class(lst) <- c("rolltable_calculation", "rollr", "by") # rollr virtual class needed for Ops methods.
  return(lst)
}

#' @export
repetitions.rolltable_calculation <- function(tbl, ...) { length(tbl[[1]]) }


#' S3 Group Generic Ops for rolltable_calculation and rolltable
#'
#' Permits mathemetical operations on rolltables or rolltable calculations in certain circumstances.
#' Namely, the number of repetitions must match between objects, or one object is a length-one vector.
#'
#' In all valid instances, the rolltable or rolltables are first calculated.
#' If there are multiple repetitions, these are all kept separate.
#' A rolltable can only be added, etc. to another rolltable with a comparable number of repetitions.
#' Repetitions will be increased to match, which will cause the rolltable to be rerolled.
#' If dies differ within a rolltable, each will be treated separately.
#'
#' @export
Ops.rollr <- function(e1, e2 = NULL) {
  # The rollr class is used similarly to how POSIXt class is used, to permit operations on rolltables or rolltable calculations, or both.
  # See, e.g., https://stackoverflow.com/questions/43066501/s3-operator-overloading-for-multiple-classes
  # The end result of an operation will always be a rolltable_calculation.

  # mostly copied from Ops.data.frame
  unary <- nargs() == 1L
  lclass <- nzchar(.Method[1L])
  rclass <- !unary && (nzchar(.Method[2L]))
  FUN <- get(.Generic, envir = parent.frame(), mode = "function")
  # f <- if (unary) { quote(FUN(left)) } else { quote(FUN(left, right)) }

  if(lclass && rclass) {
    # both objects are rolltables or rolltable calculations.
    rep1 <- repetitions(e1)
    rep2 <- repetitions(e2)
    if(rep1 != rep2 && rep1 != 1 && rep2 != 1) {
      # conform repetitions
      if(inherits(e1, "rolltable") && inherits(e2, "rolltable")) {
        warning("Number of repetitions do not match. They will be increased accordingly, which will cause the smaller table to be rerolled.")
        n_reps <- max(rep1, rep2)
        if(rep1 != n_reps) e1 <- roll(e1, repetitions = n_reps)
        if(rep2 != n_reps) e2 <- roll(e2, repetitions = n_reps)
      } else if(inherits(e1, "rolltable")) {
        warning("Number of repetitions do not match. They will be increased accordingly, which will cause the rolltable to be rerolled.")
        e1 <- roll(e1, repetitions = rep2)
      } else if(inherits(e2, "rolltable")) {
        e2 <- roll(e2, repetitions = rep1)
      } else stop("Should not be here.")
    }

  } else if(unary) {
    # no adjustment to repetitions needed


  } else if(lclass) {
    # e1 is rolltable or rolltable_calculation; e2 is vector
    if(length(e2) != 1 && repetitions(e1) != length(e2) && inherits(e1, "rolltable")) {
      warning("Number of repetitions do not match length of the vector. They will be increased accordingly, which will cause the rolltable to be rerolled.")
      e1 <- roll(e1, repetitions = length(e2))
    }

  } else if(rclass) {
    # e2 is rolltable or rolltable_calculation; e1 is vector
    if(length(e1) != 1 && repetitions(e2) != length(e1) && inherits(e2, "rolltable")) {
      warning("Number of repetitions do not match length of the vector. They will be increased accordingly, which will cause the rolltable to be rerolled.")
      e2 <- roll(e2, repetitions = length(e1))
    }

  } else stop("Should not be here.")

  # apply calculation so all that is left are either rolltable_calculations or vectors
  if(inherits(e1, "rolltable")) e1 <- calculate(e1)
  if(inherits(e2, "rolltable")) e2 <- calculate(e2)

  if(length(e1) != length(e2) &&
     length(e1) > 1 &&
     length(e2) > 1) stop("Rolltable calculations must have the same number of Dies or a single die.")

  if(unary) {
    value <- lapply(e1, FUN)
    new_labels <- paste0(.Generic, names(e1))

  } else if(lclass && rclass) {
    # both e1 and e2 are rolltable calculations
    value <- mapply(FUN, e1, e2, SIMPLIFY = FALSE)
    new_labels <- paste0("(", names(e1), " ", .Generic, " ", names(e2), ")")

  } else if(lclass) {
    # e1 is a rolltable calculation.
    # apply e2 to each grouping of e1 equally
    # might throw an error if the e2 length is not compatible with the group length (repetitions) for e1
    # may also throw an error if the e2 vector is of a non-compatible class, such as character.
    value <- lapply(e1, FUN, e2 = e2)
    new_labels <- paste0("(", names(e1), " ", .Generic, " ", paste(as.character(e2), collapse = "|"), ")")

  } else if(rclass) {
    # e2 is a rolltable calculation. Treated as mirror of lclass.
    value <- lapply(e2, FUN, e1 = e1)
    new_labels <- paste0("(", paste(as.character(e1), collapse = "|"), " ", .Generic, " ", names(e2), ")")

  } else stop("Should not be here.")

  new_rolltable_calculation(value, new_labels)

  # return(paste(class(e1), .Generic, class(e2)))
}


