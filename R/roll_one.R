# should be able to handle all of the following, in one go
# die <- tolower(c("1d6", "10d6", "20d20", "10", "2d20h1", "3d10h2", "2d20l1", "1d20r1",  "3d6!", "2d6>=5", "4d6=5", "4dF", "1d10!>9", "3d10!>=8", "1d10t10"))

construct_dice_table <- function(die) {
  cbind(Die = die,
        Repetition = 1,
        detect_dice(die),
        detect_dice_type(die)[,-1], # lose the Die column
        detect_success_test(die)[,-1], # lose the Die column
        stringsAsFactors = FALSE)
}

calculate_dice_table <- function(dice_tbl, verbose = FALSE) {
  dice_tbl <- roll_base_dice(dice_tbl)
  dice_tbl <- calculate_types(dice_tbl)
  dice_tbl <- calculate_successes(dice_tbl)
  return(dice_tbl)
}


roll_base_dice <- function(dice_tbl) {
  dice_tbl$Base.Roll <- NA
  no_dice <- is.na(dice_tbl$N.Dice)
  fate_dice <- dice_tbl$Sides == "f" & !no_dice
  simple_dice <- !no_dice & !fate_dice

  if(any(no_dice)) dice_tbl$Base.Roll[no_dice] <- as.integer(dice_tbl$Dice.String[no_dice])
  if(any(simple_dice))
    dice_tbl$Base.Roll[simple_dice] <- mapply(sample.int,
                                              n = as.integer(dice_tbl$Sides[simple_dice]),
                                              size = as.integer(dice_tbl$N.Dice[simple_dice]),
                                              replace = TRUE,
                                              SIMPLIFY = FALSE)
  if(any(fate_dice))
    dice_tbl$Base.Roll[fate_dice] <- mapply(sample,
                                            size = as.integer(dice_tbl$N.Dice[fate_dice]),
                                            replace = TRUE,
                                            SIMPLIFY = FALSE,
                                            MoreArgs = list(x = c(-1, 0, 1)))

  iteration_s <- paste0("%0", ceiling(nrow(dice_tbl) / 10), "d")
  for(i in seq_len(nrow(dice_tbl))) {
    i_str <- sprintf(iteration_s, i)
    message(sprintf("%s. %s Base roll(s): %s", i_str, dice_tbl$Die[[i]], paste(dice_tbl$Base.Roll[[i]], collapse = ", ")))
  }

  return(dice_tbl)
}

calculate_types <- function(dice_tbl) {
  dice_tbl$Calculated.Roll <- vector("list", nrow(dice_tbl))

  iteration_s <- paste0("%0", ceiling(nrow(dice_tbl) / 10), "d")
  for(i in seq_len(nrow(dice_tbl))) {
    type <- dice_tbl$Type[[i]]
    if(type %in% names(dice_modification_types)) {
      # modify the base roll in some fashion
      i_str <- sprintf(iteration_s, i)
      calculation_fn <- dice_modification_types[[type]]$calculate
      dice_tbl$Calculated.Roll[[i]] <- calculation_fn(base_roll = dice_tbl$Base.Roll[[i]],
                                                      match = dice_tbl$Type.Match[[i]],
                                                      sides = dice_tbl$Sides[[i]],
                                                      i_str = i_str)
    } else {
      # no modification required; use the base roll
      dice_tbl$Calculated.Roll[[i]] <- dice_tbl$Base.Roll[[i]]
    }
  }

  return(dice_tbl)
}

calculate_successes <- function(dice_tbl) {
  dice_tbl$Success.Outcome <- NA

  iteration_s <- paste0("%0", ceiling(nrow(dice_tbl) / 10), "d")
  for(i in seq_len(nrow(dice_tbl))) {
    type <- dice_tbl$Success[[i]]
    if(dice_tbl$Success[[i]] %in% names(success_types) & !is.na(dice_tbl$Success[[i]])) {
      i_str <- sprintf(iteration_s, i)
      calculation_fn <- success_types[[type]]$calculate
      dice_tbl$Success.Outcome[[i]] <- calculation_fn(base_roll = dice_tbl$Calculated.Roll[[i]],
                                                      match = dice_tbl$Success.Match[[i]],
                                                      i_str = i_str)
    }
  }
  return(dice_tbl)
}

detect_dice <- function(die) {
  pattern = SIMPLE_DIE_PATTERN
  res <- stringr::str_match(die, pattern)
  if(anyNA(res[,1])) stop("Dice pattern not recognized.")

  res <- as.data.frame(res, stringsAsFactors = FALSE)
  colnames(res) <- c("Dice.String", "N.Dice", "Sides")
  return(res)
}


detect_dice_type <- function(die) {
  res <- data.frame(Die = die, stringsAsFactors = FALSE)
  res$Type <- NA
  res$Type.Match <- vector("list", length(die)) # to accommodate patterns that return multiple sub-matches

  for(r in dice_modification_types) {
    idx <- stringr::str_detect(die, pattern = r$pattern)
    if(any(idx)) res$Type[idx] <- r$name
  }

  # rest should be simple or none
  idx <- stringr::str_detect(res$Die[is.na(res$Type)], simple$pattern)
  res$Type[is.na(res$Type)][idx] <- simple$name

  idx <- stringr::str_detect(res$Die[is.na(res$Type)], none$pattern)
  res$Type[is.na(res$Type)][idx] <- none$name

  stopifnot(!anyNA(res$Type))

  # match the pattern
  # Could probably do in the previous loop by modification type (and would be faster), but a bit easier to control here.
  # pattern can return more than one item; should be converted to a character vector
  for(i in seq_len(length(die))) {
    mod_type <- res$Type[[i]]
    if(mod_type %in% c(simple$name, none$name)) next;
    # if(is.na(mod_type)) next; # should not actually occur, as simple or none should pick up everything else

    pattern <- dice_modification_types[[mod_type]]$pattern
    res$Type.Match[[i]] <- stringr::str_match(string = res$Die[[i]], pattern = pattern)[,-1]
  }

  return(res)
}

detect_success_test <- function(die) {
  res <- data.frame(Die = die, stringsAsFactors = FALSE)
  res$Success <- NA
  res$Success.Match <- vector("list", length(die)) # to accommodate patterns that return multiple sub-matches

  for(s in success_types) {
    idx <- stringr::str_detect(die, s$pattern)
    if(any(idx)) res$Success[idx] <- s$name
  }

  for(i in seq_len(length(die))) {
    s_type <- res$Success[[i]]
    if(is.na(s_type)) next;
    pattern <- success_types[[s_type]]$pattern
    res$Success.Match[[i]] <- stringr::str_match(string = res$Die[[i]], pattern = pattern)[, -1]
  }

  return(res)
}

# These should all probably be classes, or a single class with various values...
# Simple die patterns ####
# • None ####
none <- list(name = "none",
             pattern = "^\\d+$",
             calculate = function(base_roll, ...) { base_roll } )

# • Simple ####
simple <- list(name = "simple",
               pattern = "^(\\d+)d(\\d+|f)",
               calculate = function(base_roll, ...) { base_roll } )

SIMPLE_DIE_PATTERN <- paste(simple$pattern, none$pattern, sep = "|")

# Modification patterns ####
# don't use $ to close the pattern as the pattern may also contain success test
# dots in the calculate argument permit additional arguments to be passed to some of the functions; otherwise ignored.
# • Keep highest ####
keep_h <- list(name = "keep highest",
              pattern = "^\\d+d\\d+h(\\d+)",
              calculate = function(base_roll, match, i_str = "1", ...) {
                out <- sort(base_roll, decreasing = TRUE)[1:as.integer(match)]
                message(i_str, '. keeping ', match, " highest(s): ", paste(out[1:as.integer(match)], collapse = ', '))
                return(out)
                })

# • Keep lowest ####
keep_l <- list(name = "keep lowest",
              pattern = "^\\d+d\\d+l(\\d+)",
              calculate = function(base_roll, match, i_str = "1", ...) {
                out <- sort(base_roll, decreasing = FALSE)[1:as.integer(match)]
                message(i_str, '. keeping ', match, " lowest(s): ", paste(out[1:as.integer(match)], collapse = ', '))
                return(out)
                })

# • Reroll ####
reroll <- list(name = "reroll",
               pattern = "^\\d+d\\d+r(\\d+)",
               calculate = function(base_roll, match, sides, i_str = "1", ...) {
                 sides <- as.integer(sides)
                 match <- as.integer(match)
                 idx <- base_roll == match
                 if(any(idx)) {
                   base_roll[idx] <- sample.int(sides, size = sum(idx), replace = TRUE)
                   message(i_str, '. rerolling ', sum(idx), ' dice: ', paste(base_roll[idx], collapse = ', '))
                 }
                 return(base_roll)
               })

# • Double ####
double <- list(name = "double",
               pattern = "^\\d+d\\d+t(\\d+)",
               calculate = function(base_roll, match, i_str = "1", ...) {
                 match <- as.integer(match)
                 idx <- base_roll == match
                 if(any(idx)) {
                   message(i_str, '. doubling ', sum(idx), ' dice: ', paste(base_roll[idx], collapse = ', '))
                   base_roll <- c(base_roll, rep.int(match, times = sum(idx)))
                 }
                 return(base_roll)
               })

# • Exploding ####
exploding <- list(name = "exploding",
                  #pattern = "^\\d+d\\d+\\!(?:((?:>=|<=|>|<|=)\\d+))?",
                  pattern = "^\\d+d\\d+\\!(?:(>=|<=|>|<|=)(\\d+))?",
                  calculate = function(base_roll, match, sides, i_str = "1", ...) {
                    stopifnot(length(match) == 2)
                    sym <- match[[1]]
                    test_num <- as.integer(match[[2]])
                    sides <- as.integer(sides)

                    if(is.na(test_num)) {
                      # explode if the die equals the highest potential roll
                      test_num <- sides
                      FUN <- get("==", envir = parent.frame(), mode = "function")
                    } else {
                      # test could be >=, <=, >, <, or =
                      # = must be converted to ==
                      stopifnot(sym %in% c(">=", "<=", ">", "<", "="))
                      if(sym == "=") sym <- "=="
                      FUN <- get(sym, envir = parent.frame(), mode = "function")

                      if(sym == ">=" && test_num == 1) stop("Exploding would cause an infinite loop.")
                      if(sym == "<=" && test_num == sides) stop("Exploding would cause an infinite loop.")
                    }

                    num_exploded <- sum(FUN(base_roll, test_num))
                    while(num_exploded > 0) {
                      new_roll <- sample.int(sides, size = num_exploded, replace = TRUE)
                      message(i_str, ". exploding ", num_exploded, " dice. New roll(s): ", paste(new_roll, collapse = ", "))
                      num_exploded <- sum(FUN(new_roll, test_num))
                      base_roll <- c(base_roll, new_roll)
                    }
                    return(base_roll)
                    })

dice_modification_types <- list(keep_h, keep_l, reroll, double, exploding)
names(dice_modification_types) <- sapply(dice_modification_types, function(x) x$name)

# Success patterns ####
# • Greater/Less Than ####
ge_success <- list(name = "success ge",
                   pattern = "[^!]([><][=]?)(\\d+)$",
                   calculate = function(base_roll, match, i_str = "1", ...) {
                     stopifnot(length(match) == 2,
                               match[[1]] %in% c(">=", "<=", ">", "<"))
                     test_num <- as.integer(match[[2]])
                     FUN <- get(match[[1]], envir = parent.frame(), mode = "function")
                     out <- sum(FUN(base_roll, test_num))
                     message(i_str, ". Number of successes: ", out)
                     out
                     })

# • Equality ####
equal_success <- list(name = "success equal",
                      pattern = "[^!><][=](\\d+)$",
                      calculate = function(base_roll, match, i_str = "1", ...) {
                        out <- sum(base_roll == as.integer(match))
                        message(i_str, ". Number of successes: ", out)
                        out
                        })
success_types <- list(ge_success, equal_success)
names(success_types) <- sapply(success_types, function(x) x$name)
