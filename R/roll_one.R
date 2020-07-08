#' Roll One
#'
#' @description Roll one die from string command.
#'
#' @param roll a string corresponding to a roll command.
#'
#' @return result of the roll
roll_one <- function(roll){
  for (r in roll_types) { # we try rolls patterns one by one
    detected = stringr::str_detect(roll,r$pattern)
    if (detected) { # when a pattern matches
      match = stringr::str_match(roll, r$pattern)
      result = r$compute(match) # it is possible to make the roll and apply its rules
      return(result)
    }
  }
  warning("This roll command is not recognized")
}

# die <- tolower(c("1d6", "10d6", "20d20", "10", "2d20h1", "3d10h2", "2d20l1", "1d20r1",  "3d6!", "2d6>=5", "4d6=5", "4dF", "1d10!>9", "3d10!>=8", "1d10t10"))



calculate_dice_table <- function(dice_tbl) {
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



  # message(sprintf("rolls: \n%s",
  #                 sprintf("\t%001d")
  #                 )
  #
  #   'rolls: ',
  #
  #         paste(rolls, collapse = ', '))

  return(dice_tbl)
}

calculate_types <- function(dice_tbl) {
  dice_tbl$Calculated.Roll <- NA

  type.lst <- c(dice_modification_types, list(simple = simple, none = none))
  for(type in dice_tbl$Type) {
    type_idx <- dice_tbl$Type == type

    if(any(type_idx)) {
      calculation_fn <- type.lst[[type]]$calculate
      dice_tbl$Calculated.Roll[type_idx] <- mapply(calculation_fn,
                                               base_roll = dice_tbl$Base.Roll[type_idx],
                                               match = dice_tbl$Type.Match[type_idx],
                                               sides = dice_tbl$Sides[type_idx],
                                               SIMPLIFY = FALSE)
    }
  }
  return(dice_tbl)
}

calculate_successes <- function(dice_tbl) {
  dice_tbl$Success.Outcome <- NA
  type.lst <- c(dice_modification_types, list(simple = simple, none = none))

  for(success_type in success_types) {
    type_idx <- dice_tbl$Success == success_type$name & !is.na(dice_tbl$Success)

    if(any(type_idx)) {
      calculation_fn <- success_type$calculate
      dice_tbl$Success.Outcome[type_idx] <- mapply(calculation_fn,
                                                   base_roll = dice_tbl$Calculated.Roll[type_idx],
                                                   match = dice_tbl$Success.Match[type_idx],
                                                   SIMPLIFY = FALSE)
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
  res$Type.Match <- NA

  for(r in dice_modification_types) {
    idx <- stringr::str_detect(die, pattern = r$pattern)
    if(any(idx)) {
      res$Type[idx] <- r$name

      # match, but lose the initial repetion of the full pattern
      match_fn <- function(string, pattern) {
        stringr::str_match(string = string, pattern = pattern)[, -1]
      }
      res$Type.Match[idx] <- mapply(match_fn, string = die[idx], pattern = r$pattern, SIMPLIFY = FALSE)
    }
  }

  # rest should be simple or none
  idx <- stringr::str_detect(res$Die[is.na(res$Type)], simple$pattern)
  res$Type[is.na(res$Type)][idx] <- simple$name

  idx <- stringr::str_detect(res$Die[is.na(res$Type)], none$pattern)
  res$Type[is.na(res$Type)][idx] <- none$name

  stopifnot(!anyNA(res$Type))
  return(res[, -1, drop = FALSE])
}

detect_success_test <- function(die) {
  res <- data.frame(Die = die, stringsAsFactors = FALSE)
  res$Success <- NA
  res$Success.Match <- NA

  for(s in success_types) {
    idx <- stringr::str_detect(die, s$pattern)
    if(any(idx)) {
    res$Success[idx] <- s$name
    match_fn <- function(string, pattern) {
      stringr::str_match(string = string, pattern = pattern)[, -1]
    }

    res$Success.Match[idx] <- mapply(match_fn, string = die[idx], pattern = s$pattern, SIMPLIFY = FALSE)
    }
  }

  return(res[, -1, drop = FALSE])
}

none <- list(name = "none",
             pattern = "^\\d+$",
             calculate = function(base_roll, ...) { base_roll } )

simple <- list(name = "simple",
               pattern = "^(\\d+)d(\\d+|f)",
               calculate = function(base_roll, ...) { base_roll } )

SIMPLE_DIE_PATTERN <- paste(simple$pattern, none$pattern, sep = "|")

# don't use $ to close the pattern as the pattern may also contain success test
# dots in the calculate argument permit additional arguments to be passed to some of the functions; otherwise ignored.
keep_h <- list(name = "keep highest",
              pattern = "^\\d+d\\d+h(\\d+)",
              calculate = function(base_roll, match, ...) { sort(base_roll, decreasing = TRUE)[1:as.integer(match)] })

keep_l <- list(name = "keep lowest",
              pattern = "^\\d+d\\d+l(\\d+)",
              calculate = function(base_roll, match, ...) { sort(base_roll, decreasing = FALSE)[1:as.integer(match)] })

reroll <- list(name = "reroll",
               pattern = "^\\d+d\\d+r(\\d+)",
               calculate = function(base_roll, match, sides, ...) {
                 sides <- as.integer(sides)
                 match <- as.integer(match)
                 idx <- base_roll == match
                 if(any(idx)) {
                   base_roll[idx] <- sample.int(sides, size = sum(idx), replace = TRUE)
                 }
                 return(base_roll)
               })

double <- list(name = "double",
               pattern = "^\\d+d\\d+t(\\d+)",
               calculate = function(base_roll, match, ...) {
                 match <- as.integer(match)
                 idx <- base_roll == match
                 if(any(idx)) {
                   base_roll <- c(base_roll, rep.int(match, times = sum(idx)))
                 }
                 return(base_roll)
               })

exploding <- list(name = "exploding",
                  pattern = "^\\d+[dD]\\d+\\!(?:[>](\\d+))?",
                  calculate = function(base_roll, match, sides, ...) {
                    match <- as.integer(match)
                    sides <- as.integer(sides)
                    explode_test <- ifelse(is.na(match), sides, match:sides)
                    num_exploded <- sum(base_roll %in% explode_test)
                    while(num_exploded > 0) {
                      new_roll <- sample.int(sides, size = num_exploded, replace = TRUE)
                      num_exploded <- sum(new_roll %in% explode_test)
                      base_roll <- c(base_roll, new_roll)
                    }
                    return(base_roll)
                    })

dice_modification_types <- list(keep_h, keep_l, reroll, double, exploding)
names(dice_modification_types) <- sapply(dice_modification_types, function(x) x$name)


ge_success <- list(name = "success ge",
                   pattern = "[>][=](\\d+)$",
                   calculate = function(base_roll, match, ...) { sum(base_roll >= as.integer(match)) })
equal_success <- list(name = "success equal",
                      pattern = "[^>][=](\\d+)$",
                      calculate = function(base_roll, match, ...) { sum(base_roll == as.integer(match)) })
success_types <- list(ge_success, equal_success)
names(success_types) <- sapply(success_types, function(x) x$name)

# no_dice = list(pattern = "^\\d+$",
#                compute = function(match) {
#                  result = match[1]
#                  return(result)
#                })
#
# simple = list(pattern = "^(\\d+)[dD](\\d+)$",
#               compute = function(match) {
#                 n = match[2]
#                 sides = match[3]
#                 rolls = sample(1:sides, n, replace = TRUE)
#                 message('rolls: ', paste(rolls, collapse = ', '))
#                 result = sum(rolls)
#               })
#
# keep_h = list(pattern = "^(\\d+)[dD](\\d+)[Hh](\\d+)$",
#               compute = function(match) {
#                 n = match[2]
#                 sides = match[3]
#                 kept = match[4]
#                 rolls = sample(1:sides, n, replace = TRUE)
#                 message('rolls: ', paste(rolls, collapse = ', '))
#                 kept_dice = sort(rolls, decreasing = T)[1:as.numeric(kept)]
#                 message('keeping ',kept, " highest(s): ", paste(kept_dice, collapse = ', '))
#                 result =  sum(kept_dice)
#               })
#
# keep_l = list(pattern = "^(\\d+)[dD](\\d+)[Ll](\\d+)$",
#               compute = function(match) {
#                 n = match[2]
#                 sides = match[3]
#                 kept = match[4]
#                 rolls = sample(1:sides, n, replace = TRUE)
#                 message('rolls: ', paste(rolls, collapse = ', '))
#                 kept_dice = sort(rolls)[1:as.numeric(kept)]
#                 message('keeping ',kept, " lowest(s): ", paste(kept_dice, collapse = ', '))
#                 result =  sum(kept_dice)
#               })
#
# exploding = list(pattern ="^(\\d+)[dD](\\d+)\\!$",
#                  compute = function(match) {
#                   n = match[2]
#                   sides = match[3]
#                   rolls = sample(1:sides, n, replace = TRUE)
#                   explode = rolls[rolls == sides]
#                   message('rolls: ', paste(rolls, collapse = ', '))
#                   message("exploding ", length(explode),' dice...')
#                   while (length(explode) != 0) {
#                     new_rolls = sample(1:sides, length(explode), replace = TRUE)
#                     message('new rolls : ', paste(new_rolls, collapse = ', '))
#                     rolls = c(rolls, new_rolls)
#                     explode = new_rolls[new_rolls==sides]
#                     if (length(explode) != 0) { message("exploding ", length(explode),' dice...') }
#                   }
#                   result = sum(rolls)
#                  })
#
# reroll = list(pattern = "^(\\d+)[dD](\\d+)[rR](\\d+)$",
#               compute = function(match) {
#                 n = match[2]
#                 sides = match[3]
#                 to_reroll = match[4]
#                 rolls = sample(1:sides, n, replace = TRUE)
#                 message('rolls: ', paste(rolls, collapse = ', '))
#                 reroll = rolls[rolls == to_reroll]
#                 message("rerolling ",length(reroll),' dice')
#                 while (length(reroll) != 0) {
#                   new_rolls = sample(1:sides, length(reroll), replace = TRUE)
#                   message('new rolls : ', paste(new_rolls, collapse = ', '))
#                   rolls[rolls == to_reroll] = new_rolls
#                   reroll = rolls[rolls == to_reroll]
#                   if (length(reroll) != 0) { message("rerolling ",length(reroll),' dice')}
#                 }
#                 result = sum(rolls)
#               })
#
# success = list(pattern = "^(\\d+)[dD](\\d+) ?([<>]?=?) ?(\\d+)$",
#                compute = function(match) {
#                  n = match[2]
#                  sides = match[3]
#                  comparator = match[4]
#                  if (comparator == "=") {comparator="=="}
#                  threshold = match[5]
#                  rolls = sample(1:sides, n, replace = TRUE)
#                  message('rolls: ', paste(rolls, collapse = ', '))
#                  success = eval(parse(text = paste("rolls[rolls",comparator,"threshold]")))
#                  result = length(success)
#                  message('number of success: ',
#                          result ,
#                          ' (', paste(sort(success,decreasing = TRUE), collapse = ', '),')')
#                  return(result)
#                  })
#
# roll_types = list(
#   no_dice = no_dice,
#   simple = simple,
#   keep_h = keep_h,
#   keep_l = keep_l,
#   exploding = exploding,
#   reroll = reroll,
#   success = success,
#   reroll = reroll
# )
