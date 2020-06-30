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


no_dice = list(pattern = "^\\d+$",
               compute = function(match) {
                 result = match[1]
                 return(result)
               })

simple = list(pattern = "^(\\d+)[dD](\\d+)$",
              compute = function(match) {
                n = match[2]
                sides = match[3]
                rolls = sample(1:sides, n, replace = TRUE)
                message('rolls: ', paste(rolls, collapse = ', '))
                result = sum(rolls)
              })

keep_h = list(pattern = "^(\\d+)[dD](\\d+)[Hh](\\d+)$",
              compute = function(match) {
                n = match[2]
                sides = match[3]
                kept = match[4]
                rolls = sample(1:sides, n, replace = TRUE)
                message('rolls: ', paste(rolls, collapse = ', '))
                kept_dice = sort(rolls, decreasing = T)[1:as.numeric(kept)]
                message('keeping ',kept, " highest(s): ", paste(kept_dice, collapse = ', '))
                result =  sum(kept_dice)
              })

keep_l = list(pattern = "^(\\d+)[dD](\\d+)[Ll](\\d+)$",
              compute = function(match) {
                n = match[2]
                sides = match[3]
                kept = match[4]
                rolls = sample(1:sides, n, replace = TRUE)
                message('rolls: ', paste(rolls, collapse = ', '))
                kept_dice = sort(rolls)[1:as.numeric(kept)]
                message('keeping ',kept, " lowest(s): ", paste(kept_dice, collapse = ', '))
                result =  sum(kept_dice)
              })

exploding = list(pattern ="^(\\d+)[dD](\\d+)\\!$",
                 compute = function(match) {
                  n = match[2]
                  sides = match[3]
                  rolls = sample(1:sides, n, replace = TRUE)
                  explode = rolls[rolls == sides]
                  message('rolls: ', paste(rolls, collapse = ', '))
                  message("exploding ", length(explode),' dice...')
                  while (length(explode) != 0) {
                    new_rolls = sample(1:sides, length(explode), replace = TRUE)
                    message('new rolls : ', paste(new_rolls, collapse = ', '))
                    rolls = c(rolls, new_rolls)
                    explode = new_rolls[new_rolls==sides]
                    if (length(explode) != 0) { message("exploding ", length(explode),' dice...') }
                  }
                  result = sum(rolls)
                 })

reroll = list(pattern = "^(\\d+)[dD](\\d+)[rR](\\d+)$",
              compute = function(match) {
                n = match[2]
                sides = match[3]
                to_reroll = match[4]
                rolls = sample(1:sides, n, replace = TRUE)
                message('rolls: ', paste(rolls, collapse = ', '))
                reroll = rolls[rolls == to_reroll]
                message("rerolling ",length(reroll),' dice')
                while (length(reroll) != 0) {
                  new_rolls = sample(1:sides, length(reroll), replace = TRUE)
                  message('new rolls : ', paste(new_rolls, collapse = ', '))
                  rolls[rolls == to_reroll] = new_rolls
                  reroll = rolls[rolls == to_reroll]
                  if (length(reroll) != 0) { message("rerolling ",length(reroll),' dice')}
                }
                result = sum(rolls)
              })

success = list(pattern = "^(\\d+)[dD](\\d+) ?([<>]?=?) ?(\\d+)$",
               compute = function(match) {
                 n = match[2]
                 sides = match[3]
                 comparator = match[4]
                 if (comparator == "=") {comparator="=="}
                 threshold = match[5]
                 rolls = sample(1:sides, n, replace = TRUE)
                 message('rolls: ', paste(rolls, collapse = ', '))
                 success = eval(parse(text = paste("rolls[rolls",comparator,"threshold]")))
                 result = length(success)
                 message('number of success: ',
                         result ,
                         ' (', paste(sort(success,decreasing = TRUE), collapse = ', '),')')
                 return(result)
                 })

roll_types = list(
  no_dice,
  simple,
  keep_h,
  keep_l,
  exploding,
  reroll,
  success,
  reroll
)
