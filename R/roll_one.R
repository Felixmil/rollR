#' Roll a die from text command
#'
#' @param roll a string corresponding to a roll command
#'
#' @return result of the roll(s)
#'
#' @examples
#' roll_one("1d6")
#'
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

keep_h = list(pattern = "^(\\d+)[dD](\\d+)[Hh](\\d+)",
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

keep_l = list(pattern = "^(\\d+)[dD](\\d+)[Ll](\\d+)",
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

exploding = list(pattern ="^(\\d+)[dD](\\d+)\\!",
                 compute = function(match) {
                  n = match[2]
                  sides = match[3]
                  rolls = sample(1:sides, n, replace = TRUE)
                  exploded = rolls[rolls == sides]
                  message('rolls: ', paste(rolls, collapse = ', '))
                  message(length(exploded),' dice exploding')
                  while (length(exploded) != 0) {
                    new_rolls = sample(1:sides, length(exploded), replace = TRUE)
                    message('new rolls : ', paste(new_rolls, collapse = ', '))
                    rolls = c(rolls, new_rolls)
                    exploded = new_rolls[new_rolls==sides]
                    if (length(exploded) != 0) { message(length(exploded),' dice exploding') }
                  }
                  result = sum(rolls)
                 })

roll_types = list(
  no_dice,
  simple,
  keep_h,
  keep_l,
  exploding
)
