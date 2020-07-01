
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RollR <img src='man/figures/logo.png' align="right" height="150" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This R package provides a simple way to make all sorts of dice rolls
using syntax inspired from tabletop roleplaying games like Dungeons &
Dragons.

Hex logo made by [Sophie](sophie1000.com).

## Installation

    # install.packages("remotes")
    remotes::install_github("felixmil/rollr")

## Examples

``` r
library(rollr)

roll_dice("1d12") # rolls one 12-sided dice

roll_dice("1d20+2") # roll one 20-sided dice then adds 2

roll_dice("2d10 + 1d4") # rolls two 10-sided dice and one 4-sided diced and sums their results 

roll_dice("4d6h3") # rolls four 6-sided dice and sum the 3 highests

roll_dice("2d20l1") # rolls two 20-sided dice and keeps the lowest one
```

## Use Case

in Dungeon & Dragons 5th Edition, ability scores are set by rolling 4d6
(four 6-sided dice) and keeping the 3 highest ones (ability scores can
range between 3 and 18).

Let’s say we want to create a new character: Dunrill, a strong and
muscular Dwarf. We roll our first set of 4 6-sided dice and keep the 3
highests.

``` r
library(rollr)

set.seed(42)

roll_results <- roll_dice("4d6h3", roll_history = TRUE) 
#> Evaluating "4d6h3" 
#> ==========
#> rolls: 1, 5, 1, 1
#> keeping 3 highest(s): 5, 1, 1
#> ==========
#>  Result is 7
```

That’s a 7. Now, the same roll (4d6h3) must be reproduced 5 more times:

``` r
more_roll_results <- replicate(5, roll_dice("4d6h3"))

all_results <- c(roll_results, more_roll_results)

print(all_results)
#> [1]  7  8 10 12  7 15
```

Now, we can assign these 6 values to Dunrill abilities (Strength,
Dexterity, Constitution…) move on with character creation.

But **were we lucky rolling these values: 7, 8, 10, 12, 7, 15 ?**

To know that, we’ll simulate 9999 4d6h3 rolls and approximate the
probability distribution.

``` r
random_rolls <- replicate(9999, roll_dice("4d6h3"))
```

Probability distribution for 4d6h3 looks like this:

<img src="man/figures/README-unnamed-chunk-6-1.png" width="80%" style="display: block; margin: auto;" />

Distribution’s mode is **13**, however, the mean of our rolls is
**9.83**:

<img src="man/figures/README-unnamed-chunk-7-1.png" width="80%" style="display: block; margin: auto;" />

## Rolls syntax (supported dice rolls)

Features list derivated from
[Sidekick](https://github.com/ArtemGr/Sidekick)

  - [x] `/r 1d8 + 2` - Roll one octahedron and add two.

  - [x] `/r 1d8 + 4d6` - Roll one octahedron and four hexahedrons.

  - [x] `/r 2d20h1` - Roll twice and keep the highest roll (D\&D 5e
    advantage).

  - [x] `/r 2d20h1 + 2` - Roll twice and keep the highest roll, with a
    modifier (D\&D 5e advantage).

  - [x] `/r 4d6h3` - Roll four hexahedrons and keep the highest three
    (D\&D 5e ability roll).

  - [x] `/r 2d20l1` - Roll twice and keep the lowest roll (D\&D 5e
    disadvantage).

  - [x] `/r 1d20r1` - Roll twenty, reroll on one (because halflings are
    lucky).

  - [x] `/r 3d6!` - Exploding dice.

  - [x] `/r 2d6>=5` - Roll two hexahedrons and take only the ones that
    turned greater or equal to five (aka difficulty check). Prints the
    number of successes.

  - [x] `/r 4d6=5` - So can this guy roll five?

  - [ ] `/r 3d10>=6f1` - oWoD roll: rolling *one* is a failure, rolling
    more failures than successes is a *botch*.

  - [ ] `/r 1d10>=8f1f2` - Rolling *one* or *two* is a failure.

  - [ ] `/r 4dF` - [Fudge/Fate
    dice](http://rpg.stackexchange.com/questions/1765/what-game-circumstance-uses-fudge-dice).

  - [ ] `/r 1d10!>9` - Explode nine and ten.

  - [ ] `/r 3d10!>=8` - nWoD roll: tens explode, eights and up are
    treated like a success.

  - [ ] `/r 1d10t10` - If a ten is rolled then count it twice.

  - [ ] `/r repeat (4d6k3, 6)` - Roll D\&D 5e ability score six times
    (to generate a new character).

  - [ ] `/r repeat (d6, 3, brief)` - In Nomine. 1, 1, 1.

  - [ ] `/r repeat (1d20+1, 5, short sum)` - Sum the rolls.

  - [ ] `/r ova (5)` - OVA. 6, 6, 1, 1, 1 = 12.

## TO DO

  - [ ] Support all rolls syntax above
  - [ ] Add function to replicate a dice command n times (don’t reparse
    dice command each time)
  - [ ] Create Vignette (from Use case ?)
