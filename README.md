
# RollR 🎲

<!-- badges: start -->
<!-- badges: end -->

This R package provides a simple way to make all sorts of dice rolls using syntax inspired from tabletop roleplaying games.


## Installation

# install.packages("remotes")
remotes::install_github("felixmil/rollr")

## Example


``` r
library(rollr)

roll_dice("1d12") # rolls one 12-sided dice

roll_dice("1d20+2") # roll one 20-sided dice then adds 2

roll_dice("2d10 + 1d4") # rolls two 10-sided dice and one 4-sided diced and sums their results 

roll_dice("4d6k3") # rolls four 6-sided dice and sum the 3 highests

roll_dice("2d20l1") # rolls two 20-sided dice and keeps the lowest one

```

## Rolls syntax (What you can do)

Features list derivated from [Sidekick](https://github.com/ArtemGr/Sidekick)

- [x] `/r 1d8 + 2` - Roll one octahedron and add two.

- [x] `/r 1d8 + 4d6` - Roll one octahedron and four hexahedrons.

- [x] `/r 2d20h1` - Roll twice and keep the highest roll (D&D 5e advantage).

- [x] `/r 2d20h1 + 2` - Roll twice and keep the highest roll, with a modifier (D&D 5e advantage).

- [x] `/r 2d20l1` - Roll twice and keep the lowest roll (D&D 5e disadvantage).

- [ ] `/r 1d20r1` - Roll twenty, reroll on one (because halflings are lucky).

- [ ] `/r 3d6!` - Exploding dice.

- [ ] `/r 2d6>=5` - Roll two hexahedrons and take only the ones that turned greater or equal to five (aka difficulty check). Prints the number of successes.

- [ ] `/r 4d6=5` - So can this guy roll five?

- [ ] `/r 3d10>=6f1` - oWoD roll: rolling *one* is a failure, rolling more failures than successes is a *botch*.

- [ ] `/r 1d10>=8f1f2` - Rolling *one* or *two* is a failure.

- [ ] `/r 4dF` - [Fudge/Fate dice](http://rpg.stackexchange.com/questions/1765/what-game-circumstance-uses-fudge-dice).

- [ ] `/r 1d10!>9` - Explode nine and ten.

- [ ] `/r 3d10!>=8` - nWoD roll: tens explode, eights and up are treated like a success.

- [ ] `/r 1d10t10` - If a ten is rolled then count it twice.

- [x] `/r 4d6k3` - Roll four hexahedrons and keep the highest three (D&D 5e ability roll).

- [ ] `/r repeat (4d6k3, 6)` - Roll D&D 5e ability score six times (to generate a new character).

- [ ] `/r repeat (d6, 3, brief)` - In Nomine. 1, 1, 1.

- [ ] `/r repeat (1d20+1, 5, short sum)` - Sum the rolls.

- [ ] `/r ova (5)` - OVA. 6, 6, 1, 1, 1 = 12.



