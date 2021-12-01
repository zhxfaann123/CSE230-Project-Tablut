# CSE230-Project-Tablut
A boarder game for CSE230 at UCSD

Team member: Xiaofeng Zhao

Language   : Haskell

**Project Proposal:**

This project aims to build an ancient boarder game called "Tablut", which is an variant of Tafl games. For more detail of this game, please see [Wiki for Tafl](https://en.wikipedia.org/wiki/Tafl_games). 

We will be implementing several functions beside the game itself. Tentative functions to realize are as follows:

1. Save and Load Game
2. Time limit for each turn
3. Notification for next move
4. (some AI players?)


**Mile Stone Update**

*Progress*:   
>We've finished the logic part (Rules, Winning condition, etc.) of the game

*Challenges*: 
>Mostly about the graphics and the interface. Without the proper support of some high-level API for certain functions, we are invastigating the Vty/Brick library to               build desired graphics and interface from low-level API.

*Future Plans*: 
>We are working hard on graphics and interface. After that, we will be writing test suits to verify the correctness of the logic. The AI part will be implemented if time permits.

