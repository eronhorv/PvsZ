## Info
- School project from 2022

## Homework Summary Description

In this task, we will implement a simplified version of the game Plants vs Zombies. In the game, zombies march from right to left on a 5-row field, while the player tries to defend themselves by placing plants. The zombies win if a zombie reaches the left side of the field. The player wins if all zombies are killed. During the game, Sun points need to be collected to purchase new plants. More information about the original video game can be found on Wikipedia.

Since our implementation will not have a graphical interface, we will only perform a simulation, meaning the turns will proceed automatically. The gameplay will be divided into discrete time intervals (called turns), after which every zombie and plant in the game can perform an automatic action. For example: The zombie moves forward, while the plant shoots.