This repository contains code files for problems 5 and 6 of the week 11 lab.

Problem 5: The function builds on my Week 10 code by converting trips and rebalancing records into inventory events; 
it sorts them chronologically and gives the number of bikes available at each station.
I then calculate the portion of each observation hour during which the inventory is positive. 
Observed departures for each group are divided by the six observation days, and then by the 
origin station's availability to estimate demand.

Problem 6: The function first identifies each origin-destination station pair and finds its maximum arrival rate. 
It then generates candidate arrival times from the homogeneous Poisson process, converting
the exponential waiting times from hours into seconds. For each candidate, the function identifies
its hour and calculates the probability of acceptance by dividing that hour's rate by the maximum rate. 
All accepted requests are combined into a data frame and sorted chronologically.

When committing a change in a repository, you're creating a sort of checkpoint or entry that describes a change in the project history. Saving simply writes a change onto a computer's hard disk. 
