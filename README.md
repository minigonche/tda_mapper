# TDA Mapper in K Dimensions
This code is simply an extension of the mapper2D created by Paul Pearson, Daniel Muellner and Gurjeet Singh. This code allows the user to use
k dimensions for the filter function in a fullty vectorized excecution (Minus the iteration over the intersected open sets).

The idea is that this code replaces both the mapper1D and mapper2D, although I have not compared them in terms of efficiency. There is still 
a section of the code that could use some optimization and it would be ideal to have a version that runs on low RAM memory, calculating only the 
necessary subset of the distance matrix in each iterartion. This would mean calculating some distances over and over again, but it would
allow the script to run in small (AWS t2.micro) machines.

