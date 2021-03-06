# TDA Mapper in K Dimensions
This code is simply an extension of the mapper2D created by Paul Pearson, Daniel Muellner and Gurjeet Singh: *https://github.com/paultpearson/TDAmapper*. This code allows the user to use
k dimensions for the filter function in a fullty vectorized excecution (Minus the iteration over the intersected open sets).

The idea is that this code replaces both the mapper1D and mapper2D, although I have not compared them in terms of efficiency. There is still 
a section of the code that could use some optimization 

The version that runs on low memory has been included. This version minimizes the ammount of RAM needed, calculating only the 
necessary subset of the distance matrix in each iterartion. This would mean calculating some distances over and over again, but it allows the script to run in small (AWS t2.micro) machines.

The files *result_constructor.R* and *TDAInteractiveResults.R* enable the user
to quickly construct results and visualize them.

The constructed script was tested against the methods mapper1D and mapper2D for random samples. They produce exact results. The test script corresponds to: *test.R*
