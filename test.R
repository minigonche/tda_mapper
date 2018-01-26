#This script compares the new version of mapper with the existing tda mapper library
#Although the existing mapper only test for 1 and 2 dimensions, we hope that testing on these 
# two dimensions will suffice

# Loads the Original Mapper Library:
library('TDAmapper')


#Loads the custom mapper library
source('mapperKD_low_RAM.R')

#Loads the igraph library for ploting
library('igraph')



#Function that compares to results from the mapper algorithm
compare_mapper_results = function(mapper_result_1, mapper_result_2)
{
  
  #Compares the number of nodes
  if(mapper_result_1$num_vertices != mapper_result_2$num_vertices)
    return(FALSE)
  
  #Compares the adjacency matrices
  if(!(all(mapper_result_2$adjacency == mapper_result_2$adjacency)))
    return(FALSE) 
  
  return(TRUE)
}


num_of_iterations = 50

#Creates several random samples and test if the two mappers are the same 

#Dimension 1

result = c()
for(i in 1:num_of_iterations)
{
  tryCatch({
    
  size = 100
  sample_data = data.frame( x = c(rnorm(size, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1))))),
                            y = c(rnorm(size, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1))))))
  
  distance_matrix = dist(sample_data)
  
  split = sample(c(1,2,4,5),1)
  filter_function = rep(rnorm(size/split, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1)))),split)
  
  num_intervals = sample(2:20,1)
  percent_overlap = sample(2:98,1)
  num_bins_when_clustering = sample(5:15,1)
  
  mapper_result_1 = mapper1D(distance_matrix = distance_matrix, 
                             filter_values = filter_function, 
                             num_intervals = num_intervals, 
                             percent_overlap = percent_overlap, 
                             num_bins_when_clustering = num_bins_when_clustering)
  
  plot(graph.adjacency(mapper_result_1$adjacency, mode = 'undirected'))
  
  mapper_result_2 = mapperKD(k = 1,
                             distance_matrix = distance_matrix,
                             filter_values = list( filter_function),
                             num_intervals = num_intervals, 
                             percent_overlap = percent_overlap, 
                             num_bins_when_clustering = num_bins_when_clustering,
                             low_ram = FALSE,
                             distance_function = function(data,indices){return(dist(data[indices,]))},
                             data = circle,
                             print_iterations = FALSE)
  
  mapper_result_3 = mapperKD(k = 1,
                             distance_matrix = distance_matrix,
                             filter_values = list( filter_function),
                             num_intervals = num_intervals, 
                             percent_overlap = percent_overlap, 
                             num_bins_when_clustering = num_bins_when_clustering,
                             low_ram = TRUE,
                             distance_function = function(data,indices){return(dist(data[indices,]))},
                             data = sample_data,
                             print_iterations = FALSE)
  
  
  
  result = c(result,compare_mapper_results(mapper_result_1, mapper_result_2), compare_mapper_results(mapper_result_1, mapper_result_3))

  }, warning = function(w) {
    
  }, error = function(e) {
    print('error')
  }, finally = {
    print(paste('finished',i))
  })
}


#Dimension 2

result2 = c()
for(i in 1:num_of_iterations)
{
  tryCatch({
    size = 100
    sample_data = data.frame( x = c(rnorm(size, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1))))),
                              y = c(rnorm(size, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1))))))
    
    distance_matrix = dist(sample_data)
    
    split = sample(c(1,2,4,5),1)
    
    
    filter_function = list(rep(rnorm(size/split, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1)))),split),
                           rep(rnorm(size/split, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1)))),split))
    
    
    temp = c(sample(2:5,1))
    num_intervals = c(temp,temp)
    percent_overlap = sample(2:98,1)
    num_bins_when_clustering = sample(5:12,1)
    
    mapper_result_1 = mapper2D(distance_matrix = distance_matrix, 
                               filter_values = filter_function, 
                               num_intervals = num_intervals, 
                               percent_overlap = percent_overlap, 
                               num_bins_when_clustering = num_bins_when_clustering)
    
    plot(graph.adjacency(mapper_result_1$adjacency, mode = 'undirected'))
    
    
    mapper_result_2 = mapperKD(k = 2,
                               distance_matrix = distance_matrix,
                               filter_values = filter_function,
                               num_intervals = num_intervals, 
                               percent_overlap = percent_overlap, 
                               num_bins_when_clustering = num_bins_when_clustering,
                               low_ram = FALSE,
                               distance_function = function(data,indices){return(dist(data[indices,]))},
                               data = sample_data,
                               print_iterations = FALSE)
    
    
    
    result2 = c(result2,compare_mapper_results(mapper_result_1, mapper_result_2))
    
  }, warning = function(w) {
    
  }, error = function(e) {
    print('error')
  }, finally = {
    print(paste('finished',i))
  })
  
}


print('-------------------------------------------------------------')
print('-------------------------------------------------------------')
print('-------------------------------------------------------------')
print('FINISHED TEST')
print('-------------------------------------------------------------')
print('Results:')
print(paste('Test for Dimension 1:', all(result == TRUE)))
print(paste('Test for Dimension 2:', all(result2 == TRUE)))

