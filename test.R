#This script compares the new version of mapper with the existing tda mapper library
#Although the existing mapper only test for 1 and 2 dimensions, we hope that testing on these 
# two dimensions will suffice

# Loads the Original Mapper Library:
library('TDAmapper')


#Loads the custom mapper library
source('mapperKD_low_RAM.R')





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


#The circle
num_intervals = 5
percent_overlap = 50
num_bins_when_clustering = 10


circle = data.frame(x = 2 * cos(0.5 * (1:100)), y = sin(1:100))

mapper_result_1 = mapper1D(distance_matrix = dist(circle), 
                      filter_values = circle$x, 
                      num_intervals = num_intervals, 
                      percent_overlap = percent_overlap, 
                      num_bins_when_clustering = num_bins_when_clustering)

mapper_result_2 = mapperKD(k = 1,
                      distance_matrix = dist(circle),
                      filter_values = list( circle$x),
                      num_intervals = num_intervals, 
                      percent_overlap = percent_overlap, 
                      num_bins_when_clustering = num_bins_when_clustering,
                      low_ram = FALSE,
                      distance_function = function(data,indices){return(dist(data[indices,]))},
                      data = circle,
                      print_iterations = FALSE)


#Creates several random samples and test if the two mappers are the same 

#Dimension 1

result = c()
for(i in 1:50)
{
  size = 100
  sample_data = data.frame( x = c(rnorm(size, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1))))),
                            y = c(rnorm(size, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1))))))
  
  distance_matrix = dist(sample_data)
  
  filter_function = rnorm(size, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1))))
  
  num_intervals = sample(2:20,1)
  percent_overlap = sample(2:98,1)
  num_bins_when_clustering = sample(5:15,1)
  
  mapper_result_1 = mapper1D(distance_matrix = distance_matrix, 
                             filter_values = filter_function, 
                             num_intervals = num_intervals, 
                             percent_overlap = percent_overlap, 
                             num_bins_when_clustering = num_bins_when_clustering)
  
  
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
  
  
  
  result = c(result,compare_mapper_results(mapper_result_1, mapper_result_2))

  print(paste('finished',i))
}


#Dimension 2

result2 = c()
for(i in 1:50)
{
  tryCatch({
    size = 100
    sample_data = data.frame( x = c(rnorm(size, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1))))),
                              y = c(rnorm(size, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1))))))
    
    distance_matrix = dist(sample_data)
    
    filter_function = list(rnorm(size, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1)))),
                           rnorm(size, mean = sample(-100:100,1), sd = abs(runif(1, 0, sample(1:20,1)))))
    
    
    temp = c(sample(2:5,1))
    num_intervals = c(temp,temp)
    percent_overlap = sample(2:98,1)
    num_bins_when_clustering = sample(5:12,1)
    
    mapper_result_1 = mapper2D(distance_matrix = distance_matrix, 
                               filter_values = filter_function, 
                               num_intervals = num_intervals, 
                               percent_overlap = percent_overlap, 
                               num_bins_when_clustering = num_bins_when_clustering)
    
    #plot(graph.adjacency(mapper_result_1$adjacency, mode = 'undirected'))
    
    
    mapper_result_2 = mapperKD(k = 2,
                               distance_matrix = distance_matrix,
                               filter_values = filter_function,
                               num_intervals = num_intervals, 
                               percent_overlap = percent_overlap, 
                               num_bins_when_clustering = num_bins_when_clustering,
                               low_ram = FALSE,
                               distance_function = function(data,indices){return(dist(data[indices,]))},
                               data = circle,
                               print_iterations = FALSE)
    
    
    
    result2 = c(result2,compare_mapper_results(mapper_result_1, mapper_result_2))
    
  }, warning = function(w) {
    
  }, error = function(e) {
    print('error')
  }, finally = {
    print(paste('finished',i))
  })
  
}

print(all(result1 == TRUE))
print(all(result2 == TRUE))

