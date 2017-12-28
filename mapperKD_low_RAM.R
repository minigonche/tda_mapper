#' This function is an extension of the function mapper2D developed by Paul Pearson, Daniel Muellner and Gurjeet Singh as part of the
#' the package TDAmapper. This version is intended to run on machines with small memory, repeating certain calculations to avoid storing large
#' sets.

#' @author Felipe Gonzalez
#' mapperkD function
#'
#' This function uses a filter function f: X -> R^k on a data set X that has n rows (observations) and m columns (variables).
#' @param k a positive integer indicating the number of dimensions to be projected on.
#' @param distance_matrix an n x n matrix of pairwise dissimilarities
#' @param filter_values a list of k length n vector of real numbers (the projections)
#' @param num_intervals a vector of k positive integers, the number of intervals to be projected on.
#' @param percent_overlap a number between 0 and 100 specifying how much adjacent intervals should overlap (for all projections)
#' @param num_bins_when_clustering a positive integer that controls whether points in the same level set end up in the same cluster. The higher the number the more cluster one will get
#' @param low_ram a boolean indicating if the algorithm should be excecuted in a memory restricted enviorment
#' @param distance_funtion a function that receives two parameters: \code{data} the data used to calculate the distance between elements, \code{logical_indices} a logical vector indicating wich elements should be taken into account on the distance computation 
#' @param data a data frame (or any other type of structure, as long as the distance_function is aware) containing the information necessary to calculate the distance
#'
#' @return An object of class \code{TDAmapper} which is a list of items named: 
#' \code{adjacency} (adjacency matrix for the edges), 
#' \code{num_vertices} (integer number of vertices), 
#' \code{level_of_vertex} (vector with \code{level_of_vertex[i]} = index of the level set for vertex i), 
#' \code{points_in_vertex} (list with \code{points_in_vertex[[i]]} = vector of indices of points in vertex i), 
#' \code{points_in_level} (list with \code{points_in_level[[i]]} = vector of indices of points in level set i, 
#' \code{vertices_in_level} (list with \code{vertices_in_level[[i]]} = vector of indices of vertices in level set i.
#'
#'
#' @examples
#' m3 <- mapperKD(
#'        3,
#'        NULL,
#'        filter_values = list( cos(1:100)*sin(101:200), sin(1:100)*sin(101:200),  cos(101:200)),
#'        num_intervals = c(5,5,5),
#'        percent_overlap = 50,
#'        num_bins_when_clustering = 10
#'        TRUE,
#'        function(data, indices)
#'        )
#' \dontrun{
#' library(igraph)
#' g3 <- graph.adjacency(m3$adjacency, mode="undirected")
#' plot(g3, layout = layout.auto(g3) )
#' }
#' @export
#'
mapperKD <- function(
  k = 2,
  distance_matrix = NULL,
  filter_values = list( cos(1:100), sin(1:100)),
  num_intervals = c(5,5),
  percent_overlap = 50,
  num_bins_when_clustering = 10,
  low_ram = TRUE,
  distance_function = function(data,indices){return(dist(data[indices,]))},
  data = data.frame( x = cos(1:100), y = sin(1:100)),
  print_iterations = FALSE #Circle
) {
  
  
  #gonche: checks if the dimensions of the parameters correspond with k
  if(k != length(filter_values) || k != length(num_intervals))
    stop("The dimension of the filter values or the number of intervals do not correpond with the value of k")
  #gonche: checks if the low_ram parameter is correctly used
  if(low_ram && (is.null(distance_function) || is.null(data)))
    stop('If low_ram = TRUE both distance_function and data must not be NULL')
  
  # initialize variables
  vertex_index <- 0
  
  # indexed from 1 to the number of vertices
  level_of_vertex <- c()
  points_in_vertex <- list()
  # filter_values_in_vertex <- list() # just use filter_values[points_in_vertex[[i]]]
  
  # indexed from 1 to the number of levels
  points_in_level <- list()
  vertices_in_level <- list()
  # filter_values_in_level <- list() # just use filter_values[points_in_level[[i]]]
  
  # gonche: Vectorize the min and max values of the filter_values
  # gonche: minimum of the filter values
  filter_min <- sapply(filter_values, min)
  
  
  # gonche: maximum of the filter values
  filter_max <- sapply(filter_values, max)
  
  
  # gonche: Vectorize the interval length
  interval_length = (filter_max - filter_min)/(num_intervals - (num_intervals - 1)*percent_overlap/100)

  # gonche: Vectorize step_size
  step_size = interval_length * (1 - percent_overlap/100)
  
  num_levels <- prod(num_intervals)
  
  #gonche: The position of the current level is modeled by an array of coordinates that starts in zeros
  level_indices = rep(0,k)
  level = 1
  
  
  #Convert filter values to matrix
  m_filter_values = matrix(unlist(filter_values), ncol = k)
  
  adjacent_levels = list()

  potence = get_potence(num_intervals)
  # begin mapper main loop
  #- for (level in 1:num_levels) 
   repeat {
     

    adjacent_levels[[level]] = get_adjacent_levels(level_indices,num_intervals, potence)
    #- level_1 <- level_indices_1[level]
    #- level_2 <- level_indices_2[level]
    
    
    #print(level)
    #print(level_indices)
    #print(adjacent_levels[[level]])
    
    # gonche: Vectorize min_value_in_level
    min_value_in_level = filter_min + (level_indices * step_size)
    # gonche: Vectorize maxvalue_in_level     
    max_value_in_level = min_value_in_level + interval_length

    #gonche: converts into matrix
    min_value_in_level_matrix = matrix(rep(min_value_in_level, nrow(m_filter_values)), ncol = k, byrow = TRUE)
    max_value_in_level_matrix = matrix(rep(max_value_in_level, nrow(m_filter_values)), ncol = k, byrow = TRUE)
    
    # gonche: Vectorize points_in_logical
    temp_logic_matrix_1 = m_filter_values >= min_value_in_level_matrix 
    temp_logic_matrix_2 = m_filter_values <= max_value_in_level_matrix
    
    
    temp_logic_matrix = temp_logic_matrix_1 & temp_logic_matrix_2
    points_in_level_logical = as.logical(apply(temp_logic_matrix, 1, prod))
    
    
      
    num_points_in_level <- sum(points_in_level_logical)
    points_in_level[[level]] <- which(points_in_level_logical)
    
    if (num_points_in_level == 0) {
      print('Level set is empty')
      
      
      
      if(!advance(level_indices,num_intervals))
      {break}
      # gonche: Advance level
      level = level+1
      next
    }
    
    if (num_points_in_level == 1) {
      print('Level set has only one point')
      num_vertices_in_level <- 1
      cluster_indices_within_level <- c(1)
    }
    
    if (num_points_in_level > 1) {
      # use as.matrix() to put the distance matrix in square form,
      # and as.dist() to put it in vector form
      # This could probably use some optimization...
      # gonche: (A lot of optimization)
      
      if(low_ram)
      {
        level_distance_matrix = as.dist(distance_function(data = data, indices = points_in_level_logical))
        
      }else
      {
        level_distance_matrix <- as.dist(as.matrix(distance_matrix)[points_in_level_logical,points_in_level_logical])
      }  
      

      level_max_distance <- max(level_distance_matrix)
      # use R's hclust (provided by stats or fastcluster)
      # in place of Matlab's linkage function.
      
      #All the points are the same and should be clusteres together
      if(level_max_distance == 0)
      {
        
        print(paste('Level set has ', sum(points_in_level_logical), ' identical points', sep = ''))
        num_vertices_in_level <- 1
        cluster_indices_within_level <- c(1,sum(points_in_level_logical))
        
      }else
      {
        level_hcluster_ouput <- hclust(level_distance_matrix,method="single")
        heights <- level_hcluster_ouput$height
        cutoff <- cluster_cutoff_at_first_empty_bin(heights, level_max_distance, num_bins_when_clustering)
        
        # use as.vector() to get rid fo the names for the vector entries
        cluster_indices_within_level <- as.vector( cutree(level_hcluster_ouput, h=cutoff) )
        num_vertices_in_level <- max( cluster_indices_within_level )
        
        rm(level_distance_matrix)
        rm(level_hcluster_ouput)
        rm(cutoff)
        gc = gc(reset = TRUE)
        
      }
    }
    
    vertices_in_level[[level]] <- vertex_index + (1:num_vertices_in_level)
    
    for (j in 1:num_vertices_in_level) {
      
      vertex_index <- vertex_index + 1
      
      # points_in_level_logical is a logical vector, so use which(points_in_level_logical==TRUE) to convert it to a numerical vector of indices
      #nodeset <- which(points_in_level_logical==TRUE)[cluster_indices_within_level == j]
      
      level_of_vertex[vertex_index] <- level
      points_in_vertex[[vertex_index]] <- which(points_in_level_logical==TRUE)[cluster_indices_within_level == j]
      #points_in_vertex[[vertex_index]] <- nodeset
      #filter_values_in_vertex[[vertex_index]] <- filter_values[nodeset]
      
    }
    

    
    if(!advance(level_indices,num_intervals))
    {break}
    
    # gonche: Advance level
    level = level+1
    
    if(print_iterations)
    {print(paste('Finished',level,'Iterations'))}
    

    
  } # end mapper main loop
  
  #gonche: vertices in level will not have the same size as number of levels.
  # Adds the missing values
  vertices_in_level[[num_levels+1]] = -1
  
  
  # Note: num_vertices = vertex index.
  # Create the adjacency matrix for the graph, starting with a matrix of zeros
  adja <- mat.or.vec( vertex_index, vertex_index )
  
  
  #gonche: The loop for the adjacent matrix is similar to the prevous one
  level_indices = rep(0,k)
  level = 1
  
 
  
  # repeat {
  #   
  #   #gonche: checks for adjacency orthogonaly in every coordinate (does not check diagonally since mapper 2D doesn't)
  #   for(i in 1:k){
  #     
  #     #gonche: only checks if there is a next one in that dimension
  #     if(level_indices[i] < num_intervals[i] - 1){
  #       
  #       k2 = level
  #       k1 = level + 1
  #       if(i>1) k1 = level + prod(num_intervals[1:i-1])
  #       
  #       # check that both level sets are nonemtpy
  #       if ( (length(vertices_in_level[[k1]]) > 0) & (length(vertices_in_level[[k2]]) > 0) ) {
  #         
  #         for (v1 in vertices_in_level[[k1]]) {
  #           for (v2 in vertices_in_level[[k2]]) {
  #             # return 1 if the intersection is nonempty
  #             adja[v1,v2] <- ( length(intersect(points_in_vertex[[v1]],
  #                                               points_in_vertex[[v2]])) > 0 )
  #             adja[v2,v1] <- adja[v1,v2]
  #           }
  #         }
  #         
  #       }
  #       
  #     }
  #     
  #   }
  #   
  #   if(!advance(level_indices,num_intervals))
  #   {break}
  #   
  #   # gonche: Advance level
  #   level = level+1
  #   
  # }# End of adjacency construction

  #brute force adjacency construction
  adja <- mat.or.vec( vertex_index, vertex_index )
  
  
  # for(k1 in 1:(num_levels - 1))
  # {
  #   for(k2 in (k1+1):num_levels)
  #   {
  #     
  #     # check that both level sets are nonemtpy
  #     if ( !is.null(vertices_in_level[[k1]]) & !is.null(vertices_in_level[[k2]]) )  {
  #       
  #       for (v1 in vertices_in_level[[k1]]) {
  #         for (v2 in vertices_in_level[[k2]]) {
  #           # return 1 if the intersection is nonempty
  #           adja[v1,v2] <- ( length(intersect(points_in_vertex[[v1]],
  #                                             points_in_vertex[[v2]])) > 0 )
  #           adja[v2,v1] <- adja[v1,v2]
  #         }
  #       }
  #       
  #     }
  #   }
  # }#end of brute force
  
  for(k1 in 1:length(adjacent_levels))
  {
    for(k2 in adjacent_levels[[k1]])
    {
      # check that both level sets are nonemtpy
      if ( k1 != k2 & !is.null(vertices_in_level[[k1]]) & !is.null(vertices_in_level[[k2]]) )  {

        for (v1 in vertices_in_level[[k1]]) {
          for (v2 in vertices_in_level[[k2]]) {
            
            # return 1 if the intersection is nonempty
            adja[v1,v2] <- ( length(intersect(points_in_vertex[[v1]],
                                              points_in_vertex[[v2]])) > 0 )
            adja[v2,v1] <- adja[v1,v2]
          }
        }

      }
    }
    
    if(print_iterations)
    {print(paste('Finished Adjacency for level: ', k1 , ' of ', length(adjacent_levels), sep = ''))}
    
  }#end of brute force
  

  
  
  

  mapperoutput <- list(adjacency = adja,
                       num_vertices = vertex_index,
                       level_of_vertex = level_of_vertex,
                       points_in_vertex = points_in_vertex,
                       #filter_values_in_vertex = filter_values_in_vertex,
                       points_in_level = points_in_level,
                       vertices_in_level = vertices_in_level
  )
  
  class(mapperoutput) <- "TDAmapper"
  
  return(mapperoutput)
  
} # end mapperKD function


# gonche: Support function for advancing the coordinates
#' @param position an array indicating the interval for each dimention which is currently being processed. This object is updated inside the method.
#' @param num_intervals a vector of k positive integers, the number of intervals to be projected on.
#'
#' @return A boolean indicating if the process continues. FALSE means the enumeration has been restarted. 
#'
advance <- function(position,num_intervals)
{
  # dimention of the array
  size = length(num_intervals)
  
  # temporal value so that the position is recieved as reference
  temp = position
  
  flag = TRUE
  
  for (i in 1:size)
  {
    temp[i] = (temp[i] + 1) %% num_intervals[i]
    if(temp[i] != 0)
    {
      # sends the new position in the received parameter 
      eval.parent(substitute(position <- temp))
      # indicates that the process continues
      return(TRUE)
    }
  }
  
  # sends the new position in the received parameter 
  eval.parent(substitute(position <- temp))
  # indicates that the process has finished.
  return(FALSE)
  
} # end advance function



# gonche: Support function for inverting the list. For some calculations it's better to invert a list of m n-dimentional arrays, to a list of n m-dimentional arrays
#' @param lis a list of m n-dimentional arrays
#'
#' @return List of n m-dimentional arrays.
#'
invert <- function(lis)
{
  
  m = NULL
  for (i in 1:length(lis))
  {
    m = cbind(m,lis[[i]])
  }
  
  return(lapply(seq_len(nrow(m)), function(i) as.vector(m[i,])))

} # end invert function


#' cluster_cutoff_at_first_empty_bin function
#' 
#' This function decides where to cut the hierarchical clustering tree to define clusters within a level set.
#'
#' @param heights Height values in hierarchical clustering.
#' @param diam Maximum distance between points in a level set.
#' @param num_bins_when_clustering Controls how many bins there are in the histogram used to determine cutoff. values
#' 
#' @return Numerical value for cutoff point of hierarchical cluster diagram.
#'
#' @author Paul Pearson, \email{pearsonp@@hope.edu}
#' @references \url{https://github.com/paultpearson/TDAmapper}
#' @seealso \code{\link{mapper1D}}, \code{\link{mapper2D}}
#'

cluster_cutoff_at_first_empty_bin <- function(heights, diam, num_bins_when_clustering) {
  
  # if there are only two points (one height value), then we have a single cluster
  if (length(heights) == 1) {
    if (heights == diam) {
      cutoff <- Inf
      return(cutoff)
    }
  }
  
  bin_breaks <- seq(from=min(heights), to=diam, 
                    by=(diam - min(heights))/num_bins_when_clustering)
  if(length(bin_breaks) == 0)
  {
    print('bin_breaks == 0')
    cutoff <- Inf
    return(cutoff)
  }

  myhist <- hist(c(heights,diam), breaks=bin_breaks, plot=FALSE)
  z <- (myhist$counts == 0)
  if (sum(z) == 0) {
    cutoff <- Inf
    return(cutoff)
  } else {
    #  which returns the indices of the logical vector (z == TRUE), min gives the smallest index
    cutoff <- myhist$mids[ min(which(z == TRUE)) ]
    return(cutoff)
  }
  
}#end of cluster_cutoff_at_first_empty_bin

#Gets the adjacent levels greater or equal to teh received parameter
# Positions start at zero, whereas level star at 1
get_adjacent_levels = function(positions, num_intervals, potence)
{
  
  dim = length(positions)
  if(dim == 1)
  {
    if(positions == (num_intervals-1))
    {
      return(c(positions+1))
    }
    return(c(positions+1,positions + 2))
  }
  combinations = as.matrix(expand.grid(rep(list(c(1,0)),length(positions))))
  
  prox_levels = matrix(rep(positions,dim(combinations)[1]), ncol = dim(combinations)[2], byrow = TRUE) + combinations
  #discards the ones that are beyond values
  possible = as.logical(apply(prox_levels < num_intervals, 1, prod))
  prox_levels = prox_levels[possible,]
  
  adjacent_levels = (prox_levels%*%potence) + 1
  
  return(adjacent_levels[,1])
  
  
}

get_potence = function(num_intervals)
{
  
  potence = c(1)
  for(i in 2:length(num_intervals))
  {
    potence = c(potence, prod(num_intervals[1:i-1]))
  }
  
  return(potence)
}



