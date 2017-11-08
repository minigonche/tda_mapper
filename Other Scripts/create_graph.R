library('igraph')
library('pracma')
# A function that constructs a graph for TDAmapper element and plots it given the parameters
#' @param df the corresponding data frame that was used for the TDAmapper analysis
#' @param tda the TDAmapper element
#' @param only_node_ids and ordered list of the node ids to be included in the graph, if NULL, all will be taken into account
#' @param min_draw_size the minimum draw size of the vertices
#' @param max_draw_size the maximum draw size of the vertices
#' @param color_scheme a string indicating the color scheme to folow for the nodes. Currently there are:
#'                    - NONE: The color of the nodes will be the value of \code{color}
#'                    - CONCENTRATION: The color of the nodes will vary from trasparent to fully colored
#'                    depending on the outcome of the \code{color_fun}. This method assumes that color_fun returns
#'                    values form 0 to 100. The color of the nodes will be the value of \code{color}
#'                    - LINEAR: The color of the nodes will vary from trasparent to fully colored
#'                    depending on the outcome of the \code{color_fun}. The numbers close to the obtained minimum
#'                    will be transparent and the ones close to the obtained maximum will be colored.#'                    
#' @param color_attributes a list containing the name of the attributes that are going to be taken into account
#'                        for the \code{color_scheme} and teh \code{color_fun}
#' @param color_fun a function that will be applied to the selected columns for the \code{color_scheme}
#' @param color the color of the nodes. Must be a vector of three elements corersponding to the values of rgb. Only Three bytes are supported
#' @param color_select_fun a function that will be applied to the selected columns for the color of the nodes. Must return a vector of three elements corersponding to the values of rgb. Only Three bytes are supported
#' @param label_scheme a string indicating the shcheme for the labels of the vertices. Currently there are:
#'                    - NONE: the vertices have no labels 
#'                    - NUMBERS: the vertices have their corresponding number
#'                    - SIZES: the vertices haver their corresponding size
#'                    - COLOR_FUN: the vertices have the result of the color_fun
#'                    - LABEL_FUN: the vertices have the result of the label_fun
#' @param label_fun a function that will be applied to the selected columns for the \code{label_scheme}
#' @param min_label_node_size a number with the minimum amount of points the node should have to include a label
#' @param max_label_node_size a number with the maximum amount of points the node should have to include a label                    
#' @param topology_only a boolean indicating if the igraph should only display the topology
#' 
#' @return An igraph corresponding to the TDAMapper element
#'
create_graph <- function(df,
                       tda,
                       only_node_ids = NULL,
                       min_draw_size = 5, 
                       max_draw_size = 35,
                       color_scheme = 'NONE',
                       color_map = NULL,
                       color_attributes = NULL, 
                       color_fun = NULL,
                       color = c(80,80,80),
                       color_select_fun = NULL,
                       label_scheme = 'NONE',
                       label_fun = NULL,
                       label_attributes = NULL,
                       min_label_node_size = 0,
                       max_label_node_size = Inf, 
                       topology_only = FALSE)
{

  
  node_ids = 1:tda$num_vertices
  
  if(!is.null(only_node_ids))
  {
    node_ids = only_node_ids
    tda$adjacency = (tda$adjacency[node_ids,])[,node_ids]
    tda$num_vertices = length(node_ids)
    tda$points_in_vertex = tda$points_in_vertex[node_ids]

  }
  g = graph.adjacency(tda$adjacency, mode="undirected")
  
  if(topology_only)
  {
    V(g)$label = rep(NA, tda$num_vertices)
    V(g)$size = rep(3, tda$num_vertices)
    
    return(g)
  }
  
  #------------
  # Sizes
  #------------
  
  #gets the amount of points in every vertex
  vert_sizes = sapply(tda$points_in_vertex,length)
  max_size = max(vert_sizes)
  min_size = min(vert_sizes)
  
  #normalize the sizes so they can be showed
  vert_draw_sizes = sapply(vert_sizes, function(size_) max(min_draw_size, (size_*max_draw_size)/max_size))
  V(g)$size =  vert_draw_sizes
  
  #initialices the color numbers
  color_numbers = rep(NA, tda$num_vertices)
                            
  #------------
  # Colors
  #------------
  color_rgbs = rep(rgb(color[1],color[2],color[3], maxColorValue = 255),tda$num_vertices)
  
  if(!is.null(color_scheme) && !strcmpi(color_scheme,'NONE'))
  {
    
    #color_fun cannot be null
    if(is.null(color_fun))
      stop('the parameter "color_fun" cannot be null when color_scheme is something other than "NONE"')
    
    #If no attribute is selected, all the columns are taken into acount
    if(is.null(color_attributes))
      color_attributes = colnames(df)
    
    #calculates the color data for each vertex
    if(length(color_attributes) == 1)
      color_data = lapply(tda$points_in_vertex, function(coord_) df[,color_attributes][coord_])
    else
      color_data = lapply(tda$points_in_vertex, function(coord_) df[,color_attributes][coord_,])
    
    #applies the function for the color scheme
    color_numbers = sapply(color_data, color_fun)

      
    
    
    
    #applies the function (if not null) for the color select
    if(!is.null(color_select_fun))
    {
      color_rgbs_coordinates = lapply(color_data, color_select_fun)
    }
    else if(!is.null(color))
    {
      color_rgbs_coordinates = lapply(rep(0,tda$num_vertices), function(x) c(color[1],color[2],color[3]))
    }
    else
    {
      stop('The parameters color and color_select_fun cannot be NULL at the same time')
    }
    
    #the percentages for coloring the nodes
    color_percentages = NULL
    
    if(strcmpi(color_scheme,'CONCENTRATION'))
    {
      #assumes the color numbers are already from 0 to 100
      color_percentages = color_numbers
    }
    else if(strcmpi(color_scheme,'LINEAR'))
    {
      min_color_number = min(color_numbers)
      max_color_number = max(color_numbers)
      if(min_color_number == max_color_number)
      {
        color_percentages = rep(100, tda$num_vertices)
      }
      else if(max_color_number == 0)
      {
        color_percentages = rep(0, tda$num_vertices)
      }
      else
        color_percentages = sapply(color_numbers, function(color_num_) round(((color_num_ - min_color_number)/(max_color_number-min_color_number))*100))
      
    }
    else
    {
      stop(paste('color scheme: ',color_scheme, ' not supported', sep = ''))
    }
    
    # Calculates the RGB colors
    # Uses the percetage for the alpha filter (the fourth byte
    color_rgbs = mapply(function(perc_, color_) {return(rgb(color_[1], color_[2], color_[3], (perc_/100)*255 , maxColorValue = 255))},color_percentages,color_rgbs_coordinates)
    
    
  }
  
  
  #assignes the colors
  V(g)$color = color_rgbs
  
  #------------
  # Labels
  #------------
  
  #The label distance to the node is a function of its size
  vert_label_dist = sapply(vert_draw_sizes, function(size_) 1 + ((size_ - min_draw_size )/(max_draw_size- min_draw_size))*0.8 )
  V(g)$label.dist = vert_label_dist
  
  # Color the labels black
  V(g)$label.color = rep('black',tda$num_vertices)
  
  # degree of the label
  V(g)$label.degree = rep(0,tda$num_vertices)
  
  
  if(strcmpi(label_scheme,'NONE'))
  {
    V(g)$label = rep(NA, tda$num_vertices)
  }
  else if(strcmpi(label_scheme,'NUMBERS'))
  {
    V(g)$label = node_ids
  } 
  else if(strcmpi(label_scheme,'SIZES'))
  {
    V(g)$label = vert_sizes  
  }
  else if(strcmpi(label_scheme,'COLOR_FUN')
          || strcmpi(label_scheme,'COLOR')
          || strcmpi(label_scheme,'COLOR FUN')
          || strcmpi(label_scheme,'Color Function'))
  {
    V(g)$label = sapply(color_numbers, function(color_num_)round(color_num_, digits = 1))  
  }
  else if(strcmpi(label_scheme,'LABEL_FUN')
          || strcmpi(label_scheme,'LABEL')
          || strcmpi(label_scheme,'LABEL FUN')
          || strcmpi(label_scheme,'Label Function'))
  {
    if(is.null(label_fun))
      stop('label_fun cannot be NULL when label_scheme is LABEL_FUN')
    #If no attribute is selected, all the columns are taken into acount
    if(is.null(label_attributes))
      label_attributes = colnames(df)
    
    #calculates the label data for each vertex
    if(length(label_attributes) == 1)
      label_data = lapply(tda$points_in_vertex, function(coord_) df[,label_attributes][coord_])
    else
      label_data = lapply(tda$points_in_vertex, function(coord_) df[,label_attributes][coord_,])
    
    V(g)$label = sapply(label_data, label_fun)
    
    
  }
  else
    stop(paste('label scheme: ',label_scheme, ' not supported', sep = ''))
  
  # Restricts the labels to the sizes
  V(g)$label[vert_sizes < min_label_node_size] = NA
  V(g)$label[vert_sizes > max_label_node_size] = NA
  
  # Puts label inside if possible
  label_sizes = sapply(V(g)$label,nchar)
  V(g)$label.dist[ as.logical((V(g)$size > 30 )*(label_sizes <= 6)) ] = 0
  V(g)$label.dist[ as.logical((V(g)$size > 26 )*(label_sizes <= 5)) ] = 0
  V(g)$label.dist[ as.logical((V(g)$size > 23 )*(label_sizes <= 4)) ] = 0
  V(g)$label.dist[ as.logical((V(g)$size > 20 )*(label_sizes <= 3)) ] = 0
  V(g)$label.dist[ as.logical((V(g)$size > 17 )*(label_sizes <= 2)) ] = 0
  V(g)$label.dist[ as.logical((V(g)$size > 9 )*(label_sizes <= 1)) ] = 0
  
  V(g)$label.degree = pi/4
  V(g)$label.dist = 0.75
  
  return(g)
  
} # end create_graph function
