#Required Libraries
library('maptools')
library('rgdal')
library('classInt')
library('RColorBrewer')
library('spdep')
library('pracma')
# A function that plots the TDA graph over a map of colombia
#' @param df the corresponding data frame that was used for the TDAmapper analysis
#' @param tda the TDAmapper element
#' @param geo_scheme the scheme for plotting the different nodes. Currently there are:
#'                    - LOCALITY: plot each node by the most common locality
#'                    - DEPARMENT: plot each node by the most common department
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
#' @param topology_only a boolean indicating if the igraph should only display the topology
#' @param lines a boolean indicating if the lines that unite the nodes should be graphed
#' @param random a boolean indicating if the unmathed localities should be seleceted randmonly (for plotting purpouses)
#' 
#'

plot_over_colombia <- function(df,
                         tda,
                         geo_scheme = 'LOCALITY',
                         only_node_ids = NULL,
                         min_draw_size = 0.5, 
                         max_draw_size = 2,
                         color_scheme = 'NONE',
                         color_map = NULL,
                         color_attributes = NULL, 
                         color_fun = NULL,
                         color = c(0,102,255),
                         color_select_fun = NULL,
                         topology_only = FALSE,
                         lines = TRUE,
                         random = TRUE)
{
  
  
  #First loads the shapefile 
  working_temp = getwd()
  setwd("~/Dropbox/Andres_Angel-Felipe_Fongalez/TDA Enfermedades Colombia/interactive_results/RScripts/maps")
  munimala <- readOGR(".", "mpio")
  setwd(working_temp)
  
  
  #Checks if the data set contains the information to plot over the shapefile
  if(!('munic' %in% colnames(working_data$data)))
  {
    plot(munimala)
    stop('The data set has to contain the column "munic" (The locality code) for the script to extract the location of the nodes')
  }
  
  
  #Selects all ids
  node_ids = 1:tda$num_vertices
  
  #Restricts the ids
  if(!is.null(only_node_ids))
  {
    node_ids = only_node_ids
    tda$adjacency = (tda$adjacency[node_ids,])[,node_ids]
    tda$num_vertices = length(node_ids)
    tda$points_in_vertex = tda$points_in_vertex[node_ids]
    
  }
  
  #Gets the coordinates of each node depending on the mode locality
  #Gets a numeric version of the shapefile data
  search = data.frame(COD_MPIO = sapply(munimala@data[['MPIO']],as.numeric),
                      COD_DPTO = sapply(munimala@data[['DPTO']],as.numeric),
                      x = coordinates(munimala)[,1],
                      y = coordinates(munimala)[,2])
  
  #Calculates the predominant locality and department
  if(strcmpi(geo_scheme,'LOCALITY')
     || strcmpi(geo_scheme,'LOC'))
  {
    col_name = 'munic'
    binary_fun = function(index_) {res = subset(search , COD_MPIO == loc[[index_]])
    if(dim(res)[1]>0)
    {
      return(1)
    }
    else
    {
      return(0)
    }}
    
    
    coor_fun = function(index_) {res = subset(search , COD_MPIO == loc[[index_]])
    if(dim(res)[1]>0)
    {
      return(c(res[1,'x'],res[1,'y']))
    }
    else
    {
      temp = sample(1:length(search[['x']]), 1)
      return(c(search[[temp,'x']],search[[temp,'y']]))
      
    }}
    
  }else if(strcmpi(geo_scheme,'DEPARTMENT')
           || strcmpi(geo_scheme,'DEP'))
  {
    col_name = 'depto'
    binary_fun = function(index_) {res = subset(search , COD_DPTO == loc[[index_]])
    if(dim(res)[1]>0)
    {
      return(1)
    }
    else
    {
      return(0)
    }}
    
    coor_fun = function(index_) {res = subset(search , COD_DPTO == loc[[index_]])
    if(dim(res)[1]>0)
    {
      return(c(res[1,'x'],res[1,'y']))
    }
    else
    {
      temp = sample(1:length(search[['x']]), 1)
      return(c(search[[temp,'x']],search[[temp,'y']]))
      
    }}
    
  }else
  {
    stop(paste('geo_scheme not supported:',geo_scheme))
  }
  
  
  loc_data = lapply(tda$points_in_vertex, function(coord_) df[,col_name][coord_])
  loc = lapply(1:tda$num_vertices, function(index_) {
                                                      ux <- unique(loc_data[[index_]])
                                                      ux[which.max(tabulate(match(loc_data[[index_]], ux)))]})
  
  #Gets the coordinates for each node
 
  #Fist finds the localities that match
  match = sapply(1:tda$num_vertices, binary_fun)
  if(!random)
  {
    
    print(paste('Before:',length(node_ids)))
    
    matched_ids = which(match == 1)
    print(paste('After:',length(matched_ids)))
    
    node_ids = matched_ids
    loc = loc[matched_ids]
    tda$adjacency = (tda$adjacency[node_ids,])[,node_ids]
    tda$num_vertices = length(node_ids)
    tda$points_in_vertex = tda$points_in_vertex[node_ids]
    
  }
    
  
  
  map_coordintes = lapply(1:tda$num_vertices, coor_fun)  
  
  #Changes the coordinates to a matrix for ploting
  map_coordintes = matrix(unlist(map_coordintes), ncol = 2, byrow = TRUE)  

  #Extracts the adjacency matrix
  adj = mat2listw(tda$adjacency)
  
  
  #------------
  # Sizes
  #------------
  
  #Constructs the sizes for each node
  vert_draw_sizes = rep(1,tda$num_vertices)  
  
  #If only topology is True, then all the nodes are drawned with size 1
  if(!topology_only)
  {
    #Gets the amount of elements in each node
    vert_sizes = sapply(tda$points_in_vertex,length)
    max_size = max(vert_sizes)
    min_size = min(vert_sizes)
    
    #normalize the sizes so they can be showed
    vert_draw_sizes = sapply(vert_sizes, function(size_) max(min_draw_size, (size_*max_draw_size)/max_size))
  }

  #------------
  # Colors
  #------------
  color_rgbs = rep(rgb(color[1],color[2],color[3], maxColorValue = 255),tda$num_vertices)
  
  if(!topology_only && !is.null(color_scheme) && !strcmpi(color_scheme,'NONE'))
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
      if(max_color_number == 0)
      {
        color_percentages = rep(0, tda$num_vertices)
      }
      else
        color_percentages = sapply(color_numbers, function(color_num_) round((color_num_/max_color_number)*100))
      
    }
    else
    {
      stop(paste('color scheme: ',color_scheme, ' not supported', sep = ''))
    }
    
    # Calculates the RGB colors
    # Uses the percetage for the alpha filter (the fourth byte
    color_rgbs = mapply(function(perc_, color_) {return(rgb(color_[1], color_[2], color_[3], (perc_/100)*255 , maxColorValue = 255))},color_percentages,color_rgbs_coordinates)
    
    
  }

  dir.create('~/Dropbox/Andres_Angel-Felipe_Fongalez/TDA Enfermedades Colombia/interactive_results/malaria_control_geo/weighted_distance_muni/age_min/plots')
  png('~/Dropbox/Andres_Angel-Felipe_Fongalez/TDA Enfermedades Colombia/interactive_results/malaria_control_geo/weighted_distance_muni/age_min/plots/mapa_sin.png',width = 1200, height = 1200)
  plot(munimala, col = "#f0e0c0")
  points(map_coordintes[,1], map_coordintes[,2], cex = vert_draw_sizes, pch = 21, bg = color_rgbs)
  dev.off()
  
  dir.create('~/Dropbox/Andres_Angel-Felipe_Fongalez/TDA Enfermedades Colombia/interactive_results/malaria_control_geo/weighted_distance_muni/age_min/plots')
  png('~/Dropbox/Andres_Angel-Felipe_Fongalez/TDA Enfermedades Colombia/interactive_results/malaria_control_geo/weighted_distance_muni/age_min/plots/mapa_con.png',width = 1200, height = 1200)
  plot(munimala, col = "#f0e0c0")
  plot(adj, map_coordintes, add = TRUE, col = "red", lty = 3, points = FALSE) 
  points(map_coordintes[,1], map_coordintes[,2], cex = vert_draw_sizes, pch = 21, bg = color_rgbs)
  dev.off()

plot(munimala, col = "#f0e0c0")
if(lines)
{
  plot(adj, map_coordintes, add = TRUE, col = "red", lty = 3, points = FALSE) 
}
points(map_coordintes[,1], map_coordintes[,2], cex = vert_draw_sizes, pch = 21, bg = color_rgbs)

}
