# Script to construct results
# Should be runned in the folder/workspace: TDA Enfermedades Colombia/RScripts
library('RMySQL')
#library('proxy')
library('pracma')
setwd("~/Dropbox/Andres_Angel-Felipe_Fongalez/TDA Enfermedades Colombia/interactive_sivigila_results/RScripts")
source('mapperKD_low_RAM.R')
source('weighted_binary_dist.R')

# ----------------------------------------------
# -------------- GENERIC PARAMETERS ------------
# ----------------------------------------------
# The Path where the data will be stored
path = '../'




# ---------------------------------------------
# -------------- CUSTOM PARAMETERS ------------
# ---------------------------------------------
# -- LOW RAM CONFIGURATION
low_ram = TRUE

# --  Prints notification each time an iteration is completed
print_iterations = TRUE


# -- DATA
# Folder/Data name. Corresponds to the data that is going to be used for the analysis
folder_data_name = 'malaria_disease_zone_3'
# --- In Case the data set is new
data_description = "Social economic data of diferent resport of infectous diseases around Colombia corresponding to outskirts populations" # The description of the data set

#This parameter indicates from where the records will be loaded.
data_base_source = NULL

#If the source file comes from a csv file
data_base_source = 'CSV'
csv_location = "~/Dropbox/Andres_Angel-Felipe_Fongalez/TDA Enfermedades Colombia/sivigila_datos_extraidos/datos_merged_complete/malaria_complete.csv"

#if the data comes from an sql database
#data_base_source = 'SQL'
SQL_statement  = 
  '' #SQL Statement

#If the data comes from an Rdata
#data_base_source = 'RDATA'
rdata_file_location = '~/Dropbox/Andres_Angel-Felipe_Fongalez/TDA Enfermedades Colombia/sivigila_datos_extraidos/datos_merged_complete/malaria_db.RData'

#Data customization function
#This is a function that receives the data so that manipulates the record as the user wishes

data_process_function = function(data)
{
  return(data[which(data$Area == '3 - AREA RURAL DISPERSA'),])
}




# -- DISTANCE
# Folder/Distance name. Corresponds to the distance that is going to be used for the analysis
folder_distance_name = 'job_area'
# --- In case the distance has not been computed
distance_description = 'The distance for this experiment corresponds to the weighted binary distance, with the following weights:
Edad (Edad) = 0.2
Sexo (Sexo) = 0.2
Etnia (etnia) = 0.2
Area (Area) = 0
Tipo de Trabajo  (Grupo) = 1


The distance matrix is calculated summing manhattan distance matrices' # The distance description

# The distance function. Recieves the data set and returns dist matrix
distance_function = function(record)
{
  weights =   list(
    'Edad' = 0.2,
    'Sexo' = 0.2,
    'Etnia' = 0.2,
    'Area' = 0,
    'Grupo' = 1)
  
  col_names = list(
    'Edad',
    'Sexo',
    'Etnia',
    'Area',
    'Grupo')
  
  # 1 is Nominal, 2 is Numeric        
  data_type =   list(
    'Edad' = 2,
    'Sexo' = 1,
    'Etnia' = 1,
    'Area' = 1,
    'Grupo' = 1)
  
  distance = dist(rep(0,length(record[[col_names[[1]]]])), method = 'manhattan')
  
  #Constructs
  for(col in col_names)
  {
    if(weights[[col]]>0)
    {
      
      if(data_type[[col]] == 1)
      {
        u_nominal = unique(record[[col]])
        #If there is only one, no distance is requiered
        if(length(u_nominal) > 1)
        {
          coor_nominal = sapply(record[[col]], function(x) as.numeric(u_nominal == x))
          distance = distance + (weights[[col]]/2)*dist(t(coor_nominal), method = 'manhattan') 
        }
      }
      else if(data_type[[col]] == 2) 
      {
        numeric_ = sapply(record[[col]],as.numeric)
        dif = max(numeric_) - min(numeric_)
        if(dif > 0)
          distance = distance + (weights[[col]]/2)*dist(numeric_, method = 'manhattan')/dif
      }
      
    }
    
    
    
  }
  
  #print(record)
  #print(distance[distance > NaN])
  return(distance)
}

distance_function_low_ram = function(data,indices){
  
  return(distance_function(record = data[indices,]))
}



# -- FILTER FUNCTION
# the name of the filter/folder of the results
filter_results_name = 'week_reported'
# The filter function description to be displayed
filter_description = "The filter used will be the case Week of the year"
# The filter function that recieves the data (in a dataFrame object) and outputs the filter values to work with.
filter_function = function(x)
{
  return(list(x$Semana.Epidemiologica))
}

# -- TDAMapper Configuration

# Number of dimensions
k = 1
# Number of Intervals
num_intervals = c(10)
# percentage of overlap
percent_overlap = 40
# number of bins when clustering
num_bins_when_clustering = 6

# The results description to be displayed
results_description = paste('The TDAmapper configuration is as follows: \n',
                            'k = ', k, '\n',
                            'num_intervals = ', num_intervals , '\n', 
                            'percent_overlap = ', percent_overlap, '\n',
                            'num_bins_when_clustering = ', num_bins_when_clustering, sep = '')

# -------------- EXCECUTES THE SCRIPT ------------


# --- DATA
# Checks if there is already a data file with the correponding data set.
# If there is such a file the scripts loads it, if not, there must be a valid sql statement to load
# the data. The script then saves it (remember a description should be included)
if(file.exists(paste(path,folder_data_name,'/RData/data.RData', sep = ''))) # If the file exists
{
  # Loads the data
  load(paste(path,folder_data_name,'/RData/data.RData', sep = ''))
  
  
} else
  # The file does not exist 
{
  if(is.null(data_description) 
     || (data_base_source == 'SQL' && is.null(SQL_statement))
     || (data_base_source == 'CVS' && is.null(csv_location))
     || (data_base_source == 'RDATA' && is.null(rdata_file_location)))
    stop('If the data is new (data.RData saved) a description and SQL statment must be provided or a .csv location must be given')
  
  # Creates the directory and loads the data
  dir.create(paste(path,folder_data_name, sep = ''), showWarnings = FALSE)
  dir.create(paste(path,folder_data_name,'/RData', sep = ''), showWarnings = FALSE)
  working_data = list(desc = data_description, data = NULL )
  
  if(data_base_source == 'SQL')
  {
    # The MySQL data base contection
    db_con = dbConnect(MySQL(), user="minigonche", password="luispolanco", dbname="col_data", host="col-data.coxl8wirabtc.us-west-1.rds.amazonaws.com")
    
    working_data$data = dbGetQuery(db_con,SQL_statement)
    
    #disconnects from db
    dbDisconnect(db_con)
    
  } else if(data_base_source == 'CSV')
  {
    working_data$data = read.csv(csv_location)
  
  }else if(data_base_source == 'RDATA')
  {
    working_data$data = load(rdata_file_location)
  }
  
  if(!is.null(data_process_function))
  {
    working_data$data = data_process_function(working_data$data)
  }  
  
  # Saves the file
  save(working_data, file = paste(path,folder_data_name,'/RData/data.RData', sep = ''))
  
} 

print('---- 1. Data Loaded')

# --- DISTANCE
# Checks if there is already a data file with the distance matrix.
# If there is such a file the scripts loads it, if not, there must be description and function to
# calculate the distance matrix. 
if(file.exists(paste(path,folder_data_name,'/',folder_distance_name, '/RData/distance.RData', sep = ''))) 
  # If the file exists
{
  # Loads the data
  if(!low_ram)
    load(paste(path,folder_data_name,'/',folder_distance_name, '/RData/distance.RData', sep = ''))
  
} else
  # The file does not exist 
{
  if(is.null(distance_description) || is.null(distance_function))
    stop('If the dsitance is new (distance.RData saved) a description and distance funciton must be provided')
  
  # Creates the directory and loads the data
  dir.create(paste(path,folder_data_name,'/',folder_distance_name, sep=''), showWarnings = FALSE)
  dir.create(paste(path,folder_data_name,'/',folder_distance_name, '/RData', sep=''), showWarnings = FALSE)
  
  #if is not in the low_ram enviorment
  if(!low_ram)
  {
    distance = list(desc = distance_description, matrix = NULL )
    # Creates the distance function as 'do_distance'
    setGeneric('do_distance', distance_function)
    distance$matrix = do_distance(working_data$data) 
    
    # Saves the file
    save(distance, file = paste(path,folder_data_name,'/',folder_distance_name, '/RData/distance.RData', sep = ''))
  }
  
} 

# Since the distance files are large and are not needed for display, a separate file is saved with the object: distance_description
save(distance_description, file = paste(path,folder_data_name,'/',folder_distance_name, '/RData/distance_description.RData', sep = ''))

print('---- 2. Distance matrix Loaded')


# -- RESULTS NAME
# Checks is there is already a folder with the given result name
if(file.exists(paste(path,folder_data_name,'/',folder_distance_name, '/', filter_results_name, sep = '')))
{
  response = readline(prompt=paste("Results with name: ",filter_results_name, ", already exists. Override?  ", sep='' ))
  if(strcmpi(response,'YES') || strcmpi(response,'Y') )
    print('OK')
  else{stop(paste('There is already a folder with the given results name: ', filter_results_name, sep = ''))}
}


# Creates the directory and its structure
# The Complete path for the case
case_complete_path = paste(path,folder_data_name,'/',folder_distance_name, '/', filter_results_name, '/', sep = '')
dir.create(paste(path,folder_data_name,'/',folder_distance_name, '/', filter_results_name, sep = ''), showWarnings = FALSE)
dir.create(paste(case_complete_path,'TDA', sep = ''), showWarnings = FALSE)
file.copy(paste(path,'results_template.Rmd', sep = ''), paste(case_complete_path,'results.Rmd', sep = ''))

print('---- 3. Results Structure Created')


# FIlTER FUNCITON
if(is.null(filter_description) || is.null(filter_function))
  stop('A filter description and function must be provided')

# Sets the filter_function as a method with name: 'do_filter'
setGeneric('do_filter', filter_function)

# Constructs the structure
filter_function = list(desc = filter_description, values = do_filter(working_data$data))
# Saves the structure
save(filter_function, file = paste(case_complete_path, 'TDA/filter.RData', sep = ''))

print('---- 4. Filter Computed')

# -- TDA
if(is.null(results_description)
   || is.null(k)
   || is.null(percent_overlap)
   || is.null(num_bins_when_clustering))
  stop('Please supply all of the TDAmapper iputs')

tda = list(desc = results_description, result = NULL)

#Excecutes the Topological Data Analysis
if(low_ram)
{
  tda$result = mapperKD(k = k,
                        distance_matrix = distance$matrix,
                        filter_values = filter_function$values,
                        num_intervals = num_intervals,
                        percent_overlap = percent_overlap,
                        num_bins_when_clustering = num_bins_when_clustering,
                        low_ram = TRUE,
                        distance_function = distance_function_low_ram,
                        data = working_data$data,
                        print_iterations = print_iterations)
  
}else
{
  tda$result = mapperKD(k = k,
                        distance_matrix = distance$matrix,
                        filter_values = filter_function$values,
                        num_intervals = num_intervals,
                        percent_overlap = percent_overlap,
                        num_bins_when_clustering = num_bins_when_clustering,
                        low_ram = FALSE,
                        distance_function = NULL,
                        data = NULL,
                        print_iterations = print_iterations)
}


save(tda, file = paste(case_complete_path, 'TDA/tda.RData', sep = ''))

print('---- 4. TDA finnished')

# -- COMEMENTS

comments = 'No Comments Available'
save(comments, file = paste(case_complete_path, 'TDA/comments.RData', sep = ''))
print('---- 5. Comments created')

# -- EXCECUTION SCRIPT
# Copy the excecution script with the specific configuration
file.copy('result_constructor.R', paste(case_complete_path, 'excecution_script.R', sep = ''))
print('---- 6. Excecution Script Coppied!!')


#removes elements from workspace
rm(comments, 
   tda, 
   filter_function,
   distance, 
   distance_description, 
   working_data,
   case_complete_path,
   data_description,
   do_filter,
   filter_description,
   filter_results_name,
   folder_distance_name,
   folder_data_name,
   k,
   num_bins_when_clustering,
   num_intervals,
   path,
   percent_overlap,
   SQL_statement,
   results_description)


print('---- 7. ALL DONE!!')




