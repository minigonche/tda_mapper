library('shiny')
source('create_graph.R')
# A function that runs a ShinyApp that enables the user to interact with the results
#' @param data_file The file containing the data used for the experiment. The object with the data saved in the previous file should be under the variable name working_data and should be stored with the structure:
#'       working_data.desc a description of the data
#'       working_data.data a dataframe with the data
#' @param distance_file A file containing the description of the distance under the variable name distance_function
#' @param tda_file A file containing the resulting TDA. The resulting TDAMapper object should be saved under the varaible name tda with the structure:
#'       tda.desc a description of the analysis (probably the configuration used)
#'      tda.result the TDAmapper object
#' @param filter_file A file with the filter function used for the experiment. The object should be under the name filter_function with the following structure: 
#'       filter_function.desc the description of the filter function
#'       filter_function.values the used values     
#' @param comments_file A file containing the possible comments the results may have, under the variable comments.
#' @param plot_location A directory where the plots will be saved           
TDAInteractiveResults = function(data_file, distance_file, filter_file, tda_file, comments_file, plot_location) 
{
  #Description Columns
  desc_col = 10
  
  #Loads the different elements used for the shiny app
  if(!file.exists(data_file)
     || !file.exists(distance_file)
     || !file.exists(filter_file)
     || !file.exists(tda_file))
    stop('Please supply existing files for the data, distance, filter and tda objects')

  #data file
  load(data_file)
  #distance description
  load(distance_file)
  #filter function
  load(filter_file)
  #tda
  load(tda_file)
  #comments
  if(!file.exists(comments_file))
  {
    comments = 'No Comments Available'
    save(comments, file = comments_file)
  }
  load(comments_file)
  
  
  #-------------------------------
  #------- User Interface --------
  #-------------------------------
  
  # Label display sizes
  min_size = min(sapply(tda$result$points_in_vertex,length))
  max_size = max(sapply(tda$result$points_in_vertex,length))
  
  # The color scheme options
  label_display_scheme_options = c('None','Node ids','Node Sizes', 'Color Scheme Value')
  label_scheme_options = c('NONE','Numbers','Sizes', 'Color Function')
  
  ui <- shinyUI(navbarPage("Results", 
  
        #Data Description                            
        tabPanel("Data",fluidPage( tags$textarea(id="data_area", rows=desc_col, style="width:100%;", working_data$desc),
                                   fluidRow(column(4,actionButton("save_data", "Save Data Description")),column(8)),
                                   fluidRow(column(12,  textOutput('data_label'))))),
        #Distance Description
        tabPanel("Distance",fluidPage( tags$textarea(id="distance_area", rows=desc_col, style="width:100%;", distance_description),
                                       fluidRow(column(2,actionButton("distance_filter", "Save Distance Description")),column(10)),
                                       fluidRow(column(12, textOutput('distance_label'))))),
        #Filter Description
        tabPanel("Filter",fluidPage( tags$textarea(id="filter_area", rows=desc_col, style="width:100%;", filter_function$desc),
                                     fluidRow(column(2,actionButton("save_filter", "Save Filter Description")),column(10)),
                                     fluidRow(column(12, textOutput('filter_label'))))),
        #tda Description
        tabPanel("TDA",fluidPage( tags$textarea(id="tda_area", rows=desc_col, style="width:100%;", tda$desc),
                                  fluidRow(column(2,actionButton("save_tda", "Save TDA Description")),column(10)),
                                  fluidRow(column(12, textOutput('tda_label'))))),
        #Topological Layout --------------------
        tabPanel("Topological Layout",fluidPage(   
          
          title = "Topology Visualization",
          plotOutput('topo_plot'),
          hr(),
          fluidRow(column(5),column(2,actionButton("run", "Plot Now")),column(5)),
          hr(),
          fluidRow(column(12,h4('Select Ids to Display')), fluidRow(column(5,checkboxInput("filter_nodes", label = "Display only the following nodes:", value = FALSE)),
                                                                    column(7,textInput("filter_nodes_ids", "Node Ids", value = NULL)))),
          fluidRow(
            column(2,
                   h4('Topology'),
                   selectInput('topology_only', 'Topology Only', c(FALSE,TRUE), selected=FALSE),
                   numericInput('min_draw_size', 'Min Node Size', 2, min = 1, max = 20),
                   numericInput('max_draw_size', 'Max Node Size', 35, min = 1, max = 50)
            ),
            column(3,
                   h4('Node Color & Scheme'),
                   selectInput('label_scheme', 'Label Scheme', label_display_scheme_options, selected=label_display_scheme_options[1]),
                   numericInput('red', 'Color (Red, Green, Blue)', 0, min = 0, max = 255, width = '100%'),
                   numericInput('green', NULL,220, min = 0, max = 255, width = '100%'),
                   numericInput('blue', NULL, 0, min = 0, max = 255, width = '100%')
            ),
            column(7,
                   h4('Node Color Scheme'),
                   fluidRow( column(4,radioButtons("scheme_type", "Scheme Option", choices = list("None" = 1,"Concentration" = 2, "Count"=3, "Mean Value" = 4 ), selected = 1)),
                             column(8,selectInput("color_attribute", "Attribute", colnames(working_data$data), selected = colnames(working_data$data)[1]))),
                   fluidRow(column(8,textInput("attribute_value", "Attribute Value to Match", value = NULL)),
                            column(4,radioButtons("data_type", "Attribute Data Type", choices = list("Nominal" = 1,"Numerical" = 2 ),selected = 1)))
            )),
          fluidRow(column(12,sliderInput("range", label = h4("Display Labels for Nodes with Number of Elements Inside:"), min = min_size , 
                                         max = round(max_size*1.1), value = c(min_size, round(max_size*1.1)), width = '100%'))),
          hr(),
          h4('Save Plot'),
          fluidRow(column(12,textInput("file_name", "File Name:", value = NULL), fluidRow(column(3, actionButton("save", "Save") ), column(1), column(8, textOutput('save_label'))) ))
          
          
        )), 
        #Node Inspection --------------------
        tabPanel("Node Inspection",fluidPage( 
          title = "Node Visualization",
          fluidRow(column(6,
                          h5('Node Size:'),
                          textOutput('node_size'))),
          plotOutput('node_plot'),
          hr(),
          fluidRow(column(5,radioButtons("data_scheme", "Nodes Selected", choices = list("Single Node" = 1,"Multiple Nodes" = 2, "All Nodes"=3),selected = 1)),
                   column(7,numericInput('node_id', 'Single Node Id', 1, min = 1, max = tda$result$num_vertices),textInput("filter_nodes_ids", "Multiple Node Ids", value = NULL))),
          fluidRow(
            column(6, 
                   selectInput("selected_attribute", "Attribute", colnames(working_data$data), selected = colnames(working_data$data)[1])
            ),
            column(3,radioButtons("orientation", "Orientation", choices = list("Horizontal" = 1,"Vertical" = 2 ),selected = 1)),
            column(3,radioButtons("mode", "Mode", choices = list("Count" = 1,"Percentage" = 2 ),selected = 1))
          ),
          fluidRow(column(12,sliderInput("range_values", label = h4("Display Labels with Percentage Between:"), min = 0 , max = 100, value = c(0, 100), width = '100%'))),
          h4('Save Plot'),
          fluidRow(column(5, textInput("file_name_plot", "File Name:", value = NULL)), column(7)),
          fluidRow(column(3, actionButton("save_plot", "Save") ), column(1), column(8, textOutput('save_plot_label'))) 
        )) ,
        #Comments Description
        tabPanel("Comments",fluidPage( tags$textarea(id="comments_area", rows=15, style="width:100%;", comments),
                                                           fluidRow(column(2,actionButton("save_comments", "Save Comments")),column(10)),
                                                           fluidRow(column(12,  textOutput('comments_label'))))) 
                            
  )) 
  
  #-------------------------------
  #---------- Server -------------
  #-------------------------------
  
  
  server <- function(input, output, session) {
    
    #DATA
    observeEvent(input$save_data, {
      working_data$desc = input$data_area
      save(working_data, file = data_file)
      output$data_label <- renderPrint({'Data Description Saved'})
    }) 
    
    observeEvent(input$data_area, {
      output$data_label <- renderPrint({'(Not Saved)'})
    }) 
    
    #DISTANCE 
    observeEvent(input$distance_filter, {
      distance_description =  input$distance_area
      save(distance_description, file = distance_file)
      output$distance_label <- renderPrint({'Distance Description Saved'})
    })
    
    observeEvent(input$distance_area, {
      output$distance_label <- renderPrint({'(Not Saved)'})
    })
    
    #FILTER  
    observeEvent(input$save_filter, {
      filter_function$desc = input$filter_area
      save(filter_function, file = filter_file)
      output$filter_label <- renderPrint({'Filter Description Saved'})
    })
    
    observeEvent(input$filter_area, {
      output$filter_label <- renderPrint({'(Not Saved)'})
    }) 
    
    #TDA
    observeEvent(input$save_tda, {
      tda$desc = input$tda_area
      save(tda, file = tda_file)
      output$tda_label <- renderPrint({'TDA Description Saved'})
      
    })
    
    observeEvent(input$tda_area, {
      output$tda_label <- renderPrint({'(Not Saved)'})
    }) 
    
    #COMMENTS
    observeEvent(input$save_comments, {
      
      comments = input$comments_area
      save(comments, file = comments_file)
      output$comments_label <- renderPrint({'Comments Saved'})
      
    })
    
    observeEvent(input$comments_area, {
      output$comments_label <- renderPrint({'(Not Saved)'})
    })   
    
    
    
    #TOPOLOGICAL LAYOUT
    graph <- eventReactive(input$run, {
      
      output$save_label <- renderPrint({'(Not Saved)'})
      
      df = working_data$data;
      tda = tda$result;
      only_node_ids = NULL
      min_draw_size = input$min_draw_size;
      max_draw_size = input$max_draw_size;
      color_scheme = 'NONE';
      color_attributes = NULL;
      color_fun = NULL;
      color = c(input$red,input$green,input$blue);
      label_scheme = label_scheme_options[which(label_display_scheme_options == input$label_scheme)[1]];
      min_label_node_size = input$range[1];
      max_label_node_size = input$range[2]
      topology_only = input$topology_only;
      
      if(input$filter_nodes)
      {
        only_node_ids = sapply(strsplit(x = input$filter_nodes_ids, split = c(',',';')), as.numeric)
        only_node_ids = unique(only_node_ids)
        only_node_ids = sort(only_node_ids)
      }
      
      
      if(input$scheme_type == 2)
      {
        color_scheme = 'CONCENTRATION'
        color_attributes = c(input$color_attribute)
        if(input$data_type == 1)
          color_fun = function(x){ (length(which(x == input$attribute_value))/length(x))*100}
        else if (input$data_type == 2) 
        {
          num = as.numeric(input$attribute_value)
          color_fun = function(x){ (length(which(x == num ))/length(x))*100}
        }
      }
      else if(input$scheme_type == 3)
      {
        color_scheme = 'LINEAR'
        color_attributes = c(input$color_attribute)
        color_fun = function(x){ length(which(x == input$attribute_value))}
      }
      else if(input$scheme_type == 4)
      {
        color_scheme = 'LINEAR'
        color_attributes = c(input$color_attribute)
        color_fun = mean
      }
      
      return(create_graph(df=df,
                          tda=tda,
                          only_node_ids = only_node_ids,
                          min_draw_size = min_draw_size,
                          max_draw_size = max_draw_size,
                          color_scheme = color_scheme,
                          color_attributes = color_attributes,
                          color_fun = color_fun,
                          color = color,
                          label_scheme = label_scheme,
                          min_label_node_size = min_label_node_size,
                          max_label_node_size = max_label_node_size, 
                          topology_only = topology_only))
      
    })
    
    observeEvent(input$save, {
      
      if(is.null(input$file_name) || input$file_name == '')
      {
        output$save_label <- renderPrint({'Please supply a valid file name'})
      }
      else
      {
        dir.create(plot_location)
        png(paste(plot_location,'/',input$file_name,'.png', sep=''))
        plot(graph())
        dev.off()
        output$save_label <- renderPrint({'Plot Saved'})
      }
      
    })
    
    output$topo_plot <- renderPlot({
      plot(graph())
    }, height = 500, width = 700)
    
    
    #NODE INSPECTION
    
    output$node_size <- renderPrint({ 
      if(input$data_scheme==3)
        return(dim(working_data$data)[1])
      else if(input$data_scheme==2)
      {
        only_node_ids = sapply(strsplit(x = input$filter_nodes_ids, split=c(',',';')), as.numeric)
        only_node_ids = unique(only_node_ids)
        only_node_ids = sort(only_node_ids)
        return(length(unlist(tda$result$points_in_vertex[only_node_ids])))
        
      }
      else  
        return(length(tda$result$points_in_vertex[[input$node_id]]))
      
    })
    
    
    output$node_plot <- renderPlot({
      
      output$save_plot_label <- renderPrint({'(Not Saved)'})
      data = working_data$data[ tda$result$points_in_vertex[[input$node_id]],]
      if(input$data_scheme==3)
        data = working_data$data
      else if(input$data_scheme==2)
      {
        only_node_ids = sapply(strsplit(x = input$filter_nodes_ids, split=c(',',';')), as.numeric)
        only_node_ids = unique(only_node_ids)
        only_node_ids = sort(only_node_ids)
        data = working_data$data[ unlist(tda$result$points_in_vertex[only_node_ids]),]
      }
      
      
      
      # Trims columns
      size = dim(data)[1]
      
      bar_data = table(data[[input$selected_attribute]])
      bar_data = bar_data[(bar_data >= (input$range_values[1]/100)*size)]
      bar_data = bar_data[(bar_data <= (input$range_values[2]/100)*size)]
      
      
      if(input$mode==1)
      {
        # The bar_data is already in this format
      }
      else if(input$mode==2)
      {
        bar_data = bar_data/size
      }
      
      if(input$orientation==1)
      {
        left_margin = max(4,sapply(working_data$data[[input$selected_attribute]], nchar))
        left_margin = min(30, left_margin)
        par(mar=c(5,left_margin,2,2))
        
        
        barplot(bar_data, horiz = TRUE, las=1)
      }
      else if(input$orientation==2)
      {
        barplot(bar_data)
      }    
      
    })
    
    observeEvent(input$save_plot, {
      
      if(is.null(input$file_name_plot) || input$file_name_plot == '')
      {
        output$save_plot_label <- renderPrint({'Please supply a valid file name'})
      }
      else
      {
        
        dir.create(plot_location)
        png(paste(plot_location,'/',input$file_name_plot,'.png', sep=''),width = 1200, height = 600)
        
        data = working_data$data[ tda$result$points_in_vertex[[input$node_id]],]
        if(input$data_scheme==3)
          data = working_data$data
        else if(input$data_scheme==2)
        {
          only_node_ids = sapply(strsplit(x = input$filter_nodes_ids, split=c(',',';')), as.numeric)
          only_node_ids = unique(only_node_ids)
          only_node_ids = sort(only_node_ids)
          data = working_data$data[ unlist(tda$result$points_in_vertex[only_node_ids]),]
        }
        
        
        
        # Trims columns
        size = dim(data)[1]
        
        bar_data = table(data[[input$selected_attribute]])
        bar_data = bar_data[(bar_data >= (input$range_values[1]/100)*size)]
        bar_data = bar_data[(bar_data <= (input$range_values[2]/100)*size)]
        
        
        if(input$mode==1)
        {
          # The bar_data is already in this format
        }
        else if(input$mode==2)
        {
          bar_data = bar_data/size
        }
        
        if(input$orientation==1)
        {
          left_margin = max(4,sapply(working_data$data[[input$selected_attribute]], nchar))
          left_margin = min(30, left_margin)
          par(mar=c(5,left_margin,2,2))
          
          
          barplot(bar_data, horiz = TRUE, las=1)
        }
        else if(input$orientation==2)
        {
          barplot(bar_data)
        }    
        
        dev.off()
        output$save_plot_label <- renderPrint({'Plot Saved'})
        
      }
      
    })
    
  }
  
  
  shinyApp(ui, server, options = list(height = 1350))
  
  
}
