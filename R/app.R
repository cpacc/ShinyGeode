# SHINYGEODE --------------------------------------------------------------

# library(shiny)

# max file size for upload
options(shiny.maxRequestSize = 50 * 1024^2)

#' A user-friendly interface for accessing the spatial mapping and analysis functions of geode
#' 
#' The ShinyGeode interface allows interactive visualization, mapping and analysis of geospatial 
#' data through point-and-click, drop-down menus, checkboxes and sliders; no R coding is required.
#' The purpose of ShinyGeode is to provide a low barrier interface that benefits a wide range of 
#' users. It allows rapid interaction with spatial data for identifying patterns, generating 
#' hypotheses, and informing further investigation and analysis.
#' 
#' @import shiny
#' @import tmap
#' 
#' @export
 


ShinyGeode <- function(...) {
  
  
  ui <- fluidPage(
    
    titlePanel(p("ShinyGeode: An interface for spatial mapping and analysis", style = "color:#1F2F88")),
    
    
    # Output display with tabs
    tabsetPanel(type = "tabs",
                

# Data import page --------------------------------------------------------

                
                tabPanel("Data", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             # import shapefile
                             helpText("*Select all related map files at once: shp, dbf, shx..."),
                             
                             fileInput(inputId = "filemap",
                                       label = "Upload spatial data...",
                                       multiple = TRUE,
                                       accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                             
                             # choose whether to simplify shapefile
                             checkboxInput(inputId = "choosesimple",
                                           label = "Simplify shapefile?",
                                           value = FALSE,
                                           width = '100%'),
                             
                             
                             br(),
                             hr(),
                             
                             # import csv attribute data
                             fileInput(inputId = "csvdata",
                                       label = "Optional: upload attribute data (csv file)...",
                                       accept = c(".csv")),
                             
                             # include CPAC logo
                             img(src = "cpac-logo.png", width = 300, height = 70)
                             
                           ),
                           
                           mainPanel(
                             
                            
                             div(DT::DTOutput(outputId = "shptable"), style = "font-size: 75%; width: 50%"),
                             
                             hr(),
                             headerPanel(""),
                             br(),
                             
                             div(DT::DTOutput(outputId = "attrtable"), style = "font-size: 75%; width: 50%")
                             
                           )
                         )
                ),
   
             

# Choropleth map page -----------------------------------------------------
                
                tabPanel("Choropleth map", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             # choose variable to plot
                             uiOutput("mapvar_ui"),
                             
                             
                             sliderInput("chorolevels", "Shading levels",
                                         min = 1, max = 20, value = 5),
                             
                             sliderInput("translevels", "Transparency",
                                         min = 0, max = 1, value = 0.5),
                             
                             # choose variable to show during hover-over
                             uiOutput("hovervar_ui"),
                             
                             # choose whether to show legend
                             checkboxInput(inputId = "legendoptions",
                                           label = "Display legend",
                                           value = TRUE,
                                           width = '100%'),
                             
                             # choose whether to show compass & scalebar on map
                             checkboxInput(inputId = "mapoptions",
                                           label = "Display scalebar",
                                           value = FALSE,
                                           width = '100%'),
                             
          
                             # include CPAC logo
                             img(src = "cpac-logo.png", width = 300, height = 70)
                             
                           ),
                           
                           
                           mainPanel(
                             
                             tmap::tmapOutput(outputId = "map"),
                             
                             br(),
                             
                             uiOutput("download_choro")
                           )
                         )
                ),
                


# Pointmap page -----------------------------------------------------------

                tabPanel("Pointmap", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             helpText("*Both spatial and attribute data must be uploaded"),
                             
                             # choose region variable to plot
                             uiOutput("pointmap_var_ui"),
                             
                             # choose points names variable
                             uiOutput("pointmap_names_ui"),
                             
                             # choose points x variable to plot
                             uiOutput("pointmap_xvar_ui"),
                             
                             # choose points y variable to plot
                             uiOutput("pointmap_yvar_ui"),
                             
                             
                             
                             # include CPAC logo
                             img(src = "cpac-logo.png", width = 300, height = 70)
                             
                           ),
                           
                           
                           mainPanel(
                             
                             tmap::tmapOutput(outputId = "pointmap"),
                             
                             br(),
                             
                             uiOutput("download_pointmap")
                             
                           )
                         )
                ),
                


# Heatmap page ------------------------------------------------------------

                tabPanel("Heatmap", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             helpText("*Both spatial and attribute data must be uploaded"),
                             
                             # choose region variable to plot
                             uiOutput("heatmap_var_ui"),
                             
                             
                             # choose points x variable to plot
                             uiOutput("heatmap_xvar_ui"),
                             
                             # choose points y variable to plot
                             uiOutput("heatmap_yvar_ui"),
                             
                             
                             
                             # include CPAC logo
                             img(src = "cpac-logo.png", width = 300, height = 70)
                             
                           ),
                           
                           
                           mainPanel(
                             
                             plotOutput(outputId = "heatmap"),
                             
                             br(),
                             
                             uiOutput("download_heatmap")
                           )
                         )
                ),
            
    

# Spatial patterns analysis page ------------------------------------------
                
                tabPanel("Spatial patterns", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             # select analysis
                             selectInput(
                               inputId = "clusteranalysis",
                               label = "Select analysis type...",
                               choices = c("local_ac",
                                           "global_ac"),
                               selected = "local_ac"),
                             
                             # choose variable for analysis
                             uiOutput("mapvarcluster_ui"),
                             
                             # choose variable for denominator
                             uiOutput("denomcluster_ui"),
                             
                             # choose variable to show during hover-over
                             uiOutput("hovervarcluster_ui"),
                             
                             sliderInput("translevelcluster", "Transparency",
                                         min = 0, max = 1, value = 0.5),
                             
                             
                             # include CPAC logo
                             img(src = "cpac-logo.png", width = 300, height = 70)
                             
                           ),
                           
                           mainPanel(
                             
                             # show cluster map
                             tmap::tmapOutput(outputId = "clustermap"),
                             
                             br(),
                             
                             uiOutput("download_clustermap")
                             
                           )
                           
                         )
                         
                ),
          
      

# Proximity analysis page -------------------------------------------------
                
                tabPanel("Proximity analysis", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             helpText("*Both spatial and attribute data must be uploaded"),
                             
                             # choose region variable to plot
                             uiOutput("proximity_geovar_ui"),
                             
                             # choose location ID variable to plot
                             uiOutput("proximity_idvar_ui"),
                             
                             # choose points x variable to plot
                             uiOutput("proximity_xvar_ui"),
                             
                             # choose points y variable to plot
                             uiOutput("proximity_yvar_ui"),
                             
                             sliderInput("proximity_n", "Number of nearest locations for proximity analysis",
                                         min = 1, max = 10, value = 3),
                             
                             sliderInput("proximity_chorolevels", "Shading levels",
                                         min = 1, max = 20, value = 5),
                             
                             
                             # include CPAC logo
                             img(src = "cpac-logo.png", width = 300, height = 70)
                             
                           ),
                           
                           mainPanel(
                             
                             # show proximity map
                             tmap::tmapOutput(outputId = "proximitymap"),
                             
                             br(),
                             
                             uiOutput("download_proximitymap")
                             
                             
                           )
                           
                         )
                         
                ),
                
                

# Cluster detection analysis page -----------------------------------------
                
                tabPanel("Cluster detection", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             # select analysis
                             selectInput(
                               inputId = "detectanalysis",
                               label = "Select cluster detection method...",
                               choices = c("kulldorff_binomial",
                                           "kulldorff_poisson"),
                               selected = "lkulldorff_binomial"),
                             
                             # choose variable for analysis
                             uiOutput("countsvar_detect_ui"),
                             
                             # choose variable for denominator
                             uiOutput("popvar_detect_ui"),
                             
                             # choose variable to show during hover-over
                             uiOutput("hovervar_detect_ui"),
                             
                             sliderInput("translevel_detect", "Transparency",
                                         min = 0, max = 1, value = 0.5),
                             
                             # include CPAC logo
                             img(src = "cpac-logo.png", width = 300, height = 70)
                             
                           ),
                           
                           mainPanel(
                             
                             # show map with detected clusters
                             tmap::tmapOutput(outputId = "detectmap"),
                             
                             br(),
                             
                             uiOutput("download_detectmap")
                             
                             
                           )
                           
                         )
                         
                )
                
    )
    
  )
  
  
  
  
  server <- function(input, output, session) {
    

# Data import functions ---------------------------------------------------

    # csv file attribute data
    attrdata <- reactive({
      req(input$csvdata)
      geode::geo_import(path = input$csvdata$datapath,
                        filetype = 'attribute')
    })
    
    # shp file map data
    mapdata <- reactive({
      
      req(input$filemap)
      shpdf <- input$filemap
      
      # temporary directory where files are uploaded
      tempdirname <- dirname(shpdf$datapath[1])
      
      # rename files with proper file extensions
      for (i in 1:nrow(shpdf)) {
        file.rename(
          shpdf$datapath[i],
          paste0(tempdirname, "/", shpdf$name[i])
        )
      }
      
      # read shape file
      mapdata <- geode::geo_import(
        path = paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"),
        filetype = 'spatial',
        simplify = input$choosesimple
      )
      
      mapdata
      
    })
    
    # create table for input shapefile without geometry column, for faster searching
    maptable <- reactive({
      req(input$filemap)
      maptable <-  sf::st_drop_geometry(mapdata())
    })
    
    
    
    # show table of input spatial data
    output$shptable <- DT::renderDataTable({
      DT::datatable(maptable(), options = list(lengthMenu = c(2, 5, 10, 25), pageLength = 2))
    })
    
    # show table of input attribute data
    output$attrtable <- DT::renderDataTable({
      DT::datatable(attrdata(), options = list(lengthMenu = c(2, 5, 10, 25), pageLength = 2))
    })
    
    
    

# Choropleth map functions ------------------------------------------------

    # select variable to map from input dataset
    output$mapvar_ui <- renderUI({
      selectInput("mapvarselected", "Select region variable for mapping...",
                  choices = c("", names(mapdata())))
    })
    
    # select hover-over variable from input dataset
    output$hovervar_ui <- renderUI({
      selectInput("hoverselected", "Select variable to show when hovering over region...",
                  choices = names(mapdata()),
                  selected = "")
    })
    
    # create download button for outputting map
    output$download_choro <- renderUI({
      req(input$mapvarselected)
      downloadButton('OutputFile_choro', 'Download map')
    })
    
    # output function for saving map
    output$OutputFile_choro <- downloadHandler(
      filename = function() { paste0("chorolpleth_map", ".html") },
      content = function(file) {
        req(choromap())
        tmap::tmap_save(tm = choromap(), filename = file)
      })
    

    
    # Create map
    
    choromap <- reactive({
      
      req(input$mapvarselected)
      
      mapdata <- mapdata()
      
      # Select data to map
      # mapfiltered <- mapdata
      
      # user specified mapping variables
      mapvar <- rlang::sym(input$mapvarselected)
      hovervar <- rlang::sym(input$hoverselected)
      
      
      # user specified whether to show legend
      if (input$legendoptions) {
        legendvar <- input$mapvarselected
      } else { legendvar <- "none"}
      
      
      # show map
      p <- geode::geo_plot(data = mapdata,
                      geography_col = !!(mapvar),
                      plot_type = 'choropleth',
                      hover_id = !!(hovervar),
                      levels = input$chorolevels,
                      transparency = input$translevels,
                      #legend_title = ,
                      scale_bar = input$mapoptions,
                      compass = input$mapoptions,
                      interactive = TRUE
      )
    
    })
    
    output$map <- tmap::renderTmap({
      choromap()
    })
    


# Pointmap functions ------------------------------------------------------
    
    # select region variable to map from input dataset
    output$pointmap_var_ui <- renderUI({
      selectInput("pointmap_varselected", "Select region variable for mapping...",
                  choices= names(mapdata()))
    })
    
    # select point names hover-over variable
    output$pointmap_names_ui <- renderUI({
      selectInput("pointmap_names_selected", "Select point locations IDs",
                  choices= names(attrdata()))
    })
    
    # select point variable x coordinates to map from input dataset
    output$pointmap_xvar_ui <- renderUI({
      selectInput("pointmap_xvar_selected", "Select point location 'x' (longitude) values",
                  choices = c("", names(attrdata())),
                  selected = "")
    })
    
    # select point variable y coordinates to map from input dataset
    output$pointmap_yvar_ui <- renderUI({
      selectInput("pointmap_yvar_selected", "Select point locations 'y' (latitude) values",
                  choices = c("", names(attrdata())),
                  selected = "")
    })
    
    # create download button for outputting map
    output$download_pointmap <- renderUI({
      req(input$pointmap_varselected, input$pointmap_xvar_selected, input$pointmap_yvar_selected)
      downloadButton('OutputFile_pointmap', 'Download map')
    })
    
    # output function for saving map
    output$OutputFile_pointmap <- downloadHandler(
      filename = function() { paste0("pointmap", ".html") },
      content = function(file) {
        req(pointmap())
        tmap::tmap_save(tm = pointmap(), filename = file)
      })
    
    
    
    # Create pointmap
    pointmap <- reactive({
      
      req(input$pointmap_xvar_selected, input$pointmap_yvar_selected)
      
      mapdata <- mapdata()
      
      # Select data to map
      # mapfiltered <- mapdata
      
      # user specified mapping variables
      pointmapvar <- rlang::sym(input$pointmap_varselected)
      pointnamesvar <- rlang::sym(input$pointmap_names_selected)
      xvar <- rlang::sym(input$pointmap_xvar_selected)
      yvar <- rlang::sym(input$pointmap_yvar_selected)
      
      # rename x and y variables for point locations
      attrdata <- dplyr::rename(attrdata(), x = xvar, y = yvar)
      
      
      # show pointmap
      p <- geode::geo_plot(data = mapdata,
                      geography_col = !!(pointmapvar),
                      attribute_data = attrdata,
                      points_col = !!(pointnamesvar),
                      plot_type = 'pointmap',
                      interactive = TRUE
      )
      
    })
    
    output$pointmap <- tmap::renderTmap({
      pointmap()
    })
    
    
    

# Heatmap functions -------------------------------------------------------
    
    # select region variable to map from input dataset
    output$heatmap_var_ui <- renderUI({
      selectInput("heatmap_varselected", "Select region variable for mapping...",
                  choices= names(mapdata()))
    })
    
    
    # select point variable x coordinates to map from input dataset
    output$heatmap_xvar_ui <- renderUI({
      selectInput("heatmap_xvar_selected", "Select point location 'x' (longitude) values",
                  choices = c("", names(attrdata())),
                  selected = "")
    })
    
    # select point variable y coordinates to map from input dataset
    output$heatmap_yvar_ui <- renderUI({
      selectInput("heatmap_yvar_selected", "Select point locations 'y' (latitude) values",
                  choices = c("", names(attrdata())),
                  selected = "")
    })
    
    # create download button for outputting map
    output$download_heatmap <- renderUI({
      req(input$heatmap_xvar_selected, input$heatmap_yvar_selected)
      downloadButton('OutputFile_heatmap', 'Download map')
    })
    
    # output function for saving map
    output$OutputFile_heatmap <- downloadHandler(
      filename = function() { paste0("heatmap", ".png") },
      content = function(file) {
        req(heatmap())
        ggplot2::ggsave(filename = file, plot = heatmap(), device = 'png', 
                        height = 11, width = 8.5, units = 'in', dpi = 300)
      })
    
    
    # Create heatmap
    heatmap <- reactive({
      
      req(input$heatmap_xvar_selected, input$heatmap_yvar_selected)
      
      mapdata <- mapdata()
      
      # Select data to map
      # mapfiltered <- mapdata
      
      # user specified mapping variables
      heatmapvar <- rlang::sym(input$heatmap_varselected)
      xvar <- rlang::sym(input$heatmap_xvar_selected)
      yvar <- rlang::sym(input$heatmap_yvar_selected)
      
      # rename x and y variables for point locations
      attrdata <-  dplyr::rename(attrdata(), x = xvar, y = yvar)
      
      
      # show heatmap
      p <- geode::geo_plot(data = mapdata,
                      geography_col = !!(heatmapvar),
                      attribute_data = attrdata,
                      plot_type = 'heatmap'
      )
      
    })
    
    output$heatmap <- renderPlot({
      print(heatmap())
    })
    
    
    

# Spatial patterns analysis functions -------------------------------------
    
    # numeric column names
    mapdata_numcols <- reactive({
      mapdata_numcols <- dplyr::select_if(mapdata(), is.numeric)
      mapdata_numcols <- names(mapdata_numcols)
    })
    
    
    # select numeric variable to analyze from input dataset
    output$mapvarcluster_ui <- renderUI({
      selectInput("mapvarselected_cluster", "Select numeric variable for analysis",
                  choices = c("", mapdata_numcols()),
                  selected = "")
    })
    
    # select numeric denominator variable from input dataset for calc of rates
    output$denomcluster_ui <- renderUI({
      selectInput("denomselected_cluster", "Optional: Select deonminator variable for rates",
                  choices = c("", mapdata_numcols()),
                  selected = "")
    })
    
    # select hover-over variable from input dataset
    output$hovervarcluster_ui <- renderUI({
      selectInput("hoverselected_cluster", "Select variable to show when hovering over region",
                  choices = names(mapdata()))
    })
    
    
    # create download button for outputting map
    output$download_clustermap <- renderUI({
      req(input$mapvarselected_cluster)
      downloadButton('OutputFile_clustermap', 'Download map')
    })
    
    # output function for saving map
    output$OutputFile_clustermap <- downloadHandler(
      filename = function() { paste0("hotspot_map", ".html") },
      content = function(file) {
        req(clustermap())
        tmap::tmap_save(tm = clustermap(), filename = file)
      })
    
    
    # show map
    clustermap <- reactive({
      
      req(input$mapvarselected_cluster)
      
      mapdata <- mapdata()
      
      # Select data to map
      # mapfiltered <- map
      
      # user specified mapping variables
      mapvar_cluster <- rlang::sym(input$mapvarselected_cluster)
      hovervar_cluster <- rlang::sym(input$hoverselected_cluster)
      
      if(input$denomselected_cluster == "") {
        
        # create cluster map without denominator var
        p <- geode::geo_calculate(data = mapdata,
                             var = !!(mapvar_cluster),
                             hover_id = !!(hovervar_cluster),
                             statistic = input$clusteranalysis,
                             transparency = input$translevelcluster,
                             create_plot = TRUE,
                             interactive = TRUE)
        
      } else {
        
        denom_cluster <- rlang::sym(input$denomselected_cluster)
        
        # create cluster map with denominator var
        p <- geode::geo_calculate(data = mapdata,
                             var = !!(mapvar_cluster),
                             denom = !!(denom_cluster),
                             hover_id = !!(hovervar_cluster),
                             statistic = input$clusteranalysis,
                             transparency = input$translevelcluster,
                             create_plot = TRUE,
                             interactive = TRUE)
      }
      
      
    })
    
    output$clustermap <- tmap::renderTmap({
      clustermap()
    })
    
    

# Proximity analysis functions --------------------------------------------
    
    # select region variable to map from input dataset
    output$proximity_geovar_ui <- renderUI({
      selectInput("proximity_mapvar_selected", "Select region variable for mapping",
                  choices = c("", names(mapdata())),
                  selected = "")
    })
    
    
    # select point variable IDs input dataset
    output$proximity_idvar_ui <- renderUI({
      selectInput("proximity_idvar_selected", "Select ID varible for point locations",
                  choices = c("", names(attrdata())),
                  selected = "")
    })              
    
    # select point variable x coordinates to map from input dataset
    output$proximity_xvar_ui <- renderUI({
      selectInput("proximity_xvar_selected", "Select point location 'x' (longitude) values",
                  choices = c("", names(attrdata())),
                  selected = "")
    })
    
    # select point variable y coordinates to map from input dataset
    output$proximity_yvar_ui <- renderUI({
      selectInput("proximity_yvar_selected", "Select point locations 'y' (latitude) values",
                  choices = c("", names(attrdata())),
                  selected = "")
    })
    
    
    # create download button for outputting map
    output$download_proximitymap <- renderUI({
      req(input$proximity_idvar_selected, input$proximity_xvar_selected, input$proximity_yvar_selected)
      downloadButton('OutputFile_proximitymap', 'Download map')
    })
    
    # output function for saving map
    output$OutputFile_proximitymap <- downloadHandler(
      filename = function() { paste0("proximity_map", ".html") },
      content = function(file) {
        req(proximitymap())
        tmap::tmap_save(tm = proximitymap(), filename = file)
      })
    
    
    # show map
    proximitymap <- reactive({
      
      req(input$proximity_idvar_selected, input$proximity_xvar_selected, input$proximity_yvar_selected)
      
      mapdata <- mapdata()
      
      
      # Select data to map
      # mapfiltered <- map
      
      # user specified mapping variables
      proximity_mapvar <- rlang::sym(input$proximity_mapvar_selected)
      proximity_idvar <- rlang::sym(input$proximity_idvar_selected)
      proximity_yvar <- rlang::sym(input$proximity_yvar_selected)
      proximity_xvar <- rlang::sym(input$proximity_xvar_selected)
      
      # rename x and y variables for point locations
      attrdata <-  dplyr::rename(attrdata(), x = proximity_xvar, y = proximity_yvar)
      
      # create proximity analysis map
      p <- geode::geo_distance(data = mapdata,
                          geography_col = !!(proximity_mapvar),
                          location_data = attrdata,
                          points_col = !!(proximity_idvar),
                          n_nearest = input$proximity_n,
                          levels = input$proximity_chorolevels
      )
      
      
      
    })
    
    output$proximitymap <- tmap::renderTmap({
      proximitymap()
    })
    
    

# Cluster detection analysis functions ------------------------------------
    
    
    # select count variable from input dataset
    output$countsvar_detect_ui <- renderUI({
      selectInput("detectmap_count_selected", "Select count variable",
                  choices = c("", names(mapdata())),
                  selected = "")
    })
    
    # select population size variable from input dataset
    output$popvar_detect_ui <- renderUI({
      selectInput("detectmap_pop_selected", "Select population size variable",
                  choices = c("", names(mapdata())),
                  selected = "")
    })
    
    # select hover-over variable
    output$hovervar_detect_ui <- renderUI({
      selectInput("detectmap_hover_selected", "Select variable to show when hovering over a region",
                  choices= names(mapdata()))
    })
    
    # create download button for outputting map
    output$download_detectmap <- renderUI({
      req(input$detectmap_count_selected, input$detectmap_pop_selected)
      downloadButton('OutputFile_detectmap', 'Download map')
    })
    
    # output function for saving map
    output$OutputFile_detectmap <- downloadHandler(
      filename = function() { paste0("Cluster_detection_map", ".html") },
      content = function(file) {
        req(detectmap())
        tmap::tmap_save(tm = detectmap(), filename = file)
      })
    
    
    # Create cluster detection map
    detectmap <- reactive({
      
      req(input$detectmap_count_selected, input$detectmap_pop_selected)
      
      mapdata <- mapdata()
      
      # Select data to map
      # mapfiltered <- mapdata
      
      # user specified mapping variables
      detect_hovervar <- rlang::sym(input$detectmap_hover_selected)
      detect_countvar <- rlang::sym(input$detectmap_count_selected)
      detect_popvar <- rlang::sym(input$detectmap_pop_selected)
      
      
      # show pointmap
      p <- geode::geo_detect(data = mapdata,
                        counts = !!(detect_countvar),
                        pop = !!(detect_popvar),
                        method = input$detectanalysis,
                        transparency = input$translevel_detect,
                        hover_id = !!(detect_hovervar)
      )
      
    })
    
    
    output$detectmap <- tmap::renderTmap({
      detectmap()
    })
    
    
  }
  
  
  
  shinyApp(ui, server, ...)
}

ShinyGeode()