library(shiny)
library(magrittr)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(Hmisc)
library(extrafont)
library(viridis)

#extrafont::font_import()
# library(plotly)

source("support_func.R")
source("DSN_theme.R")

# input <- list( gene.network = "TSC2", minAbsCor = c(0,0), maxLinks = 2, innerNodeColor = "TCLIM_Annotation_A_GENESYMBOLS", outerNodeColor = "TCLIM_Annotation_A_MEDIAN.RSA", linkDistance = 500, linkWidth = 15)

shinyServer(function(input, output, session) {
  
  # Information Modal display
  
  observeEvent( input$GeneInfoButton, {
    showModal( modalDialog(
      title = "Info",
      "Type here the locus ID of your gene of interest, it will auto-populate as the gene name is being typed in. The plot will show in the y-axis the correlation coefficient (rs) between the natural variation in the expression levels of the gene of interest and each one of the 400+ environmental variables shown in the x-axis. You can interactively select a range of these environmental variables in the plot and autopopulate the table underneath with the details of the T x E for that gene. ",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  #  observeEvent( input$MetricInfoButton, {
  #     showModal( modalDialog(
  #      title = "Info",
  #      "To differentiate between inert and essential genes use the RSA gene level metric (see the ABOUT tab for more details)",
  #      easyClose = TRUE,
  #     footer = NULL
  #    ))
  #   })
  
  # Waterfall build and display
  
  TCLIM.matrix <- reactive({
    if( input$GeneLevelMetric == 1 ){
      out <- TCLIM.mat
    } else if (input$GeneLevelMetric == 2){
      out <- TCLIM.rsa.mat
    }
    return(out)
  })
  
  
  profile_build <- reactive({
    
    showModal(modalDialog(
      title = "Loading...",
      footer = NULL,
      size = "s"))
    
    geneX.list <- input$gene.profile
    
    out.profile <- data.frame( Environmental_variable = colnames(TCLIM.matrix()), Correlation_coefficient = TCLIM.matrix()[geneX.list,] %>% t %>% as.vector, stringsAsFactors = FALSE) %>% 
      dplyr::filter(!is.na(Correlation_coefficient)) %>%
      left_join(TCLIM.cl.annot, by = c("Environmental_variable")) %>%
    arrange(-Correlation_coefficient)
    
    # %>%
   # arrange(-Correlation_coefficient) %>% mutate(Source = factor(Source, levels = Source))
    
    if( input$GeneLevelMetric == 1 ){
      sensitivity.threshold <- -1
      gene.level.metric <- "ATARiS"
    } else if (input$GeneLevelMetric == 2){
      sensitivity.threshold <- -3
      gene.level.metric <- "RSA"
    }
    
    
   # out.profile[order(-out.profile$Correlation_coefficient),]  
    out.profile.plot <- ggplot(out.profile, aes( x = Environmental_variable, y = Correlation_coefficient, fill = Correlation_coefficient)) + 
      scale_fill_viridis(option="magma",direction = -1)+
      geom_bar(stat = "identity", width = 1) +
      theme(axis.line = element_line(size=1, colour = "black"),
            panel.grid.major = element_line(size=0.05,colour = "#d3d3d3"), panel.border = element_blank(), panel.background = element_blank(), axis.ticks.length=unit(0.25,"cm")) +
      theme(axis.text.x = element_text(family = "Arial", color="black", face="bold", size=5)) +
      theme(axis.text.y = element_text(family = "Arial", color="black", face="bold", size=10)) +
      theme(axis.title.x = element_text(family = "Arial", color="black", face="bold", size=20)) +
      theme(axis.title.y = element_text(family = "Arial", color="black", face="bold", size=20)) +
      theme(plot.title = element_text(hjust = 0.5, vjust =  3, family = "Arial", color="#43464B", face="bold", size=10)) +
      geom_hline(yintercept = 0.3, color = "black", linetype = "longdash")+
      geom_hline(yintercept = 0, color = "black", linetype = "solid")+
      
      geom_hline(yintercept = -0.3, color = "black", linetype = "longdash")+
      theme(plot.margin=unit(c(1,1,1,1),"cm"))+
      theme(plot.title = element_text(family = "Arial", color="#696969",face="bold",  size=35)) +  
      theme(legend.position="none") + 
      ylab(expression(bold(paste("Correlation coefficient ", (r[s]))))) +
      xlab(expression(bold("Environmental variable")))+
      theme(legend.position="none") +
      guides(fill=guide_legend(ncol=3))
    
    removeModal()
    
    
    return(list( profile.plot = out.profile.plot, profile.data = out.profile, profile.gene = geneX.list, gene.level.metric = gene.level.metric ))
  })
  
  output$ProfilePlot <- renderPlot({
    profile_build()$profile.plot
  })
  
  brushed_data <- reactive({
    brushedPoints(profile_build()$profile.data, input$plot_brush)
  })
  
  output$cellline_info <- renderDataTable({
    brushed_data()
  })
  
  output$downloadData_profile <- downloadHandler(
    filename = function(){ paste0("GeneSensitivity_",paste(profile_build()$profile.gene,collapse="_"), "_", profile_build()$gene.level.metric, ".csv") },
    content = function(file){
      write.csv(profile_build()$profile.data, file, row.names = FALSE)
    })
  
  # Network build and display
  
  observe({
    if(is.null(input$gene.network) || input$gene.network == ""){
      shinyjs::disable("UpdateButton.network")
    } else {
      shinyjs::enable("UpdateButton.network")
    }
  })
  
  
  network_build <- eventReactive( input$UpdateButton.network, {
    
    showModal(modalDialog(
      title = "Loading...",
      footer = NULL,
      size = "s"))
    
    geneX.list <- input$gene.network
        
    e.cor.genes <- TCLIM.mat[geneX.list,] %>% as.matrix %>% t %>%
      cor( y= TCLIM.mat %>% as.matrix %>% t, use = "pairwise.complete.obs") %>% t
    
    e.ranked.gene.names <- e.cor.genes %>% apply(2,function(x){rank(-abs(x))}) %>%
      apply(1,min) %>% .[.<=max.edges.per.node] %>% names
    
    e.cor <- TCLIM.mat %>% .[e.ranked.gene.names,] %>% as.matrix %>% t %>%
      cor( y= TCLIM.mat %>% as.matrix %>% t, use = "pairwise.complete.obs") #%>% t
    
    network <- network.from.adj.mat(e.cor) %>% dplyr::filter(rank <= max.edges.per.node)
    
    removeModal()
    
    return(list(network = network, geneX.list = geneX.list))
  })
  
  output$NetworkPlot <- renderPrint({
    
    geneX.list <- isolate(input$gene.network)
    
    network.tmp <- trim.Correlation.and.maxEdges(network_build()$network, minAbsCor=isolate(input$minAbsCor), maxLinks=isolate(input$maxLinks))
    
    tmp <- geneX.adj.mat.and.annot.redux(network.tmp, annot, geneX=geneX.list)
    
    # are no edges left after reduction?
    if(dim(tmp$network)[1]<=1){
      stop("Correlation thresholds are too stringent")
    }
    
    network.tmp <- tmp[["network"]]
    annot.redux <- tmp[["annot"]]
    
    tmp <- nodes.and.links.df(network.tmp, annot.redux)      
    nodes <- tmp[["nodes"]]
    links <- tmp[["links"]]
    
    
    d3ForceNetwork(Links = links, Nodes = nodes,
                   Source = "source", Target = "target",
                   Value = "value", Name = "name", 
                   nodeInnerColour = isolate(input$innerNodeColor), nodeOuterColour = isolate(input$outerNodeColor), height = 600, width = 900,
                   linkDistance = isolate(input$linkDistance), linkWidth = isolate(input$linkWidth), 
                   charge = -120, nodeSize = 8,
                   parentElement = "#NetworkPlot",
                   d3Script = "http://d3js.org/d3.v3.min.js")
    
  })
  ###
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("T-CLIM_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(readfile, file)
    })
  
  # Network download functionalities
  CorrTable_build <- reactive({
    geneX.list <- network_build()$geneX.list
    network.cast <- network_build()$network %>% dplyr::filter( source %in% geneX.list ) %>% as.data.frame %>% transmute(source,GENESYMBOLS=target,value) %>% spread(source,value)
    return(list(table=network.cast, genes=geneX.list))
  })
  
  
  output$downloadData_correlation <- downloadHandler(
    filename = function(){ paste0("CorrelationTable_",paste(CorrTable_build()$genes,collapse="_"),".csv") },
    content = function(file){
      write.csv(CorrTable_build()$table, file, row.names = FALSE)
    })
  ###
  
   
})

