
require(whisker)


### FUNCTIONS ###

cor.coef.adj <- function(r1, n1, n2){
  out <- r1 * sqrt(n1 - 2) / sqrt(n2 -2 -r1^2 * (n2 -n1))
  return(out)
}


trim.Correlation.and.maxEdges <- function(network, minAbsCor=c(-0.1,0.1), maxLinks=5){
  network.tmp <- network %>% dplyr::filter(rank<=maxLinks) %>% dplyr::filter(value<minAbsCor[1] | value>minAbsCor[2])
  return(network.tmp)
}


geneX.adj.mat.and.annot.redux <- function(network.tmp, annot, geneX.list=genes.in.menu[1]){
  # set gene(s) of interest
  #   geneX.list <- toupper(unlist(strsplit(geneX,";| ")))
  
  symbol.geneX <- network.tmp %>%
    dplyr::filter(source%in%geneX.list) %>%
    .$target %>% as.character %>%
    c(geneX.list) %>% unique
  
  network.tmp2 <- network.tmp %>%
    dplyr::filter(source%in%symbol.geneX) %>%
    dplyr::filter(!duplicated(genepair)) %>%
    dplyr::filter(target%in%symbol.geneX)
  
  position.map <- 
    
    symbol.geneX <- unique( c( as.character(network.tmp2$source),as.character(network.tmp2$target) ) )
  annot.redux <- annot[symbol.geneX,]
  return(list(network=network.tmp2, annot = annot.redux))
}


nodes.and.links.df <- function(network.tmp, annot.redux, innerNode = unlist(scroll.down.annotation.list)[1], outerNode = unlist(scroll.down.annotation.list)[2]){
  
  links <- data.frame(network.tmp)[,c("source","target","value")]
  links$source <- as.character(links$source)
  links$target <- as.character(links$target)
  nodes <- data.frame(name=rownames(annot.redux),id=rownames(annot.redux), position=0:(dim(annot.redux)[1]-1))
  nodes <- cbind(nodes,annot.redux[,intersect(colnames(annot.redux),unlist(scroll.down.annotation.list))])
  nodes$colour <- nodes[,innerNode]
  nodes$stroke <- nodes[,outerNode]
  
  links$source <- nodes[links$source,"position"]
  links$target <- nodes[links$target,"position"]
  
  return(list(nodes=nodes,links=links))
}

network.from.adj.mat <- function(adj.mat){
  
  values.to.be.removed <- -100
  
  if( dim(adj.mat)[2] > dim(adj.mat)[1]){
    adj.mat <- t(adj.mat)
  }
  
  if( dim(adj.mat)[1] != dim(adj.mat)[2]){
    adj.mat.rank <- adj.mat %>% apply(2, function(x){
      out <- rank( - abs(x))
      return(out)
    })
  }
  
  dim.names <- intersect(rownames(adj.mat), colnames(adj.mat))
  mat <- adj.mat[dim.names,dim.names]
  mat.rank.ts <- adj.mat.rank[dim.names,dim.names] %>% as.data.frame %>%
    mutate(target = rownames(.)) %>% gather(source, rank, -target) %>%
    mutate(rank = rank -1 ) %>% dplyr::filter(rank != 0)
  
  mat[ upper.tri(mat, diag = T)] <- values.to.be.removed
  mat.ts <- mat %>% as.data.frame %>%
    mutate(target = rownames(.)) %>% gather(source, value, -target) %>%
    dplyr::filter(value != values.to.be.removed)
    
  e.cor.ts <- mat.ts %>% 
    mutate( genepair = paste0( source, "_", target))
  e.cor.ts2 <- e.cor.ts %>% mutate( direction = 1 ) %>% rbind( e.cor.ts %>%  rename( source = target, target = source) %>%  mutate(direction = 2))
  e.cor.ts3 <- e.cor.ts2 %>% left_join(mat.rank.ts, by = c("source","target"))
  
  return(e.cor.ts3)
}

# D3 FUNCTIONS

toJSONarray <- function (dtf) 
{
  clnms <- colnames(dtf)
  name.value <- function(i) {
    quote <- ""
    if (class(dtf[, i]) != "numeric" && class(dtf[, i]) != 
          "integer") {
      quote <- "\""
    }
    paste("\"", i, "\" : ", quote, dtf[, i], quote, sep = "")
  }
  objs <- apply(sapply(clnms, name.value), 1, function(x) {
    paste(x, collapse = ", ")
  })
  objs <- paste("{", objs, "}")
  res <- paste("[", paste(objs, collapse = ", "), "]")
  return(res)
}

BasicHead <- function () 
{
  "<!DOCTYPE html>\n<meta charset=\"utf-8\">\n<body> \n"
}

ForceMainStyleSheet <- function() 
{
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"network.css\"> \n
   <script src=\"d3.min.js\"></script> \n
   <script src=\"network.js\"></script> \n
  <script> \n"
}



MainForceJS <- function() 
{
  "network(nodes,links,\"{{Name}}\",\"{{nodeInnerColour}}\",\"{{nodeOuterColour}}\",{{nodeSize}},\"{{Value}}\",\"{{Value}}\",\"{{linkWidth}}\",\"{{linkDistance}}\",\"\",\" {{parentElement}} \" ) \n
  //network(nodes, links, nodeName, nodeInnerColour, nodeOuterColour, nodeSize, linkValue, linkColour, linkWidth, linkDistance, charge, parentElement) \n
  </script>"
}


d3ForceNetwork <- function (Links, Nodes, Source, Target, Value = "", Name = "", 
                            nodeInnerColour = "", nodeOuterColour = "", height = 600, width = 900,
                            linkDistance = 5, linkWidth = 20, 
                            charge = -120, nodeSize = 8,
                            parentElement = "body",
                            d3Script = "http://d3js.org/d3.v3.min.js") 
{
  if (is.null(Value)) {
    LinksDF <- data.frame(Links[, Source], Links[, Target])
    names(LinksDF) <- c(Source, Target)
  } else if (!is.null(Value)) {
    LinksDF <- data.frame(Links[, Source], Links[, Target], 
                          Links[, Value])
    names(LinksDF) <- c(Source, Target, Value)
  }
  NodesDF <- data.frame(Nodes)
  node.colnames <- intersect(colnames(NodesDF),c(Name, nodeInnerColour, nodeOuterColour))
  NodesDF <- NodesDF[,node.colnames]
  if( nodeInnerColour == "empty") nodeInnerColour <- ""
  if( nodeOuterColour == "empty") nodeOuterColour <- ""
  LinkData <- toJSONarray(LinksDF)
  LinkData <- paste("var links =", LinkData, "; \n")
  NodesData <- toJSONarray(NodesDF)
  NodesData <- paste("var nodes =", NodesData, "; \n")
  PageHead <- BasicHead()
  NetworkCSS <- ForceMainStyleSheet()
  

    #     MainScript <- whisker:::whisker.render(d3Network:::MainForceJS())
    MainScript <- whisker:::whisker.render(MainForceJS())

  cat(NetworkCSS, LinkData, NodesData, MainScript)
  
}

# tsne_function <- function(groupX.list, cl.annot, perplexity, theta)
# {
# #   cl.subset <- cl.annot %>% filter( GROUP %in% groupX.list) %>% .$CLEANNAME_PRIMARYSITE %>% unique
#   cl.subset <- cl.annot %>% filter( GROUP %in% groupX.list) %>% .$CLEANNAME_PRIMARYSITE %>% unique # %>% .[1:10]
#   if(length(cl.subset) == 0){
#     cl.subset <- colnames(epic.data)
#     groupX.list = "ALL"
#   }
#   
#   shinyjs::logjs(cl.subset %>% length)
#   cat("1")
# #   updateProgress()  
#   # e.cor <- epic.data[, cl.subset] %>% as.matrix %>% t %>% rcorr
#   e.cor <- epic.data[1:1000, cl.subset] %>% as.matrix %>% t %>% rcorr
# 
# shinyjs::logjs(e.cor %>% dim)
# cat("2")
# #   updateProgress()
#   e.cor.dist <- (1-e.cor$r)^2 %>% as.dist
# 
# 
# shinyjs::logjs(e.cor %>% length)
# cat("3")
# #   updateProgress()
#   set.seed(123456)
#   tsne <- Rtsne( e.cor.dist , is_dist = T, perplexity = perplexity, theta = theta) # , initial_dims = isolate(input$initial_dims)
# 
# cat("4")
# #   updateProgress()
#   tsne.df <- tsne$Y %>% as.data.frame %>% set_colnames(c("tSNE1","tSNE2")) %>%
#     cbind(annot[rownames(e.cor$r),]) %>% set_colnames( gsub( paste0("^.*",sep), "", colnames(.)) ) %>%
#     mutate(groupSpecific_MedianScore = apply(epic.data[rownames(e.cor$r), cl.subset], 1, function(x)median(x,na.rm = T)) )
#   
#   
#   return(list(tsne = tsne.df, corMat = e.cor, group = ifelse("ALL" %in% groupX.list, "Complete", paste0(groupX.list, collapse = "_")) ))
# }