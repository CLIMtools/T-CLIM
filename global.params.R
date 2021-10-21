library(magrittr)
library(dplyr)
library(readr)

# load data
TCLIM.mat <- read.csv("TCLIM_Arabidopsis2.csv", header=TRUE, sep=",", row.names=1)
annot <- read.csv("annot_Arabidopsis.csv", header=TRUE, sep=",", row.names=NULL)
TCLIM.cl.annot <- read.csv("TCLIM.cl.annot_Arabidopsis.csv", header=TRUE, sep=",", row.names=NULL) %>% dplyr::filter(Environmental_variable %in% colnames(TCLIM.mat))
head(TCLIM.cl.annot)
TCLIM.rsa.mat <- read.csv("TCLIM_Arabidopsis.csv", header=TRUE, sep=",", row.names=1)
readfile <- read.csv("CLIMATE TWAS.csv",row.names = NULL)


# sidebar panel initial values
step.cor <- 0.01
initial.cor.values <- c(-0.3,0.3)
max.edges.per.node <- 15

initial.link.distance <- 500
initial.link.width <- 15

# sidebar panel scroll down for the node annotation generated from network file
genes.in.menu <- TCLIM.mat %>% rownames %>% unique %>% sort
genes.in.menu.rsa <- TCLIM.rsa.mat %>% rownames %>% unique %>% sort

# Identify in the annot file which columns to use for annotation
# the structure is as follows:
# X_sep_Y, whereby X is the annotation class, sep is the seperator and Y is the annotation name
# the distinction in classes and annotation name is only for aestethics purposes in the final shiny app
colnames(annot) <- colnames(annot) %>% paste0( "TCLIM_Annotation_A_", .)
sep <- "_A_"
annot.colnames <- annot %>% .[,grepl(sep,colnames(.))] %>% colnames
list.names <- annot.colnames %>% gsub(paste0(sep,".*$"),"",.) %>% unique
scroll.down.annotation.list <- lapply(list.names,function(name){
  annot.colnames %>% .[grepl(name,.)] %>% set_names(gsub(paste0(name,sep),"",.)) %>% return
}) %>% set_names(list.names)


# sidebar panel initial values
step.cor <- 0.01
initial.cor.values <- c(-0.3,0.3)
max.edges.per.node <- 15

initial.link.distance <- 500
initial.link.width <- 15

# sidebar panel scroll down for the node annotation generated from network file
genes.in.menu <- TCLIM.mat %>% rownames %>% unique %>% sort
genes.in.menu.rsa <- TCLIM.rsa.mat %>% rownames %>% unique %>% sort

# Identify in the annot file which columns to use for annotation
# the structure is as follows:
# X_sep_Y, whereby X is the annotation class, sep is the seperator and Y is the annotation name
# the distinction in classes and annotation name is only for aestethics purposes in the final shiny app
colnames(annot) <- colnames(annot) %>% paste0( "TCLIM_Annotation_A_", .)
sep <- "_A_"
annot.colnames <- annot %>% .[,grepl(sep,colnames(.))] %>% colnames
list.names <- annot.colnames %>% gsub(paste0(sep,".*$"),"",.) %>% unique
scroll.down.annotation.list <- lapply(list.names,function(name){
  annot.colnames %>% .[grepl(name,.)] %>% set_names(gsub(paste0(name,sep),"",.)) %>% return
}) %>% set_names(list.names)










