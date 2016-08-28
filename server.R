########################################################################
# networkd3 for D3 by Ryan Collingwood
#
# server.R - Where the magic happens
#
# Shiny visualisation built in R using networkd3 
# (of the D3 visualisation library) to explore a dataset 
# relating to Diablo III (otherwise also known as D3). 
# Other notable packages used: plotly, shinyjs.
#
# @ryancollingwood - twitter
# https://github.com/ryancollingwood - github
# www.linkedin.com/in/ryancollingwood - LinkedIn
# 
# Code is provided as is, under the MIT License
#
# Figured I would share this as finding functional and interesting examples
# of networkd3 visualisation in R
#
# Diablo® III
# Diablo is a trademark or registered trademark of Blizzard Entertainment, Inc., 
# in the U.S. and/or other countries.
#
########################################################################


####################################
#server code that is run on startup
####################################
library(ggplot2)
library(scales)
library(RColorBrewer)
library(networkD3)
library(igraph)
library(reshape2)
library(plyr)
library(dplyr)
library(shiny)
library(shinyjs)
library(plotly)

#original data
data_original = read.csv("data.csv")
#column names for our data
colnames =  colnames(data_original)

#returns the column names in a nicer format
colnameLabel <- function(colName) {    
  return(gsub("_", " ", colName))
}

#returns the name with spaces replaced
linknameLabel <- function(colName) {    
  temp <- gsub(" ", "-", colName)
  temp <- gsub("'", "", temp)
  temp <- gsub(",", "", temp)
  return(tolower(temp))
}

#replacement max function
max2 <- function ( x ) { 
  y <- suppressWarnings(max(x)) 
  
  if ( y==-Inf ) {
    y <- 0 
  }
  
  return (y) 
}

#themes for ggplots
theme <- list()

theme.diablo_chart <- 
  theme(legend.position = "right") +
  theme(plot.title = element_text(size=16, family="Trebuchet MS", face="bold", hjust=0, color="#666666")) +
  theme(axis.title = element_text(size=12, family="Trebuchet MS", face="bold", color="#666666")) +
  theme(axis.title.y = element_text(angle=90, size=12, family="Trebuchet MS", face="bold", color="#666666")) 

# SCATTERPLOT THEME
theme.diablo_chart_SCATTER <- theme.diablo_chart +
  theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# HISTOGRAM THEME
theme.diablo_chart_HIST <- theme.diablo_chart +
  theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# SMALL MULTIPLE THEME
theme.diablo_chart_SMALLM <- theme.diablo_chart +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size=14, family="Trebuchet MS", face="bold", color="#666666"))


getFilteredData <- function(x = NA, xRange = NA, y = NA, yRange = NA, filter = NA) {
  filterdata = data_original
  
  if (! is.na(filter)) {
    for (i in 1:length(filter)) {
      key = names(filter)[i]
      value = filter[[key]]
      
      filterdata = filterdata[
        filterdata[,paste(key)] %in% value,
        colnames]
    }
  }
  
  ranges <- list()
  
  if  ( (!is.na(x)) ) {
    ranges$xValuePercent <- ( max2(filterdata[,paste(x)]) / 100)
    
    filterdata <- subset(filterdata, eval(parse(text = x)) >= (xRange[1] * ranges$xValuePercent))
    filterdata <- subset(filterdata, eval(parse(text = x)) <= (xRange[2] * ranges$xValuePercent))
  }
  
  if  ( (!is.na(y)) ) {
    ranges$yValuePercent <- ( max2(filterdata[,paste(x)]) / 100)
  
    filterdata <- subset(filterdata, eval(parse(text = y)) >= (yRange[1] * ranges$yValuePercent))
    filterdata <- subset(filterdata, eval(parse(text = y)) <= (yRange[2] * ranges$yValuePercent))
  }
  
  return(filterdata)
}


##################################################################################################################
# Create a  scatter plot
##################################################################################################################
scatterPlot <- function(xValue, xValueRange, yValue, yValueRange, filterBy, groupBy, colorBy = NA, sizeBy = NA, groupShapes = F) {
  filterdata <- getFilteredData(xValue, xValueRange, yValue, yValueRange, filterBy)

  #check input parameters, shiney seems to pass NA as a string literal
  if (! is.na(colorBy)) {
    if (colorBy == "NA") {
      colorBy = NA
    }
  }
  
  if (! is.na(sizeBy)) {
    if (sizeBy == "NA") {
      sizeBy = NA
    }
  }
  
  plot1  <- ggplot(
    data_original, 
    aes(x = eval(parse(text = xValue)), 
        y = eval(parse(text = yValue)), 
        color = eval(parse(text = colorBy)), 
        #group = eval(parse(text = groupBy)) )
        group = eval(groupBy) )
    ) +
    theme.diablo_chart +
    scale_x_continuous(colnameLabel(eval(xValue)), labels = comma) +
    scale_y_continuous(colnameLabel(eval(yValue)), labels = comma)
  
  values <- list()  
  print(paste("grouping by: ", groupBy))
  values$groupBy = as.vector(unique(filterdata[,eval(groupBy)]))  
  
  if (! is.na(colorBy)) {
    values$colorBy = as.vector(unique(filterdata[,paste(colorBy)]))
  } else {
    values$colorBy = as.vector(unique(filterdata[,paste(groupBy)]))
  }
  
  values$colors = brewer.pal(length(values$colorBy), "Set3")  
  values$palette = colorRampPalette(values$colors)    
  

  for (i in 1:length(values$groupBy)) {  
    key <- values$groupBy[i]
    
    #get the columns needed
    data_segment_columns = c(eval(xValue),
                             eval(yValue),
                             eval(groupBy))
    
    segment_colorBy = groupBy
    if (! is.na(colorBy)) {
      data_segment_columns = append(data_segment_columns, eval(colorBy))
      segment_colorBy = colorBy
    }
    
    if (! is.na(sizeBy)) {
      data_segment_columns = append(data_segment_columns, eval(sizeBy))
    }
    
    
    #segment the data
     data_segment = filterdata[
       filterdata[,eval(groupBy)]==key, 
       data_segment_columns
       ]

    
    #create our aes 
    aes_segment = NA
    
    if (groupShapes) {
      if (! is.na(sizeBy)) {
        aes_segment = aes(
          x = eval(parse(text = xValue)),
          y = eval(parse(text = yValue)),
          color = eval(parse(text = segment_colorBy)),
          shape = eval(parse(text = groupBy)),
          size = eval(parse(text = sizeBy)),
          text = paste(
            paste(colnameLabel(segment_colorBy), colnameLabel(eval(parse(text = segment_colorBy))), sep = ": "),
            paste(colnameLabel(xValue), eval(parse(text = xValue)), sep = ": "),
            paste(colnameLabel(yValue), eval(parse(text = yValue)), sep = ": "),
            paste(colnameLabel(groupBy), eval(parse(text = groupBy)), sep = ": "),
            paste(colnameLabel(sizeBy), eval(parse(text = sizeBy)), sep = ": "),
            sep = "<br>"
            )
        )
      } else {
        aes_segment = aes(
          x = eval(parse(text = xValue)),
          y = eval(parse(text = yValue)),
          color = eval(parse(text = segment_colorBy)),
          shape = eval(parse(text = groupBy)),
          text = paste(
            paste(colnameLabel(segment_colorBy), colnameLabel(eval(parse(text = segment_colorBy))), sep = ": "),
            paste(colnameLabel(xValue), eval(parse(text = xValue)), sep = ": "),
            paste(colnameLabel(yValue), eval(parse(text = yValue)), sep = ": "),
            paste(colnameLabel(groupBy), eval(parse(text = groupBy)), sep = ": "),
            sep = "<br>"
            )
          )
      }
    } else {
      if (! is.na(sizeBy)) {
        aes_segment = aes(
          x = eval(parse(text = xValue)),
          y = eval(parse(text = yValue)),
          color = eval(parse(text = segment_colorBy)),
          size = eval(parse(text = sizeBy)),
          text = paste(
            paste(colnameLabel(segment_colorBy), colnameLabel(eval(parse(text = segment_colorBy))), sep = ": "),
            paste(colnameLabel(xValue), eval(parse(text = xValue)), sep = ": "),
            paste(colnameLabel(yValue), eval(parse(text = yValue)), sep = ": "),
            paste(colnameLabel(sizeBy), eval(parse(text = sizeBy)), sep = ": "),
            sep = "<br>"
            )
          )
      } else {
        aes_segment = aes(
          x = eval(parse(text = xValue)),
          y = eval(parse(text = yValue)),
          color = eval(parse(text = segment_colorBy)),
          text = paste(
            paste(colnameLabel(segment_colorBy), colnameLabel(eval(parse(text = segment_colorBy))), sep = ": "),
            paste(colnameLabel(xValue), eval(parse(text = xValue)), sep = ": "),
            paste(colnameLabel(yValue), eval(parse(text = yValue)), sep = ": "),
            sep = "<br>"
            )
          )
      }
    }
    
    #add it to the plot
    plot1 <- plot1 + 
      geom_point(
        data = data_segment, 
        aes_segment
      )
    
  }
  

  #shape legend
  plot1 <- plot1 + scale_shape(name = colnameLabel(eval(groupBy)))
  
  #size legend
  if (! is.na(sizeBy)) {
    plot1 <- plot1 + scale_size(name = colnameLabel(eval(sizeBy)))
  }
  
  #color legend
  if (! is.na(colorBy)) {
    plot1 <- plot1 + scale_color_brewer(name = colnameLabel(eval(colorBy)), palette = "Set2")
  } else {
    plot1 <- plot1 + scale_color_brewer(name = colnameLabel(eval(groupBy)), palette = "Set2")
  }
  
  #return(plot1)
  plot2 <- ggplotly(plot1 , tooltip=c("text"))
  return(plot2)
}


##################################################################################################################
# Create a histogram
##################################################################################################################
histoPlot <- function(xValue, xValueRange, yValue, yValueRange, filterBy, groupBy, colorBy = NA, sizeBy = NA, groupShapes = F, binSize = 25) {
  
  filterdata <- getFilteredData(xValue, xValueRange, yValue, yValueRange, filterBy)
  
  data_slice <- list()
  data_slice.xValue <- filterdata[,eval(xValue)]

  facet_formula = as.formula(paste("~", eval(groupBy)))
  
  plot1 <- ggplot(data=filterdata, aes(x=data_slice.xValue)) +
    geom_histogram(fill="#880011", bins = binSize) +
    ggtitle(paste(colnameLabel(xValue),"grouped by",colnameLabel(groupBy) )) +
    labs(x=colnameLabel(xValue), y="Count\nof Records") +
    facet_wrap(facet_formula, nrow = 3) +
    theme.diablo_chart_SMALLM
  
  plot2 <- ggplotly(plot1, tooltip="all")
}

bin2dPlot <- function(xValue, xValueRange, yValue, yValueRange, filterBy, groupBy, colorBy = NA, sizeBy = NA, groupShapes = F, binSize = 25) {
  filterdata <- getFilteredData(xValue, xValueRange, yValue, yValueRange, filterBy)
  
  plot1 <- ggplot(data = filterdata) +
    geom_hex(
      mapping = aes(
        x = eval(parse(text = xValue)), 
        y = eval(parse(text = yValue))
        ), 
      bins = c(binSize, binSize)
      ) +
      ggtitle(paste("Concentration of Observations:\n",colnameLabel(xValue)," by ",colnameLabel(groupBy), sep = "" )) +
      labs(x=colnameLabel(xValue), y=colnameLabel(yValue)) +
      theme.diablo_chart
  
  plot2 <- ggplotly(plot1 , tooltip="value")
  return(plot2)
}

##################################################################################################################
# Network Related Visualisations
##################################################################################################################
getPivotedList <- function(d, cols) {
  #melt down to list with the item names by values
  pivoted_melted <- list()
  
  for(i in 1:length(cols)) {
    pivoted_melted[[cols[i]]] <- melt(d, id.vars=(eval(cols[i]))) 
  }
  
  return(pivoted_melted)
}

getLinks <- function(d, cols) {
  pivoted_melted <- getPivotedList(d, cols)
  
  #now take the list of df and make it a single df
  network_links <-  rbind.fill(pivoted_melted)
  
  #create "id" for melting purposes so that we retain each row
  network_links$ID<-seq.int(nrow(network_links))
  
  #collapse (melt) the pivoted cols into a value column
  network_links <- melt(network_links, id.vars=c("ID","value"), measure.vars = cols, na.rm = TRUE)
  
  #rename
  names(network_links)[4] <- "end"
  names(network_links)[3] <- "value"
  names(network_links)[2] <- "start"
  
  #get rid of any empties
  network_links<-network_links[!(is.na(network_links$start)),]
  network_links<-network_links[!(is.na(network_links$end)),]
  
  #generate a key that is the start and end node sorted alphabetically, as the
  #network doesn't have directional links
  network_links <- network_links %>% rowwise() %>% mutate(start_new = sort(c(start,end))[1],
                                                          end_new = sort(c(start,end))[2])
  
  #this gets rid of duplicates and rolls up into a weight col
  network_links <- ddply(network_links,~start_new+end_new,summarise,weight=length(value))
  
  #give meaningful names again
  names(network_links)[2] <- "end"
  names(network_links)[1] <- "start"
  
  return(network_links)
}

getLinksCountTotal <- function(d, itemName = NA) {
  #get the count of start nodes - conditionally if looking for a specific item
  if (is.na(itemName)) {
    d_count_start <- ddply(d,~start,summarise,count=sum(weight))
  } else {
    d_count_start <- ddply(filter_(d, ~ end == itemName | start == itemName ),~start,summarise,count=sum(weight))
  }
  
  #get the count of end nodes - conditionally if looking for a specific item
  if (is.na(itemName)) {
    d_count_end <- ddply(d,~end,summarise,count=sum(weight))
  } else {
    d_count_end <- ddply(filter_(d, ~ start == itemName | end == itemName),~end,summarise,count=sum(weight))
  }
  
  #give columns same names
  names(d_count_start)[1] <- "name"
  names(d_count_end)[1] <- "name"
  
  #create a union of the starts and ends
  d_count_totals <- union(d_count_start, d_count_end)
  
  #remove uneeded dfs 
  d_count_end <- NA
  d_count_start <- NA
  
  #in the words of the violent femmes - add it up
  d_count_totals <- ddply(d_count_totals,~name,summarise,size=sum(count))
  
  return(d_count_totals)
}

getNodesFromLinks <- function(d, lstNodeAttributes) {
  unique_start <- unique(d[,1])
  unique_end <-  unique(d[,2])
  
  nodes <- data.frame("name" = union(unique_start, unique_end), stringsAsFactors = FALSE)
  
  #do a join ala SQL to get out sizes, thank you plyr my fast becomming favourite R package
  for (i in 1:length(lstNodeAttributes)) {
    key = names(lstNodeAttributes)[i]
    value = lstNodeAttributes[[key]]    
    
    nodes <- as.data.frame(join(nodes, value, by = "name", type = "inner", match = "first"))
  }
  
  nodes <- nodes[order(nodes$name),]
  nodes$ID<-seq.int(nrow(nodes))
  nodes <- nodes[order(nodes$ID),] 
  
  #make 0 based indexes for Javascript
  #nodes$ID<-nodes$ID-1
  
  #remove row names
  row.names(nodes) <- NULL
  
  return(nodes)
}

#With significant aid from the resources below
#http://rpackages.ianhowson.com/cran/networkD3/man/forceNetwork.html
#https://disqus.com/home/discussion/d3network/d3network/

getGraph <- function(columns, filter = NA, itemName = NA, cutoff = NA, size_cutoff = NA, drawEdges = TRUE, networkEdgesQuarters = TRUE) {
  
  data_temp <- filterdata <- getFilteredData(filter =  filter)
  #apply any filters
  if (!is.na(filter)) {
    for (i in 1:length(filter)) {
      key = names(filter)[i]
      value = filter[[key]]
      
      data_temp = data_temp[
        data_temp[,paste(key)] %in% value,]
    }    
  }
  
  data_temp <- data_temp[,columns]
  #make "" into NA
  data_temp[data_temp==""] <- NA
  
  pivoted_columns <- names(data_temp)
  
  #now take the list of df and make it a single df
  network_links <-  getLinks(data_temp, pivoted_columns)
  
  network_links_count_totals <- getLinksCountTotal(network_links, itemName)
  
  #we can use network_links_count_totals to cull based on number of connections
  size_sd = sd(network_links_count_totals$size)
  size_mean = mean(network_links_count_totals$size)
  
  #if not looking for an item then set our size cutoff for +1 standard deviation from the mean
  if (is.na(size_cutoff)) {
    if (is.na(itemName)) {
      size_cutoff = size_sd + size_mean
    } else {
      #if looking for an item lets return the uppmost quantile
      size_cutoff = quantile(network_links_count_totals$size)[[4]]
    }
  } else {
    if (is.na(itemName)) {
      size_cutoff = (max2(network_links_count_totals$size) / 100) * size_cutoff
    } else {
      #as we might cutt off the item we are focusing on
      size_cutoff = 0
    }
  }
  
  #do the cutoff
  if (size_cutoff > 0) {
    network_links_count_totals <- subset(network_links_count_totals, network_links_count_totals$size > size_cutoff)
  }
  #update the network links to only include items that survived the cull
  network_links <- filter_(network_links, ~ start %in% network_links_count_totals$name | end %in% network_links_count_totals$name)
  
  #use sd to determine cutoff point for weighted items
  weight_sd = sd(network_links$weight)
  weight_mean = mean(network_links$weight)
  
  #get our cutoff point - this is actually inbetweenness cutoff
  weight_cut = weight_mean + weight_sd
  
  #if the user passed in a inbetweenness cutoff use it
  if (! is.na(cutoff)) {
    if (cutoff == 0) {
      #keeping all
    } else {
      #cutoff from user is a percentage
      cutoff <- (max2(network_links$weight) / 100) * cutoff
      network_links <- network_links[network_links$weight > cutoff,]
    }
  } else {
    if (is.na(itemName)) {
      network_links <- filter_(network_links, ~ weight > weight_cut)
    } else {
      network_links <- filter_(network_links, ~ weight > weight_cut | start == itemName | end == itemName) 
    }
  }
  
  nodeAttrs <- list()
  nodeAttrs$count <- network_links_count_totals
  
  nodes <- getNodesFromLinks(network_links, nodeAttrs)
  
  #benefit of 0 based indexes in Javascript
  network_links$start.index = match(network_links$start, nodes$name)-1
  network_links$end.index = match(network_links$end, nodes$name)-1
  
  #Remove any NA start or end indexes, as our network won't work then
  network_links<- filter_(network_links, ~ !is.na(start.index) )
  network_links<- filter_(network_links, ~ !is.na(end.index) )
  
  #use igraph's community functions, cannot find equivalent functionality in networkD3
  g <- graph_from_data_frame(network_links, directed = FALSE, vertices = NULL)
  
  #graphCommunity <- fastgreedy.community(g, merges = TRUE, membership = TRUE)
  graphCommunity <- walktrap.community(g)
  
  #there may be too many communities depending on how specifically you've filtered this
  commLength <- length(graphCommunity)
  if ( (commLength < 21) & (commLength > 0) ){
    print(paste("graph community length:", commLength))
    #update the nodes with graphCommunity information
    for(i in 1:commLength) {
      nodes <- join( 
        data.frame(name = matrix(unlist(graphCommunity[i]), byrow=FALSE, ncol = 1), group = i), 
        nodes,
        by = "name", type = "full", match = "all")
    }
    row.names(nodes) <- NULL
  } else {
    nodes$group <- 1
    commLength <- 1
  }

  #resort our nodes as the above operation resorts the nodes making for a mismatch when passing to JS and d3
  #TBH I don't think it even refers to the ID column, it's really by order of the dataframe it seems 
  #more information is required
  nodes <- nodes[order(nodes$ID),]
  
  
  #fill in the itemname for filtering
  MyClickScript <- '$( "#itemName" ).val(d.name); $( "#itemName" ).change(); '
  
  widths <- quantile(network_links$weight)
  
  maxPixelRadiusSize = 20
  
  #resize nodes so that we're better able to display them on screen
  nodes$size <- sqrt(nodes$size)
  maxSize = max(nodes$size)
  
  #if looking for an specifc item then get the next biggest item, as the specific item will be substainally bigger than the others
  if (! is.na(itemName) ) {
    #Added check in case the item we're searching is the only item returned
    nodesNotInSearch <- nodes$size[nodes$name!=itemName]
    if (length(nodesNotInSearch) > 1) {
      maxSize = max2(nodes$size[nodes$name!=itemName])
      #also make the main event more reasonable size
      itemNameSize <- nodes$size[nodes$name==itemName]
      if (! is.na(itemNameSize)) {
        #make it at most 1.1 bigger than the largest
        if (itemNameSize > (maxSize * 1.1)) {
          nodes$size[nodes$name==itemName] <- (maxSize * 1.1)
        }
      } else {
        print("WARNING: Focused Item was cut from dataset!!")
      }
    }
  }
  
  #if we are looking for a specific item give it a different colour
  if (! is.na(itemName)) {
    itemNameGroup = commLength + 1
    nodes$group[nodes$name == itemName] <- itemNameGroup
  }  
  
  linkDistance = log(commLength) * sqrt(commLength) * 350 
  
  #setup radius calculation
  radiusCalc <- JS('(d.nodesize/',maxSize,')*',maxPixelRadiusSize)
  
  #setup color scale
  colourScale <- JS("d3.scale.category10()")
  
  if (commLength > 10) {
    colourScale <- JS("d3.scale.category20()")
  } 
  
  #if drawing edges do so by quartile
  if (drawEdges) {
    if (networkEdgesQuarters) {
      linkWidth <- JS('function(d) {', 
                      'if (d.value <= ',widths[1],') return 1;',
                      'if (d.value <= ',widths[2],') return 2.5;',
                      'if (d.value <= ',widths[3],') return 5;',
                      'if (d.value <= ',widths[4],') return 7.5;',
                      'if (d.value >= ',widths[5],') return 10;',
                      '}'
      )
    } else {
      maxWidth = max2(network_links$weight)
      linkWidth <- JS('function(d) {', 
                      'return (d.value/',maxWidth,')*10;',
                      '}')
    }
  } else {
    linkWidth <- JS('function(d) { return 0; }')
  }
  
  chargeMax <- log(commLength) * (sqrt(commLength)*-50)
  
  d3 = forceNetwork(Links = network_links, Nodes = nodes,
                    Source = "start.index", Target = "end.index",
                    Value = "weight",
                    Nodesize = "size",
                    NodeID = "name", Group="group",
                    zoom=TRUE, 
                    radiusCalculation = radiusCalc,
                    #legend=TRUE,
                    fontSize = 14,
                    opacity = 0.9, 
                    fontFamily = "Arial, Helvetica, sans-serif",
                    opacityNoHover = 0.4,
                    charge=max(-100, chargeMax),
                    clickAction = MyClickScript,
                    linkDistance = min(linkDistance,350),
                    linkWidth = linkWidth,
                    linkColour = "#dadaeb",
                    colourScale = colourScale
                    
  )
  
  result <- list()
  result$graph <- d3
  result$nodes <- nodes
  result$links <- network_links
  result$igraph <- g
  result$adjacency <- get.adjacency(g, attr="weight", sparse=F, type = "both")
  result$corMatrix <- melt(cor(result$adjacency))
  
  return(result)
}



##################################################################################################################
# Start ShinyServer Code
##################################################################################################################
shinyServer(function(input, output) {  
  #######################################
  #server code run once for each client
  ######################################
  
  output$scatterPlot <-renderPlotly({
    #######################################
    #reactive server code
    ######################################
    
    #getPlot(input)
    filterBy = list()
    filterBy$character_class = input$character_class
    
    scatterPlot (
      xValue = input$xValue,
      xValueRange = input$xValueRange,
      yValue = input$yValue,
      yValueRange = input$yValueRange,
      filterBy = filterBy,
      groupBy = input$groupBy,
      sizeBy = input$sizeBy
    )    
  })
  
  output$histoPlot <-renderPlotly({
    #######################################
    #reactive server code
    ######################################
    
    filterBy = list()
    filterBy$character_class = input$character_class
    
    histoPlot (
      xValue = input$xValue,
      xValueRange = input$xValueRange,
      yValue = input$yValue,
      yValueRange = input$yValueRange,
      filterBy = filterBy,
      groupBy = input$groupBy,
      sizeBy = input$sizeBy,
      binSize = input$binValueSize
    )    
  })
  
  output$bin2dPlot <-renderPlotly({

    filterBy = list()
    filterBy$character_class = input$character_class
    
    bin2dPlot (
      xValue = input$xValue,
      xValueRange = input$xValueRange,
      yValue = input$yValue,
      yValueRange = input$yValueRange,
      filterBy = filterBy,
      groupBy = input$groupBy,
      binSize = input$binValueSize
    )
  })
  
  #as the network visualisations aren't the quickest I've wrapped the output in a reactive var
  networkOutput <- reactiveValues(data = NULL, itemNameA = NULL, itemNameB = NULL)
  
  #Here we wait for user to click "Refresh Network"
  observeEvent(input$getGraph, {
    data_width = as.numeric(length(data_original))
    column_start = data_width - 15
    column_end = data_width
    columns = c(column_start:column_end)
    
    filterBy = list()
    filterBy$character_class = input$network_character_class
    
    itemName <- input$itemName
    if (itemName == "") {
      itemName <- NA
    }
    
    print(paste("itemName:",itemName))
    
    out <- getGraph(columns, 
                    filterBy, 
                    itemName = itemName, 
                    drawEdges = input$networkEdges, 
                    cutoff = input$networkWeightCutOff, 
                    size_cutoff = input$networkSizeCutOff,
                    networkEdgesQuarters = input$networkEdgesQuarters)
    
    networkOutput$data <- out
  })
  
  output$itemsNetwork <- renderForceNetwork({
    
    if (is.null(networkOutput$data)) {
      return()
    }
    
    networkOutput$data$graph
  })
  
  output$itemsChordDiagram <- renderchordNetwork({
    if (is.null(networkOutput$data)) {
      empty <- data.frame(matrix(ncol = 1, nrow = 1))
      return(chordNetwork(empty))
    }
    
    chordNetwork(networkOutput$data$adjacency,
                 width = 600, 
                 height = 600, 
                 labels = as.vector(networkOutput$data$nodes$name), 
                 fontSize = 10,
                 #padding = 50,
                 labelDistance = 100)    
  })
  
  output$correlationMatrix <- renderPlotly({
    if (is.null(networkOutput$data)) {
      return()
    }
    
    corMatrix <- networkOutput$data$corMatrix  #melt(cor(networkOutput$data$adjacency))
        
    ggplot(data = corMatrix, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme.diablo_chart + 
      theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                       size = 10, hjust = 1))+
      xlab("") +
      ylab("") +
      coord_fixed()
  })
  
  output$networkFrameA <- renderUI({
    if (is.null(networkOutput$data)) {
      return("")
    }
    
    if (is.null(networkOutput$data$itemNameA)) {
      return("")
    }
    result <- tags$iframe(src=paste("http://www.diablohub.com/database/item/",linknameLabel(networkOutput$data$itemNameA),"/information" , sep = ""), height=600, width=535)
    result
  })  
  
  output$networkFrameB <- renderUI({
    if (is.null(networkOutput$data)) {
      return("")
    }
    
    if (is.null(networkOutput$data$itemNameB)) {
      return("")
    }
    
    #would have preferred to use the actual Blizard Diablo 3 item database, but have some cross site (CORS) 
    #issue, so using diablohub sadly it's not exactly up to date with all of the items in the dataset 
    result <- tags$iframe(src=paste("http://www.diablohub.com/database/item/",linknameLabel(networkOutput$data$itemNameB),"/information" , sep = ""), height=600, width=535)
    result
  })    
  
  
  #this listens to all plotly clicks, but I'm particularly interested in reacting to the correlation matrix clicks
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) {
      return("")
    } else {
      Xvalue <- as.numeric(d$x[[1]])
      Yvalue <- as.numeric(d$y[[1]])
      
      networkOutput$data$itemNameA <- networkOutput$data$corMatrix$Var1[Yvalue]
      networkOutput$data$itemNameB <- networkOutput$data$corMatrix$Var1[Xvalue]
      
      return(paste(networkOutput$data$itemNameA, networkOutput$data$itemNameB, sep = " & "))
    }
    
  })  
})