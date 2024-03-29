visLayout <- function(edgelist, vInfo, key, spacing) {

  require(ggraph)

  vInfo <- vInfo %>% arrange(level, Node)
  edgelist <- edgelist %>% arrange(layer)


  # DUMMY VERTEX ------------------------------------------------------------
  VINFO <- vInfo %>% filter(level == min(level))

  EDGELIST <- edgelist %>% add_row(.before = 1, layer = "dummyLayer", from = "dummyVertex", to = VINFO$Node)

  VINFO <-
    vInfo %>%
    mutate(level = level + 1) %>%
    add_row(.before = 1, level = 1, levelName = "dummyLevel", Node = "dummyVertex")

  IGRAPH <- EDGELIST %>% select(from, to, weight) %>% graph_from_data_frame(directed = FALSE)


  # SUGIYAMA LAYOUT TEMPLATE ------------------------------------------------
  LAYOUT <-
    (IGRAPH %>%
       layout_with_sugiyama(layers = VINFO$level))$layout %>%
    as.data.frame %>%
    setNames(c("x", "y")) %>%
    slice(-1) %>%
    cbind(vInfo, .) %>%
    as_tibble() %>%
    group_by(level, levelName) %>%
    mutate(x = scales::rescale(x, to = c(-1,1)),
           y = -level,
           pos = 1:n()) %>%
    ungroup %>%
    arrange(level, x, y) %>%
    split(.$level)


  # VERTEX SPACING ----------------------------------------------------------
  LAYOUT <- lapply(LAYOUT, function(i) {

    if(nrow(i) == 1) { dt <- i } else {

      dt <- i

      for(j in 1:(nrow(dt)-1)) {

        diff = abs(dt$x[[j]] - dt$x[[j + 1]])

        if(diff < spacing[[1]]) { dt$x[-(1:j)] <- dt$x[-(1:j)] + (spacing[[1]] - diff) }
        if(diff > spacing[[2]]) { dt$x[-(1:j)] <- dt$x[-(1:j)] - (diff - spacing[[2]]) }

      }

    }

    return(dt)

  })


  # RADIAL LAYOUT -----------------------------------------------------------
  makeRadial <- function(dataInput, dataKey) {

    midPoint <- 270

    # Convert angles to radian
    min <- midPoint - dataKey$angle
    min <- min * pi / 180
    max <- dataKey$angle + midPoint
    max <- max * pi / 180

    # S = R0, arc length is equal to radius multiplied by theta
    minS <- min * dataInput$y[[1]]
    maxS <- max * dataInput$y[[1]]

    # (Optional) Adjustments to radius
    R <- dataKey$addR + dataInput$level[[1]]

    # Convert to polar coordinates
    dataInput %>%
      mutate(x = scales::rescale(x, to = c(minS, maxS))) %>%
      mutate(thetaRad2 = x/y,
             x2 = R * cos(thetaRad2),
             y2 = R * sin(thetaRad2))

  }

  LAYOUT_GG <- lapply(1:length(LAYOUT), function(x) { makeRadial(LAYOUT[[x]], key[x,]) }) %>% bind_rows()

  LAYOUT_GG <-
    LAYOUT_GG %>%
    select(-x, -y) %>%
    rename(x = x2, y = y2, theta = thetaRad2) %>%
    create_layout(edgelist, layout = .)


  # EXTRACT -----------------------------------------------------------------
  findEdges <- get_edges("short", collapse = "none")

  edges <-
    findEdges(LAYOUT_GG) %>%
    as_tibble %>%
    select(fromLevel = node1.level,  toLevel = node2.level, layer,
           from = node1.name, to = node2.name,
           x = node1.x, y = node1.y, xend = node2.x, yend = node2.y) %>%
    mutate(layer = factor(layer, levels = edgelist %>% pull(layer) %>% levels))

  findNodes <- get_nodes()

  vertices <-
    findNodes(LAYOUT_GG) %>%
    as_tibble %>%
    select(level, levelName, Node = name, definition, x, y, theta) 


  # OUTPUT ------------------------------------------------------------------
  vertices$Node <- gsub("'", '', vertices$Node)
  edges$to <- gsub("'", '', edges$to)

  output <- list(edges = edges, vertices = vertices)
  return(output)

}

visPlot <- function(layout, key = NULL,
                       vecOpacity = c(1,1,1,1),
                       vecSize = c(5,4,3.5,3,2.5)) {

  require(gginnards)

  # Edges and vertices
  p1 <- suppressWarnings({
    layout$edges %>%
      arrange(desc(layer)) %>%
      ggplot() +
      geom_segment(aes(x = x, xend = xend, y = y, yend = yend,
                       alpha = layer),
                   size = 0.3, colour = "#34495E",
                   show.legend = FALSE) +

      scale_alpha_manual(values = vecOpacity,
                         labels = {
                           layout$edges %>%
                             arrange(desc(layer)) %>%
                             group_by(layer) %>%
                             summarise %>%
                             select(layer) %>%
                             unlist %>%
                             as.vector
                         }
      ) +

      geom_point(data = layout$vertices %>% mutate(level = fct_inorder(as.character(level), ordered = TRUE)),
                 aes(x = x, y = y,
                     size = level,
                     fill = level,

                     # Warning suppressed, need to add this for plotly interaction
                     label = Node),

                 shape = 21, colour = "#34495E") +

      scale_size_manual(values = vecSize, labels = unique(layout$vertices)) +
      scale_fill_viridis_d(labels = unique(layout$vertices), direction = -1)

  })

  output <- p1

  # Circles
  if(!is.null(key)) {
    internal_circleFun <- function(rVec, center = c(0,0), npoints = 100){

      tt <- seq(0, 2 * pi, length.out = npoints)

      lapply(rVec, function(r) {

        # Circle for plotting as geom_path

        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)

        tibble(r = r, x = xx, y = yy)

      }) %>%
        bind_rows()

    }

    p2 <-
      ggplot() +
      geom_path(data = internal_circleFun(key$r) %>% filter(y < 0),
                aes(x = x, y = y, group = r),
                size = 0.25, alpha = 1, colour = "#D6DBDF")

    p2b <- extract_layers(p2, "GeomPath")

    output <- output %>% append_layers(p2b, position = "bottom")

  }

  # Aesthetics
  output <-
    output +

    theme_void() +
    theme(panel.grid = element_blank(), # Manually added for ggplotly
          axis.line = element_blank(), # Manually added for ggplotly
          legend.position = "top") +
    coord_fixed() +

    guides(fill = guide_legend(title.position = "top", hjust = 0.5,
                               override.aes = list(size = vecSize)),
           size = FALSE,
           alpha = guide_legend(title.position = "top", hjust = 0.5)) +
    labs(fill = "AH level: ", size = "AH level:")

  return(output)

}

findOrphans <- function(edgelist) {
  
  edgelist %>%
    group_by(from) %>%
    summarise(sum = sum(weight), .groups = "drop_last") %>%
    filter(sum == 0) %>%
    pull(from)
  
}

vNetwork <- function(Node, vInfo, edgelist, direction = c("both", "up", "down")) {
  
  # FUNCTIONS
  networkDown <- function(Node, vInfo, edgelist) {
    
    edges <- list()
    vertices <- list()
    
    myVertex <- Node
    myLevel <- vInfo %>% filter(Node %in% myVertex) %>% pull(level)
    
    vertices[[myLevel]] <- myVertex
    
    levels <- seq(myLevel, max(edgelist$fromLevel), 1)
    
    for(i in levels) {
      
      edges[[i]] <- edgelist %>% filter(fromLevel == i) %>% filter(from %in% vertices[[i]])
      vertices[[i+1]] <- edges[[i]] %>% pull(to) %>% unique
      
    }
    
    vertices <- do.call(c, vertices)
    vertices <- vInfo %>% filter(Node %in% vertices)
    edges <- do.call(rbind, edges)
    
    list(vertices = vertices, edges = edges)
    
  }
  
  networkUp <- function(Node, vInfo, edgelist) {
    
    edges <- list()
    vertices <- list()
    
    myVertex <- Node
    myLevel <- vInfo %>% filter(Node %in% myVertex) %>% pull(level)
    
    vertices[[myLevel]] <- myVertex
    
    levels <- seq(myLevel, min(edgelist$toLevel), -1)
    
    for(i in levels) {
      
      edges[[i]] <- edgelist %>% filter(toLevel == i) %>% filter(to %in% vertices[[i]])
      vertices[[i-1]] <- edges[[i]] %>% pull(from) %>% unique
      
    }
    
    vertices <- do.call(c, vertices)
    vertices <- vInfo %>% filter(Node %in% vertices)
    edges <- do.call(rbind, edges)
    
    list(vertices = vertices, edges = edges)
    
  }
  
  
  # APPLIED
  edges <- list()
  vertices <- list()
  
  myVertex <- Node
  myLevel <- vInfo %>% filter(Node %in% myVertex) %>% pull(level)
  
  if((direction == "both" | direction == "up") & myLevel != 1) {
    
    tmp <- networkUp(Node, vInfo, edgelist)
    edges[["up"]] <- tmp$edges
    vertices[["up"]] <- tmp$vertices
    
  }
  
  if((direction == "both" | direction == "down") & myLevel != 5) {
    
    tmp <- networkDown(Node, vInfo, edgelist)
    edges[["down"]] <- tmp$edges
    vertices[["down"]] <- tmp$vertices
    
  }
  
  vertices <- do.call(rbind, vertices)
  edges <- do.call(rbind, edges)
  
  list(vertices = vertices, edges = edges)
  
}