findOrphans <- function(edgelist) {

  edgelist %>%
    group_by(from) %>%
    summarise(sum = sum(weight), .groups = "drop_last") %>%
    filter(sum == 0) %>%
    pull(from)

}

vNetwork <- function(vName, vInfo, edgelist, direction = c("both", "up", "down")) {

  # FUNCTIONS
  networkDown <- function(vName, vInfo, edgelist) {

    edges <- list()
    vertices <- list()

    myVertex <- vName
    myLevel <- vInfo %>% filter(vName %in% myVertex) %>% pull(level)

    vertices[[myLevel]] <- myVertex

    levels <- seq(myLevel, max(edgelist$fromLevel), 1)

    for(i in levels) {

      edges[[i]] <- edgelist %>% filter(fromLevel == i) %>% filter(from %in% vertices[[i]])
      vertices[[i+1]] <- edges[[i]] %>% pull(to) %>% unique

    }

    vertices <- do.call(c, vertices)
    vertices <- vInfo %>% filter(vName %in% vertices)
    edges <- do.call(rbind, edges)

    list(vertices = vertices, edges = edges)

  }

  networkUp <- function(vName, vInfo, edgelist) {

    edges <- list()
    vertices <- list()

    myVertex <- vName
    myLevel <- vInfo %>% filter(vName %in% myVertex) %>% pull(level)

    vertices[[myLevel]] <- myVertex

    levels <- seq(myLevel, min(edgelist$toLevel), -1)

    for(i in levels) {

      edges[[i]] <- edgelist %>% filter(toLevel == i) %>% filter(to %in% vertices[[i]])
      vertices[[i-1]] <- edges[[i]] %>% pull(from) %>% unique

    }

    vertices <- do.call(c, vertices)
    vertices <- vInfo %>% filter(vName %in% vertices)
    edges <- do.call(rbind, edges)

    list(vertices = vertices, edges = edges)

  }


  # APPLIED
  edges <- list()
  vertices <- list()

  myVertex <- vName
  myLevel <- vInfo %>% filter(vName %in% myVertex) %>% pull(level)

  if((direction == "both" | direction == "up") & myLevel != 1) {

    tmp <- networkUp(vName, vInfo, edgelist)
    edges[["up"]] <- tmp$edges
    vertices[["up"]] <- tmp$vertices

  }

  if((direction == "both" | direction == "down") & myLevel != 5) {

    tmp <- networkDown(vName, vInfo, edgelist)
    edges[["down"]] <- tmp$edges
    vertices[["down"]] <- tmp$vertices

  }

  vertices <- do.call(rbind, vertices)
  edges <- do.call(rbind, edges)

  list(vertices = vertices, edges = edges)

}
