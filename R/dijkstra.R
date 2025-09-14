#' Dijkstra algorithm.
#'
#' Returns the shortest path to every other node from the starting node
#' as a vector. See \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#' for details.
#'
#' @param graph A data.frame with three variables (v1, v2 and w) that contains
#' the edges of the graph (from v1 to v2) with the weight of the edge (w).
#' @param init A number indicating the starting node.
#'
#' @returns A numeric vector with shortest path from the starting node to other
#' nodes.
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' wiki_graph <-
#' data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#' v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#' w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#'
dijkstra <- function(graph, init) {
  # Validate graph structure
  if (!is.data.frame(graph)) {
    stop("graph must be a data.frame")
  }
  required_cols <- c("v1", "v2", "w")
  if (!all(required_cols %in% names(graph)) || ncol(graph) != 3) {
    stop("graph must have exactly three columns named 'v1', 'v2' and 'w'")
  }
  # Ensure correct types and no missing/inf values
  if (!is.numeric(graph$v1) || !is.numeric(graph$v2) || !is.numeric(graph$w)) {
    stop("columns 'v1', 'v2', and 'w' must be numeric")
  }
  if (any(is.na(graph$v1) | is.na(graph$v2) | is.na(graph$w))) {
    stop("graph contains NA values")
  }
  if (any(!is.finite(graph$v1) | !is.finite(graph$v2) | !is.finite(graph$w))) {
    stop("graph contains non-finite values")
  }
  if (any(graph$w < 0)) {
    stop("edge weights 'w' must be non-negative for Dijkstra's algorithm")
  }

  # Validate init node
  if (!(is.numeric(init) && length(init) == 1 && !is.na(init) && is.finite(init))) {
    stop("init must be a finite numeric scalar")
  }

  # Build sorted unique node list (numeric)
  nodes <- sort(unique(c(graph$v1, graph$v2)))
  if (!(init %in% nodes)) {
    stop("init node must exist in the graph (either in v1 or v2)")
  }

  # Build adjacency list: for each node, list neighbors and weights
  node_names <- as.character(nodes)
  adj <- setNames(vector("list", length(nodes)), node_names)
  for (nm in node_names) {
    adj[[nm]] <- list(neigh = numeric(0), w = numeric(0))
  }
  # Fill adjacency (directed edges from v1 -> v2)
  for (i in seq_len(nrow(graph))) {
    u <- as.character(graph$v1[i])
    v <- graph$v2[i]
    w <- graph$w[i]
    adj[[u]]$neigh <- c(adj[[u]]$neigh, v)
    adj[[u]]$w     <- c(adj[[u]]$w, w)
  }

  # Initialize distances and visited set
  dist <- setNames(rep(Inf, length(nodes)), node_names)
  dist[as.character(init)] <- 0
  visited <- setNames(rep(FALSE, length(nodes)), node_names)

  # Dijkstra main loop (simple O(V^2) approach)
  repeat {
    # select unvisited node with smallest distance
    unvisited_nodes <- node_names[!visited]
    if (length(unvisited_nodes) == 0) break
    d_vals <- dist[unvisited_nodes]
    min_idx <- which.min(d_vals)
    u_name <- unvisited_nodes[min_idx]
    # if smallest distance is Inf, remaining nodes unreachable
    if (!is.finite(dist[u_name])) break
    # mark visited
    visited[u_name] <- TRUE
    # relax edges from u
    neighs <- adj[[u_name]]$neigh
    weights <- adj[[u_name]]$w
    if (length(neighs) > 0) {
      for (k in seq_along(neighs)) {
        v <- as.character(neighs[k])
        if (!visited[v]) {
          alt <- dist[u_name] + weights[k]
          if (alt < dist[v]) dist[v] <- alt
        }
      }
    }
  }

  # Return distances as numeric vector named by node values (sorted ascending)
  # Keep numeric values (Inf for unreachable)
  # Names will be character representations of the node values
  return(unname(dist))
}
