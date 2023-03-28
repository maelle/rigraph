# styler: off

gorder_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_vcount, graph)


  res
}

graph_from_lcf_impl <- function(n, shifts, repeats=1) {
  # Argument checks
  n <- as.integer(n)
  shifts <- as.numeric(shifts)
  repeats <- as.integer(repeats)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_lcf_vector, n, shifts, repeats)

  if (igraph_opt("add.params")) {
    res$name <- 'LCF graph'
  }

  res
}

#' Creating a graph from LCF notation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `graph.lcf()` was renamed to `graph_from_lcf()` to create a more
#' consistent API.
#' @inheritParams graph_from_lcf
#' @keywords internal
#' @export
graph.lcf <- function(n , shifts , repeats = 1) {
   lifecycle::deprecate_soft("1.5.0", "graph.lcf()", "graph_from_lcf()")
   graph_from_lcf(n = n, shifts = shifts, repeats = repeats)
}

graph_from_adj_list_impl <- function(adjlist, mode=c("out", "in", "all", "total"), duplicate=TRUE) {
  # Argument checks
  adjlist <- lapply(adjlist, function(x) as.integer(x)-1L)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  duplicate <- as.logical(duplicate)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_adjlist, adjlist, mode, duplicate)

  res
}

#' Create graphs from adjacency lists
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `graph.adjlist()` was renamed to `graph_from_adj_list()` to create a more
#' consistent API.
#' @inheritParams graph_from_adj_list
#' @keywords internal
#' @export
graph.adjlist <- function(adjlist , mode = c("out","in","all","total") , duplicate = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "graph.adjlist()", "graph_from_adj_list()")
   graph_from_adj_list(adjlist = adjlist, mode = mode, duplicate = duplicate, x = x)
}

realize_degseq_impl <- function(out.deg, in.deg=NULL, allowed.edge.types=c("simple", "loops", "multi", "all"), method=c("smallest", "largest", "index")) {
  # Argument checks
  out.deg <- as.numeric(out.deg)
  if (!is.null(in.deg)) in.deg <- as.numeric(in.deg)
  allowed.edge.types <- switch(igraph.match.arg(allowed.edge.types),
    "simple"=0L, "loop"=1L, "loops"=1L, "multi"=6L, "multiple"=6L, "all"=7L)
  method <- switch(igraph.match.arg(method), "smallest"=0L, "largest"=1L, "index"=2L)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_realize_degree_sequence, out.deg, in.deg, allowed.edge.types, method)

  if (igraph_opt("add.params")) {
    res$name <- 'Graph from degree sequence'
    res$out.deg <- out.deg
    res$in.deg <- in.deg
    res$allowed.edge.types <- allowed.edge.types
    res$method <- method
  }

  res
}

sample_forestfire_impl <- function(nodes, fw.prob, bw.factor=1, ambs=1, directed=TRUE) {
  # Argument checks
  nodes <- as.integer(nodes)
  fw.prob <- as.numeric(fw.prob)
  bw.factor <- as.numeric(bw.factor)
  ambs <- as.integer(ambs)
  directed <- as.logical(directed)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_forest_fire_game, nodes, fw.prob, bw.factor, ambs, directed)

  if (igraph_opt("add.params")) {
    res$name <- 'Forest fire model'
    res$fw.prob <- fw.prob
    res$bw.factor <- bw.factor
    res$ambs <- ambs
  }

  res
}

#' Forest Fire Network Model
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `forest.fire.game()` was renamed to `sample_forestfire()` to create a more
#' consistent API.
#' @inheritParams sample_forestfire
#' @keywords internal
#' @export
forest.fire.game <- function(nodes , fw.prob , bw.factor = 1 , ambs = 1 , directed = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "forest.fire.game()", "sample_forestfire()")
   sample_forestfire(nodes = nodes, fw.prob = fw.prob, bw.factor = bw.factor, ambs = ambs, directed = directed)
}

sample_islands_impl <- function(islands.n, islands.size, islands.pin, n.inter) {
  # Argument checks
  islands.n <- as.integer(islands.n)
  islands.size <- as.integer(islands.size)
  islands.pin <- as.numeric(islands.pin)
  n.inter <- as.integer(n.inter)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_simple_interconnected_islands_game, islands.n, islands.size, islands.pin, n.inter)

  if (igraph_opt("add.params")) {
    res$name <- 'Interconnected islands model'
    res$islands.n <- islands.n
    res$islands.size <- islands.size
    res$islands.pin <- islands.pin
    res$n.inter <- n.inter
  }

  res
}

#' A graph with subgraphs that are each a random graph.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `interconnected.islands.game()` was renamed to `sample_islands()` to create a more
#' consistent API.
#' @inheritParams sample_islands
#' @keywords internal
#' @export
interconnected.islands.game <- function(islands.n , islands.size , islands.pin , n.inter) {
   lifecycle::deprecate_soft("1.5.0", "interconnected.islands.game()", "sample_islands()")
   sample_islands(islands.n = islands.n, islands.size = islands.size, islands.pin = islands.pin, n.inter = n.inter)
}

sample_fitness_impl <- function(no.of.edges, fitness.out, fitness.in=NULL, loops=FALSE, multiple=FALSE) {
  # Argument checks
  no.of.edges <- as.integer(no.of.edges)
  fitness.out <- as.numeric(fitness.out)
  if (!is.null(fitness.in)) fitness.in <- as.numeric(fitness.in)
  loops <- as.logical(loops)
  multiple <- as.logical(multiple)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_static_fitness_game, no.of.edges, fitness.out, fitness.in, loops, multiple)

  if (igraph_opt("add.params")) {
    res$name <- 'Static fitness model'
    res$loops <- loops
    res$multiple <- multiple
  }

  res
}

#' Random graphs from vertex fitness scores
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `static.fitness.game()` was renamed to `sample_fitness()` to create a more
#' consistent API.
#' @inheritParams sample_fitness
#' @keywords internal
#' @export
static.fitness.game <- function(no.of.edges , fitness.out , fitness.in = NULL , loops = FALSE , multiple = FALSE) {
   lifecycle::deprecate_soft("1.5.0", "static.fitness.game()", "sample_fitness()")
   sample_fitness(no.of.edges = no.of.edges, fitness.out = fitness.out, fitness.in = fitness.in, loops = loops, multiple = multiple)
}

sample_fitness_pl_impl <- function(no.of.nodes, no.of.edges, exponent.out, exponent.in=-1, loops=FALSE, multiple=FALSE, finite.size.correction=TRUE) {
  # Argument checks
  no.of.nodes <- as.integer(no.of.nodes)
  no.of.edges <- as.integer(no.of.edges)
  exponent.out <- as.numeric(exponent.out)
  exponent.in <- as.numeric(exponent.in)
  loops <- as.logical(loops)
  multiple <- as.logical(multiple)
  finite.size.correction <- as.logical(finite.size.correction)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_static_power_law_game, no.of.nodes, no.of.edges, exponent.out, exponent.in, loops, multiple, finite.size.correction)

  if (igraph_opt("add.params")) {
    res$name <- 'Static power law model'
    res$exponent.out <- exponent.out
    res$exponent.in <- exponent.in
    res$loops <- loops
    res$multiple <- multiple
    res$finite.size.correction <- finite.size.correction
  }

  res
}

#' Scale-free random graphs, from vertex fitness scores
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `static.power.law.game()` was renamed to `sample_fitness_pl()` to create a more
#' consistent API.
#' @inheritParams sample_fitness_pl
#' @keywords internal
#' @export
static.power.law.game <- function(no.of.nodes , no.of.edges , exponent.out , exponent.in = -1 , loops = FALSE , multiple = FALSE , finite.size.correction = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "static.power.law.game()", "sample_fitness_pl()")
   sample_fitness_pl(no.of.nodes = no.of.nodes, no.of.edges = no.of.edges, exponent.out = exponent.out, exponent.in = exponent.in, loops = loops, multiple = multiple, finite.size.correction = finite.size.correction)
}

sample_k_regular_impl <- function(no.of.nodes, k, directed=FALSE, multiple=FALSE) {
  # Argument checks
  no.of.nodes <- as.integer(no.of.nodes)
  k <- as.integer(k)
  directed <- as.logical(directed)
  multiple <- as.logical(multiple)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_k_regular_game, no.of.nodes, k, directed, multiple)

  if (igraph_opt("add.params")) {
    res$name <- 'k-regular graph'
    res$k <- k
  }

  res
}

#' Create a random regular graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `k.regular.game()` was renamed to `sample_k_regular()` to create a more
#' consistent API.
#' @inheritParams sample_k_regular
#' @keywords internal
#' @export
k.regular.game <- function(no.of.nodes , k , directed = FALSE , multiple = FALSE) {
   lifecycle::deprecate_soft("1.5.0", "k.regular.game()", "sample_k_regular()")
   sample_k_regular(no.of.nodes = no.of.nodes, k = k, directed = directed, multiple = multiple)
}

sample_sbm_impl <- function(n, pref.matrix, block.sizes, directed=FALSE, loops=FALSE) {
  # Argument checks
  n <- as.integer(n)
  pref.matrix <- as.matrix(structure(as.double(pref.matrix), dim=dim(pref.matrix)))
  block.sizes <- as.integer(block.sizes)
  directed <- as.logical(directed)
  loops <- as.logical(loops)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_sbm_game, n, pref.matrix, block.sizes, directed, loops)

  if (igraph_opt("add.params")) {
    res$name <- 'Stochastic block model'
    res$loops <- loops
  }

  res
}

#' Sample stochastic block model
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `sbm.game()` was renamed to `sample_sbm()` to create a more
#' consistent API.
#' @inheritParams sample_sbm
#' @keywords internal
#' @export
sbm.game <- function(n , pref.matrix , block.sizes , directed = FALSE , loops = FALSE) {
   lifecycle::deprecate_soft("1.5.0", "sbm.game()", "sample_sbm()")
   sample_sbm(n = n, pref.matrix = pref.matrix, block.sizes = block.sizes, directed = directed, loops = loops)
}

hsbm_1_game_impl <- function(n, m, rho, C, p) {
  # Argument checks
  n <- as.integer(n)
  m <- as.integer(m)
  rho <- as.numeric(rho)
  C <- as.matrix(structure(as.double(C), dim=dim(C)))
  p <- as.numeric(p)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_hsbm_game, n, m, rho, C, p)

  if (igraph_opt("add.params")) {
    res$name <- 'Hierarchical stochastic block model'
    res$m <- m
    res$rho <- rho
    res$C <- C
    res$p <- p
  }

  res
}

hsbm_list_game_impl <- function(n, mlist, rholist, Clist, p) {
  # Argument checks
  n <- as.integer(n)
  mlist <- as.integer(mlist)
  if (!all(sapply(Clist, is.matrix))) {
    stop("Clist is not a list of matrices")
  }
  p <- as.numeric(p)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_hsbm_list_game, n, mlist, rholist, Clist, p)

  if (igraph_opt("add.params")) {
    res$name <- 'Hierarchical stochastic block model'
    res$p <- p
  }

  res
}

sample_correlated_gnp_impl <- function(old.graph, corr, p=edge_density(old.graph), permutation=NULL) {
  # Argument checks
  if (!is_igraph(old.graph)) { stop("Not a graph object") }
  corr <- as.numeric(corr)
  p <- as.numeric(p)
  if (!is.null(permutation)) permutation <- as.numeric(permutation)-1

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_correlated_game, old.graph, corr, p, permutation)

  if (igraph_opt("add.params")) {
    res$name <- 'Correlated random graph'
    res$corr <- corr
    res$p <- p
  }

  res
}

sample_correlated_gnp_pair_impl <- function(n, corr, p, directed=FALSE, permutation=NULL) {
  # Argument checks
  n <- as.integer(n)
  corr <- as.numeric(corr)
  p <- as.numeric(p)
  directed <- as.logical(directed)
  if (!is.null(permutation)) permutation <- as.numeric(permutation)-1

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_correlated_pair_game, n, corr, p, directed, permutation)

  res
}

sample_dot_product_impl <- function(vecs, directed=FALSE) {
  # Argument checks
  vecs <- as.matrix(structure(as.double(vecs), dim=dim(vecs)))
  directed <- as.logical(directed)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_dot_product_game, vecs, directed)

  res
}

sample_sphere_surface_impl <- function(dim, n=1, radius=1, positive=TRUE) {
  # Argument checks
  dim <- as.integer(dim)
  n <- as.integer(n)
  radius <- as.numeric(radius)
  positive <- as.logical(positive)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_sample_sphere_surface, dim, n, radius, positive)

  res
}

sample_sphere_volume_impl <- function(dim, n=1, radius=1, positive=TRUE) {
  # Argument checks
  dim <- as.integer(dim)
  n <- as.integer(n)
  radius <- as.numeric(radius)
  positive <- as.logical(positive)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_sample_sphere_volume, dim, n, radius, positive)

  res
}

sample_dirichlet_impl <- function(n, alpha) {
  # Argument checks
  n <- as.integer(n)
  alpha <- as.numeric(alpha)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_sample_dirichlet, n, alpha)

  res
}

harmonic_centrality_impl <- function(graph, vids=V(graph), mode=c("out", "in", "all", "total"), weights=NULL, normalized=FALSE, cutoff=-1) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  normalized <- as.logical(normalized)
  cutoff <- as.numeric(cutoff)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_harmonic_centrality_cutoff, graph, vids-1, mode, weights, normalized, cutoff)
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res) <- vertex_attr(graph, "name", vids)
  }
  res
}

page_rank_impl <- function(graph, algo=c("prpack", "arpack"), vids=V(graph), directed=TRUE, damping=0.85, personalized=NULL, weights=NULL, options=NULL) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  algo <- switch(igraph.match.arg(algo), "arpack"=1L, "prpack"=2L)
  vids <- as.igraph.vs(graph, vids)
  directed <- as.logical(directed)
  damping <- as.numeric(damping)
  if (!is.null(personalized)) personalized <- as.numeric(personalized)
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  if (is.null(options)) {
    if (algo == 0L) {
      options <- list(niter=1000, eps=0.001)
    } else if (algo == 1L) {
      options <- arpack_defaults
    } else {
      options <- NULL
    }
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_personalized_pagerank, graph, algo, vids-1, directed, damping, personalized, weights, options)
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res$vector) <- vertex_attr(graph, "name", vids)
  }
  res
}

#' The Page Rank algorithm
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `page.rank()` was renamed to `page_rank()` to create a more
#' consistent API.
#' @inheritParams page_rank
#' @keywords internal
#' @export
page.rank <- function(graph , algo = c("prpack","arpack") , vids = V(graph) , directed = TRUE , damping = 0.85 , personalized = NULL , weights = NULL , options = NULL) {
   lifecycle::deprecate_soft("1.5.0", "page.rank()", "page_rank()")
   page_rank(graph = graph, algo = algo, vids = vids, directed = directed, damping = damping, personalized = personalized, weights = weights, options = options)
}

reverse_edges_impl <- function(graph, eids=E(graph)) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  eids <- as.igraph.es(graph, eids)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_reverse_edges, graph, eids-1)

  res
}

mean_distance_impl <- function(graph, weights=NULL, directed=TRUE, unconnected=TRUE, details=FALSE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  directed <- as.logical(directed)
  unconnected <- as.logical(unconnected)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_average_path_length_dijkstra, graph, weights, directed, unconnected)
  if (!details) {
    res <- res$res
  }
  res
}

#' Shortest (directed or undirected) paths between vertices
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `average.path.length()` was renamed to `distance_table()` to create a more
#' consistent API.
#' @inheritParams distance_table
#' @keywords internal
#' @export
average.path.length <- function(graph , weights = NULL , directed = TRUE , unconnected = TRUE , details = FALSE) {
   lifecycle::deprecate_soft("1.5.0", "average.path.length()", "distance_table()")
   distance_table(graph = graph, weights = weights, directed = directed, unconnected = unconnected, details = details)
}

distance_table_impl <- function(graph, directed=TRUE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  directed <- as.logical(directed)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_path_length_hist, graph, directed)

  res
}

#' Shortest (directed or undirected) paths between vertices
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `path.length.hist()` was renamed to `distance_table()` to create a more
#' consistent API.
#' @inheritParams distance_table
#' @keywords internal
#' @export
path.length.hist <- function(graph , directed = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "path.length.hist()", "distance_table()")
   distance_table(graph = graph, directed = directed)
}

simplify_impl <- function(graph, remove.multiple=TRUE, remove.loops=TRUE, edge.attr.comb=igraph_opt("edge.attr.comb")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  remove.multiple <- as.logical(remove.multiple)
  remove.loops <- as.logical(remove.loops)
  edge.attr.comb <- igraph.i.attribute.combination(edge.attr.comb)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_simplify, graph, remove.multiple, remove.loops, edge.attr.comb)

  res
}

feedback_arc_set_impl <- function(graph, weights=NULL, algo=c("approx_eades", "exact_ip")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  algo <- switch(igraph.match.arg(algo), "exact_ip"=0L, "approx_eades"=1L)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_feedback_arc_set, graph, weights, algo)
  if (igraph_opt("return.vs.es")) {
    res <- create_es(graph, res)
  }
  res
}

which_loop_impl <- function(graph, eids=E(graph)) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  eids <- as.igraph.es(graph, eids)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_loop, graph, eids-1)

  res
}

#' Find the multiple or loop edges in a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `is.loop()` was renamed to `which_multiple()` to create a more
#' consistent API.
#' @inheritParams which_multiple
#' @keywords internal
#' @export
is.loop <- function(graph , eids = E(graph)) {
   lifecycle::deprecate_soft("1.5.0", "is.loop()", "which_multiple()")
   which_multiple(graph = graph, eids = eids)
}

is_dag_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_dag, graph)

  res
}

#' Directed acyclic graphs
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `is.dag()` was renamed to `is_dag()` to create a more
#' consistent API.
#' @inheritParams is_dag
#' @keywords internal
#' @export
is.dag <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "is.dag()", "is_dag()")
   is_dag(graph = graph)
}

is_simple_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_simple, graph)

  res
}

#' Simple graphs
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `is.simple()` was renamed to `simplify()` to create a more
#' consistent API.
#' @inheritParams simplify
#' @keywords internal
#' @export
is.simple <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "is.simple()", "simplify()")
   simplify(graph = graph)
}

which_multiple_impl <- function(graph, eids=E(graph)) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  eids <- as.igraph.es(graph, eids)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_multiple, graph, eids-1)

  res
}

#' Find the multiple or loop edges in a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `is.multiple()` was renamed to `which_multiple()` to create a more
#' consistent API.
#' @inheritParams which_multiple
#' @keywords internal
#' @export
is.multiple <- function(graph , eids = E(graph)) {
   lifecycle::deprecate_soft("1.5.0", "is.multiple()", "which_multiple()")
   which_multiple(graph = graph, eids = eids)
}

any_loop_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_has_loop, graph)

  res
}

any_multiple_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_has_multiple, graph)

  res
}

#' Find the multiple or loop edges in a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `has.multiple()` was renamed to `which_multiple()` to create a more
#' consistent API.
#' @inheritParams which_multiple
#' @keywords internal
#' @export
has.multiple <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "has.multiple()", "which_multiple()")
   which_multiple(graph = graph)
}

count_multiple_impl <- function(graph, eids=E(graph)) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  eids <- as.igraph.es(graph, eids)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_count_multiple, graph, eids-1)

  res
}

#' Find the multiple or loop edges in a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `count.multiple()` was renamed to `which_multiple()` to create a more
#' consistent API.
#' @inheritParams which_multiple
#' @keywords internal
#' @export
count.multiple <- function(graph , eids = E(graph)) {
   lifecycle::deprecate_soft("1.5.0", "count.multiple()", "which_multiple()")
   which_multiple(graph = graph, eids = eids)
}

eigen_centrality_impl <- function(graph, directed=FALSE, scale=TRUE, weights=NULL, options=arpack_defaults) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  directed <- as.logical(directed)
  scale <- as.logical(scale)
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  options.tmp <- arpack_defaults; options.tmp[ names(options) ] <- options ; options <- options.tmp

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_eigenvector_centrality, graph, directed, scale, weights, options)
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res$vector) <- vertex_attr(graph, "name", V(graph))
  }
  res
}

#' Find Eigenvector Centrality Scores of Network Positions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `evcent()` was renamed to `eigen_centrality()` to create a more
#' consistent API.
#' @inheritParams eigen_centrality
#' @keywords internal
#' @export
evcent <- function(graph , directed = FALSE , scale = TRUE , weights = NULL , options = arpack_defaults) {
   lifecycle::deprecate_soft("1.5.0", "evcent()", "eigen_centrality()")
   eigen_centrality(graph = graph, directed = directed, scale = scale, weights = weights, options = options)
}

hub_score_impl <- function(graph, scale=TRUE, weights=NULL, options=arpack_defaults) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  scale <- as.logical(scale)
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  options.tmp <- arpack_defaults; options.tmp[ names(options) ] <- options ; options <- options.tmp

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_hub_score, graph, scale, weights, options)
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res$vector) <- vertex_attr(graph, "name", V(graph))
  }
  res
}

#' Kleinberg's hub and authority centrality scores.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `hub.score()` was renamed to `hub_score()` to create a more
#' consistent API.
#' @inheritParams hub_score
#' @keywords internal
#' @export
hub.score <- function(graph , scale = TRUE , weights = NULL , options = arpack_defaults) {
   lifecycle::deprecate_soft("1.5.0", "hub.score()", "hub_score()")
   hub_score(graph = graph, scale = scale, weights = weights, options = options)
}

authority_score_impl <- function(graph, scale=TRUE, weights=NULL, options=arpack_defaults) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  scale <- as.logical(scale)
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  options.tmp <- arpack_defaults; options.tmp[ names(options) ] <- options ; options <- options.tmp

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_authority_score, graph, scale, weights, options)
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res$vector) <- vertex_attr(graph, "name", V(graph))
  }
  res
}

#' Kleinberg's hub and authority centrality scores.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `authority.score()` was renamed to `hub_score()` to create a more
#' consistent API.
#' @inheritParams hub_score
#' @keywords internal
#' @export
authority.score <- function(graph , scale = TRUE , weights = NULL , options = arpack_defaults) {
   lifecycle::deprecate_soft("1.5.0", "authority.score()", "hub_score()")
   hub_score(graph = graph, scale = scale, weights = weights, options = options)
}

which_mutual_impl <- function(graph, eids=E(graph)) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  eids <- as.igraph.es(graph, eids)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_mutual, graph, eids-1)

  res
}

#' Find mutual edges in a directed graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `is.mutual()` was renamed to `which_mutual()` to create a more
#' consistent API.
#' @inheritParams which_mutual
#' @keywords internal
#' @export
is.mutual <- function(graph , eids = E(graph)) {
   lifecycle::deprecate_soft("1.5.0", "is.mutual()", "which_mutual()")
   which_mutual(graph = graph, eids = eids)
}

max_cardinality_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_maximum_cardinality_search, graph)
  if (igraph_opt("return.vs.es")) {
    res$alpham1 <- create_vs(graph, res$alpham1)
  }
  res
}

#' Maximum cardinality search
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `maximum.cardinality.search()` was renamed to `max_cardinality()` to create a more
#' consistent API.
#' @inheritParams max_cardinality
#' @keywords internal
#' @export
maximum.cardinality.search <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "maximum.cardinality.search()", "max_cardinality()")
   max_cardinality(graph = graph)
}

knn_impl <- function(graph, vids=V(graph), mode=c("all", "out", "in", "total"), neighbor.degree.mode=c("all", "out", "in", "total"), weights=NULL) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  neighbor.degree.mode <- switch(igraph.match.arg(neighbor.degree.mode), "out"=1, "in"=2, "all"=3, "total"=3)
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_avg_nearest_neighbor_degree, graph, vids-1, mode, neighbor.degree.mode, weights)
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res$knn) <- vertex_attr(graph, "name", vids)
  }
  res
}

#' Average nearest neighbor degree
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `graph.knn()` was renamed to `knn()` to create a more
#' consistent API.
#' @inheritParams knn
#' @keywords internal
#' @export
graph.knn <- function(graph , vids = V(graph) , mode = c("all","out","in","total") , neighbor.degree.mode = c("all","out","in","total") , weights = NULL) {
   lifecycle::deprecate_soft("1.5.0", "graph.knn()", "knn()")
   knn(graph = graph, vids = vids, mode = mode, neighbor.degree.mode = neighbor.degree.mode, weights = weights)
}

strength_impl <- function(graph, vids=V(graph), mode=c("all", "out", "in", "total"), loops=TRUE, weights=NULL) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  loops <- as.logical(loops)
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_strength, graph, vids-1, mode, loops, weights)
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res) <- vertex_attr(graph, "name", vids)
  }
  res
}

#' Strength or weighted vertex degree
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `graph.strength()` was renamed to `strength()` to create a more
#' consistent API.
#' @inheritParams strength
#' @keywords internal
#' @export
graph.strength <- function(graph , vids = V(graph) , mode = c("all","out","in","total") , loops = TRUE , weights = NULL) {
   lifecycle::deprecate_soft("1.5.0", "graph.strength()", "strength()")
   strength(graph = graph, vids = vids, mode = mode, loops = loops, weights = weights)
}

centralize_impl <- function(scores, theoretical.max=0, normalized=TRUE) {
  # Argument checks
  scores <- as.numeric(scores)
  theoretical.max <- as.numeric(theoretical.max)
  normalized <- as.logical(normalized)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_centralization, scores, theoretical.max, normalized)


  res
}

#' Centralization of a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `centralize.scores()` was renamed to `centralize()` to create a more
#' consistent API.
#' @inheritParams centralize
#' @keywords internal
#' @export
centralize.scores <- function(scores , theoretical.max = 0 , normalized = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "centralize.scores()", "centralize()")
   centralize(scores = scores, theoretical.max = theoretical.max, normalized = normalized)
}

centr_degree_impl <- function(graph, mode=c("all", "out", "in", "total"), loops=TRUE, normalized=TRUE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  loops <- as.logical(loops)
  normalized <- as.logical(normalized)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_centralization_degree, graph, mode, loops, normalized)

  res
}

#' Centralize a graph according to the degrees of vertices
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `centralization.degree()` was renamed to `centr_degree()` to create a more
#' consistent API.
#' @inheritParams centr_degree
#' @keywords internal
#' @export
centralization.degree <- function(graph , mode = c("all","out","in","total") , loops = TRUE , normalized = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "centralization.degree()", "centr_degree()")
   centr_degree(graph = graph, mode = mode, loops = loops, normalized = normalized)
}

centr_betw_impl <- function(graph, directed=TRUE, normalized=TRUE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  directed <- as.logical(directed)
  normalized <- as.logical(normalized)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_centralization_betweenness, graph, directed, normalized)

  res
}

centr_betw_tmax_impl <- function(graph=NULL, nodes=0, directed=TRUE) {
  # Argument checks
  if (!is.null(graph) && !is_igraph(graph)) { stop("Not a graph object") }
  nodes <- as.integer(nodes)
  directed <- as.logical(directed)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_centralization_betweenness_tmax, graph, nodes, directed)

  res
}

#' Theoretical maximum for betweenness centralization
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `centralization.betweenness.tmax()` was renamed to `centr_betw_tmax()` to create a more
#' consistent API.
#' @inheritParams centr_betw_tmax
#' @keywords internal
#' @export
centralization.betweenness.tmax <- function(graph = NULL , nodes = 0 , directed = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "centralization.betweenness.tmax()", "centr_betw_tmax()")
   centr_betw_tmax(graph = graph, nodes = nodes, directed = directed)
}

centr_clo_impl <- function(graph, mode=c("out", "in", "all", "total"), normalized=TRUE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  normalized <- as.logical(normalized)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_centralization_closeness, graph, mode, normalized)

  res
}

#' Centralize a graph according to the closeness of vertices
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `centralization.closeness()` was renamed to `centr_clo()` to create a more
#' consistent API.
#' @inheritParams centr_clo
#' @keywords internal
#' @export
centralization.closeness <- function(graph , mode = c("out","in","all","total") , normalized = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "centralization.closeness()", "centr_clo()")
   centr_clo(graph = graph, mode = mode, normalized = normalized)
}

centr_clo_tmax_impl <- function(graph=NULL, nodes=0, mode=c("out", "in", "all", "total")) {
  # Argument checks
  if (!is.null(graph) && !is_igraph(graph)) { stop("Not a graph object") }
  nodes <- as.integer(nodes)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_centralization_closeness_tmax, graph, nodes, mode)

  res
}

#' Theoretical maximum for closeness centralization
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `centralization.closeness.tmax()` was renamed to `centr_clo_tmax()` to create a more
#' consistent API.
#' @inheritParams centr_clo_tmax
#' @keywords internal
#' @export
centralization.closeness.tmax <- function(graph = NULL , nodes = 0 , mode = c("out","in","all","total")) {
   lifecycle::deprecate_soft("1.5.0", "centralization.closeness.tmax()", "centr_clo_tmax()")
   centr_clo_tmax(graph = graph, nodes = nodes, mode = mode)
}

centr_eigen_impl <- function(graph, directed=FALSE, scale=TRUE, options=arpack_defaults, normalized=TRUE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  directed <- as.logical(directed)
  scale <- as.logical(scale)
  options.tmp <- arpack_defaults; options.tmp[ names(options) ] <- options ; options <- options.tmp
  normalized <- as.logical(normalized)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_centralization_eigenvector_centrality, graph, directed, scale, options, normalized)

  res
}

#' Centralize a graph according to the eigenvector centrality of vertices
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `centralization.evcent()` was renamed to `centr_eigen()` to create a more
#' consistent API.
#' @inheritParams centr_eigen
#' @keywords internal
#' @export
centralization.evcent <- function(graph , directed = FALSE , scale = TRUE , options = arpack_defaults , normalized = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "centralization.evcent()", "centr_eigen()")
   centr_eigen(graph = graph, directed = directed, scale = scale, options = options, normalized = normalized)
}

centr_eigen_tmax_impl <- function(graph=NULL, nodes=0, directed=FALSE, scale=TRUE) {
  # Argument checks
  if (!is.null(graph) && !is_igraph(graph)) { stop("Not a graph object") }
  nodes <- as.integer(nodes)
  directed <- as.logical(directed)
  scale <- as.logical(scale)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_centralization_eigenvector_centrality_tmax, graph, nodes, directed, scale)

  res
}

#' Theoretical maximum for betweenness centralization
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `centralization.evcent.tmax()` was renamed to `centr_eigen_tmax()` to create a more
#' consistent API.
#' @inheritParams centr_eigen_tmax
#' @keywords internal
#' @export
centralization.evcent.tmax <- function(graph = NULL , nodes = 0 , directed = FALSE , scale = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "centralization.evcent.tmax()", "centr_eigen_tmax()")
   centr_eigen_tmax(graph = graph, nodes = nodes, directed = directed, scale = scale)
}

assortativity_nominal_impl <- function(graph, types, directed=TRUE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  types <- as.numeric(types)-1
  directed <- as.logical(directed)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_assortativity_nominal, graph, types, directed)

  res
}

#' Assortativity coefficient
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `assortativity.nominal()` was renamed to `assortativity()` to create a more
#' consistent API.
#' @inheritParams assortativity
#' @keywords internal
#' @export
assortativity.nominal <- function(graph , types , directed = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "assortativity.nominal()", "assortativity()")
   assortativity(graph = graph, types = types, directed = directed)
}

assortativity_impl <- function(graph, types1, types2=NULL, directed=TRUE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  types1 <- as.numeric(types1)
  if (!is.null(types2)) types2 <- as.numeric(types2)
  directed <- as.logical(directed)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_assortativity, graph, types1, types2, directed)

  res
}

assortativity_degree_impl <- function(graph, directed=TRUE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  directed <- as.logical(directed)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_assortativity_degree, graph, directed)

  res
}

#' Assortativity coefficient
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `assortativity.degree()` was renamed to `assortativity()` to create a more
#' consistent API.
#' @inheritParams assortativity
#' @keywords internal
#' @export
assortativity.degree <- function(graph , directed = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "assortativity.degree()", "assortativity()")
   assortativity(graph = graph, directed = directed)
}

contract_impl <- function(graph, mapping, vertex.attr.comb=igraph_opt("vertex.attr.comb")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  mapping <- as.numeric(mapping)-1
  vertex.attr.comb <- igraph.i.attribute.combination(vertex.attr.comb)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_contract_vertices, graph, mapping, vertex.attr.comb)

  res
}

#' Contract several vertices into a single one
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `contract.vertices()` was renamed to `contract()` to create a more
#' consistent API.
#' @inheritParams contract
#' @keywords internal
#' @export
contract.vertices <- function(graph , mapping , vertex.attr.comb = igraph_opt("vertex.attr.comb")) {
   lifecycle::deprecate_soft("1.5.0", "contract.vertices()", "contract()")
   contract(graph = graph, mapping = mapping, vertex.attr.comb = vertex.attr.comb)
}

eccentricity_impl <- function(graph, vids=V(graph), mode=c("all", "out", "in", "total")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_eccentricity, graph, vids-1, mode)
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res) <- vertex_attr(graph, "name", vids)
  }
  res
}

radius_impl <- function(graph, mode=c("all", "out", "in", "total")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_radius, graph, mode)

  res
}

diversity_impl <- function(graph, weights=NULL, vids=V(graph)) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  vids <- as.igraph.vs(graph, vids)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_diversity, graph, weights, vids-1)
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res) <- vertex_attr(graph, "name", vids)
  }
  res
}

#' Graph diversity
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `graph.diversity()` was renamed to `diversity()` to create a more
#' consistent API.
#' @inheritParams diversity
#' @keywords internal
#' @export
graph.diversity <- function(graph , weights = NULL , vids = V(graph)) {
   lifecycle::deprecate_soft("1.5.0", "graph.diversity()", "diversity()")
   diversity(graph = graph, weights = weights, vids = vids)
}

random_walk_impl <- function(graph, start, steps, mode=c("out", "in", "all", "total"), stuck=c("return", "error")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  start <- as.igraph.vs(graph, start)
  if (length(start) == 0) {
    stop("No vertex was specified")
  }
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  steps <- as.integer(steps)
  stuck <- switch(igraph.match.arg(stuck), "error" = 0L, "return" = 1L)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_random_walk, graph, start-1, mode, steps, stuck)
  if (igraph_opt("return.vs.es")) {
    res <- create_vs(graph, res)
  }
  res
}

random_edge_walk_impl <- function(graph, start, steps, weights=NULL, mode=c("out", "in", "all", "total"), stuck=c("return", "error")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  start <- as.igraph.vs(graph, start)
  if (length(start) == 0) {
    stop("No vertex was specified")
  }
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  steps <- as.integer(steps)
  stuck <- switch(igraph.match.arg(stuck), "error" = 0L, "return" = 1L)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_random_edge_walk, graph, weights, start-1, mode, steps, stuck)
  if (igraph_opt("return.vs.es")) {
    res <- create_es(graph, res)
  }
  res
}

global_efficiency_impl <- function(graph, weights=NULL, directed=TRUE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  directed <- as.logical(directed)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_global_efficiency, graph, weights, directed)

  res
}

local_efficiency_impl <- function(graph, vids=V(graph), weights=NULL, directed=TRUE, mode=c("all", "out", "in", "total")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  directed <- as.logical(directed)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_local_efficiency, graph, vids-1, weights, directed, mode)
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res) <- vertex_attr(graph, "name", vids)
  }
  res
}

average_local_efficiency_impl <- function(graph, weights=NULL, directed=TRUE, mode=c("all", "out", "in", "total")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  directed <- as.logical(directed)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_average_local_efficiency, graph, weights, directed, mode)

  res
}

is_graphical_impl <- function(out.deg, in.deg=NULL, allowed.edge.types=c("simple", "loops", "multi", "all")) {
  # Argument checks
  out.deg <- as.numeric(out.deg)
  if (!is.null(in.deg)) in.deg <- as.numeric(in.deg)
  allowed.edge.types <- switch(igraph.match.arg(allowed.edge.types),
    "simple"=0L, "loop"=1L, "loops"=1L, "multi"=6L, "multiple"=6L, "all"=7L)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_graphical, out.deg, in.deg, allowed.edge.types)

  res
}

#' Is a degree sequence graphical?
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `is.graphical.degree.sequence()` was renamed to `is_graphical()` to create a more
#' consistent API.
#' @inheritParams is_graphical
#' @keywords internal
#' @export
is.graphical.degree.sequence <- function(out.deg , in.deg = NULL , allowed.edge.types = c("simple","loops","multi","all")) {
   lifecycle::deprecate_soft("1.5.0", "is.graphical.degree.sequence()", "is_graphical()")
   is_graphical(out.deg = out.deg, in.deg = in.deg, allowed.edge.types = allowed.edge.types)
}

bipartite_projection_size_impl <- function(graph, types=NULL) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  types <- handle_vertex_type_arg(types, graph)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_bipartite_projection_size, graph, types)

  res
}

#' Project a bipartite graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `bipartite.projection.size()` was renamed to `bipartite_projection()` to create a more
#' consistent API.
#' @inheritParams bipartite_projection
#' @keywords internal
#' @export
bipartite.projection.size <- function(graph , types = NULL) {
   lifecycle::deprecate_soft("1.5.0", "bipartite.projection.size()", "bipartite_projection()")
   bipartite_projection(graph = graph, types = types)
}

bipartite_mapping_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_bipartite, graph)

  res
}

#' Decide whether a graph is bipartite
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `bipartite.mapping()` was renamed to `bipartite_mapping()` to create a more
#' consistent API.
#' @inheritParams bipartite_mapping
#' @keywords internal
#' @export
bipartite.mapping <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "bipartite.mapping()", "bipartite_mapping()")
   bipartite_mapping(graph = graph)
}

is_connected_impl <- function(graph, mode=c("weak", "strong")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  mode <- switch(igraph.match.arg(mode), "weak"=1, "strong"=2)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_connected, graph, mode)

  res
}

#' Connected components of a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `is.connected()` was renamed to `component_distribution()` to create a more
#' consistent API.
#' @inheritParams component_distribution
#' @keywords internal
#' @export
is.connected <- function(graph , mode = c("weak","strong")) {
   lifecycle::deprecate_soft("1.5.0", "is.connected()", "component_distribution()")
   component_distribution(graph = graph, mode = mode)
}

articulation_points_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_articulation_points, graph)
  if (igraph_opt("return.vs.es")) {
    res <- create_vs(graph, res)
  }
  res
}

#' Articulation points and bridges of a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `articulation.points()` was renamed to `articulation_points()` to create a more
#' consistent API.
#' @inheritParams articulation_points
#' @keywords internal
#' @export
articulation.points <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "articulation.points()", "articulation_points()")
   articulation_points(graph = graph)
}

biconnected_components_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_biconnected_components, graph)
  if (igraph_opt("return.vs.es")) {
    es <- E(graph)
    for (i_ in seq_along(res$tree.edges)) {
      res$tree.edges[[i_]] <- unsafe_create_es(graph, res$tree.edges[[i_]], es = es)
    }
  }
  if (igraph_opt("return.vs.es")) {
    es <- E(graph)
    for (i_ in seq_along(res$component.edges)) {
      res$component.edges[[i_]] <- unsafe_create_es(graph, res$component.edges[[i_]], es = es)
    }
  }
  if (igraph_opt("return.vs.es")) {
    verts <- V(graph)
    for (i_ in seq_along(res$components)) {
      res$components[[i_]] <- unsafe_create_vs(graph, res$components[[i_]], verts = verts)
    }
  }
  if (igraph_opt("return.vs.es")) {
    res$articulation.points <- create_vs(graph, res$articulation.points)
  }
  res
}

#' Biconnected components
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `biconnected.components()` was renamed to `biconnected_components()` to create a more
#' consistent API.
#' @inheritParams biconnected_components
#' @keywords internal
#' @export
biconnected.components <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "biconnected.components()", "biconnected_components()")
   biconnected_components(graph = graph)
}

bridges_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_bridges, graph)
  if (igraph_opt("return.vs.es")) {
    res <- create_es(graph, res)
  }
  res
}

cliques_impl <- function(graph, min=0, max=0) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  min <- as.integer(min)
  max <- as.integer(max)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_cliques, graph, min, max)
  if (igraph_opt("return.vs.es")) {
    verts <- V(graph)
    for (i_ in seq_along(res)) {
      res[[i_]] <- unsafe_create_vs(graph, res[[i_]], verts = verts)
    }
  }
  res
}

all_clique_size_counts_impl <- function(graph, min.size=0, max.size=0) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  min.size <- as.integer(min.size)
  max.size <- as.integer(max.size)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_clique_size_hist, graph, min.size, max.size)

  res
}

largest_cliques_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_largest_cliques, graph)
  if (igraph_opt("return.vs.es")) {
    verts <- V(graph)
    for (i_ in seq_along(res)) {
      res[[i_]] <- unsafe_create_vs(graph, res[[i_]], verts = verts)
    }
  }
  res
}

#' Functions to find cliques, i.e. complete subgraphs in a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `largest.cliques()` was renamed to `cliques()` to create a more
#' consistent API.
#' @inheritParams cliques
#' @keywords internal
#' @export
largest.cliques <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "largest.cliques()", "cliques()")
   cliques(graph = graph)
}

maximal_clique_size_counts_impl <- function(graph, min.size=0, max.size=0) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  min.size <- as.integer(min.size)
  max.size <- as.integer(max.size)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_maximal_cliques_hist, graph, min.size, max.size)

  res
}

clique_num_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_clique_number, graph)

  res
}

#' Functions to find cliques, i.e. complete subgraphs in a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `clique.number()` was renamed to `cliques()` to create a more
#' consistent API.
#' @inheritParams cliques
#' @keywords internal
#' @export
clique.number <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "clique.number()", "cliques()")
   cliques(graph = graph)
}

weighted_cliques_impl <- function(graph, vertex.weights=NULL, min.weight=0, max.weight=0, maximal=FALSE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (is.null(vertex.weights) && "weight" %in% vertex_attr_names(graph)) {
    vertex.weights <- V(graph)$weight
  }
  if (!is.null(vertex.weights) && any(!is.na(vertex.weights))) {
    vertex.weights <- as.numeric(vertex.weights)
  } else {
    vertex.weights <- NULL
  }
  min.weight <- as.numeric(min.weight)
  max.weight <- as.numeric(max.weight)
  maximal <- as.logical(maximal)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_weighted_cliques, graph, vertex.weights, min.weight, max.weight, maximal)
  if (igraph_opt("return.vs.es")) {
    verts <- V(graph)
    for (i_ in seq_along(res)) {
      res[[i_]] <- unsafe_create_vs(graph, res[[i_]], verts = verts)
    }
  }
  res
}

largest_weighted_cliques_impl <- function(graph, vertex.weights=NULL) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (is.null(vertex.weights) && "weight" %in% vertex_attr_names(graph)) {
    vertex.weights <- V(graph)$weight
  }
  if (!is.null(vertex.weights) && any(!is.na(vertex.weights))) {
    vertex.weights <- as.numeric(vertex.weights)
  } else {
    vertex.weights <- NULL
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_largest_weighted_cliques, graph, vertex.weights)
  if (igraph_opt("return.vs.es")) {
    verts <- V(graph)
    for (i_ in seq_along(res)) {
      res[[i_]] <- unsafe_create_vs(graph, res[[i_]], verts = verts)
    }
  }
  res
}

weighted_clique_num_impl <- function(graph, vertex.weights=NULL) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (is.null(vertex.weights) && "weight" %in% vertex_attr_names(graph)) {
    vertex.weights <- V(graph)$weight
  }
  if (!is.null(vertex.weights) && any(!is.na(vertex.weights))) {
    vertex.weights <- as.numeric(vertex.weights)
  } else {
    vertex.weights <- NULL
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_weighted_clique_number, graph, vertex.weights)

  res
}

similarity.jaccard_impl <- function(graph, vids=V(graph), mode=c("all", "out", "in", "total"), loops=FALSE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  loops <- as.logical(loops)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_similarity_jaccard, graph, vids-1, mode, loops)

  res
}

similarity.dice_impl <- function(graph, vids=V(graph), mode=c("all", "out", "in", "total"), loops=FALSE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  loops <- as.logical(loops)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_similarity_dice, graph, vids-1, mode, loops)

  res
}

similarity.invlogweighted_impl <- function(graph, vids=V(graph), mode=c("all", "out", "in", "total")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_similarity_inverse_log_weighted, graph, vids-1, mode)

  res
}

sample_hrg_impl <- function(hrg) {
  # Argument checks
  if (is.null(hrg)) {
    hrg <- list(left=c(), right=c(), prob=c(), edges=c(), vertices=c())
  }
  hrg <- lapply(hrg[c("left","right","prob","edges","vertices")], as.numeric)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_hrg_game, hrg)

  if (igraph_opt("add.params")) {
    res$name <- 'Hierarchical random graph model'
  }

  res
}

#' Sample from a hierarchical random graph model
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `hrg.game()` was renamed to `sample_hrg()` to create a more
#' consistent API.
#' @inheritParams sample_hrg
#' @keywords internal
#' @export
hrg.game <- function(hrg) {
   lifecycle::deprecate_soft("1.5.0", "hrg.game()", "sample_hrg()")
   sample_hrg(hrg = hrg)
}

hrg_tree_impl <- function(hrg) {
  # Argument checks
  if (is.null(hrg)) {
    hrg <- list(left=c(), right=c(), prob=c(), edges=c(), vertices=c())
  }
  hrg <- lapply(hrg[c("left","right","prob","edges","vertices")], as.numeric)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_hrg_dendrogram, hrg)

  res
}

#' Create an igraph graph from a hierarchical random graph model
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `hrg.dendrogram()` was renamed to `hrg_tree()` to create a more
#' consistent API.
#' @inheritParams hrg_tree
#' @keywords internal
#' @export
hrg.dendrogram <- function(hrg) {
   lifecycle::deprecate_soft("1.5.0", "hrg.dendrogram()", "hrg_tree()")
   hrg_tree(hrg = hrg)
}

consensus_tree_impl <- function(graph, hrg=NULL, start=FALSE, num.samples=10000) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (is.null(hrg)) {
    hrg <- list(left=c(), right=c(), prob=c(), edges=c(), vertices=c())
  }
  hrg <- lapply(hrg[c("left","right","prob","edges","vertices")], as.numeric)
  start <- as.logical(start)
  num.samples <- as.integer(num.samples)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_hrg_consensus, graph, hrg, start, num.samples)

  res
}

#' Create a consensus tree from several hierarchical random graph models
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `hrg.consensus()` was renamed to `consensus_tree()` to create a more
#' consistent API.
#' @inheritParams consensus_tree
#' @keywords internal
#' @export
hrg.consensus <- function(graph , hrg = NULL , start = FALSE , num.samples = 10000) {
   lifecycle::deprecate_soft("1.5.0", "hrg.consensus()", "consensus_tree()")
   consensus_tree(graph = graph, hrg = hrg, start = start, num.samples = num.samples)
}

hrg_impl <- function(graph, prob) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  prob <- as.numeric(prob)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_hrg_create, graph, prob)

  class(res) <- "igraphHRG"
  res
}

#' Create a hierarchical random graph from an igraph graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `hrg.create()` was renamed to `hrg()` to create a more
#' consistent API.
#' @inheritParams hrg
#' @keywords internal
#' @export
hrg.create <- function(graph , prob) {
   lifecycle::deprecate_soft("1.5.0", "hrg.create()", "hrg()")
   hrg(graph = graph, prob = prob)
}

graphlets_impl <- function(graph, weights=NULL, niter=1000) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  niter <- as.integer(niter)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_graphlets, graph, weights, niter)
  if (igraph_opt("return.vs.es")) {
    verts <- V(graph)
    for (i_ in seq_along(res$cliques)) {
      res$cliques[[i_]] <- unsafe_create_vs(graph, res$cliques[[i_]], verts = verts)
    }
  }
  res
}

as.directed_impl <- function(graph, mode=c("mutual", "arbitrary", "random", "acyclic")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  mode <- switch(igraph.match.arg(mode), "arbitrary"=0, "mutual"=1, "random"=2, "acyclic"=3)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_to_directed, graph, mode)

  res
}

dyad_census_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_dyad_census, graph)

  res
}

#' Dyad census of a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `dyad.census()` was renamed to `dyad_census()` to create a more
#' consistent API.
#' @inheritParams dyad_census
#' @keywords internal
#' @export
dyad.census <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "dyad.census()", "dyad_census()")
   dyad_census(graph = graph)
}

triad_census_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_triad_census, graph)

  res
}

#' Triad census, subgraphs with three vertices
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `triad.census()` was renamed to `triad_census()` to create a more
#' consistent API.
#' @inheritParams triad_census
#' @keywords internal
#' @export
triad.census <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "triad.census()", "triad_census()")
   triad_census(graph = graph)
}

count_triangles_impl <- function(graph, vids=V(graph)) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_adjacent_triangles, graph, vids-1)

  res
}

#' Find triangles in graphs
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `adjacent.triangles()` was renamed to `triangles()` to create a more
#' consistent API.
#' @inheritParams triangles
#' @keywords internal
#' @export
adjacent.triangles <- function(graph , vids = V(graph)) {
   lifecycle::deprecate_soft("1.5.0", "adjacent.triangles()", "triangles()")
   triangles(graph = graph, vids = vids)
}

triangles_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_list_triangles, graph)
  if (igraph_opt("return.vs.es")) {
    res <- create_vs(graph, res)
  }
  res
}

max_flow_impl <- function(graph, source, target, capacity=NULL) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  source <- as.igraph.vs(graph, source)
  if (length(source) == 0) {
    stop("No vertex was specified")
  }
  target <- as.igraph.vs(graph, target)
  if (length(target) == 0) {
    stop("No vertex was specified")
  }
  if (is.null(capacity) && "capacity" %in% edge_attr_names(graph)) {
    capacity <- E(graph)$capacity
  }
  if (!is.null(capacity) && any(!is.na(capacity))) {
    capacity <- as.numeric(capacity)
  } else {
    capacity <- NULL
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_maxflow, graph, source-1, target-1, capacity)
  if (igraph_opt("return.vs.es")) {
    res$partition1 <- create_vs(graph, res$partition1)
  }
  if (igraph_opt("return.vs.es")) {
    res$partition2 <- create_vs(graph, res$partition2)
  }
  res
}

#' Maximum flow in a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `graph.maxflow()` was renamed to `max_flow()` to create a more
#' consistent API.
#' @inheritParams max_flow
#' @keywords internal
#' @export
graph.maxflow <- function(graph , source , target , capacity = NULL) {
   lifecycle::deprecate_soft("1.5.0", "graph.maxflow()", "max_flow()")
   max_flow(graph = graph, source = source, target = target, capacity = capacity)
}

dominator_tree_impl <- function(graph, root, mode=c("out", "in", "all", "total")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  root <- as.igraph.vs(graph, root)
  if (length(root) == 0) {
    stop("No vertex was specified")
  }
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_dominator_tree, graph, root-1, mode)
  if (igraph_opt("return.vs.es")) {
    res$leftout <- create_vs(graph, res$leftout)
  }
  res
}

st_cuts_impl <- function(graph, source, target) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  source <- as.igraph.vs(graph, source)
  if (length(source) == 0) {
    stop("No vertex was specified")
  }
  target <- as.igraph.vs(graph, target)
  if (length(target) == 0) {
    stop("No vertex was specified")
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_all_st_cuts, graph, source-1, target-1)
  if (igraph_opt("return.vs.es")) {
    es <- E(graph)
    for (i_ in seq_along(res$cuts)) {
      res$cuts[[i_]] <- unsafe_create_es(graph, res$cuts[[i_]], es = es)
    }
  }
  if (igraph_opt("return.vs.es")) {
    verts <- V(graph)
    for (i_ in seq_along(res$partition1s)) {
      res$partition1s[[i_]] <- unsafe_create_vs(graph, res$partition1s[[i_]], verts = verts)
    }
  }
  res
}

#' List all (s,t)-cuts of a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `stCuts()` was renamed to `st_cuts()` to create a more
#' consistent API.
#' @inheritParams st_cuts
#' @keywords internal
#' @export
stCuts <- function(graph , source , target) {
   lifecycle::deprecate_soft("1.5.0", "stCuts()", "st_cuts()")
   st_cuts(graph = graph, source = source, target = target)
}

st_min_cuts_impl <- function(graph, source, target, capacity=NULL) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  source <- as.igraph.vs(graph, source)
  if (length(source) == 0) {
    stop("No vertex was specified")
  }
  target <- as.igraph.vs(graph, target)
  if (length(target) == 0) {
    stop("No vertex was specified")
  }
  if (is.null(capacity) && "weight" %in% edge_attr_names(graph)) {
    capacity <- E(graph)$weight
  }
  if (!is.null(capacity) && any(!is.na(capacity))) {
    capacity <- as.numeric(capacity)
  } else {
    capacity <- NULL
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_all_st_mincuts, graph, source-1, target-1, capacity)
  if (igraph_opt("return.vs.es")) {
    es <- E(graph)
    for (i_ in seq_along(res$cuts)) {
      res$cuts[[i_]] <- unsafe_create_es(graph, res$cuts[[i_]], es = es)
    }
  }
  if (igraph_opt("return.vs.es")) {
    verts <- V(graph)
    for (i_ in seq_along(res$partition1s)) {
      res$partition1s[[i_]] <- unsafe_create_vs(graph, res$partition1s[[i_]], verts = verts)
    }
  }
  res
}

#' List all minimum \((s,t)\)-cuts of a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `stMincuts()` was renamed to `st_min_cuts()` to create a more
#' consistent API.
#' @inheritParams st_min_cuts
#' @keywords internal
#' @export
stMincuts <- function(graph , source , target , capacity = NULL) {
   lifecycle::deprecate_soft("1.5.0", "stMincuts()", "st_min_cuts()")
   st_min_cuts(graph = graph, source = source, target = target, capacity = capacity)
}

is_separator_impl <- function(graph, candidate) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  candidate <- as.igraph.vs(graph, candidate)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_separator, graph, candidate-1)

  res
}

#' Vertex separators
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `is.separator()` was renamed to `is_separator()` to create a more
#' consistent API.
#' @inheritParams is_separator
#' @keywords internal
#' @export
is.separator <- function(graph , candidate) {
   lifecycle::deprecate_soft("1.5.0", "is.separator()", "is_separator()")
   is_separator(graph = graph, candidate = candidate)
}

is_min_separator_impl <- function(graph, candidate) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  candidate <- as.igraph.vs(graph, candidate)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_minimal_separator, graph, candidate-1)

  res
}

#' Minimal vertex separators
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `is.minimal.separator()` was renamed to `is_min_separator()` to create a more
#' consistent API.
#' @inheritParams is_min_separator
#' @keywords internal
#' @export
is.minimal.separator <- function(graph , candidate) {
   lifecycle::deprecate_soft("1.5.0", "is.minimal.separator()", "is_min_separator()")
   is_min_separator(graph = graph, candidate = candidate)
}

min_st_separators_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_all_minimal_st_separators, graph)
  if (igraph_opt("return.vs.es")) {
    verts <- V(graph)
    for (i_ in seq_along(res)) {
      res[[i_]] <- unsafe_create_vs(graph, res[[i_]], verts = verts)
    }
  }
  res
}

#' Minimum size vertex separators
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `minimal.st.separators()` was renamed to `min_st_separators()` to create a more
#' consistent API.
#' @inheritParams min_st_separators
#' @keywords internal
#' @export
minimal.st.separators <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "minimal.st.separators()", "min_st_separators()")
   min_st_separators(graph = graph)
}

min_separators_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_minimum_size_separators, graph)
  if (igraph_opt("return.vs.es")) {
    verts <- V(graph)
    for (i_ in seq_along(res)) {
      res[[i_]] <- unsafe_create_vs(graph, res[[i_]], verts = verts)
    }
  }
  res
}

#' Minimum size vertex separators
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `minimum.size.separators()` was renamed to `min_separators()` to create a more
#' consistent API.
#' @inheritParams min_separators
#' @keywords internal
#' @export
minimum.size.separators <- function(graph) {
   lifecycle::deprecate_soft("1.5.0", "minimum.size.separators()", "min_separators()")
   min_separators(graph = graph)
}

graph.isoclass_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_isoclass, graph)

  res
}

graph.isomorphic_impl <- function(graph1, graph2) {
  # Argument checks
  if (!is_igraph(graph1)) { stop("Not a graph object") }
  if (!is_igraph(graph2)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_isomorphic, graph1, graph2)

  res
}

graph_from_isomorphism_class_impl <- function(size, number, directed=TRUE) {
  # Argument checks
  size <- as.integer(size)
  number <- as.integer(number)
  directed <- as.logical(directed)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_isoclass_create, size, number, directed)

  res
}

#' Create a graph from an isomorphism class
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `graph.isocreate()` was renamed to `graph_from_isomorphism_class()` to create a more
#' consistent API.
#' @inheritParams graph_from_isomorphism_class
#' @keywords internal
#' @export
graph.isocreate <- function(size , number , directed = TRUE) {
   lifecycle::deprecate_soft("1.5.0", "graph.isocreate()", "graph_from_isomorphism_class()")
   graph_from_isomorphism_class(size = size, number = number, directed = directed)
}

graph.isomorphic.vf2_impl <- function(graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2) {
  # Argument checks
  if (!is_igraph(graph1)) { stop("Not a graph object") }
  if (!is_igraph(graph2)) { stop("Not a graph object") }
  if (missing(vertex.color1)) {
    if ("color" %in% vertex_attr_names(graph1)) {
      vertex.color1 <- V(graph1)$color
    } else {
      vertex.color1 <- NULL
    }
  }
  if (!is.null(vertex.color1)) {
    vertex.color1 <- as.integer(vertex.color1)-1L
  }
  if (missing(vertex.color2)) {
    if ("color" %in% vertex_attr_names(graph2)) {
      vertex.color2 <- V(graph2)$color
    } else {
      vertex.color2 <- NULL
    }
  }
  if (!is.null(vertex.color2)) {
    vertex.color2 <- as.integer(vertex.color2)-1L
  }
  if (missing(edge.color1)) {
    if ("color" %in% edge_attr_names(graph1)) {
      edge.color1 <- E(graph1)$color
    } else {
      edge.color1 <- NULL
    }
  }
  if (!is.null(edge.color1)) {
    edge.color1 <- as.integer(edge.color1)-1L
  }
  if (missing(edge.color2)) {
    if ("color" %in% edge_attr_names(graph2)) {
      edge.color2 <- E(graph2)$color
    } else {
      edge.color2 <- NULL
    }
  }
  if (!is.null(edge.color2)) {
    edge.color2 <- as.integer(edge.color2)-1L
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_isomorphic_vf2, graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2)

  res
}

graph.count.isomorphisms.vf2_impl <- function(graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2) {
  # Argument checks
  if (!is_igraph(graph1)) { stop("Not a graph object") }
  if (!is_igraph(graph2)) { stop("Not a graph object") }
  if (missing(vertex.color1)) {
    if ("color" %in% vertex_attr_names(graph1)) {
      vertex.color1 <- V(graph1)$color
    } else {
      vertex.color1 <- NULL
    }
  }
  if (!is.null(vertex.color1)) {
    vertex.color1 <- as.integer(vertex.color1)-1L
  }
  if (missing(vertex.color2)) {
    if ("color" %in% vertex_attr_names(graph2)) {
      vertex.color2 <- V(graph2)$color
    } else {
      vertex.color2 <- NULL
    }
  }
  if (!is.null(vertex.color2)) {
    vertex.color2 <- as.integer(vertex.color2)-1L
  }
  if (missing(edge.color1)) {
    if ("color" %in% edge_attr_names(graph1)) {
      edge.color1 <- E(graph1)$color
    } else {
      edge.color1 <- NULL
    }
  }
  if (!is.null(edge.color1)) {
    edge.color1 <- as.integer(edge.color1)-1L
  }
  if (missing(edge.color2)) {
    if ("color" %in% edge_attr_names(graph2)) {
      edge.color2 <- E(graph2)$color
    } else {
      edge.color2 <- NULL
    }
  }
  if (!is.null(edge.color2)) {
    edge.color2 <- as.integer(edge.color2)-1L
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_count_isomorphisms_vf2, graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2)

  res
}

graph.subisomorphic.vf2_impl <- function(graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2) {
  # Argument checks
  if (!is_igraph(graph1)) { stop("Not a graph object") }
  if (!is_igraph(graph2)) { stop("Not a graph object") }
  if (missing(vertex.color1)) {
    if ("color" %in% vertex_attr_names(graph1)) {
      vertex.color1 <- V(graph1)$color
    } else {
      vertex.color1 <- NULL
    }
  }
  if (!is.null(vertex.color1)) {
    vertex.color1 <- as.integer(vertex.color1)-1L
  }
  if (missing(vertex.color2)) {
    if ("color" %in% vertex_attr_names(graph2)) {
      vertex.color2 <- V(graph2)$color
    } else {
      vertex.color2 <- NULL
    }
  }
  if (!is.null(vertex.color2)) {
    vertex.color2 <- as.integer(vertex.color2)-1L
  }
  if (missing(edge.color1)) {
    if ("color" %in% edge_attr_names(graph1)) {
      edge.color1 <- E(graph1)$color
    } else {
      edge.color1 <- NULL
    }
  }
  if (!is.null(edge.color1)) {
    edge.color1 <- as.integer(edge.color1)-1L
  }
  if (missing(edge.color2)) {
    if ("color" %in% edge_attr_names(graph2)) {
      edge.color2 <- E(graph2)$color
    } else {
      edge.color2 <- NULL
    }
  }
  if (!is.null(edge.color2)) {
    edge.color2 <- as.integer(edge.color2)-1L
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_subisomorphic_vf2, graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2)

  res
}

graph.count.subisomorphisms.vf2_impl <- function(graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2) {
  # Argument checks
  if (!is_igraph(graph1)) { stop("Not a graph object") }
  if (!is_igraph(graph2)) { stop("Not a graph object") }
  if (missing(vertex.color1)) {
    if ("color" %in% vertex_attr_names(graph1)) {
      vertex.color1 <- V(graph1)$color
    } else {
      vertex.color1 <- NULL
    }
  }
  if (!is.null(vertex.color1)) {
    vertex.color1 <- as.integer(vertex.color1)-1L
  }
  if (missing(vertex.color2)) {
    if ("color" %in% vertex_attr_names(graph2)) {
      vertex.color2 <- V(graph2)$color
    } else {
      vertex.color2 <- NULL
    }
  }
  if (!is.null(vertex.color2)) {
    vertex.color2 <- as.integer(vertex.color2)-1L
  }
  if (missing(edge.color1)) {
    if ("color" %in% edge_attr_names(graph1)) {
      edge.color1 <- E(graph1)$color
    } else {
      edge.color1 <- NULL
    }
  }
  if (!is.null(edge.color1)) {
    edge.color1 <- as.integer(edge.color1)-1L
  }
  if (missing(edge.color2)) {
    if ("color" %in% edge_attr_names(graph2)) {
      edge.color2 <- E(graph2)$color
    } else {
      edge.color2 <- NULL
    }
  }
  if (!is.null(edge.color2)) {
    edge.color2 <- as.integer(edge.color2)-1L
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_count_subisomorphisms_vf2, graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2)

  res
}

graph.isomorphic.34_impl <- function(graph1, graph2) {
  # Argument checks
  if (!is_igraph(graph1)) { stop("Not a graph object") }
  if (!is_igraph(graph2)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_isomorphic_34, graph1, graph2)

  res
}

canonical_permutation_impl <- function(graph, colors, sh=c("fm", "f", "fs", "fl", "flm", "fsm")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (missing(colors)) {
    if ("color" %in% vertex_attr_names(graph)) {
      colors <- V(graph)$color
    } else {
      colors <- NULL
    }
  }
  if (!is.null(colors)) {
    colors <- as.integer(colors)-1L
  }
  sh <- switch(igraph.match.arg(sh), "f"=0, "fl"=1, "fs"=2, "fm"=3, "flm"=4, "fsm"=5)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_canonical_permutation, graph, colors, sh)

  res
}

#' Canonical permutation of a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `canonical.permutation()` was renamed to `canonical_permutation()` to create a more
#' consistent API.
#' @inheritParams canonical_permutation
#' @keywords internal
#' @export
canonical.permutation <- function(graph , colors , sh = c("fm","f","fs","fl","flm","fsm")) {
   lifecycle::deprecate_soft("1.5.0", "canonical.permutation()", "canonical_permutation()")
   canonical_permutation(graph = graph, colors = colors, sh = sh)
}

permute_impl <- function(graph, permutation) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  permutation <- as.numeric(permutation)-1

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_permute_vertices, graph, permutation)

  res
}

#' Permute the vertices of a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `permute.vertices()` was renamed to `permute()` to create a more
#' consistent API.
#' @inheritParams permute
#' @keywords internal
#' @export
permute.vertices <- function(graph , permutation) {
   lifecycle::deprecate_soft("1.5.0", "permute.vertices()", "permute()")
   permute(graph = graph, permutation = permutation)
}

graph.isomorphic.bliss_impl <- function(graph1, graph2, colors1, colors2, sh=c("fm", "f", "fs", "fl", "flm", "fsm")) {
  # Argument checks
  if (!is_igraph(graph1)) { stop("Not a graph object") }
  if (!is_igraph(graph2)) { stop("Not a graph object") }
  if (missing(colors1)) {
    if ("color" %in% vertex_attr_names(graph1)) {
      colors1 <- V(graph1)$color
    } else {
      colors1 <- NULL
    }
  }
  if (!is.null(colors1)) {
    colors1 <- as.integer(colors1)-1L
  }
  if (missing(colors2)) {
    if ("color" %in% vertex_attr_names(graph2)) {
      colors2 <- V(graph2)$color
    } else {
      colors2 <- NULL
    }
  }
  if (!is.null(colors2)) {
    colors2 <- as.integer(colors2)-1L
  }
  sh <- switch(igraph.match.arg(sh), "f"=0, "fl"=1, "fs"=2, "fm"=3, "flm"=4, "fsm"=5)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_isomorphic_bliss, graph1, graph2, colors1, colors2, sh)

  res
}

count_automorphisms_impl <- function(graph, colors, sh=c("fm", "f", "fs", "fl", "flm", "fsm")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (missing(colors)) {
    if ("color" %in% vertex_attr_names(graph)) {
      colors <- V(graph)$color
    } else {
      colors <- NULL
    }
  }
  if (!is.null(colors)) {
    colors <- as.integer(colors)-1L
  }
  sh <- switch(igraph.match.arg(sh), "f"=0, "fl"=1, "fs"=2, "fm"=3, "flm"=4, "fsm"=5)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_automorphisms, graph, colors, sh)

  res
}

#' Number of automorphisms
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `graph.automorphisms()` was renamed to `count_automorphisms()` to create a more
#' consistent API.
#' @inheritParams count_automorphisms
#' @keywords internal
#' @export
graph.automorphisms <- function(graph , colors , sh = c("fm","f","fs","fl","flm","fsm")) {
   lifecycle::deprecate_soft("1.5.0", "graph.automorphisms()", "count_automorphisms()")
   count_automorphisms(graph = graph, colors = colors, sh = sh)
}

#' Number of automorphisms
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `automorphisms()` was renamed to `count_automorphisms()` to create a more
#' consistent API.
#' @inheritParams count_automorphisms
#' @keywords internal
#' @export
automorphisms <- function(graph , colors , sh = c("fm","f","fs","fl","flm","fsm")) {
   lifecycle::deprecate_soft("1.5.0", "automorphisms()", "count_automorphisms()")
   count_automorphisms(graph = graph, colors = colors, sh = sh)
}

automorphism_group_impl <- function(graph, colors, sh=c("fm", "f", "fs", "fl", "flm", "fsm"), details=FALSE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  if (missing(colors)) {
    if ("color" %in% vertex_attr_names(graph)) {
      colors <- V(graph)$color
    } else {
      colors <- NULL
    }
  }
  if (!is.null(colors)) {
    colors <- as.integer(colors)-1L
  }
  sh <- switch(igraph.match.arg(sh), "f"=0, "fl"=1, "fs"=2, "fm"=3, "flm"=4, "fsm"=5)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_automorphism_group, graph, colors, sh)
  if (igraph_opt("return.vs.es")) {
    verts <- V(graph)
    for (i_ in seq_along(res$generators)) {
      res$generators[[i_]] <- unsafe_create_vs(graph, res$generators[[i_]], verts = verts)
    }
  }
  if (!details) {
    res <- res$generators
  }
  res
}

scg_eps_impl <- function(V, groups, mtype=c("symmetric", "laplacian", "stochastic"), p=NULL, norm=c("row", "col")) {
  # Argument checks
  V <- as.matrix(structure(as.double(V), dim=dim(V)))
  groups <- as.numeric(groups)-1
  mtype <- switch(igraph.match.arg(mtype), "symmetric"=1, "laplacian"=2, "stochastic"=3)
  if (!is.null(p)) p <- as.numeric(p)
  norm <- switch(igraph.match.arg(norm), "row"=1, "col"=2)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_scg_norm_eps, V, groups, mtype, p, norm)

  res
}

#' Error of the spectral coarse graining (SCG) approximation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `scgNormEps()` was renamed to `scg_eps()` to create a more
#' consistent API.
#' @inheritParams scg_eps
#' @keywords internal
#' @export
scgNormEps <- function(V , groups , mtype = c("symmetric","laplacian","stochastic") , p = NULL , norm = c("row","col")) {
   lifecycle::deprecate_soft("1.5.0", "scgNormEps()", "scg_eps()")
   scg_eps(V = V, groups = groups, mtype = mtype, p = p, norm = norm)
}

embed_adjacency_matrix_impl <- function(graph, no, weights=NULL, which=c("lm", "la", "sa"), scaled=TRUE, cvec=graph.strength(graph, weights=weights)/(vcount(graph)-1), options=igraph.arpack.default) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  no <- as.integer(no)
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  which <- switch(igraph.match.arg(which), "lm"=0L, "la"=2L, "sa"=3L)
  scaled <- as.logical(scaled)
  cvec <- as.numeric(cvec)
  options.tmp <- arpack_defaults; options.tmp[ names(options) ] <- options ; options <- options.tmp

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_adjacency_spectral_embedding, graph, no, weights, which, scaled, cvec, options)

  res
}

embed_laplacian_matrix_impl <- function(graph, no, weights=NULL, which=c("lm", "la", "sa"), type=c("default", "D-A", "DAD", "I-DAD", "OAP"), scaled=TRUE, options=igraph.arpack.default) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  no <- as.integer(no)
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  } else {
    weights <- NULL
  }
  which <- switch(igraph.match.arg(which), "lm"=0L, "la"=2L, "sa"=3L)
  type <- switch(igraph.match.arg(type),
    "default"=if (is.directed(graph)) 3L else 0L,
    "da"=0L, "d-a"=0L, "idad"=1L, "i-dad"=1L, "dad"=2L,
    "oap"=3L)
  scaled <- as.logical(scaled)
  options.tmp <- arpack_defaults; options.tmp[ names(options) ] <- options ; options <- options.tmp

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_laplacian_spectral_embedding, graph, no, weights, which, type, scaled, options)

  res
}

spectrum_impl <- function(graph, algorithm=c("arpack", "auto", "lapack", "comp_auto", "comp_lapack", "comp_arpack"), which=list(), options=arpack_defaults) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  algorithm <- switch(igraph.match.arg(algorithm), "auto"=0, "lapack"=1,
    "arpack"=2, "comp_auto"=3, "comp_lapack"=4,
    "comp_arpack"=5)
  which.tmp <- eigen_defaults;
  which.tmp[ names(which) ] <- which ; which <- which.tmp
  options.tmp <- arpack_defaults; options.tmp[ names(options) ] <- options ; options <- options.tmp

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_eigen_adjacency, graph, algorithm, which, options)

  res
}

#' Eigenvalues and eigenvectors of the adjacency matrix of a graph
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `graph.eigen()` was renamed to `spectrum()` to create a more
#' consistent API.
#' @inheritParams spectrum
#' @keywords internal
#' @export
graph.eigen <- function(graph , algorithm = c("arpack","auto","lapack","comp_auto","comp_lapack","comp_arpack") , which = list() , options = arpack_defaults) {
   lifecycle::deprecate_soft("1.5.0", "graph.eigen()", "spectrum()")
   spectrum(graph = graph, algorithm = algorithm, which = which, options = options)
}

sir_impl <- function(graph, beta, gamma, no.sim=100) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  beta <- as.numeric(beta)
  gamma <- as.numeric(gamma)
  no.sim <- as.integer(no.sim)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_sir, graph, beta, gamma, no.sim)

  class(res) <- "sir"
  res
}

convex_hull_impl <- function(data) {
  # Argument checks
  data <- as.matrix(structure(as.double(data), dim=dim(data)))

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_convex_hull, data)

  res
}

#' Convex hull of a set of vertices
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `convex.hull()` was renamed to `convex_hull()` to create a more
#' consistent API.
#' @inheritParams convex_hull
#' @keywords internal
#' @export
convex.hull <- function(data) {
   lifecycle::deprecate_soft("1.5.0", "convex.hull()", "convex_hull()")
   convex_hull(data = data)
}

dim_select_impl <- function(sv) {
  # Argument checks
  sv <- as.numeric(sv)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_dim_select, sv)

  res
}

is_eulerian_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_eulerian, graph)

  res
}

eulerian_path_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_eulerian_path, graph)
  if (igraph_opt("return.vs.es")) {
    res$epath <- create_es(graph, res$epath)
  }
  if (igraph_opt("return.vs.es")) {
    res$vpath <- create_vs(graph, res$vpath)
  }
  res
}

eulerian_cycle_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_eulerian_cycle, graph)
  if (igraph_opt("return.vs.es")) {
    res$epath <- create_es(graph, res$epath)
  }
  if (igraph_opt("return.vs.es")) {
    res$vpath <- create_vs(graph, res$vpath)
  }
  res
}

is_tree_impl <- function(graph, mode=c("out", "in", "all", "total"), details=FALSE) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_is_tree, graph, mode)
  if (igraph_opt("return.vs.es")) {
    res$root <- create_vs(graph, res$root)
  }
  if (!details) {
    res <- res$res
  }
  res
}

make_from_prufer_impl <- function(prufer) {
  # Argument checks
  prufer <- as.integer(prufer)-1L

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_from_prufer, prufer)

  if (igraph_opt("add.params")) {
    res$name <- 'Tree from Prufer sequence'
    res$prufer <- prufer
  }

  res
}

to_prufer_impl <- function(graph) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_to_prufer, graph)
  res <- res+1
  res
}

sample_spanning_tree_impl <- function(graph, vid=0) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  vid <- as.igraph.vs(graph, vid)
  if (length(vid) == 0) {
    stop("No vertex was specified")
  }

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_random_spanning_tree, graph, vid-1)
  if (igraph_opt("return.vs.es")) {
    res <- create_es(graph, res)
  }
  res
}

sample_tree_impl <- function(n, directed=FALSE, method=c("lerw", "prufer")) {
  # Argument checks
  n <- as.integer(n)
  directed <- as.logical(directed)
  method <- switch(igraph.match.arg(method), "prufer"=0L, "lerw"=1L)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_tree_game, n, directed, method)

  res
}

greedy_vertex_coloring_impl <- function(graph, heuristic=c("colored_neighbors")) {
  # Argument checks
  if (!is_igraph(graph)) { stop("Not a graph object") }
  heuristic <- switch(igraph.match.arg(heuristic), "colored_neighbors"=0L)

  on.exit( .Call(C_R_igraph_finalizer) )
  # Function call
  res <- .Call(C_R_igraph_vertex_coloring_greedy, graph, heuristic)
  res <- res+1L
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res) <- vertex_attr(graph, "name", )
  }
  res
}

