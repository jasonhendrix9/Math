library(igraph)

# Function to perform the dss operation
dss <- function(n) {
  n <- as.character(n)
  n <- as.numeric(unlist(strsplit(n, "")))
  n <- sum(n)^2
  return(n)
}

# Function to iterate dss until a loop is detected
dss_until_loop <- function(n) {
  is_loop <- FALSE
  iteration_vector <- n

  while (!is_loop) {
    n <- dss(n)
    is_loop <- n %in% iteration_vector

    iteration_vector <- c(iteration_vector, n)
  }

  return(iteration_vector)
}

# Function to create a plot of the dss iterations
dss_plot <- function(n) {
  dss_iterations <- dss_until_loop(n)
  return_plot <- make_empty_graph()

  # Loop to add vertices and edges
  for (i in 1:length(dss_iterations)) {
    cat(i, ": ", dss_iterations[i], "\n")

    vertex_name <- as.character(dss_iterations[i])
    if (!(vertex_name %in% V(return_plot)$name)) {
      return_plot <- return_plot |>
        add_vertices(1, name = vertex_name)
    }

    if (i > 1) {
      from_vertex <- which(V(return_plot)$name == as.character(dss_iterations[i-1]))
      to_vertex <- which(V(return_plot)$name == vertex_name)

      if (from_vertex != to_vertex) {
        return_plot <- return_plot |>
          add_edges(c(from_vertex, to_vertex))
      }
    }
  }

  # Add an edge between the last two elements
  last_vertex <- which(V(return_plot)$name == as.character(dss_iterations[length(dss_iterations)]))
  second_last_vertex <- which(V(return_plot)$name == as.character(dss_iterations[length(dss_iterations) - 1]))

  return_plot <- return_plot |>
    add_edges(c(second_last_vertex, last_vertex))

  return(return_plot)
}

# Example usage:
dss_plot(123) |> plot()
