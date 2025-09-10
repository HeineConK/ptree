#' @export compute_leaf_probs
compute_leaf_probs <- function(nodes, log = FALSE){

  nodes <- col_id_tier( nodes )

  max_tier <- max( nodes$tier )
  # nodes$is_leaf <- nodes$tier == max_tier
  nodes$leaf_p <- NA

  il <- which( nodes$is_leaf )
  for(i in il){
    k <- i
    probs <- numeric()
    while(T){
      probs <- c(probs, nodes$p[ k ])
      k <- nodes$parent[k]
      if(k == 0) break
    }

    if(log == T){
      nodes$leaf_p[ i ] <- sum( log( probs ))
    }
    else{
      nodes$leaf_p[ i ] <- prod( probs )
    }
  }

  if(any(nodes$leaf_p == 0, na.rm = T)){
    warning("Some leaf nodes have zero probability.")
  }
  return( nodes )
}
