
#' Step-Down algorithm to control the FDP
#'
#' @param target_scores a vector of numeric values representing
#' the target scores
#' @param decoy_scores a vector of numeric values representing the corresponding
#' decoy scores
#' @param alpha the FDP control threshold, must be a numeric value between 0 and 1
#' @param gamma the confidence level will be 1 - gamma
#' @param randomised boolean
#'
#' @return It returns a threshold and a list of indices of the dicvoeries
#' @export
#'
#' @examples
#' x <- rnorm(1000, 1, 1)
#' y <- rnorm(1000)
#' target_scores <- apply(cbind(x, y), 1, max)
#' decoy_scores <- rnorm(1000)
#' step_down(target_scores, decoy_scores, 0.2, 0.1)
step_down <- function(
  target_scores,
  decoy_scores,
  alpha,
  gamma,
  randomised = T
){
  cutoff_rank <- 0
  i0 <- get_i0(alpha, gamma)

  indicator_list <- indicator_list_maker_knockoff(
    target_scores,
    decoy_scores,
    return_order_only = FALSE
  )

  if(randomised){
    delta <- delta_generator_coinflip(
      max_i = nrow(indicator_list), alpha, gamma)
  }else{
    # when there is no coin-flip, delta is fixed
    delta <- delta_generator(max_i = nrow(indicator_list), alpha, gamma)
  }

  # if(j %% percentile == 0)
  #   cat(j,' ')
  m <- sum(!is.na(indicator_list[, 2]))

  if(i0 > m){
    return(
      list(
        indices_discoveries = NULL,
        cutoff_score = Inf
      )
    )
  }

  row <- rev(indicator_list[1:m, 2])
  d <- rep(NA, nrow(indicator_list))

  if(i0 <= 1){
    d_i <- 0
  }else{
    d_i <- sum(row[1:(i0-1)] == 3)
  }

  for(i in i0:m){
    d_i <- d_i + (row[i] == 3)
    d[i] <- d_i
    if(d_i <= delta[i]){
      cutoff_rank <- m - i + 1
    }else{
      break
    }
  }
  if(cutoff_rank < 1){
    indices_discoveries <- NULL
    cutoff_score <- Inf
  }else{
    indices_above_cutoff <- indicator_list[1:cutoff_rank,1]
    labels_above_cutoff <- indicator_list[1:cutoff_rank,2]
    indices_discoveries <- indices_above_cutoff[labels_above_cutoff < 3]
    cutoff_score <- indicator_list[cutoff_rank, 3]
  }

  return(
    list(
      indices_discoveries = indices_discoveries,
      cutoff_score = cutoff_score
    )
  )
}

# read in target scores and decoy socres and return a vector of vector/target
# labels sorted by the winning scores
# where 2 represent a target win, and 3 represent a decoy win
# when the ground truth is known, 2 represent a false discovery and 1 is a
# true discovery
indicator_list_maker_knockoff <- function(
  target_s,
  decoy_s,
  true_features = NULL,
  return_order_only = TRUE) {
  is_cor<-1:length(target_s) %in% true_features
  r <- cbind(target_s, decoy_s,1:length(target_s))
  r <- cbind(r, apply(r[,1:2], 1, function(x){
    if(x[1]==x[2]){
      return(sample(1:2,1))
    }else{
      return(which.max(x))
    }
  }))
  r <- cbind(r, apply(r, 1, function(x) x[x[4]]))
  r[r[, 4] == 2, 4] <- 3
  r[r[, 4] == 1, 4] <- ifelse(is_cor[r[, 4] == 1], 1, 2)
  if(return_order_only){
    return(r[order(r[, 5], stats::rnorm(nrow(r)), decreasing = FALSE), 4])
  }else{
    return(r[order(r[, 5], stats::rnorm(nrow(r)), decreasing = FALSE), 3:5])
  }
}

# the smallest possible positive number of discoveries from FDP-SD
get_i0 <- function(alpha = 0.1, c = 0.05){
  m_c <- ceiling(-log(c)/log(2))
  ceiling((m_c-1)/alpha)
}

# for non-randomised TDC-SD
delta_generator <- function(max_i = 100, alpha = 0.1, c = 0.05){
  delta <- NULL
  d <- 0
  for(i in 1:max_i){
    k <- floor((i - d) * alpha) + 1
    while(stats::pbinom(d, k + d, 1/2) <= c){
      d <- d + 1
      k <- floor((i - d) * alpha) + 1
    }
    delta[i] <- d - 1
  }
  delta
}

# for randomised FDP-SD
delta_generator_coinflip <- function(
  max_i = 100, alpha = 0.1, c = 0.05){
  gamma <- NULL
  d <- 0
  delta <- delta_generator(max_i, alpha, c)

  # i = 1
  d_1 <- delta[1]
  k <- floor((1 - d_1) * alpha) + 1
  k_upper <- floor((1 - (d_1 + 1)) * alpha) + 1
  p_lower <- stats::pbinom(d_1, k + d_1, 1/2)
  p_upper <- stats::pbinom(d_1 + 1, k_upper + (d_1 + 1), 1/2)
  w <- (p_upper - c)/(p_upper - p_lower)
  if(stats::runif(1) < w){
    gamma[1] <- d_1
  }else{
    gamma[1] <- d_1 + 1
  }

  if(max_i < 2){
    return(gamma)
  }

  for(i in 2:max_i){
    #cat(i, ' ')
    k <- floor((i - delta[i]) * alpha) + 1
    k_upper <- floor((i - (delta[i] + 1)) * alpha) + 1
    p_lower <- stats::pbinom(delta[i], k + delta[i], 1/2)
    p_upper <- stats::pbinom(delta[i] + 1, k_upper + (delta[i] + 1), 1/2)
    w_prev <- w
    w <- (p_upper - c)/(p_upper - p_lower)
    if(delta[i] > delta[i-1]){
      gamma[i] <- ifelse(stats::runif(1) < w, delta[i], delta[i] + 1)
    }else if(gamma[i-1] == delta[i] + 1){
      gamma[i] <- gamma[i-1]
    }else{
      w_prime <- w/w_prev
      gamma[i] <- ifelse(stats::runif(1) < w_prime, delta[i], delta[i] + 1)
    }
  }
  return(gamma)
}
