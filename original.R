lambda <- 5  # Pragmatic reasoning weight
n_depth <- 2  # Set recursion depth

# Test listener and speaker reasoning at depth 2
test_prob_L2 <- L_n(n_depth, "w1", "Qex", "NPsg", interpretations[["iLitLitLitExh"]])
test_prob_S2 <- S_n(n_depth, "NPsg", "w1", "Qex", interpretations[["iLitLitExhExh"]])

print(test_prob_L2)
print(test_prob_S2)


# Load required library
library(ggplot2)

# ==========================
# Function for Visualization
# ==========================

plot_distribution <- function(data, title, x_label, y_label) {
  ggplot(data, aes(x = message, y = probability, fill = world)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    scale_fill_brewer(palette = "Set2")
}

# ==========================
# Compute Probabilities for Visualization
# ==========================

# Literal Listener (L0)
L0_data <- expand.grid(world = worlds, message = messages)
L0_data$probability <- mapply(function(w, u) L_0(w, "Qex", u, interpretations[["iLitLit"]]), L0_data$world, L0_data$message)

# Pragmatic Listener (L2)
L2_data <- expand.grid(world = worlds, message = messages)
L2_data$probability <- mapply(function(w, u) L_n(2, w, "Qex", u, interpretations[["iLitLit"]]), L2_data$world, L2_data$message)

# Pragmatic Speaker (S2)
S2_data <- expand.grid(world = worlds, message = messages)
S2_data$probability <- mapply(function(u, w) S_n(2, u, w, "Qex", interpretations[["iLitLit"]]), S2_data$message, S2_data$world)

# ==========================
# Generate and Display Plots
# ==========================

print(plot_distribution(L0_data, "Literal Listener (L0)", "Messages", "Probability"))
print(plot_distribution(L2_data, "Pragmatic Listener (L2)", "Messages", "Probability"))
print(plot_distribution(S2_data, "Pragmatic Speaker (S2)", "Messages", "Probability"))










# Speaker Utility 1
U1 <- function(u, w, Q) {
  sum_values <- sum(sapply(names(interpretations), function(i_name) {
    i <- interpretations[[i_name]]
    P_i(i_name) * log(sum(sapply(Q_equiv(Q, w), function(v) {
      L0(v, Q, u, i)
    })) + 1e-10)  # Adding small constant to avoid log(0)
  }))
  return(sum_values - cost(u))
}



# Speaker 1
S1 <- function(u, w, Q) {
  exp(lambda * U1(u, w, Q))
}


L_n <- function(n, w, Q, u, i) {
  if (n == 0) {
    probs <- sapply(worlds, function(w_prime) P_w(w_prime) * P_Q(Q) * interpret(u, w_prime, i))
    return(normalize(probs)[w])
  } else {
    posteriors <- sapply(worlds, function(w_prime) S_n(n - 1, u, w_prime, Q, i) * P_w(w_prime))
    return(normalize(posteriors)[w])
  }
}

S_n <- function(n, u, w, Q, i) {
  if (n == 0) {
    return(exp(lambda * L_n(0, w, Q, u, i)) / exp(cost(u)))
  } else {
    utilities <- sapply(messages, function(m) exp(lambda * L_n(n, w, Q, m, i)) / exp(cost(m)))
    return(normalize(utilities)[u])
  }
}


# Utility n+1
U_n_plus_1 <- function(n, u, w, Q) {
  L_values <- sapply(Q_equiv(Q, w), function(v) { L_n(n, v, Q, u) })
  L_sum <- sum(L_values)
  if (L_sum == 0) {
    return(-Inf)  # Assigning lowest utility
  } else {
    return(log(L_sum) - cost(u))
  }
}

# Speaker n
S_n <- function(n, u, w, Q) {
  if (n == 1) {
    return(exp(lambda * U1(u, w, Q)))
  } else {
    return(exp(lambda * U_n_plus_1(n - 1, u, w, Q)))
  }
}



# Check
P_w("w0")  # Should return 1/3
P_Q("Qex") # Should return 1/3
P_i("iLitLitLitLit")  # Should return 1/6

# Example run for Speaker 1
S1_values <- sapply(messages, function(u) S1(u, "w0", "Qex"))
norm_S1_values <- normalize(S1_values)
names(norm_S1_values) <- messages
print(norm_S1_values)


round(S_n(1, "NPsg", "w0", "Qex"), 2)  # Should match S1
round(S_n(2, "NPsg", "w0", "Qex"), 2)  # More refined probabilities

normalize_S_n <- function(n, w, Q) {
  S_values <- sapply(messages, function(u) S_n(n, u, w, Q))
  normalize(S_values)  # Ensure probabilities sum to 1
}


# Model results

round(S_n(2, "NPsg", "w0", "Qex"), 2)  # Speaker 5 probability for "NPsg"





round(S_n(3, "NP"))