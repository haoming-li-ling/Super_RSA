

# Parameters and initial setup
lambda <- 5  # Rationality parameter
cost <- function(u) {
  if (u == "NPpl" || u == "NPsg") {
    return(0)
  } else if (u == "nNPpl" || u == "nNPsg") {
    return(1.5)
  } else if (u == "!1") {
    return(2.5)
  } else if (u == "n!1"){
    return(4)  
    } else {
    stop("Unknown message")
    }
}





# Prior distributions over worlds and QuDs
P_w <- function(w) {
  if (w %in% c("w0", "w1", "w2+")) {
    return(1 / 3)
  } else {
    stop("Unknown world")
  }
}

P_Q <- function(Q) {
  if (Q == "Qex") {
    return(0.8)
  } else if (Q %in% c("Qml", "Qfine")) {
    return(0.1)
  } else {
    stop("Unknown QuD")
  }
}

P_i <- function(i) {
  if (i %in% c("iLitLitLitLit", "iLitLitLitExh", "iLitLitExhLit", "iLitLitExhExh",   
               "iLitExhLitLit", "iLitExhLitExh", "iLitExhExhLit", "iLitExhExhExh",
               "iExhLitLitLit", "iExhLitLitExh", "iExhLitExhLit", "iExhLitExhExh",
               "iExhExhLitLit", "iExhExhLitExh", "iExhExhExhLit", "iExhExhExhExh")) {
    return(1 / 16)
  } else {
    stop("Unknown interpretation")
  }
}


# Sub-interpretation function
# Worlds: w0, w1, w2+
iSG_Lit = function(u, w){
  if (w == "w0") return(0)
  else return(1)
}

inSG_Lit = function(u, w) {
  return(1 - iSG_Lit(u, w))  
}


iSG_Exh = function(u, w){
  if (w == "w0") return(0)
  if (w == "w2+") return(0)
  else return(1)
}


inSG_Exh = function(u, w) {
  return(1 - iSG_Exh(u, w))  
}


# Note iSG_Lit and inSG_Lit are essentially the same as iPL_Lit and inPL_Lit


iPL_Lit = function(u, w){
  if (w == "w0") return(0)
  else return(1)
}

inPL_Lit = function(u, w) {
  return(1 - iPL_Lit(u, w))  
}


iPL_Exh = function(u, w){
  if (w == "w0") return(0)
  if (w == "w1") return(0)
  else return(1)
}

inPL_Exh = function(u, w) {
  return(1 - iPL_Exh(u, w))  
}

i1 = function(u, w){
  if (w == "w1") return (1)
  else return(0)
}

in1 = function(u, w){
  return(1 - i1(u, w))
}

# Interpretation function: [[u]]_i(w) -> {0, 1}
interpretations <- list(
  iLitLitLitLit = function(u, w) { 
    if (u == "NPsg") return(iSG_Lit(u, w))
    if (u == "NPpl") return(iPL_Lit(u, w))
    if (u == "nNPsg") return(inSG_Lit(u, w))
    if (u == "nNPpl") return(inPL_Lit(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iLitLitLitExh = function(u, w) { 
    if (u == "NPsg") return(iSG_Lit(u, w))
    if (u == "NPpl") return(iPL_Lit(u, w))
    if (u == "nNPsg") return(inSG_Lit(u, w))
    if (u == "nNPpl") return(inPL_Exh(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iLitLitExhLit = function(u, w) { 
    if (u == "NPsg") return(iSG_Lit(u, w))
    if (u == "NPpl") return(iPL_Lit(u, w))
    if (u == "nNPsg") return(inSG_Exh(u, w))
    if (u == "nNPpl") return(inPL_Lit(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iLitLitExhExh = function(u, w) { 
    if (u == "NPsg") return(iSG_Lit(u, w))
    if (u == "NPpl") return(iPL_Lit(u, w))
    if (u == "nNPsg") return(inSG_Exh(u, w))
    if (u == "nNPpl") return(inPL_Exh(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  
  iLitExhLitLit = function(u, w) { 
    if (u == "NPsg") return(iSG_Lit(u, w))
    if (u == "NPpl") return(iPL_Exh(u, w))
    if (u == "nNPsg") return(inSG_Lit(u, w))
    if (u == "nNPpl") return(inPL_Lit(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iLitExhLitExh = function(u, w) { 
    if (u == "NPsg") return(iSG_Lit(u, w))
    if (u == "NPpl") return(iPL_Exh(u, w))
    if (u == "nNPsg") return(inSG_Lit(u, w))
    if (u == "nNPpl") return(inPL_Exh(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iLitExhExhLit = function(u, w) { 
    if (u == "NPsg") return(iSG_Lit(u, w))
    if (u == "NPpl") return(iPL_Exh(u, w))
    if (u == "nNPsg") return(inSG_Exh(u, w))
    if (u == "nNPpl") return(inPL_Lit(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iLitExhExhExh = function(u, w) { 
    if (u == "NPsg") return(iSG_Lit(u, w))
    if (u == "NPpl") return(iPL_Exh(u, w))
    if (u == "nNPsg") return(inSG_Exh(u, w))
    if (u == "nNPpl") return(inPL_Exh(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  
  iExhLitLitLit = function(u, w) { 
    if (u == "NPsg") return(iSG_Exh(u, w))
    if (u == "NPpl") return(iPL_Lit(u, w))
    if (u == "nNPsg") return(inSG_Lit(u, w))
    if (u == "nNPpl") return(inPL_Lit(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iExhLitLitExh = function(u, w) { 
    if (u == "NPsg") return(iSG_Exh(u, w))
    if (u == "NPpl") return(iPL_Lit(u, w))
    if (u == "nNPsg") return(inSG_Lit(u, w))
    if (u == "nNPpl") return(inPL_Exh(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iExhLitExhLit = function(u, w) { 
    if (u == "NPsg") return(iSG_Exh(u, w))
    if (u == "NPpl") return(iPL_Lit(u, w))
    if (u == "nNPsg") return(inSG_Exh(u, w))
    if (u == "nNPpl") return(inPL_Lit(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iExhLitExhExh = function(u, w) { 
    if (u == "NPsg") return(iSG_Exh(u, w))
    if (u == "NPpl") return(iPL_Lit(u, w))
    if (u == "nNPsg") return(inSG_Exh(u, w))
    if (u == "nNPpl") return(inPL_Exh(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  
  iExhExhLitLit = function(u, w) { 
    if (u == "NPsg") return(iSG_Exh(u, w))
    if (u == "NPpl") return(iPL_Exh(u, w))
    if (u == "nNPsg") return(inSG_Lit(u, w))
    if (u == "nNPpl") return(inPL_Lit(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iExhExhLitExh = function(u, w) { 
    if (u == "NPsg") return(iSG_Exh(u, w))
    if (u == "NPpl") return(iPL_Exh(u, w))
    if (u == "nNPsg") return(inSG_Lit(u, w))
    if (u == "nNPpl") return(inPL_Exh(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iExhExhExhLit = function(u, w) { 
    if (u == "NPsg") return(iSG_Exh(u, w))
    if (u == "NPpl") return(iPL_Exh(u, w))
    if (u == "nNPsg") return(inSG_Exh(u, w))
    if (u == "nNPpl") return(inPL_Lit(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    },
  iExhExhExhExh = function(u, w) { 
    if (u == "NPsg") return(iSG_Exh(u, w))
    if (u == "NPpl") return(iPL_Exh(u, w))
    if (u == "nNPsg") return(inSG_Exh(u, w))
    if (u == "nNPpl") return(inPL_Exh(u, w))
    if (u == "!1") return(i1(u, w))
    if (u == "n!1") return(in1(u, w))
    }
)

interpret <- function(u, w, i) {
  return(i(u, w))
}

# Worlds, QuDs, messages
worlds <- c("w0", "w1", "w2+")
QuDs <- c("Qex", "Qml", "Qfine")
messages <- c("NPpl", "NPsg", "nNPpl", "nNPsg", "!1", "n!1")


# Equivalence relation: Q(w) -> set of worlds equivalent to w under Q
Q_equiv <- function(Q, w) {
  if (Q == "Qex" && w == "w0") {
    return(c("w0"))
  } else if (Q == "Qex" && w == "w1") {
    return(c("w1", "w2+"))  
  } else if (Q == "Qex" && w == "w2+") {
    return(c("w1", "w2+"))   
  } else if (Q == "Qml" && w == "w0") {
    return(c("w0", "w1"))
  } else if (Q == "Qml" && w == "w1") {
    return(c("w0", "w1"))
  } else if (Q == "Qml" && w == "w2+") {
    return(c("w2+"))
  } else if (Q == "Qfine" && w == "w0") {
    return(c("w0"))
  } else if (Q == "Qfine" && w == "w1") {
    return(c("w1"))
  } else if (Q == "Qfine" && w == "w2+") {
    return(c("w2+"))
  } else {
    stop("Unknown QuD")
  }
}


# # Listener 0
# L0 <- function(w, Q, u, i) {
#   P_w(w) * P_Q(Q) * interpret(u, w, i)
# }

# ==========================
# Normalization Function
# ==========================
# normalize <- function(probabilities) {
#   total <- sum(probabilities)
#   return(to_vec(for(x in probabilities) (x / total)))
# }

# normalize <- function(probs) {
#   total <- sum(values(probs))
#   if (total == 0) {
#     return(probs)
#   }
#   for (w in keys(probs)) {
#     probs[[w]] <- probs[[w]] / total
#   }
#   probs
# }
#
# normalize(hash("a"=1, "b"=2))


expand.grid(worlds, QuDs, messages)

# library(hash)

# L0_gen <- function(Q, u, i) {
#   probs <- hash()
#   for (w in worlds) {
#     probs[[w]] <- P_w(w) * P_Q(Q) * interpret(u, w, i)
#   }
#   normalize(probs)
# }


library(comprehenr)
library(data.table)

normalize <- function(dt) {
  dt[, probs := if (sum(probs) != 0) probs / sum(probs) else probs][]
}

# ==========================
# Literal Listener (L0)
# ==========================

L0_gen <- function(Q, u, i) {
  probs <- to_vec(for (w in worlds) {
    P_w(w) * P_Q(Q) * interpret(u, w, i)
    # print(P_w(w))
    # print(P_Q(Q))
    # print(interpret(u, w, i))
  })
  dt <- data.table(
    worlds = worlds,
    probs = probs
  )
  normalize(dt)
}

interpret("nNPsg", "w0", interpretations[["iLitLitLitLit"]])
interpret("!1", "w0", interpretations[["iLitLitLitLit"]])
interpret("!1", "w1", interpretations[["iLitLitLitLit"]])
interpret("!1", "w2+", interpretations[["iLitLitLitLit"]])
interpret("n!1", "w0", interpretations[["iLitLitLitLit"]])
interpret("n!1", "w1", interpretations[["iLitLitLitLit"]])
interpret("n!1", "w2+", interpretations[["iLitLitLitLit"]])

L0 <- function(w, Q, u, i) {
  L0_gen(Q, u, i)[worlds == w, probs]
}

L0_gen("Qfine", "NPsg", interpretations[["iLitLitLitLit"]])
L0_gen("Qfine", "n!1", interpretations[["iLitLitLitLit"]])
L0_gen("Qfine", "!1", interpretations[["iLitLitLitLit"]])
L0_gen("Qex", "!1", interpretations[["iLitLitLitLit"]])
L0_gen("Qex", "nNPsg", interpretations[["iLitLitLitLit"]])

L0("w0", "Qex", "nNPsg", interpretations[["iLitLitLitLit"]])







# ==========================
# Recursive Pragmatic Layers
# ==========================
lambda <- 5
U1 <- function(w, Q, u) {
  prob_surprisal_vec <- vector()
  for (i in names(interpretations)) {
    L0_vec <- vector()
    for (v in Q_equiv(Q, w)) {
      L0_vec <- append(L0_vec, L0(v, Q, u, interpretations[[i]])) 
      L0_sum <- sum(L0_vec)
    }
    prob_surprisal_vec <- append(prob_surprisal_vec, P_i(i) * log(L0_sum))
  }
  sum(prob_surprisal_vec) - cost(u)
}
U1("w0", "Qex", "!1")
U1("w1", "Qex", "!1")
U1("w2+", "Qex", "!1")
S1_gen <- function(Q, u) {
  probs <- sapply(worlds, function(w) { exp(lambda * U1(w, Q, u)) } )
  dt <- data.table(worlds = worlds, probs = probs)
  normalize(dt)
}

S1 <- function(w, Q, u) {
  S1_gen(Q, u)[worlds == w, probs]
}

S1_gen("Qex", "n!1")
S1_gen("Qex", "!1")
S1_gen("Qfine", "!1")

S1("w0", "Qex", "nNPpl")

Ln_gen <- function(n, Q, u) {
  probs <- sapply(worlds, function(w) {P_w(w) * P_Q(Q) * Sn(w, n, Q, u)})
  dt <- data.table(worlds = worlds, probs = probs)
  normalize(dt)
}

Ln <- function(w, n, Q, u) {
  Ln_gen(n, Q, u)[worlds == w, probs]
}

Ln("w0", 1, "Qex", "NPsg")
Ln("w1", 1, "Qex", "NPsg")
Ln("w2+", 1, "Qex", "NPsg")


Un <- function(w, n, Q, u) {
  if (n == 1) {
    U1(w, Q, u)
  } else {
    # s <- sum(to_vec(for (v in Q_equiv(Q, w)) {
    #                   log(Ln(v, n - 1, Q, u))
    #            }))
    # print(s)
    log(sum(to_vec(for (v in Q_equiv(Q, w)) {
      Ln(v, n - 1, Q, u)
    }))) - cost(u)
  }
}

U1("w0", "Qex", "NPsg")
Un("w0", 2, "Qex", "NPsg")

Sn_gen <- function(n, Q, u) {
  if (n == 1) {
    S1_gen(Q, u)
  } else {
    probs <- sapply(worlds, function(w) {
      exp(lambda * Un(w, n, Q, u))
    })
    dt <- data.table(worlds = worlds, probs = probs)
    normalize(dt)
  }
}

Sn_gen(2, "Qex", "NPsg")

Sn <- function(w, n, Q, u) {
  Sn_gen(n, Q, u)[worlds == w, probs]
}

Sn("w0", 2, "Qex", "NPsg")



# ==========================
# Parameters & Testing
# ==========================

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

