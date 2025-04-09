normalize <- function(dt) {
  dt %>% mutate(prob = if (sum(prob) != 0) prob / sum(prob) else prob)
}


L0_gen <- function(Q, u, i) {
  data.table(world = worlds) %>%
    rmutate(prob = P_w(world) * P_Q(Q) * interpret(u, world, i)) %>%
    normalize()
}

L0_gen("Qfine", "NPsg", interpretations[["iLitLitLitLit"]])
L0_gen("Qfine", "n!1", interpretations[["iLitLitLitLit"]])
L0_gen("Qfine", "!1", interpretations[["iLitLitLitLit"]])
L0_gen("Qex", "!1", interpretations[["iLitLitLitLit"]])
L0_gen("Qex", "nNPsg", interpretations[["iLitLitLitLit"]])


U1_gen <- function(Q, u) {
  L0_dt <- data.table(inter = names(interpretations)) %>%
    rmutate(L0 = list(L0_gen(Q, u, interpretations[[inter]])))

  data.table(world = worlds) %>%
    rmutate(util = {
      ec <- Q_equiv(Q, world)
      L0_dt %>%
        rmutate(L0 = L0 %>%
          filter(world %in% ec) %>%
          pull(prob) %>%
          sum() %>%
          {
            P_i(inter) * log(.)
          }) %>%
        pull(L0) %>%
        sum()
    })
}

U1_gen("Qex", "NPsg")

S1_gen <- function(Q, u) {
  U1_gen(Q, u) %>%
    transmute(world, prob = exp(lambda * util)) %>%
    normalize()
}

Ln_gen <- function(n, Q, u) {
  if (n == 0) {
    stop("n must be at least 1")
  } else {
    Sn_gen(n, Q, u) %>%
      mutate(prob = Vectorize(P_w)(world) * P_Q(Q) * prob) %>%
      normalize()
  }
}

Un_gen <- function(n, Q, u) {
  if (n == 1) {
    U1_gen(Q, u)
  } else {
    Ln_dt <- Ln_gen(n - 1, Q, u)
    Ln_dt %>%
      rtransmute(world, util = {
        ec <- Q_equiv(Q, world)
        Ln_dt %>%
          filter(world %in% ec) %>%
          pull(prob) %>%
          sum() %>%
          {
            log(.) - cost(u)
          }
      })
  }
}

Sn_gen <- function(n, Q, u) {
  if (n == 1) {
    S1_gen(Q, u)
  } else {
    Un_gen(n, Q, u) %>%
      transmute(world, prob = exp(lambda * util)) %>%
      normalize()
  }
}

U1("w0", "Qex", "!1")
U1("w1", "Qex", "!1")
U1("w2+", "Qex", "!1")
U1_gen("Qex", "!1")
S1_gen("Qex", "n!1")
S1_gen("Qex", "!1")
S1_gen("Qfine", "!1")

S1("w0", "Qex", "nNPpl")
Ln("w0", 1, "Qex", "NPsg")
Ln("w1", 1, "Qex", "NPsg")
Ln("w2+", 1, "Qex", "NPsg")
Sn_gen(2, "Qex", "NPsg")
Sn("w0", 2, "Qex", "NPsg")

QuDs %>% walk(\(q) {
  messages %>% walk(\(u) {
    cat(q, u, "\n")
    print(Sn_gen(5, q, u))
    cat("\n")
  })
})