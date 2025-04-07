# Parameters and initial setup
lambda <- 5 # Rationality parameter
cost <- function(u) {
  switch(u,
    NPpl = 0,
    NPsg = 0,
    nNPpl = 1.5,
    nNPsg = 1.5,
    "!1" = 2.5,
    "n!1" = 4,
    stop("Unknown message")
  )
}

# Prior distributions over worlds and QuDs
P_w <- function(w) {
  switch(w,
    "w0" = 1 / 3,
    "w1" = 1 / 3,
    "w2+" = 1 / 3,
    stop("Unknown world")
  )
}

P_Q <- function(Q) {
  switch(Q,
    Qex = 0.8,
    Qml = 0.1,
    Qfine = 0.1,
    stop("Unknown QuD")
  )
}

P_i <- function(i) {
  if (i %in% c(
    "iLitLitLitLit", "iLitLitLitExh", "iLitLitExhLit", "iLitLitExhExh",
    "iLitExhLitLit", "iLitExhLitExh", "iLitExhExhLit", "iLitExhExhExh",
    "iExhLitLitLit", "iExhLitLitExh", "iExhLitExhLit", "iExhLitExhExh",
    "iExhExhLitLit", "iExhExhLitExh", "iExhExhExhLit", "iExhExhExhExh"
  )) {
    1 / 16
  } else {
    stop("Unknown interpretation")
  }
}


# Sub-interpretation function
# Worlds: w0, w1, w2+
iSG_Lit <- function(u, w) {
  switch(w,
    "w0" = 0,
    1
  )
}

inSG_Lit <- function(u, w) {
  1 - iSG_Lit(u, w)
}


iSG_Exh <- function(u, w) {
  switch(w,
    "w1" = 1,
    0
  )
}

inSG_Exh <- function(u, w) {
  1 - iSG_Exh(u, w)
}

# Note iSG_Lit and inSG_Lit are essentially the same as iPL_Lit and inPL_Lit
iPL_Lit <- function(u, w) {
  iSG_Lit(u, w)
}

inPL_Lit <- function(u, w) {
  1 - iPL_Lit(u, w)
}

iPL_Exh <- function(u, w) {
  switch(w,
    "w2+" = 1,
    0
  )
}

inPL_Exh <- function(u, w) {
  1 - iPL_Exh(u, w)
}

i1 <- function(u, w) {
  iSG_Exh(u, w)
}

in1 <- function(u, w) {
  1 - i1(u, w)
}

# Interpretation function: [[u]]_i(w) -> {0, 1}
interpretations <- list(
  iLitLitLitLit = function(u, w) {
    switch(u,
      "NPsg" = iSG_Lit(u, w),
      "NPpl" = iPL_Lit(u, w),
      "nNPsg" = inSG_Lit(u, w),
      "nNPpl" = inPL_Lit(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iLitLitLitExh = function(u, w) {
    switch(u,
      "NPsg" = iSG_Lit(u, w),
      "NPpl" = iPL_Lit(u, w),
      "nNPsg" = inSG_Lit(u, w),
      "nNPpl" = inPL_Exh(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iLitLitExhLit = function(u, w) {
    switch(u,
      "NPsg" = iSG_Lit(u, w),
      "NPpl" = iPL_Lit(u, w),
      "nNPsg" = inSG_Exh(u, w),
      "nNPpl" = inPL_Lit(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iLitLitExhExh = function(u, w) {
    switch(u,
      "NPsg" = iSG_Lit(u, w),
      "NPpl" = iPL_Lit(u, w),
      "nNPsg" = inSG_Exh(u, w),
      "nNPpl" = inPL_Exh(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iLitExhLitLit = function(u, w) {
    switch(u,
      "NPsg" = iSG_Lit(u, w),
      "NPpl" = iPL_Exh(u, w),
      "nNPsg" = inSG_Lit(u, w),
      "nNPpl" = inPL_Lit(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iLitExhLitExh = function(u, w) {
    switch(u,
      "NPsg" = iSG_Lit(u, w),
      "NPpl" = iPL_Exh(u, w),
      "nNPsg" = inSG_Lit(u, w),
      "nNPpl" = inPL_Exh(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iLitExhExhLit = function(u, w) {
    switch(u,
      "NPsg" = iSG_Lit(u, w),
      "NPpl" = iPL_Exh(u, w),
      "nNPsg" = inSG_Exh(u, w),
      "nNPpl" = inPL_Lit(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iLitExhExhExh = function(u, w) {
    switch(u,
      "NPsg" = iSG_Lit(u, w),
      "NPpl" = iPL_Exh(u, w),
      "nNPsg" = inSG_Exh(u, w),
      "nNPpl" = inPL_Exh(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w),
    )
  },
  iExhLitLitLit = function(u, w) {
    switch(u,
      "NPsg" = iSG_Exh(u, w),
      "NPpl" = iPL_Lit(u, w),
      "nNPsg" = inSG_Lit(u, w),
      "nNPpl" = inPL_Lit(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w),
    )
  },
  iExhLitLitExh = function(u, w) {
    switch(u,
      "NPsg" = iSG_Exh(u, w),
      "NPpl" = iPL_Lit(u, w),
      "nNPsg" = inSG_Lit(u, w),
      "nNPpl" = inPL_Exh(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iExhLitExhLit = function(u, w) {
    switch(u,
      "NPsg" = iSG_Exh(u, w),
      "NPpl" = iPL_Lit(u, w),
      "nNPsg" = inSG_Exh(u, w),
      "nNPpl" = inPL_Lit(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iExhLitExhExh = function(u, w) {
    switch(u,
      "NPsg" = iSG_Exh(u, w),
      "NPpl" = iPL_Lit(u, w),
      "nNPsg" = inSG_Exh(u, w),
      "nNPpl" = inPL_Exh(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iExhExhLitLit = function(u, w) {
    switch(u,
      "NPsg" = iSG_Exh(u, w),
      "NPpl" = iPL_Exh(u, w),
      "nNPsg" = inSG_Lit(u, w),
      "nNPpl" = inPL_Lit(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iExhExhLitExh = function(u, w) {
    switch(u,
      "NPsg" = iSG_Exh(u, w),
      "NPpl" = iPL_Exh(u, w),
      "nNPsg" = inSG_Lit(u, w),
      "nNPpl" = inPL_Exh(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iExhExhExhLit = function(u, w) {
    switch(u,
      "NPsg" = iSG_Exh(u, w),
      "NPpl" = iPL_Exh(u, w),
      "nNPsg" = inSG_Exh(u, w),
      "nNPpl" = inPL_Lit(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  },
  iExhExhExhExh = function(u, w) {
    switch(u,
      "NPsg" = iSG_Exh(u, w),
      "NPpl" = iPL_Exh(u, w),
      "nNPsg" = inSG_Exh(u, w),
      "nNPpl" = inPL_Exh(u, w),
      "!1" = i1(u, w),
      "n!1" = in1(u, w)
    )
  }
)

interpret <- function(u, w, i) {
  i(u, w)
}

interpret("nNPsg", "w0", interpretations[["iLitLitLitLit"]])
interpret("nNPsg", "w1", interpretations[["iLitLitLitLit"]])
interpret("nNPsg", "w2+", interpretations[["iLitLitLitLit"]])

interpret("nNPsg", "w0", interpretations[["iLitLitExhLit"]])
interpret("nNPsg", "w1", interpretations[["iLitLitExhLit"]])
interpret("nNPsg", "w2+", interpretations[["iLitLitExhLit"]])

interpret("!1", "w0", interpretations[["iLitLitLitLit"]])
interpret("!1", "w1", interpretations[["iLitLitLitLit"]])
interpret("!1", "w2+", interpretations[["iLitLitLitLit"]])
interpret("n!1", "w0", interpretations[["iLitLitLitLit"]])
interpret("n!1", "w1", interpretations[["iLitLitLitLit"]])
interpret("n!1", "w2+", interpretations[["iLitLitLitLit"]])

# Worlds, QuDs, messages
worlds <- c("w0", "w1", "w2+")
QuDs <- c("Qex", "Qml", "Qfine")
messages <- c("NPpl", "NPsg", "nNPpl", "nNPsg", "!1", "n!1")


# Equivalence relation: Q(w) -> set of worlds equivalent to w under Q
Q_equiv <- function(Q, w) {
  switch(Q,
    Qex = switch(w,
      "w0" = c("w0"),
      "w1" = c("w1", "w2+"),
      "w2+" = c("w1", "w2+"),
      stop("Unknown QuD")
    ),
    Qml = switch(w,
      "w0" = c("w0", "w1"),
      "w1" = c("w0", "w1"),
      "w2+" = c("w2+"),
      stop("Unknown QuD")
    ),
    Qfine = switch(w,
      "w0" = c("w0"),
      "w1" = c("w1"),
      "w2+" = c("w2+"),
      stop("Unknown QuD")
    ),
    stop("Unknown QuD")
  )
}

expand.grid(worlds, QuDs, messages)

# library(comprehenr)
library(data.table)
library(tidyverse)

normalize <- function(dt) {
  dt %>% mutate(prob = if (sum(prob) != 0) prob / sum(prob) else prob)
}

# ==========================
# Literal Listener (L0)
# ==========================

L0_gen <- function(Q, u, i) {
  data.table(world = worlds) %>%
    rowwise() %>%
    mutate(prob = P_w(world) * P_Q(Q) * interpret(u, world, i)) %>%
    ungroup() %>%
    normalize()
}


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

# Do rowwise mutate and ungroup
rmutate <- function(data, ...) {
  data %>%
    rowwise() %>%
    mutate(...) %>%
    ungroup()
}

rtransmute <- function(data, ...) {
  data %>%
    rowwise() %>%
    transmute(...) %>%
    ungroup()
}

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

S1 <- function(w, Q, u) {
  S1_gen(Q, u)[world == w, prob]
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

Ln <- function(w, n, Q, u) {
  Ln_gen(n, Q, u)[world == w, prob]
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

Un <- function(w, n, Q, u) {
  Un_gen(n, Q, u)[world == w, util]
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

Sn <- function(w, n, Q, u) {
  Sn_gen(n, Q, u)[world == w, prob]
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
