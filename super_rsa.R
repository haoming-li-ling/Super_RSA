options(pillar.sigfig = 7)
library(tidyverse)
library(magrittr)
library(dplyr)

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

worlds <- c("w0", "w1", "w2+")
QuDs <- c("Qex", "Qml", "Qfine")
messages <- c("NPsg", "NPpl", "nNPsg", "nNPpl", "!1", "n!1")


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
  if (i %in% i_names) {
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

ambi <- c("Lit", "Exh")
interprs <- setNames(
  list(
    setNames(c(iSG_Lit, iSG_Exh), ambi),
    setNames(c(iPL_Lit, iPL_Exh), ambi),
    setNames(c(inSG_Lit, inSG_Exh), ambi),
    setNames(c(inPL_Lit, inPL_Exh), ambi)
  ),
  messages[1:4]
)

# Interpretation function: [[u]]_i(w) -> {0, 1}

interpretations <- expand.grid(
  iNPsg = ambi,
  iNPpl = ambi,
  inNPsg = ambi,
  inNPpl = ambi,
  stringsAsFactors = FALSE
) %>%
  rmutate(
    inter = {
      inpsg <- iNPsg
      inppl <- iNPpl
      innpsg <- inNPsg
      innppl <- inNPpl
      list(function(u, w) {
        switch(u,
          "NPsg" = interprs[["NPsg"]][[inpsg]](u, w),
          "NPpl" = interprs[["NPpl"]][[inppl]](u, w),
          "nNPsg" = interprs[["nNPsg"]][[innpsg]](u, w),
          "nNPpl" = interprs[["nNPpl"]][[innppl]](u, w),
          "!1" = i1(u, w),
          "n!1" = in1(u, w)
        )
      })
    },
    name = paste0("i", iNPsg, iNPpl, inNPsg, inNPpl)
  ) %$%
  setNames(inter, name) %T>% print()


i_names <- names(interpretations)

interpret <- function(u, w, i) {
  i(u, w)
}

interpret("nNPsg", "w0", interpretations[["iLitLitLitLit"]])
interpret("nNPsg", "w1", interpretations[["iLitLitLitLit"]])
interpret("NPsg", "w2+", interpretations[["iLitLitLitLit"]])
interpret("NPpl", "w2+", interpretations[["iLitLitLitLit"]])

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

# library(comprehenr)
# library(data.table)
# library(tidyverse)
# library(magrittr)
# library(dplyr)



# dt <- expand.grid(world = worlds, QuD = QuDs, message = messages, inter = i_names, stringsAsFactors = FALSE) %T>%
#   print()
# ==========================
# Literal Listener (L0)
# ==========================

L0 <- function() {
  expand.grid(
    world = worlds,
    QuD = QuDs,
    message = messages,
    inter = i_names,
    stringsAsFactors = FALSE
  ) %>%
    rmutate(prob = {
      P_w(world) * P_Q(QuD) * interpretations[[inter]](message, world)
    }) %>%
    group_by(message, inter) %>%
    mutate(prob = prob / sum(prob)) %>%
    ungroup() %>%
    arrange(message, inter)
}

l0 <- L0() %T>% print(n = 54)



# ==========================
# Recursive Pragmatic Layers
# ==========================
lambda <- 5

# Do rowwise mutate and ungroup
U1 <- function() {
  l0 <- L0()
  l0 %>%
    group_by(world, QuD, message) %>%
    summarise() %>%
    ungroup() %>%
    rmutate(util = {
      u <- message
      ec <- Q_equiv(QuD, world)
      Q <- QuD
      l0 %>%
        filter(world %in% ec & QuD == Q & message == u) %>%
        group_by(inter) %>%
        summarise(per_i_util = P_i(inter[1]) * log(sum(prob))) %$%
        sum(per_i_util) - cost(u)
    }) %>%
    arrange(world, QuD)
}

u1 <- U1() %T>% print(n = 54)


S1 <- function() {
  U1() %>%
    transmute(world, QuD, message, prob = exp(lambda * util)) %>%
    group_by(world, QuD) %>%
    mutate(prob = prob / sum(prob)) %>%
    ungroup() %>%
    arrange(world, QuD)
}

s1 <- S1() %T>% print(n = 54)

Ln <- function(n) {
  if (n == 0) {
    L0()
  } else {
    Sn(n) %>%
      rmutate(prob = P_w(world) * P_Q(QuD) * prob) %>%
      group_by(message) %>%
      mutate(prob = prob / sum(prob)) %>%
      ungroup() %>%
      arrange(message)
  }
}

Un <- function(n) {
  if (n == 1) {
    U1()
  } else {
    ln_1 <- Ln(n - 1)
    ln_1 %>%
      rmutate(util = {
        ec <- Q_equiv(QuD, world)
        Q <- QuD
        u <- message
        ln_1 %>%
          filter(world %in% ec & QuD == Q & message == u) %$%
          log(sum(prob)) - cost(u)
      }) %>%
      arrange(world, QuD)
  }
}

Sn <- function(n) {
  Un(n) %>%
    transmute(world, QuD, message, prob = exp(lambda * util)) %>%
    group_by(world, QuD) %>%
    mutate(prob = prob / sum(prob)) %>%
    arrange(world, QuD)
}

Sn(3) %>% print(n = 54)

Ln(3) %>% print(n = 54)
Ln(1) %>% print(n = 54)
