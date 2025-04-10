options(pillar.sigfig = 4)

library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)

worlds <- c("w0", "w1", "w2+")
QuDs <- c("Qex", "Qml", "Qfine")
messages <- c("NPsg", "NPpl", "nNPsg", "nNPpl", "!1", "n!1")
lambda <- 5 # Rationality parameter

# Equivalence relation: Q(w) -> set of worlds equivalent to w under Q
Q_equiv <- function(Q, w) {
  case_when(
    Q == "Qex" & w == "w0" ~ list(c("w0")),
    Q == "Qex" & w == "w1" ~ list(c("w1", "w2+")),
    Q == "Qex" & w == "w2+" ~ list(c("w1", "w2+")),
    Q == "Qml" & w == "w0" ~ list(c("w0", "w1")),
    Q == "Qml" & w == "w1" ~ list(c("w0", "w1")),
    Q == "Qml" & w == "w2+" ~ list(c("w2+")),
    Q == "Qfine" & w == "w0" ~ list(c("w0")),
    Q == "Qfine" & w == "w1" ~ list(c("w1")),
    Q == "Qfine" & w == "w2+" ~ list(c("w2+"))
  )
}

# Parameters and initial setup
cost <- function(u) {
  case_when(
    u == "NPpl" ~ 0,
    u == "NPsg" ~ 0,
    u == "nNPpl" ~ 1.5,
    u == "nNPsg" ~ 1.5,
    u == "!1" ~ 2.5,
    u == "n!1" ~ 4
  )
}

# Prior distributions over worlds and QuDs
P_w <- function(w) {
  case_when(
    w == "w0" ~ 1 / 3,
    w == "w1" ~ 1 / 3,
    w == "w2+" ~ 1 / 3
  )
}

P_Q <- function(Q) {
  case_when(
    Q == "Qex" ~ 0.8,
    Q == "Qml" ~ 0.1,
    Q == "Qfine" ~ 0.1
  )
}

# Sub-interpretation function
# Worlds: w0, w1, w2+
iSG_Lit <- function(w) {
  case_when(
    w == "w0" ~ 0,
    TRUE ~ 1
  )
}

inSG_Lit <- function(w) {
  1 - iSG_Lit(w)
}


iSG_Exh <- function(w) {
  case_when(
    w == "w1" ~ 1,
    TRUE ~ 0
  )
}

inSG_Exh <- function(w) {
  1 - iSG_Exh(w)
}

# Note iSG_Lit and inSG_Lit are essentially the same as iPL_Lit and inPL_Lit
iPL_Lit <- function(w) {
  iSG_Lit(w)
}

inPL_Lit <- function(w) {
  1 - iPL_Lit(w)
}

iPL_Exh <- function(w) {
  case_when(
    w == "w2+" ~ 1,
    TRUE ~ 0
  )
}

inPL_Exh <- function(w) {
  1 - iPL_Exh(w)
}

i1 <- function(w) {
  iSG_Exh(w)
}

in1 <- function(w) {
  1 - i1(w)
}

LitExh <- c("Lit", "Exh")
interprs <- setNames(
  list(
    setNames(c(iSG_Lit, iSG_Exh), LitExh),
    setNames(c(iPL_Lit, iPL_Exh), LitExh),
    setNames(c(inSG_Lit, inSG_Exh), LitExh),
    setNames(c(inPL_Lit, inPL_Exh), LitExh)
  ),
  messages[1:4]
)

# Interpretation function: [[u]]_i(w) -> {0, 1}

interpretations <- expand_grid(
  iNPsg = LitExh,
  iNPpl = LitExh,
  inNPsg = LitExh,
  inNPpl = LitExh,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    name = paste0("i", iNPsg, iNPpl, inNPsg, inNPpl),
    inter = {
      inpsg <- iNPsg
      inppl <- iNPpl
      innpsg <- inNPsg
      innppl <- inNPpl
      list(function(u, w) {
        case_when(
          u == "NPsg" ~ interprs[["NPsg"]][[inpsg]](w),
          u == "NPpl" ~ interprs[["NPpl"]][[inppl]](w),
          u == "nNPsg" ~ interprs[["nNPsg"]][[innpsg]](w),
          u == "nNPpl" ~ interprs[["nNPpl"]][[innppl]](w),
          u == "!1" ~ i1(w),
          u == "n!1" ~ in1(w)
        )
      })
    }
  ) %$%
  setNames(inter, name)


inters <- names(interpretations)

P_i <- function(i) {
  case_when(
    i %in% inters ~ 1 / 16
  )
}

interpret <- function(ms, ws, is) {
  pmap_vec(list(ms, ws, is), \(u, w, i) interpretations[[i]](u, w))
}

L0 <- function() {
  expand_grid(
    world = worlds,
    QuD = QuDs,
    message = messages,
    inter = inters,
    stringsAsFactors = FALSE
  ) %>%
    mutate(prob = P_w(world) * P_Q(QuD) * interpret(message, world, inter)) %>%
    group_by(message, inter) %>%
    mutate(prob = prob / sum(prob)) %>%
    ungroup() %>%
    arrange(message, inter)
}

U1 <- function() {
  L0() %>%
    mutate(Qw = Q_equiv(QuD, world)) %>%
    group_by(inter, QuD, message, Qw) %>%
    mutate(sum_equiv = sum(prob)) %>%
    group_by(world, QuD, message) %>%
    summarise(util = sum(P_i(inter) * (log(sum_equiv) - cost(message)))) %>%
    ungroup()
}


Ln <- function(n) {
  if (n == 0) {
    L0()
  } else {
    Sn(n) %>%
      mutate(prob = P_w(world) * P_Q(QuD) * prob) %>%
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
    Ln(n - 1) %>%
      mutate(Qw = Q_equiv(QuD, world)) %>%
      group_by(QuD, message, Qw) %>%
      mutate(util = log(sum(prob)) - cost(message)) %>%
      ungroup() %>%
      arrange(world, QuD)
  }
}

Sn <- function(n) {
  Un(n) %>%
    transmute(world, QuD, message, prob = exp(lambda * util)) %>%
    group_by(world, QuD) %>%
    mutate(prob = prob / sum(prob)) %>%
    ungroup() %>%
    arrange(world, QuD)
}

U1() %>% print(n = 54)
Sn(3) %>% print(n = 54)

Ln(3) %>% print(n = 54)
Ln(1) %>% print(n = 54)
