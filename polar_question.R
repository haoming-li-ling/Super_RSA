source("synthesis.R")

questions <- c("polNPsg", "polNPpl", "pol1+", "pol!1")

q_to_decl <- function(question) {
  case_when(
    question == "polSg" ~ "NPsg",
    question == "polPl" ~ "NPpl",
    question == "pol1+" ~ "1+",
    question == "pol!1" ~ "!1"
  )
}

viable <- function(partition, QuD) {
  all(partition[[1]] <= QuD[[1]]) & all(partition[[2]] <= QuD[[2]])
}

QL0 <- function() {
  expand_grid(
    QuD = QuDs,
    question = questions,
    NPsg = LitExh,
    nNPsg = LitExh,
    NPpl = LitExh,
    nNPpl = LitExh,
  ) %>%
    rowwise() %>%
    mutate(prob = {
      NPsgw <- interprs[["NPsg"]][[NPsg]](worlds) %T>% print()
      nNPsgw <- interprs[["nNPsg"]][[nNPsg]](worlds) %T>% print()
      NPplw <- interprs[["NPsg"]][[NPpl]](worlds) %T>% print()
      nNPplw <- interprs[["NPsg"]][[nNPpl]](worlds) %T>% print()
      question
    }) %>%
    mutate() -> dt
}
