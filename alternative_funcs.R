U1_gen <- function(Q, u) {
  L0_i_list <- interpretations %>%
    map(\(i) L0_gen(Q, u, i))
  # utilities <- worlds %>%
  #   map_vec(\(w) {
  #     L0_i_list %>%
  #       imap_vec(\(dt, i) {
  #         Q_equiv(Q, w) %>%
  #           map_vec(\(v) dt[world == v, prob]) %>%
  #           sum() %>%
  #           {
  #             P_i(i) * log(.)
  #           }
  #       }) %>%
  #       sum()
  #   })
  # data.table(world = worlds, util = utilities)
  data.table(world = worlds) %>%
    rowwise() %>%
    mutate(util = {
      ec <- Q_equiv(Q, world)
      L0_i_list %>%
        imap_vec(\(dt, i) {
          dt %>%
            filter(world %in% ec) %>%
            pull(prob) %>%
            sum() %>%
            {
              P_i(i) * log(.)
            }
        }) %>%
        sum()
    }) %>%
    ungroup()
}

Un_gen <- function(n, Q, u) {
  if (n == 1) {
    U1_gen(Q, u)
  } else {
    Ln_dt <- Ln_gen(n - 1, Q, u)
    Ln_dt %>%
      rowwise() %>%
      mutate(util = {
        ec <- Q_equiv(Q, world)
        Ln_dt %>%
          filter(world %in% ec) %>%
          pull(prob) %>%
          sum() %>%
          {
            log(.) - cost(u)
          }
      }) %>%
      ungroup() %>%
      select(world, util)

    # Ln_dt <- Ln_gen(n - 1, Q, u)
    # Ln_dt %>%
    #   rowwise() %>%
    # mutate(util = Q_equiv(Q, world) %>%
    #   map_vec(\(v) {
    #     Ln_dt %>%
    #       filter(world == v) %>%
    #       pull(prob)
    #   }) %>%
    #   sum() %>%
    #   log() %>%
    #   {
    #     . - cost(u)
    #   }) %>%
    # ungroup() %>%
    # select(world, util)

    # Ln_dt <- Ln_gen(n - 1, Q, u)
    # utilities <- worlds %>%
    #   map_vec(\(w) {
    #     Q_equiv(Q, w) %>%
    #       map_vec(\(v) Ln_dt[world == v, prob]) %>%
    #       sum() %>%
    #       log() %>%
    #       {
    #         . - cost(u)
    #       }
    #   })
    # data.table(world = worlds, util = utilities)
  }
}