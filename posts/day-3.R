library(aocr)
library(tidyverse)

l_init <-
  aoc_get_data_as_tibble(2023, 3) |>
  unlist() %>%
  str_split("")

.x <-
  l_init[[2]]

l_d <-
  l_init %>%
  imap(~ {
    str_loc <- str_which(.x, "[0-9]")
    loc_value <- tibble(
      x = str_loc,
      value = .x[str_loc]
    )

    d_values_init <-
      tibble(
        n = as.numeric(.x),
        na_tst = !is.na(n),
        n2 = ifelse(is.na(n), 0, n),
        n3 = as.numeric(!is.na(n)) + 1,
        is_n = abs(n3 - lag(n3, default = 0)),
        tst = cumsum(is_n)
      ) %>%
      mutate(
        x = row_number()
      ) %>%
      filter(!is.na(n)) %>%
      group_by(tst) %>%
      mutate(
        whole_num = as.numeric(paste0(n, collapse = "")),
        y = .y
      ) %>%
      ungroup() %>%
      select(
        -(n:is_n)
      )

    # d_values_init <-
    #   loc_value %>%
    #   mutate(
    #     num_group = cumsum(x - lag(x, default = 0) > 2)
    #   ) %>%
    #   group_by(num_group) %>%
    #   mutate(
    #     whole_num = paste0(value, collapse = ""),
    #     y = .y
    #   ) %>%
    #   ungroup()


    d_values <-
      bind_rows(
        d_values_init,
        d_values_init %>%
          mutate(x = x - 1),
        d_values_init %>%
          mutate(x = x + 1)
      ) %>%
      complete(
        nesting(
          # row_id,
          x,
          tst,
          # num_group,
          whole_num
        ),
        y = c(.y - 1, .y + 1)
      ) %>%
      filter(
        x > 0,
        y > 0
      ) %>%
      mutate(
        i = .y
      ) %>%
      dplyr::select(
        i,
        # row_id,
        tst,
        whole_num,
        x,
        y
      ) %>%
      dplyr::arrange(x, y)

    sym_x <- str_which(.x, "[^[:alnum:].]")

    d_sym <- tibble(
      x = sym_x,
      y = .y
    )

    result <- list(
      d_values = d_values,
      d_sym = d_sym
    )

    return(result)
  })

l_combine <-
  l_d %>%
  transpose() %>%
  map(bind_rows)

l_combine$d_values %>%
  distinct() %>%
  semi_join(
    y = l_combine$d_sym,
    by = c("x", "y")
  ) %>%
  arrange(y, x) %>%
  # dplyr::distinct(
  #   i,
  #   whole_num
  # ) %>%
  summarize(
    x = sum(whole_num)
  )

# Part 2
l_d <-
  l_init %>%
  imap(~ {
    str_loc <- str_which(.x, "[0-9]")
    loc_value <- tibble(
      x = str_loc,
      value = .x[str_loc]
    )

    d_values_init <-
      tibble(
        n = as.numeric(.x),
        na_tst = !is.na(n),
        n2 = ifelse(is.na(n), 0, n),
        n3 = as.numeric(!is.na(n)) + 1,
        is_n = abs(n3 - lag(n3, default = 0)),
        tst = cumsum(is_n)
      ) %>%
      mutate(
        x = row_number()
      ) %>%
      filter(!is.na(n)) %>%
      group_by(tst) %>%
      mutate(
        whole_num = as.numeric(paste0(n, collapse = "")),
        y = .y
      ) %>%
      ungroup() %>%
      select(
        -(n:is_n)
      )

    # d_values_init <-
    #   loc_value %>%
    #   mutate(
    #     num_group = cumsum(x - lag(x, default = 0) > 2)
    #   ) %>%
    #   group_by(num_group) %>%
    #   mutate(
    #     whole_num = paste0(value, collapse = ""),
    #     y = .y
    #   ) %>%
    #   ungroup()


    d_values <-
      bind_rows(
        d_values_init,
        d_values_init %>%
          mutate(x = x - 1),
        d_values_init %>%
          mutate(x = x + 1)
      ) %>%
      complete(
        nesting(
          # row_id,
          x,
          tst,
          # num_group,
          whole_num
        ),
        y = c(.y - 1, .y + 1)
      ) %>%
      filter(
        x > 0,
        y > 0
      ) %>%
      mutate(
        i = .y
      ) %>%
      dplyr::select(
        i,
        # row_id,
        tst,
        whole_num,
        x,
        y
      ) %>%
      dplyr::arrange(x, y)

    sym_x <- str_which(.x, "[^[:alnum:].]")

    d_sym <- tibble(
      x = sym_x,
      y = .y,
      symbol = .x[sym_x]
    ) %>%
      filter(symbol == "*") %>%
      mutate(
        sym_id = row_number()
      )

    result <- list(
      d_values = d_values,
      d_sym = d_sym
    )

    return(result)
  })

l_combine <-
  l_d %>%
  transpose() %>%
  map(bind_rows)

l_combine$d_sym %>%
  distinct() %>%
  inner_join(
    y = l_combine$d_values,
    by = c("x", "y")
  ) %>%
  distinct() %>%
  arrange(y, x) %>%
  dplyr::count(
    y,
    x,
    sym_id,
    sort = TRUE
  ) %>%
  filter(n > 1) %>%
  left_join(
    y = l_combine$d_values %>%
      select(x, y, whole_num)
  ) %>%
  # dplyr::distinct(
  #   i,
  #   whole_num
  # ) %>%
  group_by(sym_id, x, y) %>%
  summarize(
    product = prod(whole_num),
    .groups = "drop"
  ) %>%
  summarize(
    final_sum = sum(product)
  )
