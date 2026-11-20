#' Ordered One-to-one Pairing by Business Key
#'
#' @description
#' `pair_join()` pairs the rows of two `data.frame`s within a time tolerance, as a one-to-one, non-crossing
#' sequence alignment. Within each business key group, the rows of each side are ordered by their time, ties being
#' broken by their stable row identifier, and paired so that:
#'
#' * every row is paired at most once, and the pairs do not cross: if row `i` of `x` is paired with row `j` of `y`,
#'   no earlier row of `x` can be paired with a later row of `y`;
#' * the times of a pair differ by at most `tolerance`;
#' * the number of pairs is maximised first, and among pairings with the most pairs, the total absolute time
#'   difference is minimised.
#'
#' Because the rows are also ordered by their stable identifier, physically duplicated rows, which have identical
#' values but different identifiers, are never collapsed: each is paired (or left unpaired) on its own. Rows whose
#' business key or time is missing can never be paired and are reported as unpaired.
#'
#' @param x,y The `data.frame`s to pair, taken as the left and right sides respectively.
#' @param by A character vector of the business key columns. Use a named vector to pair columns with different names,
#' e.g. `by = c("a" = "b")` matches `x$a` to `y$b`. `NULL`, the default, pairs the whole tables as one group. Rows
#' with a missing key are never paired.
#' @param time_by A length one character vector naming the time column. Use a named element, e.g.
#' `time_by = c("ts" = "scan_time")`, when the columns have different names in `x` and `y`.
#' @param value_by A length one character vector naming the business value column, used for the right-minus-left
#' value difference of each pair. Use a named element when the columns have different names in `x` and `y`.
#' @param tolerance `numeric(1)`. The maximum absolute time difference of a pair. Must be non-negative and of length
#' one. The time columns are cast to a common type first, so the tolerance is in the units of that type, e.g. seconds
#' for date-times and days for dates.
#' @param suffix `character(2)`. Suffixes added to disambiguate columns of `x` and `y` which share a name.
#'
#' @return A named `list` of four `data.frame`s:
#'
#' * `pairs`: every pair, with all the original columns of both sides, plus `.time_diff`, the signed difference
#'   `time(y) - time(x)`, and `.value_diff`, the signed difference `value(y) - value(x)`. The pairs are ordered by
#'   business key and then by the ordering of the rows.
#' * `unpaired_x`: the rows of `x` which were not paired, in their original order, with the columns of `y` filled
#'   with missing values.
#' * `unpaired_y`: the rows of `y` which were not paired, in their original order, with the columns of `x` filled
#'   with missing values.
#' * `summary`: one row per business key group, including groups without any pair, giving the group keys, the number
#'   of pairs (`n_pairs`), the numbers of unpaired rows (`n_unpaired_x`, `n_unpaired_y`), the total absolute time
#'   difference (`total_time_diff`) and the total right-minus-left value difference (`total_value_diff`). The totals
#'   are missing, rather than zero, for a group with no pairs. Rows whose business key is missing form a final group
#'   whose key values are all missing.
#'
#' Every output column keeps the type of its source column: missing values are never treated as zero.
#'
#' @examples
#' scans <- data.frame(
#'   route = c("r1", "r1", "r1", "r1", "r2"),
#'   ts    = c(10, 12, 12, 20, 5),
#'   qty   = c(1, 2, 2, 3, 9),
#'   rid   = c(1L, 2L, 3L, 4L, 5L)
#' )
#' readings <- data.frame(
#'   route = c("r1", "r1", "r1"),
#'   ts    = c(11, 13, 25),
#'   qty   = c(1, 4, 8),
#'   rid   = c(10L, 11L, 12L)
#' )
#' pair_join(
#'   scans, readings,
#'   by = "route", time_by = "ts", value_by = "qty", tolerance = 2
#' )
#'
#' @export
pair_join <- function(
  x, y, by = NULL, time_by, value_by, tolerance, suffix = c(".x", ".y")
) {
  check_is_dataframe(x)
  check_is_dataframe(y)
  tolerance <- check_pair_tolerance(tolerance)
  if (length(suffix) != 2L || !is.character(suffix) || anyNA(suffix)) {
    stop("`suffix` must be a character vector of length two without missing values.")
  }

  time_names <- pair_column_names(time_by, "time_by")
  value_names <- pair_column_names(value_by, "value_by")
  check_pair_columns(time_names, x, y)
  check_pair_columns(value_names, x, y)
  key_names <- if (is.null(by)) list(x = character(), y = character()) else join_key_names(by, names(x), names(y))
  check_pair_columns(key_names, x, y)

  # Cast the time and value columns to their common types, as the join keys are, so comparisons and differences are
  # well defined and the output carries the cast values.
  time_cast <- join_key_cast(
    .subset2(x, time_names$x), .subset2(y, time_names$y), time_names$x, mutating = TRUE
  )
  value_cast <- join_key_cast(
    .subset2(x, value_names$x), .subset2(y, value_names$y), value_names$x, mutating = TRUE
  )
  key_casts <- lapply(seq_along(key_names$x), function(i) {
    join_key_cast(
      .subset2(x, key_names$x[i]), .subset2(y, key_names$y[i]), key_names$x[i], mutating = TRUE
    )
  })

  n_x <- nrow(x)
  n_y <- nrow(y)
  x_time <- time_cast$x
  y_time <- time_cast$y
  x_value <- value_cast$x
  y_value <- value_cast$y

  # Group ids: rows with a missing business key take id NA and can never match
  if (length(key_names$x) == 0L) {
    # With no business keys, all rows form one group, but two empty inputs have no groups
    n_groups <- if (n_x + n_y == 0L) 0L else 1L
    x_gid <- rep(1L, n_x)
    y_gid <- rep(1L, n_y)
    has_na_group <- FALSE
  } else {
    key_cols <- lapply(key_casts, `[[`, "key")
    # `group_ids()` gives missing character keys an id of their own, so the rows which are missing a key are found
    # explicitly and the ids they occupy are removed before the ordinary groups are numbered
    key_na <- Reduce(`|`, lapply(key_cols, is.na))
    raw_ids <- group_ids(key_cols, n = n_x + n_y)
    present <- sort(unique(raw_ids[!key_na]))
    remap <- integer(max(raw_ids, 0L))
    remap[present] <- seq_along(present)
    ids <- remap[raw_ids]
    n_groups <- length(present)
    ids[key_na] <- NA_integer_
    x_gid <- ids[seq_len(n_x)]
    y_gid <- ids[n_x + seq_len(n_y)]
    has_na_group <- any(key_na)
  }

  x_data <- as.list(.subset(x))
  y_data <- as.list(.subset(y))
  x_data[match(time_names$x, names(x))] <- list(x_time)
  y_data[match(time_names$y, names(y))] <- list(y_time)
  x_data[match(value_names$x, names(x))] <- list(x_value)
  y_data[match(value_names$y, names(y))] <- list(y_value)
  x_data[match(key_names$x, names(x))] <- lapply(key_casts, `[[`, "x")
  y_data[match(key_names$y, names(y))] <- lapply(key_casts, `[[`, "y")

  # Unique internal names keep the two sides apart until the output names are applied
  x_internal <- paste0("..x..", names(x))
  y_internal <- paste0("..y..", names(y))
  names(x_data) <- x_internal
  names(y_data) <- y_internal

  pair_x <- integer()
  pair_y <- integer()
  # Difference columns are always plain numeric, even for date-time inputs whose subtraction gives a difftime
  pair_time_diff <- numeric()
  pair_value_diff <- numeric()
  group_stats <- vector("list", n_groups + has_na_group)

  for (g in seq_len(n_groups)) {
    xr <- which(x_gid == g)
    yr <- which(y_gid == g)
    solved <- solve_pairs(
      xr[order(.subset(x_time, xr), xr)],
      yr[order(.subset(y_time, yr), yr)],
      x_time, y_time, tolerance
    )
    if (length(solved$x) > 0L) {
      pair_x <- c(pair_x, solved$x)
      pair_y <- c(pair_y, solved$y)
      pair_time_diff <- c(pair_time_diff, pair_time_difference(
        .subset(y_time, solved$y), .subset(x_time, solved$x)
      ))
      pair_value_diff <- c(pair_value_diff, .subset(y_value, solved$y) - .subset(x_value, solved$x))
    }
    group_stats[[g]] <- pair_group_summary(
      g, xr, yr, solved, x_time, y_time, x_value, y_value, key_casts, key_names
    )
  }

  if (has_na_group) {
    xr <- which(is.na(x_gid))
    yr <- which(is.na(y_gid))
    group_stats[[n_groups + 1L]] <- pair_group_summary(
      NA_integer_, xr, yr, list(x = integer(), y = integer()),
      x_time, y_time, x_value, y_value, key_casts, key_names
    )
  }

  paired_x <- logical(n_x)
  paired_y <- logical(n_y)
  paired_x[pair_x] <- TRUE
  paired_y[pair_y] <- TRUE
  unpaired_x_rows <- which(!paired_x)
  unpaired_y_rows <- which(!paired_y)

  # The output columns, in the order of x followed by y, with the suffix rules of the mutating joins
  out_x_names <- suffix_clashes(names(x), names(y), suffix[1L])
  out_y_names <- suffix_clashes(names(y), names(x), suffix[2L])
  out_names <- c(out_x_names, out_y_names)

  pairs_frame <- build_pair_frame(
    x_data, y_data, pair_x, pair_y,
    pair_time_diff, pair_value_diff, out_names
  )
  unpaired_x_frame <- build_unpaired_frame(
    x_data, y_data, unpaired_x_rows, use_x = TRUE,
    n_rows = length(unpaired_x_rows), out_names = out_names
  )
  unpaired_y_frame <- build_unpaired_frame(
    x_data, y_data, unpaired_y_rows, use_x = FALSE,
    n_rows = length(unpaired_y_rows), out_names = out_names
  )
  summary_frame <- build_summary_frame(group_stats, key_names, key_casts, n_groups, has_na_group)

  list(
    pairs = pairs_frame,
    unpaired_x = unpaired_x_frame,
    unpaired_y = unpaired_y_frame,
    summary = summary_frame
  )
}

#' @rdname pair_join
#' @export
ordered_pair_join <- pair_join

#' Signed time difference as a plain numeric vector
#'
#' Subtracting two date-time vectors gives a `difftime`, whose units vary with the data; the difference is stripped to
#' its underlying numeric value in the common units of the cast columns, matching those of `tolerance`.
#' @noRd
pair_time_difference <- function(y_time, x_time) {
  res <- y_time - x_time
  if (inherits(res, "difftime")) as.numeric(res) else res
}

#' Validate the tolerance of a pairing
#' @noRd
check_pair_tolerance <- function(tolerance) {
  if (length(tolerance) != 1L || anyNA(tolerance) || !is.numeric(tolerance) || tolerance < 0) {
    stop("`tolerance` must be a non-negative numeric value of length one.")
  }
  as.double(tolerance)
}

#' Resolve the two column names of a `c(x = "y")` style argument
#' @noRd
pair_column_names <- function(arg, arg_name) {
  if (length(arg) != 1L || !is.character(arg) || anyNA(arg) || !nzchar(arg)) {
    stop(sprintf("`%s` must be a length one, non-missing character vector.", arg_name))
  }
  x_name <- names(arg)
  if (is.null(x_name) || !nzchar(x_name)) x_name <- unname(arg)
  list(x = x_name, y = unname(arg))
}

#' Check that pairing columns exist on both sides
#' @noRd
check_pair_columns <- function(names, x, y) {
  check_join_columns(names$x, base::names(x))
  check_join_columns(names$y, base::names(y))
}

#' Solve the ordered one-to-one pairing of two sorted sequences
#'
#' The rows are already ordered by `(time, stable row identifier)`. A sequence-alignment dynamic programme chooses,
#' for each pair of positions, to skip the row of `x`, skip the row of `y`, or pair them when their times are within
#' the tolerance. Each state stores the number of pairs and the total absolute time difference: the number is
#' maximised first, then the difference minimised, so pairing wins a tie against leaving both rows unpaired. Back
#' tracking recovers the chosen pairs. Both the ordering and the tie rule are deterministic, so the optimum does not
#' depend on the original order of the rows.
#'
#' @param x_rows,y_rows Integer vectors of row identifiers, ordered by time and identifier.
#' @param x_time,y_time The time columns, indexed by the identifiers.
#' @param tolerance The maximum absolute time difference of a pair.
#' @return A list with `x` and `y`, the paired identifiers in pairing order.
#' @noRd
solve_pairs <- function(x_rows, y_rows, x_time, y_time, tolerance) {
  n <- length(x_rows)
  m <- length(y_rows)
  if (n == 0L || m == 0L) return(list(x = integer(), y = integer()))

  # Only the previous row of the DP table is needed while filling it; back tracking needs every row
  counts <- matrix(0L, nrow = n + 1L, ncol = m + 1L)
  costs <- matrix(0, nrow = n + 1L, ncol = m + 1L)
  choices <- matrix(0L, nrow = n + 1L, ncol = m + 1L) # 1: skip x, 2: skip y, 3: pair

  for (i in seq_len(n)) {
    xi <- x_rows[i]
    t_x <- x_time[xi]
    for (j in seq_len(m)) {
      up <- list(count = counts[i, j + 1L], cost = costs[i, j + 1L], choice = 1L)
      left <- list(count = counts[i + 1L, j], cost = costs[i + 1L, j], choice = 2L)
      best <- if (up$count > left$count || (up$count == left$count && up$cost <= left$cost)) up else left

      gap <- abs(y_time[y_rows[j]] - t_x)
      if (!is.na(gap) && gap <= tolerance) {
        diag_count <- counts[i, j] + 1L
        diag_cost <- costs[i, j] + as.numeric(gap)
        if (diag_count > best$count || (diag_count == best$count && diag_cost < best$cost)) {
          best <- list(count = diag_count, cost = diag_cost, choice = 3L)
        }
      }
      counts[i + 1L, j + 1L] <- best$count
      costs[i + 1L, j + 1L] <- best$cost
      choices[i + 1L, j + 1L] <- best$choice
    }
  }

  pair_x <- integer(counts[n + 1L, m + 1L])
  pair_y <- integer(counts[n + 1L, m + 1L])
  k <- length(pair_x)
  i <- n
  j <- m
  while (i > 0L && j > 0L) {
    switch(
      choices[i + 1L, j + 1L],
      `1` = { i <- i - 1L },
      `2` = { j <- j - 1L },
      `3` = {
        pair_x[k] <- x_rows[i]
        pair_y[k] <- y_rows[j]
        k <- k - 1L
        i <- i - 1L
        j <- j - 1L
      }
    )
  }
  list(x = pair_x, y = pair_y)
}

#' Compute the summary row of one business key group
#' @noRd
pair_group_summary <- function(g, xr, yr, solved, x_time, y_time, x_value, y_value, key_casts, key_names) {
  n_pairs <- length(solved$x)
  if (n_pairs > 0L) {
    total_time <- sum(abs(pair_time_difference(
      .subset(y_time, solved$y), .subset(x_time, solved$x)
    )))
    total_value <- sum(.subset(y_value, solved$y) - .subset(x_value, solved$x))
  } else {
    total_time <- NA_real_
    total_value <- NA_real_
  }
  keys <- if (length(key_names$x) == 0L) list() else lapply(seq_along(key_casts), function(i) {
    col <- c(.subset2(key_casts[[i]], "x"), .subset2(key_casts[[i]], "y"))
    # Every row of the group shares the key; take it from the first row present on either side. The missing-key
    # group passes no rows and gets a missing value of the common key type.
    first_row <- c(xr, length(x_time) + yr)[1L]
    if (is.na(first_row)) col[NA_integer_] else col[first_row]
  })
  names(keys) <- key_names$x
  c(
    keys,
    list(
      n_pairs = n_pairs,
      n_unpaired_x = length(xr) - n_pairs,
      n_unpaired_y = length(yr) - n_pairs,
      total_time_diff = total_time,
      total_value_diff = total_value
    )
  )
}

#' Build the data frame of pairs with their differences
#' @noRd
build_pair_frame <- function(x_data, y_data, pair_x, pair_y, time_diff, value_diff, out_names) {
  cols <- c(
    lapply(x_data, vec_slice, i = pair_x),
    lapply(y_data, vec_slice, i = pair_y)
  )
  cols[[".time_diff"]] <- time_diff
  cols[[".value_diff"]] <- value_diff
  out <- new_data_frame(cols, length(pair_x))
  names(out) <- c(out_names, ".time_diff", ".value_diff")
  out
}

#' Build a data frame of the unpaired rows of one side
#'
#' The columns of the other side are present but entirely missing, with the type of that side.
#' @noRd
build_unpaired_frame <- function(x_data, y_data, rows, use_x, n_rows, out_names) {
  missing_x <- rep(NA_integer_, n_rows)
  missing_y <- rep(NA_integer_, n_rows)
  if (use_x) {
    cols <- c(
      lapply(x_data, vec_slice, i = rows),
      lapply(y_data, vec_slice, i = missing_y)
    )
  } else {
    cols <- c(
      lapply(x_data, vec_slice, i = missing_x),
      lapply(y_data, vec_slice, i = rows)
    )
  }
  out <- new_data_frame(cols, n_rows)
  names(out) <- out_names
  out
}

#' Combine the per-group summaries into one data frame
#' @noRd
build_summary_frame <- function(group_stats, key_names, key_casts, n_groups, has_na_group) {
  if (length(group_stats) == 0L) {
    cols <- c(
      pair_empty_keys(key_casts, key_names),
      list(
        n_pairs = integer(),
        n_unpaired_x = integer(),
        n_unpaired_y = integer(),
        total_time_diff = double(),
        total_value_diff = double()
      )
    )
    return(new_data_frame(cols, 0L))
  }
  metric_names <- c("n_pairs", "n_unpaired_x", "n_unpaired_y", "total_time_diff", "total_value_diff")
  cols <- list()
  for (name in key_names$x) cols[[name]] <- do.call(c, lapply(group_stats, `[[`, name))
  for (name in metric_names) cols[[name]] <- do.call(c, lapply(group_stats, `[[`, name))
  new_data_frame(cols, length(group_stats))
}

#' Zero-length key columns with the common type of the keys, for empty summaries
#' @noRd
pair_empty_keys <- function(key_casts, key_names) {
  cols <- lapply(key_casts, function(cast) vec_slice(cast$x, integer()))
  names(cols) <- key_names$x
  cols
}
