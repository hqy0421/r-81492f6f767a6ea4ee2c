# Ordered one-to-one pairing by business key
scans <- data.frame(
  route = c("r1", "r1", "r1", "r1", "r2"),
  ts = c(10, 12, 12, 20, 5),
  qty = c(1, 2, 2, 3, 9),
  rid = c(1L, 2L, 3L, 4L, 5L)
)
readings <- data.frame(
  route = c("r1", "r1", "r1"),
  ts = c(11, 13, 25),
  qty = c(1, 4, 8),
  rid = c(10L, 11L, 12L)
)

res <- pair_join(scans, readings, by = "route", time_by = "ts", value_by = "qty", tolerance = 2)

# -- Output structure -----------------------------------------------------------
expect_equal(
  names(res),
  c("pairs", "unpaired_x", "unpaired_y", "summary"),
  info = "The result is a list of pairs, both unpaired frames and a summary"
)
expect_equal(
  names(res$pairs),
  c("route.x", "ts.x", "qty.x", "rid.x", "route.y", "ts.y", "qty.y", "rid.y", ".time_diff", ".value_diff"),
  info = "Pairs carry the original columns of both sides plus the two differences"
)
expect_equal(
  names(res$summary),
  c("route", "n_pairs", "n_unpaired_x", "n_unpaired_y", "total_time_diff", "total_value_diff"),
  info = "Summary columns"
)

# -- Maximise the number of pairs, then minimise the total time difference ------
expect_equal(res$pairs$rid.x, c(1L, 2L), info = "The two closest rows pair first")
expect_equal(res$pairs$rid.y, c(10L, 11L))
expect_equal(res$pairs$.time_diff, c(1, 1), info = "Signed time difference y - x")
expect_equal(res$pairs$.value_diff, c(0, 2), info = "Signed value difference y - x")
expect_equal(nrow(res$pairs), 2L)

expect_equal(
  res$unpaired_x$rid.x,
  c(3L, 4L, 5L),
  info = "Unpaired x rows keep their original order"
)
expect_equal(res$unpaired_y$rid.y, 12L)
expect_true(all(is.na(res$unpaired_x$ts.y)), info = "The other side's columns are missing, not zero")
expect_true(all(is.na(res$unpaired_y$qty.x)))

# -- Groups without pairs still appear, with missing totals ---------------------
expect_equal(res$summary$route, c("r1", "r2"))
r1_sum <- res$summary[res$summary$route == "r1", ]
r2_sum <- res$summary[res$summary$route == "r2", ]
expect_equal(r1_sum$n_pairs, 2L)
expect_equal(r1_sum$n_unpaired_x, 2L)
expect_equal(r1_sum$n_unpaired_y, 1L)
expect_equal(r1_sum$total_time_diff, 2)
expect_equal(r1_sum$total_value_diff, 2)
expect_equal(r2_sum$n_pairs, 0L)
expect_true(is.na(r2_sum$total_time_diff), info = "Missing totals are NA, not zero")
expect_true(is.na(r2_sum$total_value_diff))

# Every row is paired or unpaired exactly once
expect_equal(
  sort(c(res$pairs$rid.x, res$unpaired_x$rid.x)),
  scans$rid
)
expect_equal(
  sort(c(res$pairs$rid.y, res$unpaired_y$rid.y)),
  readings$rid
)

# -- Physical duplicates are not deduplicated by value --------------------------
dup_x <- data.frame(g = "a", t = c(1, 1, 5), v = c(10, 10, 50), id = 1:3)
dup_y <- data.frame(g = "a", t = c(1, 2), v = c(11, 20), id = 11:12)
dup_res <- pair_join(dup_x, dup_y, by = "g", time_by = "t", value_by = "v", tolerance = 1)
expect_equal(nrow(dup_res$pairs), 2L, info = "Both duplicate rows keep their identity and can pair")
expect_equal(sort(c(dup_res$pairs$id.x, dup_res$unpaired_x$id.x)), 1:3)
expect_equal(anyDuplicated(dup_res$pairs$id.x), 0L, info = "One-to-one pairing")
expect_equal(anyDuplicated(dup_res$pairs$id.y), 0L)

# -- The optimum does not depend on the order of the rows -----------------------
set.seed(7)
shuffled <- pair_join(
  dup_x[sample(nrow(dup_x)), , drop = FALSE],
  dup_y[sample(nrow(dup_y)), , drop = FALSE],
  by = "g", time_by = "t", value_by = "v", tolerance = 1
)
expect_equal(nrow(shuffled$pairs), nrow(dup_res$pairs))
expect_equal(
  sum(dup_res$pairs$.time_diff),
  sum(shuffled$pairs$.time_diff),
  info = "The minimal total time difference is invariant to row order"
)
expect_equal(setequal(dup_res$pairs$id.x, shuffled$pairs$id.x), TRUE)
expect_equal(setequal(dup_res$pairs$id.y, shuffled$pairs$id.y), TRUE)

# -- Missing business keys can never match --------------------------------------
key_x <- data.frame(g = c("a", NA, "a"), t = c(1, 1, 3), v = c(1, 2, 3), id = 1:3)
key_y <- data.frame(g = c("a", NA, "a"), t = c(1, 2, 3), v = c(10, 20, 30), id = 4:6)
key_res <- pair_join(key_x, key_y, by = "g", time_by = "t", value_by = "v", tolerance = 0)
expect_equal(nrow(key_res$pairs), 2L)
expect_true(!anyNA(key_res$pairs$g.x))
expect_true(is.na(tail(key_res$summary$g, 1)), info = "The missing-key group appears in the summary")
na_group <- tail(key_res$summary, 1)
expect_equal(na_group$n_pairs, 0L)
expect_equal(na_group$n_unpaired_x, 1L)
expect_equal(na_group$n_unpaired_y, 1L)
expect_true(is.na(na_group$total_time_diff))
expect_true(is.na(na_group$total_value_diff))

# -- Missing times can never match and are not taken as zero --------------------
time_x <- data.frame(t = c(NA, 5, 0), v = c(1, 2, 3), id = 1:3)
time_y <- data.frame(t = c(NA, 5, 0), v = c(10, 20, 30), id = 4:6)
time_res <- pair_join(time_x, time_y, time_by = "t", value_by = "v", tolerance = 0)
expect_equal(nrow(time_res$pairs), 2L)
expect_true(!anyNA(time_res$pairs$t.x))
expect_true(0 %in% time_res$pairs$t.x, info = "A real zero pairs; an NA does not")
expect_equal(nrow(time_res$unpaired_x), 1L)
expect_true(is.na(time_res$unpaired_x$t.x))

# -- Empty inputs keep the column types -----------------------------------------
empty_x <- data.frame(g = character(0), t = numeric(0), v = integer(0), id = integer(0))
empty_y <- data.frame(g = character(0), t = numeric(0), v = integer(0), id = integer(0))
empty_res <- pair_join(empty_x, empty_y, by = "g", time_by = "t", value_by = "v", tolerance = 1)
expect_equal(nrow(empty_res$pairs), 0L)
expect_equal(nrow(empty_res$unpaired_x), 0L)
expect_equal(nrow(empty_res$unpaired_y), 0L)
expect_equal(nrow(empty_res$summary), 0L)
expect_equal(typeof(empty_res$pairs$g.x), "character")
expect_equal(typeof(empty_res$pairs$t.x), "double")
expect_equal(typeof(empty_res$pairs$id.y), "integer")
expect_equal(typeof(empty_res$pairs$.time_diff), "double")
expect_equal(typeof(empty_res$summary$n_pairs), "integer")
expect_equal(typeof(empty_res$summary$total_value_diff), "double")

one_empty <- pair_join(key_x, empty_y, by = "g", time_by = "t", value_by = "v", tolerance = 1)
expect_equal(nrow(one_empty$pairs), 0L)
expect_equal(nrow(one_empty$unpaired_x), 3L)
expect_equal(sum(one_empty$summary$n_unpaired_x), 3L)
expect_true(all(is.na(one_empty$summary$total_time_diff)))

# -- The pairing drives the native verbs ----------------------------------------
work <- res$pairs |>
  mutate(abs_td = abs(.time_diff), net = .value_diff) |>
  group_by(route.x) |>
  summarise(matches = n(), total_td = sum(abs_td), total_vd = sum(net), .groups = "drop")
expect_equal(work$matches, 2L)
expect_equal(work$total_td, 2)
expect_equal(work$total_vd, 2)
expect_equal(group_vars(work), character(0))

# -- Different column names, suffixes and date-times ----------------------------
px <- data.frame(
  grp = "a",
  ts = as.POSIXct(c("2024-01-01 00:00:00", "2024-01-01 00:00:05"), tz = "UTC"),
  amount = c(1.5, 2.5)
)
py <- data.frame(
  route = "a",
  scan_ts = as.POSIXct(c("2024-01-01 00:00:02", "2024-01-01 00:00:20"), tz = "UTC"),
  qty = c(10, 20)
)
p_res <- pair_join(
  px, py,
  by = c("grp" = "route"), time_by = c("ts" = "scan_ts"), value_by = c("amount" = "qty"),
  tolerance = 3
)
expect_equal(nrow(p_res$pairs), 1L)
expect_equal(inherits(p_res$pairs$ts, "POSIXct"), TRUE)
expect_equal(inherits(p_res$pairs$scan_ts, "POSIXct"), TRUE)
expect_equal(inherits(p_res$pairs$.time_diff, "difftime"), FALSE, info = "Time diffs are plain numeric seconds")
expect_equal(p_res$pairs$.time_diff, 2)
expect_equal(p_res$pairs$.value_diff, 10 - 1.5)

date_x <- data.frame(d = as.Date(c("2024-01-01", "2024-01-10")), v = 1:2)
date_y <- data.frame(d = as.Date(c("2024-01-03", "2024-01-25")), v = 3:4)
date_res <- pair_join(date_x, date_y, time_by = "d", value_by = "v", tolerance = 3)
expect_equal(nrow(date_res$pairs), 1L)
expect_equal(inherits(date_res$pairs$d.x, "Date"), TRUE)
expect_equal(date_res$pairs$.time_diff, 2, info = "Date differences are in days")

shared <- pair_join(
  data.frame(g = "a", t = 1, v = 1),
  data.frame(g = "a", t = 1, v = 2, extra = 9),
  by = "g", time_by = "t", value_by = "v", tolerance = 0
)
expect_equal(
  names(shared$pairs),
  c("g.x", "t.x", "v.x", "g.y", "t.y", "v.y", "extra", ".time_diff", ".value_diff")
)

# -- Validation -----------------------------------------------------------------
expect_error(
  pair_join(scans, readings, by = "route", time_by = "ts", value_by = "qty", tolerance = -1),
  pattern = "tolerance"
)
expect_error(
  pair_join(scans, readings, by = "route", time_by = "ts", value_by = "qty", tolerance = c(1, 2)),
  pattern = "tolerance"
)
expect_error(
  pair_join(scans, readings, by = "nope", time_by = "ts", value_by = "qty", tolerance = 1),
  pattern = "Join columns"
)
expect_error(
  pair_join(list(a = 1), readings, time_by = "ts", value_by = "qty", tolerance = 1),
  pattern = "data.frame"
)

# -- The alias is the same function ---------------------------------------------
expect_equal(ordered_pair_join, pair_join)
