# Internal feather reader.
#
# Every feather read in this package goes through here. Two reasons.
#
# 1. ALTREP. Arrow converts columns to ALTREP vectors by default, which defer
#    materialisation. Slicing those - which is what any dplyr::filter() on the
#    result does - has been observed to fail with
#
#        Error in `vec_slice_altrep()`: negative length vectors are not allowed
#
#    That is R refusing an allocation whose size arrived negative: a
#    memory-layout symptom, not a data one. It surfaces far from its cause,
#    deep inside a user's dplyr chain, on data that is perfectly valid. Almost
#    nothing about the message points at the read that produced it.
#
#    Observed 17 Sep 2026 on R 4.5.2 / Windows, filtering a 2.8M-row spreads
#    frame in the stat arb notebooks.
#
# 2. The `feather` package is deprecated. It was superseded by `arrow` years
#    ago, and critically its ALTREP is its own - so `arrow.use_altrep` does not
#    reach it. Reads had to move to arrow for the option above to apply at all.
#
# The option is set and restored locally rather than globally: a package should
# not quietly change a user's options, and a member who wants ALTREP elsewhere
# should keep it.

rw_read_feather <- function(path, ...) {
  old <- options(arrow.use_altrep = FALSE)
  on.exit(options(old), add = TRUE)
  arrow::read_feather(path, ...)
}
