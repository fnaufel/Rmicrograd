library(vctrs)


# Helper
value <- function(x = 0) {

  nv <- new_value(x)
  validate_value(nv)

}


# Validator
validate_value <- function(x) {

  data <- unclass(x)

  if (length(data) > 1) {
    stop(
      'Data must be a scalar, not a vector',
      call. = FALSE
    )
  }

  if (!is.numeric(data)) {
    stop(
      'Data must be numeric.',
      call. = FALSE
    )
  }

  x

}


# Constructor
new_value <- function(x, children = list(), op = '') {

  structure(
    x,
    prev = unique(children),
    op = op,
    backward = function(x) NULL,
    class = 'rmg_value'
  )

}


# Set children
set_children <- function(v, children) {
  UseMethod('set_children')
}

set_children.rmg_value <- function(v, children) {

  children <- unique(children)
  attr(v, 'prev') <- children
  v

}

# `+` <- function(x, y) {
#
#   if (inherits(x, 'mg_value') && inherits(y, 'mg_value')) {
#     new_value()
#
#   }
#
# }
