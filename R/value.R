
Class <- R6::R6Class

#' @export
Value <- Class(
  'Value',
  list(
    data = 0,
    prev = list(),
    op = '',
    backward = NULL,
    grad = 0,

    # Constructor
    initialize = function(
      data = 0,
      prev = list(),
      op = '',
      backward = \() NULL
    ) {
      stopifnot('data must be numeric' = is.numeric(data))
      stopifnot('data must be length 1' = length(data) == 1)
      self$data = data
      self$prev = prev
      self$op = op
      self$backward = backward
    },

    # print
    print = function(...) {
      cat('Value:\n')
      cat('  data: ', self$data, '\n')
      cat('  grad: ', self$grad, '\n')
      cat('  op:   ', self$op  , '\n')
      if (length(self$prev) > 0) {
        cat('  prev: ', '\n')
        print(self$prev)
      }
      invisible(self)
    }
  )
)


# Operations

#' @export
`+.Value` = function(v1, v2) {

  if (!inherits(v1, 'Value')) {
    v1 <- Value$new(v1)
  }
  if (!inherits(v2, 'Value')) {
    v2 <- Value$new(v2)
  }

  out <- Value$new(
    v1$data + v2$data,
    prev = list(v1, v2),
    op = '+'
  )
  out$backward <- \() {
    v1$grad <- v1$grad + out$grad
    v2$grad <- v2$grad + out$grad
  }
  out

}


#' @export
`*.Value` = function(v1, v2) {

  if (!inherits(v1, 'Value')) {
    v1 <- Value$new(v1)
  }
  if (!inherits(v2, 'Value')) {
    v2 <- Value$new(v2)
  }

  out <- Value$new(
    v1$data * v2$data,
    prev = list(v1, v2),
    op = '*'
  )
  out$backward <- \() {
    v1$grad <- v1$grad + v2$data * out$grad
    v2$grad <- v2$grad + v1$data * out$grad
  }
  out

}

# TODO: check and fix backward


# Tests -------------------------------------------------------------------

v1 <- Value$new(2)
v1$grad <- 1

v2 <- Value$new(3)
v2$grad <- 2

v3 <- v1 * v2
v3$grad <- 5
v3$backward()

v1
v2
v3
