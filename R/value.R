Class <- R6::R6Class


# Value class -------------------------------------------------------------

#' @export
Value <- Class(
  'Value',
  list(
    data = 0,
    prev = list(),
    op = '',
    grad = 0,
    # To be assigned depending on op
    backward_ = NULL,
    # Performs topological sorting and calls backward_ on self
    backward = function() {
      topo <- list()
      visited <- list()

      # TODO: fix this: topo and visited have only one element
      build_topo <- function(v) {

        browser()

        # The problem is that the following test always returns FALSE!
        # I.e., R thinks that v has already been visited, even when it has not!
        # Is it because I did not implement equality for Value objects?
        # Too much work. Go to Julia.
        if (!(list(v) %in% visited)) {
          visited <<- unique(append(visited, list(v)))
          for (child in v$prev) {
            build_topo(child)
          }
          topo <<- append(topo, list(v))
        }
      }

      build_topo(self)
      self$grad <- 1
      for (v in rev(topo)) {
        v$backward_()
      }
    },

    # Constructor
    initialize = function(
      data = 0,
      prev = list(),
      op = '',
      backward_ = \() NULL
    ) {
      stopifnot('data must be numeric' = is.numeric(data))
      stopifnot('data must be length 1' = length(data) == 1)
      self$data = data
      self$prev = prev
      self$op = op
      self$backward_ = backward_
    },

    # ReLU method (calls function that returns new Value object)
    relu = function() {
      relu(self)
    },

    # Clear this node's and all ancestors' gradients
    clear_grads = function() {
      self$grad <- 0
      for (p in self$prev) p$clear_grads()
    },

    # Print info on this node and its ancestors
    print = function(...) {
      s <- self$print_helper()
      cat(s)
      invisible(self)
    },

    # Print helper
    print_helper = function(v = self, indent = '') {
      # This node's fields
      s <- paste0(
        indent, ' |\\------------\n',
        indent, ' |  data: ', v$data, '\n',
        indent, ' |  grad: ', v$grad, '\n',
        indent, ' |  op:   ', v$op  , '\n'
      )
      # Ancestors
      s2 <- ''
      if (length(v$prev) > 0) {
        s <- paste0(s, indent, ' |  prev ', '\n')
        indent <- paste0(indent, ' | ')
        s2 <- paste0(
          s2,
          sapply(v$prev, v$print_helper, indent),
          collapse = ''
        )
      }
      # Return his node's fields and ancestors
      paste0(s, s2, collapse = '')
    }
  )
)


# Operations --------------------------------------------------------------

# Addition ----------------------------------------------------------------
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
  out$backward_ <- \() {
    v1$grad <- v1$grad + out$grad
    v2$grad <- v2$grad + out$grad
  }
  out

}

# Subtraction and negation ------------------------------------------------
#' @export
`-.Value` = function(v1, v2 = NULL) {

  if (is.null(v2)) {
    v1 * (-1)
  } else {
    v1 + (-v2)
  }

}

# Multiplication ----------------------------------------------------------
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
  out$backward_ <- \() {
    v1$grad <- v1$grad + v2$data * out$grad
    v2$grad <- v2$grad + v1$data * out$grad
  }
  out

}

# Division ----------------------------------------------------------------
#' @export
`/.Value` = function(v1, v2) {

  v1 * v2^(-1)

}

# Pow ---------------------------------------------------------------------
#' @export
`^.Value` = function(v1, v2) {

  if (!inherits(v1, 'Value')) {
    v1 <- Value$new(v1)
  }
  stopifnot('exponent must be a number' = is.numeric(v2))

  out <- Value$new(
    v1$data^v2,
    prev = list(v1),
    op = paste0('^', v2)
  )
  out$backward_ <- \() {
    v1$grad <- v1$grad + v2 * v1$data^(v2 - 1) * out$grad
  }
  out

}

# ReLU --------------------------------------------------------------------
#' @export
relu = function(v1) {

  if (!inherits(v1, 'Value')) {
    v1 <- Value$new(v1)
  }

  out <- Value$new(
    max(0, v1$data),
    prev = list(v1),
    op = 'ReLU'
  )
  out$backward_ <- \() {
    v1$grad <- v1$grad + ifelse(v1$data <= 0, 0, 1) * out$grad
  }
  out

}


