
# Helper
value <- function(x = 0) {

  nv <- new_value(x)
  validate_value(nv)

}


# Validator
validate_value <- function(v) {

  vctrs::obj_check_vector(v)
  l <- unclass(v)
  vctrs::vec_check_size(l$data, 1L, arg = 'data')
  v

}


# Constructor
new_value <- function(x, children = list(), op = '') {

  vctrs::new_vctr(
    list(
      data = x,
      prev = unique(children),
      op = op,
      backward = \(x) NULL
    ),
    class = 'rmg_value'
  )

}


# Set children
# This function does not really change the field in-place.
# Should use RC to avoid copying object.
# See https://stackoverflow.com/questions/21243359/modify-s3-object-without-returning-it
`children<-` <- function(v, value) {
  UseMethod('children<-')
}

`children<-.rmg_value` <- function(v, value) {

  v$prev <- unique(value)
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
