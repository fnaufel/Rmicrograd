
# Tests -------------------------------------------------------------------

v1 <- Value$new(10)
v2 <- Value$new(20)
v3 <- Value$new(30)

# Expressions --------------------------------------------------------------

e <- v1 + v2 * v3
e

e <- (v1 + v1) * (v2 + v3)
e

# Addition ----------------------------------------------------------------

vsum <- v1 + v2
vsum$clear_grads()
vsum$grad <- 5
vsum$backward()

v1
v2
vsum

vmult <- v1 * v2
vmult$clear_grads()
vmult$grad <- 5
vmult$backward()

# Multiplication ----------------------------------------------------------

v1
v2
vmult
