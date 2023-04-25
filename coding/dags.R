## DAGs
## POLI 273

# DGP 1 (target rel: Tee --> Y)
U1 <- rnorm(1000)
U2 <- rnorm(1000)
X <- U1 + U2 + rnorm(1000) 
Tee <- X + rnorm(1000)
C <- X + Tee + rnorm(1000)
Y <- -1 * U1 + U2 + C + X + rnorm(1000)

# Biased regression
summary(lm(Y ~ Tee))

# Unbiased regression
summary(lm(Y ~ Tee + C + X))

# DGP 2 (target rel: Tee --> Y)
Tee <- rnorm(1000)
Y <- rnorm(1000)
C <- Tee + Y + rnorm(1000) # Collider
Z <- C + rnorm(1000) # Child of a Collider

# Unbiased regression
summary(lm(Y ~ Tee))

# Biased regression
summary(lm(Y ~ Tee + C))

# Biased regression
summary(lm(Y ~ Tee + Z))

# DGP 3 (target rel: Tee --> Y)
Tee <- rnorm(1000)
Y <- rnorm(1000)
C <- Tee - Y + rnorm(1000) # Collider
Z <- C + rnorm(1000) # Child of a Collider

# Unbiased regression
summary(lm(Y ~ Tee))

# Biased regression
summary(lm(Y ~ Tee + C))

# Biased regression
summary(lm(Y ~ Tee + Z))

# DGP 4 (target rel: X --> Y)
X <- rnorm(1000)
U <- rnorm(1000)
Z2 <- rnorm(1000)
Z1 <- U + X + Z2 + rnorm(1000)
Z3 <- U + Z2 + rnorm(1000)
Y <- Z3 + Z2 + rnorm(1000)

# Unbiased regression
summary(lm(Y ~ X))

# Unbiased regression
summary(lm(Y ~ X + Z2 + Z3))

# Unbiased regression
summary(lm(Y ~ X + Z2))

# Unbiased regression
summary(lm(Y ~ X + Z3))

# Biased regression (control for collider)
summary(lm(Y ~ X + Z1))

# Biased regression (control for collider)
# Not closing all the backdoors
summary(lm(Y ~ X + Z1 + Z2))

# Biased regression (control for collider)
# Not closing all the backdoors
summary(lm(Y ~ X + Z1 + Z3))

# Unbiased regression 
# (control for collider but then closing the backdoors)
summary(lm(Y ~ X + Z1 + Z2 + Z3))

# DGP 5 (target rel: Tee --> Y)
A <- rnorm(1000)
B <- rnorm(1000)
C <- A + B + rnorm(1000)
D <- B + rnorm(1000)
Tee <- -1 * B + rnorm(1000)
E <- Tee + D + rnorm(1000)
Fe <- Tee + rnorm(1000)
Y <- A + C + Fe + E + D + rnorm(1000)

# Regs?
summary(lm(Y ~ Tee + B))


# DGP 6 (target rel: Tee --> Y)
A <- rnorm(1000)
B <- rnorm(1000)
C <- A + B + rnorm(1000)
Tee <- A + C + rnorm(1000)
D <- B + rnorm(1000)
E <- Tee + rnorm(1000)
Y <- A + C + D + rnorm(1000)

# Regs?

# DGP 7 -- Random Assignment (target rel: Tee --> Y)
Z <- rnorm(1000)
U <- rnorm(1000)
Y <- Z + U + rnorm(1000)

# Unbiased
summary(lm(Y ~ Z))

# DGP 8 -- CIA (target rel: Tee --> Y)
U <- rnorm(1000)
X <- rnorm(1000)
Z <- X + rnorm(1000)
Y <- Z - 2*X + rnorm(1000)

# Biased
summary(lm(Y ~ Z))

# Unbiased
summary(lm(Y ~ Z + X))
