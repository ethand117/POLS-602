##Clear your environment
rm(list = ls())

#Packages
library(ggplot2)
library(dpylr)
set.seed(117)

#Problem 1 Set-up
var_x <- 2
var_z <- 3

#True Data Generating Process
tdgp <- function(n){
  z <- rnorm(n)
  x <- 0.3 * z + rnorm(n)
  e <- rexp(n, rate = 1) - 1
  y <- 1 + var_x * x + var_z * z + e
  data.frame(y, x, z)
}

#Data-set + Summary Table
observs <- 100
dta <- tdgp(observs)
reg1 <- lm(y ~ x + z, data = dta)
summary(reg1)

#1a.
mnt_car_coef <- function(formula, n = observs, trials = 500){
  out <- numeric(trials)
  ses <- numeric(trials)
  for (m in 1:trials){
    dta2 <- tdgp(n)
    f <- lm(formula, data = dta2)
    out[m] <- coef(f)["x"]
    ses[m] <- sqrt(diag(vcov(f)))["x"]
  }
  data.frame(b_x = out, se_x = ses)
}

mnt_car_full <- mnt_car_coef(y ~ x + z)
summary(mnt_car_full)

hist1 <- ggplot(mnt_car_full, aes(x = b_x)) +
  geom_histogram(aes(y = after_stat(density)), fill = "blue", color = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(mnt_car_full$b_x), sd = sd(mnt_car_full$b_x)), color = "green", linewidth = 1) +
  geom_vline(xintercept = var_x) +
  labs(title = "Sampling Distribution of Treatment Coefficient", subtitle = "Green = normal curve with same mean and deviation, line = true value", x= expression(hat(beta)[x]), y = "Density") +
  theme_bw()
hist1

#1b.
B <- 5000
boot_b <- numeric(B)
for (b in 1:B) {
  idx <- sample(seq_len(observs), size = observs, replace = TRUE)
  boot_b[b] <- coef(lm(y ~ x + z, data = dta[idx, ]))["x"]
}

#1c.
omit_fit <- lm(y ~ x, data = dta)
print(summary(omit_fit))
mnt_car_omit <- mnt_car_coef(y ~ x)

hist2 <- ggplot(mnt_car_omit, aes(x = b_x)) +
  geom_histogram(aes(y = after_stat(density)), fill = "blue", color = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(mnt_car_omit$b_x), sd = sd(mnt_car_omit$b_x)), color = "green", linewidth = 1) +
  geom_vline(xintercept = var_x) +
  labs(title = "Sampling Distribution of Treatment Coefficient", x= expression(hat(beta)[x]), y = "Density") +
  theme_bw()
hist2

#Problem 2 Data
#R's built in data-set
cars <- mtcars
cars$transmission <- factor(cars$am, levels = c(0, 1), labels = c("Automatic", "Manual"))

#2a.
tt <- t.test(mpg ~ transmission, data = cars, var.equal = FALSE)
print(tt)
obs_diff <- mean(cars$mpg[cars$transmission == "Manual"]) -
  mean(cars$mpg[cars$transmission == "Automatic"])

#2b.
lm_fit <- lm(mpg ~ transmission, data = cars)
print(summary(lm_fit))

#2c.
n_perm <- 500
perm_diffs <- numeric(n_perm)
for (i in 1:n_perm) {
  shuffled <- sample(cars$transmission)
  perm_diffs[i] <- mean(cars$mpg[shuffled == "Manual"]) -
    mean(cars$mpg[shuffled == "Automatic"])
}
                                                         
p2_perm <- ggplot(data.frame(perm_diffs), aes(x = perm_diffs)) +
  geom_histogram(fill = "magenta", colour = "white") +
  geom_vline(xintercept = c(-obs_diff, obs_diff), colour = "green3",linetype = "dashed") +
  geom_vline(xintercept = obs_diff, colour = "red4") +
  labs(title = "Permutation null distribution of the difference in mean mpg", subtitle = ("Solid red = observed; dashed = mirror image for the two-sided test"), x = "Difference in means (Manual - Automatic) under H0", y = "Count") +
  theme_bw()
p2_perm
