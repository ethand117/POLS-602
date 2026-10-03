##Clear your environment
rm(list = ls())

#Problem 1 Set-up
set.seed(255)
real_var <- 4
real_sd <- 2

#1a.
estimator <- function(x){
  n <- length(x)
  dev <- x - mean(x)
  div_n <- sum(dev^2)/n
  var_n <- var(x)
  sd_n <- sd(x)
  c(div_n = div_n, var_n = var_n, sd_n = sd_n)
}

sam10 <- rnorm(10, mean = 0, sd = real_sd)
estimator(sam10)
all.equal(estimator(sam10)[["div_n"]], var(sam10) * 9/10)

#1b.
sim_sam1000 <- function(n, reps = 1000){
  res <- t(replicate(reps, estimator(rnorm(n, 0, real_sd))))
  data.frame(res)
}

sim5 <- sim_sam1000(5)
head(sim5)

d_div  <- density(sim5$div_n)
d_var <- density(sim5$var_n)

plot(d_div, col = "red", lwd = 2, xlim = c(0, 20),
     ylim = c(0, max(d_div$y, d_var$y)),
     main = "Sampling distributions of variance estimators (n = 5)",
     xlab = "Estimated variance")
lines(d_var, col = "blue", lwd = 2)
abline(v = real_var, lwd = 2, lty = 1)                    
abline(v = mean(sim5$div_n),  col = "red",  lty = 2)
abline(v = mean(sim5$var_n), col = "blue", lty = 2)  
legend("topright", bty = "n",
       legend = c("Divide by n", "Divide by n-1", "True variance (4)"),
       col = c("red", "blue", "black"), lwd = 2)

mean(sim5$div_n)  
mean(sim5$var_n)

#1c.
sim3 <- sim_sam1000(3)
sim10 <- sim_sam1000(10)
sim30 <- sim_sam1000(30)
sim100 <- sim_sam1000(100)

mean(sim3$div_n)   - 4
mean(sim5$div_n)   - 4
mean(sim10$div_n)  - 4
mean(sim30$div_n)  - 4
mean(sim100$div_n) - 4

mean(sim3$var_n)   - 4
mean(sim5$var_n)   - 4
mean(sim10$var_n)   - 4
mean(sim30$var_n)   - 4
mean(sim100$var_n)   - 4

samp_size <- c(3, 5, 10, 30, 100)

bias_div_n <- c(mean(sim3$div_n)   - 4,
                mean(sim5$div_n)   - 4,
                mean(sim10$div_n)  - 4,
                mean(sim30$div_n)  - 4,
                mean(sim100$div_n) - 4)

bias_var_n <- c(mean(sim3$var_n)   - 4,
                mean(sim5$var_n)   - 4,
                mean(sim10$var_n)  - 4,
                mean(sim30$var_n)  - 4,
                mean(sim100$var_n) - 4)

plot(samp_size, bias_div_n, type = "b", col = "red", lwd = 2,
     ylim = range(c(bias_div_n, bias_var_n)),   # make room for both lines
     xlab = "Sample size (n)", ylab = "Estimated bias",
     main = "Bias of variance estimators by sample size")
lines(samp_size, bias_var_n, type = "b", col = "blue", lwd = 2)
legend("bottomright", legend = c("Divide by n", "Divide by n-1"),
       col = c("red", "blue"), lwd = 2, bty = "n")

#1d.
bias_sd <- c(mean(sim3$sd_n)   - 2,
             mean(sim5$sd_n)   - 2,
             mean(sim10$sd_n)  - 2,
             mean(sim30$sd_n)  - 2,
             mean(sim100$sd_n) - 2)
data.frame(samp_size, bias_sd)

#1e.
sd(sim5$div_n)
sd(sim5$var_n) 

#Problem 2 Set-up
b0 <- 1
b1 <- 2
b2 <- 3

#2a.
n <- 100
rho <- 0.5
z_i <- rnorm(n)
u_i <- rnorm(n)
x_i <- rho * z_i + sqrt(1-rho^2) * u_i
eps <- rnorm(n)
y_i <- b0 + b1 * x_i + b2 * z_i + eps
df <- data.frame(y_i, x_i, z_i)

#check distributions
cor(df$x_i, df$z_i) 
var(df$x_i)

#2b.
small <- lm(y_i ~ x_i, data = df)
large <- lm(y_i ~ x_i + z_i, data = df)
coef(small)
coef(large)

plot(df$x_i, df$y_i, pch = 19, col = "blue",
     xlab = "x (news consumption)", ylab = "y (political knowledge)",
     main = "n = 100, rho = 0.5")
abline(a = 1, b = 2, lwd = 2, col = "red")
abline(small, col = "purple", lwd = 2)
abline(a = coef(large)["(Intercept)"] + coef(large)["z_i"] * mean(df$z_i),
       b = coef(large)["x_i"], col = "green", lwd = 2) 
legend("bottom", legend = c("y ~ x", "y ~ x + z", "True slope (2)"),
       col = c("purple", "green", "red"), lwd = 2, bty = "n")

#2c.
n <- 1000
rho <- 0.5
z_i <- rnorm(n)
u_i <- rnorm(n)
x_i <- rho * z_i + sqrt(1-rho^2) * u_i
eps <- rnorm(n)
y_i <- b0 + b1 * x_i + b2 * z_i + eps
df <- data.frame(y_i, x_i, z_i)

#check distributions
cor(df$x_i, df$z_i) 
var(df$x_i)
small <- lm(y_i ~ x_i, data = df)
large <- lm(y_i ~ x_i + z_i, data = df)
coef(small)
coef(large)

plot(df$x_i, df$y_i, pch = 19, col = "blue",
     xlab = "x (news consumption)", ylab = "y (political knowledge)",
     main = "n = 1000, rho = 0.5")
abline(a = 1, b = 2, lwd = 2, col = "red")
abline(small, col = "purple", lwd = 2)
abline(a = coef(large)["(Intercept)"] + coef(large)["z_i"] * mean(df$z_i),
       b = coef(large)["x_i"], col = "green", lwd = 2) 
legend("bottom", legend = c("y ~ x", "y ~ x + z", "True slope (2)"),
       col = c("purple", "green", "red"), lwd = 2, bty = "n")


#2d.
n <- 1000
rho <- 0
z_i <- rnorm(n)
u_i <- rnorm(n)
x_i <- rho * z_i + sqrt(1-rho^2) * u_i
eps <- rnorm(n)
y_i <- b0 + b1 * x_i + b2 * z_i + eps
df <- data.frame(y_i, x_i, z_i)

#check distributions
cor(df$x_i, df$z_i) 
var(df$x_i)
small <- lm(y_i ~ x_i, data = df)
large <- lm(y_i ~ x_i + z_i, data = df)
coef(small)
coef(large)

plot(df$x_i, df$y_i, pch = 19, col = "blue",
     xlab = "x (news consumption)", ylab = "y (political knowledge)",
     main = "n = 1000, rho = 0")
abline(a = 1, b = 2, lwd = 2, col = "red")
abline(small, col = "purple", lwd = 2)
abline(a = coef(large)["(Intercept)"] + coef(large)["z_i"] * mean(df$z_i),
       b = coef(large)["x_i"], col = "green", lwd = 2) 
legend("bottom", legend = c("y ~ x", "y ~ x + z", "True slope (2)"),
       col = c("purple", "green", "red"), lwd = 2, bty = "n")

