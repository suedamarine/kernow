T <- 20
P_bp <- 760
P_wv <- 4.7603 * exp(1)^(0.0645 * T)
K <- 1.97681
K_0 <- exp(-58.0931 + 90.5069 * (100 / (T + 273.15)) + 22.2940 * log((T + 273.15) / 100))
Beta <- K_0 * 22.263
X <- 0.0015
C <- 1000 * 1.97681 * Beta * X * (P_bp - P_wv) / 760


alk <- 100 # alkalinity
k0 <- 1.58e-3
k1 <- 2.83e-4
k2 <- 4.68e-11
kw <- 1*10^-14
h <- 1 * 10^-(7.012) # ph value
h2co3 <- (alk / 50000 - (kw /h) - h) * (1 / (k0 * k1 / h + k0 * k1 * k2 / h^2))
h2co3.mass <- 1 * 2 + 12 + 3 * 16
co2.mass <- 12 + 2 * 16


ggplot() +
  geom_function(fun = function(x) 230 * sin(50 * x), n = 1000,
                color = "red") +
  geom_function(fun = function(x) 230 * sin(50 * (x + 1/3*pi)), n = 1000,
                color = "black") +
  geom_function(fun = function(x) 230 * sin(50 * (x + 2/3*pi)), n = 1000,
                color = "blue") +
  lims(x = c(0, 1), y = c(-250, 250)) +
  theme_minimal()

vector.1 <- c(5, 10, 15, 20)
vector.2 <- c(25, 30, 35, 40, 45, 50, 55, 60)
final <- array(c(vector.1, vector.2), dim = c(4,4,3))
print(final)
