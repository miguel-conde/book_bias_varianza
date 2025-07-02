
library(tidyverse)


mean_squared_error <- function(x, y) {
  mean((x - y)^2)
}


### Generación datos mock
make_mock_data <- function(
    N = 100, alpha = 1, beta = 2, sigma_epsilon = 1.0,
    sigma_x = 1.0, sigma_error_x= 0.0, seed = 2025
    ) {

        if (!is.null(seed)) {
            set.seed(seed)
        }

        x <- rnorm(N, 0, sigma_x)
        x_hat <- rnorm(N, x, sigma_error_x)
        y <- rnorm(N, alpha + beta*x, sigma_epsilon)

        the_tibble = tibble(
            x = x,
            x_hat = x_hat,
            y = y
        )

        m_0 <- lm(y ~ 1, the_tibble)
        m_1x <- lm(y ~ x, the_tibble)
        m_1x_hat <- lm(y ~ x_hat, the_tibble)

        the_tibble <- the_tibble %>%
            mutate(
            f_0 = predict(m_0, the_tibble),
            f_1x = predict(m_1x, the_tibble),
            f_1x_x_hat = predict(m_1x, the_tibble %>% select(x = x_hat)),
            f_1x_hat_x_hat = predict(m_1x_hat, the_tibble)
            )

        mse_0 <- mean_squared_error(the_tibble$f_0, the_tibble$y)
        mse_1x <- mean_squared_error(the_tibble$f_1x, the_tibble$y)
        mse_1x_x_hat <- mean_squared_error(the_tibble$f_1x_x_hat, the_tibble$y)
        mse_1x_hat_x_hat <- mean_squared_error(the_tibble$f_1x_hat_x_hat, the_tibble$y)

        out <- list(
            tibble = the_tibble,
            m_0 = m_0,
            m_1x = m_1x,
            m_1x_hat = m_1x_hat,
            mse_0 = mse_0,
            mse_1x = mse_1x,
            mse_1x_x_hat = mse_1x_x_hat,
            mse_1x_hat_x_hat = mse_1x_hat_x_hat
        )

        return(out)
}

# Entre usar x_hat y eliminar la variable
# Mejor
make_mock_data(sigma_error_x = 1000)

# Initializae empty list with 10 elements
res_list <- vector("list", 100)
sigmas_error_x <- seq(0, 3, length.out = 100)
for (i in 1:100) {
  aux <- make_mock_data(sigma_error_x = sigmas_error_x[i])
  res_list[[i]] <- c(
    sigma_error_x = sigmas_error_x[i],
    mse_0 = aux$mse_0, 
    mse_1x = aux$mse_1x, 
    mse_1x_x_hat = aux$mse_1x_x_hat,
    mse_1x_hat_x_hat = aux$mse_1x_hat_x_hat
    )
}

res <- sapply(res_list, function(x) {x}) %>% t

res

plot(res[, "sigma_error_x"], res[, "mse_0"],
     type = "l", col = "blue",
     xlab = "sigma_error_x", ylab = "MSE",
     main = "MSE vs sigma_error_x",
     xlim = c(0, max(res[, "sigma_error_x"]) + 0.1),
     ylim = c(0, max(res[, c("mse_0", "mse_1x", "mse_1x_x_hat", "mse_1x_hat_x_hat")]) + 1))
lines(res[, "sigma_error_x"], res[, "mse_1x"], col = "red")
lines(res[, "sigma_error_x"], res[, "mse_1x_x_hat"], col = "green")
lines(res[, "sigma_error_x"], res[, "mse_1x_hat_x_hat"], col = "purple")
legend("topright",
       legend = c("MSE modelo sin x",
                  "MSE modelo con x",
                  "MSE modelo con x imputando x_hat",
                  "MSE modelo con x_hat"),
       col = c("blue", "red", "green"), lty = 1)

res2 <- make_mock_data(sigma_error_x = sigmas_error_x[i])


beta_hat <- res2$m_1x$coefficients[2]
mejora <- (res2$mse_0 - res2$mse_1x)

umbral_mse <- (mejora / beta_hat^2)^.5

umbral_mse