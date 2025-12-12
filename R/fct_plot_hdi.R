library(dplyr)
library(HDInterval)

true.par <- rnorm(10000)
true.mse <- .1
sim.par <- rnorm(10000, true.par, sd = sqrt(true.mse))

err <- sim.par-true.par
abs.err <- abs(sim.par-true.par)
sq.err <- (sim.par-true.par)^2

data.test <- data.frame(
  type = as.factor(rep(c("err","abs.err","sq.err"), each = 10000)),
  val = c(err,abs.err,sq.err)
)

test.sum.stats <- data.test %>%
  group_by(type) %>%
  summarise(mu = mean(val),
            sd = sd(val),
            med = median(val),
            hdi.lb = hdi(val, credMass = .8)[1],
            hdi.ub = hdi(val, credMass = .8)[2])
sim.mse = mean((sim.par-true.par)^2)


ggplot(data.test, aes(x = val, fill = type)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = test.sum.stats,
             aes(xintercept = hdi.lb, color = type),
             linetype = "dashed") +
  geom_vline(data = test.sum.stats,
             aes(xintercept = hdi.ub, color = type),
             linetype = "dashed") +
  geom_vline(data = test.sum.stats,
             aes(xintercept = mu, color = type),
             linetype = "solid")

res.test
data.test %>%
  group_by(type)
hdi(test.sum.stats$val, credMass = .8)[2]
