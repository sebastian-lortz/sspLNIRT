library(dplyr)
library(HDInterval)

true.par <- rnorm(2000)
true.mse <- .1
sim.par <- rnorm(2000, true.par, sd = sqrt(true.mse))

err <- sim.par-true.par
abs.err <- abs(sim.par-true.par)
sq.err <- (sim.par-true.par)^2

data.test <- data.frame(
  type = as.factor(rep(c("err","abs.err","sq.err"), each = 2000)),
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
             linetype = "solid") +
  annotate(x=test.sum.stats$mu,y=0,label="Mean",vjust=2,geom="text") +
  xlim(c(-1,1))


#####

true.par <- rnorm(2000, 0, sd = 1)

true.mse <- .1
sim.pars <- lapply(true.par, FUN = function(x) {
  temp <- rnorm(1, x, sd = sqrt(true.mse))
  rnorm(1000, temp, sd = .2)
})

# conventional
sim.par <- c(sapply(sim.pars, mean))
err <- (sim.par-true.par)/(1+true.par)
err <- (sim.par-true.par)

abs.err <- abs(err)
sq.err <- err^2


data.test <- data.frame(
  type = as.factor(rep(c("err","abs.err","sq.err"), each = length(err))),
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

test.sum.stats <- data.test %>%
  group_by(type) %>%
  summarise(mu = mean(val),
            sd = sd(val),
            med = median(val),
            hdi.lb = quantile(val, .2),
            hdi.ub = quantile(val, .8))
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
             linetype = "solid") +
  annotate(x=test.sum.stats$mu,y=0,label="Mean",vjust=2,geom="text") +
  xlim(c(-1,1))
