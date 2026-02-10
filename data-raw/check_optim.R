#### Script to check the bias of the optimizer
mono.err <- logical(nrow(all_tbl))
for (i in 1:nrow(all_tbl)) {
  row <- all_tbl[i,]

if (nrow(row$track.dat[[1]]) > 2) {

  mono.err[i] <- which(row$track.dat[[1]]$N.temp %in% as.numeric(row$N)) < 6
} else {
  mono.err[i] <- NA
}
}

sum(mono.err, na.rm = TRUE)
which(mono.err)
sub.tbl <- subset(all_tbl, subset = mono.err)
sub.tbl$track.dat
sub.tbl$N


out <- as.data.frame(matrix(nrow = nrow(all_tbl), ncol = 4))
names(out) <- c("a", "b", "r2", "n.lm")
for (i in 1:nrow(all_tbl)) {

row <- all_tbl[i,]

if (nrow(row$track.dat[[1]]) > 2) {

fit <- lm(log(row$track.dat[[1]]$res.temp) ~ log(row$track.dat[[1]]$N.temp))
out[i,1] <- a <- coef(fit)[1]
out[i,2] <- b <- coef(fit)[2]
out[i,3] <- summary(fit)$r.squared
N.min <- exp((log(row$thresh) - a) / b)
out[i,4] <- ceiling(N.min)
}
}


summary(out$r2, na.rm = TRUE)

# .91

# check for the biggest discrepancies
out <- data.frame(out,
                  N.min = all_tbl$N)
sub.out <- subset(out, !is.na(out$a))

sum(sub.out$n.lm > sub.out$N.min) / nrow(sub.out)
idc <- which(abs(out$n.lm - as.numeric(out$N.min)) > 100)

idx <- idc[4]

row <- all_tbl[idx,]
out.row <- out[idx,]
Ns <- row$track.dat[[1]]$N.temp
res <- row$track.dat[[1]]$res.temp

plot(Ns, res)
abline(a = row$thresh, b = 0)
abline(v = row$N, col = "blue")
abline(v = out.row$n.lm, col = "red")

out.row$r2

plot(log(Ns), log(res))
abline(a = out.row$a, b = out.row$b)
abline(h = log(row$thresh))
abline(v = log(as.numeric(out.row$N.min)), col = "blue")
abline(v = log(as.numeric(out.row$n.lm)), col = "red")

