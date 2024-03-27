# generate example data with heteroscedasticity and one control variable
dta <- data.frame(id = rep(1:50, each=10),
                  t = rep(1:10, n=50),
                  z = abs(rep(rnorm(50, mean=0, sd=2), each=10)),
                  x = rnorm(500, mean=4, sd=1))

dta$e <- rnorm(500, mean=0, sd=sqrt(1+dta$z))
dta$y <- 1+ dta$x + dta$e
dta$group <- 0
dta$group[dta$id==1] <- 6
dta$group[dta$id==2] <- 5
dta$y[dta$group <= dta$t] <- dta$y[dta$group <= dta$t] + 4

# run simple_staggered_did
output <- simple_staggered_did(yname = "y",
                               tname = "t",
                               idname = "id",
                               gname = "group",
                               xformula = "x",
                               varformula = "z",
                               unit_cluster = F,
                               data = dta)
