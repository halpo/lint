



system.time(replicate(1e5, {
    x <-rnorm(1)
    x*x
})) -> y1

system.time(replicate(1e5, {
    x <-rnorm(1)
    x ^ 2L
})) -> y2

system.time(replicate(1e5, {
    x <-rnorm(1)
    x ^ 2
})) -> y3

rbind(y1,y2,y3)









