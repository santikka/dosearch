to_vec <- function(dec, n) {
    if (n == 0) return(numeric())
    b <- numeric(n)
    for (i in 1:n) {
        b[n - i + 1] <- (dec %% 2)
        dec <- (dec %/% 2)
    }
    return(rev(b))
}