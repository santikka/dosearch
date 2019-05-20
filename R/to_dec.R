to_dec <- function(set, n) {
    if (is.null(set)) return(0)
    return(sum(2^(0:(n-1))[set]))
}