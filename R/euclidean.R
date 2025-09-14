#' Euclidean algorithm
#'
#' Find the greatest common divisor of two numbers.
#' See \url{https://en.wikipedia.org/wiki/Euclidean_algorithm} for details.
#'
#' @param a An integer up to 2^53.
#' @param b An integer up to 2^53.
#'
#' @returns An integer representing the greatest common divisor of the two input
#' numbers.
#' @export
#'
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)

# euclidean <- function(a, b) {
#   # Helper to validate each argument is a finite numeric scalar and integer-valued
#   validate_scalar_integer <- function(x, name) {
#     if (!(is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x))) {
#       stop(sprintf("'%s' must be a finite numeric scalar.", name), call. = FALSE)
#     }
#     tol <- .Machine$double.eps^0.5
#     if (abs(x - round(x)) > tol) {
#       stop(sprintf("'%s' must be integer-valued (no fractional part).", name), call. = FALSE)
#     }
#   }
#
#   validate_scalar_integer(a, "a")
#   validate_scalar_integer(b, "b")
#
#   # Work with absolute integer values
#   A <- as.integer(abs(round(a)))
#   B <- as.integer(abs(round(b)))
#
#   # Handle gcd(0,0) as 0
#   if (A == 0L && B == 0L) return(as.integer(0L))
#
#   # Euclidean algorithm
#   while (B != 0L) {
#     r <- A %% B
#     A <- B
#     B <- r
#   }
#
#   return(A)
# }

euclidean <- function(a, b) {
  # Validate numeric scalar and integer-valued (within floating tolerance)
  validate_scalar_integer <- function(x, name) {
    if (!(is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x))) {
      stop(sprintf("'%s' must be a finite numeric scalar.", name), call. = FALSE)
    }
    tol <- .Machine$double.eps^0.5
    if (abs(x - round(x)) > tol) {
      stop(sprintf("'%s' must be integer-valued (no fractional part).", name), call. = FALSE)
    }
  }

  validate_scalar_integer(a, "a")
  validate_scalar_integer(b, "b")

  # Work with numeric (double) integer values to allow values > 2^31-1.
  # (Exact up to 2^53; for arbitrarily large integers consider the 'gmp' package.)
  A <- abs(round(as.numeric(a)))
  B <- abs(round(as.numeric(b)))

  # gcd(0,0) convention
  if (A == 0 && B == 0) return(0)

  # Euclidean algorithm using numeric modulo (works correctly for integer-valued doubles)
  while (B != 0) {
    r <- A %% B
    A <- B
    B <- r
  }

  # Return integer-valued result (as integer if it fits in R integer range)
  if (A <= .Machine$integer.max) {
    return(as.integer(A))
  } else {
    return(A)
  }
}
