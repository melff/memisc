sQuote <- function (x)
{
    if (length(x) == 0)
        return(character())
    paste("'", x, "'", sep = "")
}

dQuote <- function (x)
{
    if (length(x) == 0)
        return(character())
    paste("\"", x, "\"", sep = "")
}

