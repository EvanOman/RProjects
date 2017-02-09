# vector = character()
# index = 0
# for (i in 2:nrow(codings2))
# {
#     if (!all(codings2[i - 1,1 : 3] == codings2[i, 1 : 3]))
#     {
#         vector[index] = paste(toString(codings2[i - 1,1 : 3]),  " -> " ,toString(codings2[i, 1 : 3]), " at ", codings2[i, 6])
#         index = index + 1
#     }
# }

getData <- function(){
    codings <- read.csv("~/codings.csv", header = FALSE)
    val <- ncol(codings) - 1
    codings[, 1:val] <- lapply(codings[, 1:val], as.character)
    return(codings)
}

analyzeN <- function(n = 2, file = "~/codingsN.txt", dat)
{
    vector = character()
    index = 1
    for (i in 2:nrow(dat))
    {
        vals <- c(dat[i - 1, n], dat[i, n])
        if (vals[1] != vals[2])
        {
            msg <- character()
            fChar <- character()
            if ((vals[1] == "R" && vals[2] == "F") ||  (vals[1] == "F" && vals[2] == "R"))
            {
                msg <- paste0("PREFIXED after ", n-1, " iterates")
                fChar <- "P"
            }
            else if ( (vals[1] == "F" && vals[2] == "L") ||  (vals[1] == "L" && vals[2] == "F"))
            {
                msg <- paste0("Period ", n - 1, " orbit")
                fChar <- "C"
            }
            else if ( (vals[1] == "L" && vals[2] == "r") ||  (vals[1] == "r" && vals[2] == "L"))
            {
                msg <- paste0("PREZERO after ", n-1, " iterates")
                fChar <- "0"
            }
            vector[index] = paste(paste(toString(dat[i - 1, 1 :(n-1)]), fChar, sep=","), " at ", dat[i, ncol(dat)], msg, sep="\t")
            index = index + 1
        }
    }
    write(vector, file)
}

analyzeThroughN <- function(n = 5, data)
{
    for (i in 1:n)
    {
        analyzeN(n = i, file=paste0("./codings",i,".txt"), dat = data)
    }
}

# for (i in 1:20)
# {
#     analyzeN(n = i, paste0("./codings",i,".txt"), codings)
# }