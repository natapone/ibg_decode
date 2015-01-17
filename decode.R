alpha_base = length(letters) # 26

# convert string to integer
alpha_to_int <- function (a) {
    # split
    parts = strsplit(a, "")[[1]]
    
    if (length(parts) == 0) {
        return(NULL)
    }
    
    parts = rev(parts)
    cat("Input", a, "=>", parts, "\n")
    
    # power and sum
    num = 0
    for(i in 1:length(parts)) {
        n = char_to_num(parts[i])
        num = num + n * alpha_base^(i-1)
        cat(" #", i, parts[i], "=>", n, "=>", num, "\n")
        
    }
    return(num)
}

# return index of letter
char_to_num <- function(a) {
    which(letters == a) - 1 
}
