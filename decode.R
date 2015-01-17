alpha_base = length(letters) # 26

int_to_alpha <- function (i=0, a=NULL) {
    cat(i, a , "\n", sep=" - ")
    
    if (i < alpha_base) {
        # convert to alphabet
        a[1] = num_to_char(i)
        
        # revert order
        return(rev(a))
    } else {
        digit = log(i, base=alpha_base)
        power = floor(digit)
        m = mod(i, alpha_base^power)
        if (m[1] > 0) {
            # insert to alphabet by order
            if(is.null(a)) a = init_int_alpha(power+1)
            a[power+1] = num_to_char(m[1])
        }
        int_to_alpha( m[2], a )
    } 
}

# start point all zero
init_int_alpha <- function(i) rep("a", i)

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
char_to_num <- function(a) which(letters == a) - 1

num_to_char <- function(i) letters[i + 1]

mod <- function(x, y) c(floor(x / y), x %% y )
