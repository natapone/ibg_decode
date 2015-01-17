# USAGE
# Testing
#> test_example()
#> test_parse()

# Decode
#> decode(email)

alpha_base = length(letters) # 26

test_parse <- function() {
    
    test <- function(x) {
        cat("TEST:", x, "\n")
        print(parse_str(x))
    }
    
    test_list = c(
        "a+ a == a",
        "z + b == ba",
        "lina + luna == xdaa",
        "is + and == vv"
    )
    
    # Apply test to list
    sapply(test_list, test )
}

parse_str <- function(str) {
    # init
    alpha_base = length(letters) # 26
    
    vars = strsplit(str, split="\\W+" , perl = T)
    
    for(var in vars[[1]]) {
        alpha = alpha_to_int(var)
        
        # assign variable
        var_int = paste(var, "=", alpha, collapse = " ")
        if (!is.null(alpha)) {
            cat(var_int , "\n")
            eval(parse(text = var_int ))
        }
    }
    
    str = fix_r_compat(str)
#     print(str)
    # http://stat.ethz.ch/R-manual/R-devel/library/base/html/sum.html
    # Loss of accuracy can occur when summing values of different signs
    # , but this is platform-dependent.
    
    # It's seem to have problem on my Mac. but on Windows, it woeks fine
    eval(parse(text = str ))
}


test_example <- function() {
    cat("Test: a+ a = a \n")
    print(alpha_to_int("a") + alpha_to_int("a") == alpha_to_int("a"))
    
    cat("Test: z + b = ba \n")
    print(alpha_to_int("z") + alpha_to_int("b") == alpha_to_int("ba"))
}


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

fix_r_compat <- function(str) {
    # fix [] square bracket
    str = gsub("\\[", "\\(", str, perl = TRUE)
    str = gsub("\\]", "\\)", str, perl = TRUE)
    
    # remove tricky zero
    # R will return NaN when cal 0^x
    sub("a\\*\\(.*?\\)", "a", str, perl = TRUE)
}
