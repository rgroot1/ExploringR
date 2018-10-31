## function 
## varaible defined inside of a function are only defined or perhaps redefined inside of that function 
## outside the function == global 
v <- 'i am a global variable'
stuff <- 'i am global stuff'

fun <- function(stuff) {
	print(v)
	stuff <-'reassign stuff inside of this function'
	print(stuff)
}

fun(stuff)
# 'i am a global variable'
# 'reassign stuff inside of this function'

print(stuff)
# 'i am a global variable'
# 'reassign stuff inside of this function'
# 'i am global stuff'