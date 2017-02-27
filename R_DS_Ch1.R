#Apply functions
x = cbind(x1 = 7, x2 = c(7:1, 2:5))
x
col.sums = apply(x, 2 , sum)
row.sums = apply(x, 1 , sum)

#lapply or list apply
x = list(x = 7:1, x2 = c(7:1,2:5))
x
lapply(x,mean)

#Using sapply with custome function
v.in = 1:10
v.out = sapply(v.in, function(x) x^2)
v.out


#mapply is multivariate sapply
mapply(rep,1:6,6:1)
#function call rep is called with input 1:6 and is replicated 6:1 using 2nd dimension of the mapply function


#tapply applied to a function to each cell of the ragged array
dat = list(c(4,2,6,1,5), c("P","S","N","K","K"))
dat
tapply(1:5, dat, sum) #Dont understand this

#rapply is a recursive function for lapply
x = list(list(a = pi, b = list(c = 1:1)), d = "atest")
x
rapply(x, sqrt, classes = "numeric", how = "replace")
