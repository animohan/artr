
# mean of 100 random numbers
mean(abs(rnorm(100)))

# 10 random normal variables
rnorm(10)

x = c(1,2,4)

q = c(x, x, 8)

x[1]
x[1:3]
mean(x)
sd(x)

#Internal R datasets
data()

#Nile Dataset
mean(Nile)
sd(Nile)
hist(Nile)
hist(Nile, breaks = 20)

#Functions
oddcount = function(x){
  k = 0
  for(n in x){
   if(n%%2 == 1) k = k + 1 
  }
  return(k)  
}

oddcount(c(1,2,3,4,5,6,7,8,9))
oddcount(c(1,3,5,7))

#Scope
y = 5 #Y is global variable.
func = function(x) return(x+y) # y: global variable returned in the function
func(3)

#Default Arguments
func2 = function (a, b = 10){
  return(a+b)
}

func2(2,10)
func2(10)

#String
y = c("abc"," def"," ghi")
y
u = paste("abc","def","ghi")
u
k = strsplit(u," ")
k


#Matrices
m = rbind(c(1,4),c(2,2)) #row bind
m
m[1,]
m[,1]

m = cbind(c(1,4),c(2,2)) #row bind
m
m[1,]
m[,1]

#Lists:
#Contents can be different data types
x = list(u = 2, v= "abc")
x
hn = hist(Nile)
print(hn)
hn$density


#DataFrames
d = data.frame(list(kids = c("jack","annie"),ages = c(10,12)))
d$kids
d$ages

#Classes
hn = hist(Nile)
print(hn)
