#Scalars, Vectors, Arrays and Matrices

#Adding and Deleting Vector Elements

# Declaration
x = c(1,2,3,4)

#inserting and element
x = c(x[1:3],199,x[4])
x
# Obtaining length of vector
length(x)

#function to find element with index 1
f1 = function(x){
  return(x[1])
}

f1(x)

#Matrics and Arrays as Vectors
m = rbind(c(1,2),c(3,4))
m

# Causes addition of 10,11,12,13 to the matrix
m + 10:13

#Declaraitons
x = 2 #works fine

#want y to be an two element vector
#y[1] = 2 #Throws error
#y[2] = 4 #throws error

#declare the vector fixt
y = vector(length=2)
y[1]= 2
y[2]= 4

#Another way
z = c(2,4)
z

#Recycling
#When applying operation to two vectors that require same length, R "recycles" or extends them
c(1,2,3) + c(1,2,3,4,5,6)

x= rbind(c(1,1),c(1,1),c(1,1))
x + c(1,5) #(1,1) was replicated and added everywhere 
#c(1,5) was replciated as c(1,5,1,5,1,5) and when changed into matrix i became [[1,5],[5,1],[1,5]]

# Look at the result of the folling
matrix(nrow = 3, ncol = 2, data= c1,2,1,2,1,2)

#Commong Vector Operations
2+3
"+"(2,3) #+ as a fucntion

x = c(1,2,3)
x + c(1,1,1)

x*c(1,0,1) #per element multiplciation
x/c(2,2,2)
x%%c(2,2,2)

#Vector Indexing
y = c(2,4,6,8,10,12,14,16,18)
y[c(1,3)]
y[1:3]
y[c(1,1,3)]

# -ve subscripts
y[-1] # exclude first element
y[-1:-3] #exclude 1st to 3rd element
y[-length(y)] #exclude last element

#Generating useful vectors with the : Operator
5:8
i = 10
j = 1:i-5 # operator precendence; This is read as i:i then -1
j # all the values from 1:10 are reduced by -1
j = 1:(i-5)
j

#Generating Vector Sequences with seq()
seq(from = 10, to = 20, by = 2)
seq(2,20,2)

seq(2,10, length = 5)
seq(10)

#An error to be aware of
x = NULL
#length of x = null
for(i in 1:length(x)){
  print(i)
}

#because length of x is null, the for loop runs from 1:0
# avoid this
x = NULL
for(i in seq(x)){
  print(i)
}
#Nothing is printed


#Repeating Vector Constant with rep()
x = rep(8,4)
x

x = rep(c(1,2,3), 3) # can be applied to vector
x

x = rep(1:3, 3) #applied to series
x

x = rep(seq(3), 3) #applied to sequence
x

x = rep(1:3, each = 2) #per item is repeated
x

#Using all() and any()
#any() function reports if any of the values are true
# all () functions reports if all the values are true
x = seq(1,10, 2)
any(x>8)
all(x>8)
all(x<12)

#Extended Example: Funding Runs of consecurive ones

findruns = function(x){
  runindex = NULL
  if(length(x) == 0) return(0)
  
  runcount = 0;
  for(i in 1:length(x)){
    if(x[i]==0){
      if(runcount > 1){append(runindex,current.run.index)}
      runcount = 0
    }
    if(x[i] == 1 ){
      if(runcount ==0){ current.run.index = i}
      runcount = runcount + 1
    }
  }
  return(runindex)
}

findruns(c(1,1)) #NEED TO DEBUG THIS CODE

#BOOKS VERSION (This has good potential; think about it)
findrunsbook = function(x,k){
  n = length(x)
  runs = NULL
  for (i in 1:(n-k+1)){
    if(all(x[i:i+k-1] == 1)) {runs = c(runs, i) }
  }
  return(runs)
}
y = c(1,1,1)
findrunsbook(y,3)


# Extended Example: Predicting Discrete Valued Time Series:
# Time series data: 1 = rain, 0 = no rain.
# Predict rain for tomorrow, based on majority for last k days

pred.rain = function(x, k) {
  n = length(x)
  result = rep(NA,n)
  for(i in 1:(n-k+1)){
    if(sum(x[i:(i+k-1)])>=k/2){
      result[i+k] = 1
    }else{
      result[i+k]  = 0
    }
  }
  return(result)
}

y = c(1,1,1,1,0,1,0,1,1,1,0,0,0)
pred.rain(y, 3)
#Take a deeper look at the example later.

#Book Solution 1
preda = function(x,k){
  n = length(x) 
  k2 = k/2
  pred = vector(n-k) # Predicted vector will not include prediction for first k days because we do not have data to predict those
  
  for ( i in 1:(n-k)){
    # Get sum of k days if it is greater than k/2 i.e # of 1s is higher than # of 0s then predict the next day as 1
    # Note prediction is stored with index 'i' in prediction vector
    if (sum(x[i:(i+k-1)]) >= k2) pred[i] = 1 else pred[i] = 0
  }
  # x[(k+1):n] are the given vector minus the first k days
  # abs(pred-x[(k+1):n]) gives the entries where the prediction and actual answer differed. Mean gives the # avg error
  return(mean(abs(pred-x[(k+1):n])))
}

#Book solution 2
predb = function(x,k){
  n = length(x)
  k2 = k/2
  
  # Predicted vector will not include prediction for first k days because we do not have data to predict those
  pred = vector(length = n-k)
  
  # First k days
  sm = sum(x[1:k])
  
  #prediction for first k days
  if(sm>=k2) pred[1] = 1 else pred[1] = 0
  
  if(n-k>=2){ #if n-k == 1 then we can only predict 1 day that we already predicted ; if n-k<1 then can't really predict anything
    for ( i in 2:(n-k)){ # Day k+1 (i = 1) = first prediction; prediction for i from 2 to n-k; this gives prediction for k+1th day to nth
      sm = sm + x[i+k-1] -x[i-1] # Sliding window; Remove yesterday's data and get data from 1 day after kth day
      if(sm >= k2) pred[i] = 1 else pred[i] =0
    }  
  }
  # x[(k+1):n] are the given vector minus the first k days
  # abs(pred-x[(k+1):n]) gives the entries where the prediction and actual answer differed. Mean gives the # avg error
  return(mean(abs(pred-x[(k+1):n])))
}


# This solution uses function cumsum that gives a cumulative sum of the vector
#eg.
y = c(5,2,-3,8)
cumsum(y) #cumulative sum of y = 5,7,4,12 

predc = function(x,k){
  n = length(x)
  k2 = k/2
  pred = vector(length = n-k)
  sm = sum(x[1:k])
  sm = cumsum(x)
  
  if(sm[k] > k2) pred[1] =1 else pred[1] = 0
  if(n-k>=2){
    for ( i in (2:(n-k))){
      if((sm[i+k-1]-sm[i-1])>=2) pred[i] = 1 else pred[i] = 0
    }
  }
  return(mean(abs(pred-x[(k+1):n])))  
}

y = c(1,1,1,1,0,1,0,1,1,1,0,0,0)
predc(y, 3)

#Vectorized Operations
u = c(5,2,8)
v = c(1,3,9)
u > v

#Function on vector
w = function(x) return(x+1)
w(u)

#Sqrt function on a vector
sqrt(1:9)

#Rounding fucntion on a vector
 y = c(1.1, 3.4, 5.5)
 z = round(y)
z


# + Function
y = c(1,2,3,4)
y+1

#Vectorized function wiht scalar arguments
f = function(x,c) return((x+c)^2)
f(1:3,0) # was vectorized in the fuction
f(1:3,1:3)

#if c is indeed a scalar; need to check explicity
f = function(x,c){
  if(length(c)!= 1){
    stop("Vector not allowed")
  }
  return((x+c)^2)
}

f(1:3,1:3)

#Vector in, Matrix out
z12 = function(z){
  return(c(z,(z^2)))
}

z12(5)
z12(1:2)

#using sapply (simplify apply)
sapply(1:8, z12)
#sapply(x,f) applied the function f() to each element of x and converts the result to matrix

#Lets try examples of sapply
sapply(1:8, mean)
sapply(-1:-8, abs)
sapply(1:8, sqrt)

z3 = function(z){
  return(c(z,z^2,z^3))
}
sapply(1:5, z3)


#NA and NULL Values
#Using NA:
x = c(88, NA, 12, 168, 13)
x
mean(x)
mean(x, na.rm = TRUE) #na.rm = remove null values

x = c(88, NULL, 12, 168, 13)
mean(x) # Null doesnt have same issue as NA

#There are multiple NA values for each mode
x = c(5, NA, 12)
mode(x[1])
mode(x[2])


y = c("abc","def", NA)
mode(y[1])
mode(y[2])
mode(y[3])
# NA is character in 2nd one where its numeric in first example

#Using NULL
#Use of NUll is to build up vectors in loops, in which each iteration adds another element to the vector
#In the simple example, we build up a vector of even number

#build a vector of even numbers in 10
z = NULL
for (i in 1:10) if (i%%2 == 0 ) z = c(z,i)
z

#NULL is a special object of no-value in r, whereas NA has a value that is not available
length(NULL)
length(NA)


#Filtering
# Generating filter indices
z = c(5,2,-3, 8)
w = z[z*z>8] # all the values such that val*Val is greater than 8
w
z*z > 8 # This returns TRUE FALSE boolean values and the TRUE FALSE values are used to select from z

# the results is same here
z[c(TRUE, FALSE, TRUE, TRUE)]
z[z*z>8]

#Extracting condition from one vector and applying the result to another vector
z = c(5,2,-3, 8)
j = z*z>8
j

y = c(1,2,30,5)
y[j]

# MOre filtering
x = c(1,35, 34, 2, 39)
x[x>30] #elements greater than 30

x[x>30] = 0 #assign 0 to elements that are greater than 30
x


#Filtering with subset() function
#subset cna be used for filtering; How NA values are handled are the different
x = c(6, 1:3, NA, 12)
x
x[x>5] # NA is counted in here
subset(x,x>5) #NA is ignored here.

#Selection function  which()
#filtering gives you the values, which() gives you the position
z = c(5,2, -3, 7)
which(z*z>8)
z[which(z*z>8)]


#Location of element that meets a condition
find1 = function(x) return(which(x==1))

x = c(3,1,3,4)
find1(x)

x = c(2,1,4,5,1)
find1(x)


#A Vectorized if-then-else: The ifelse function
x = 1:10
y = ifelse(x %% 2 == 0,5,12)
y

#same as this
for(i in x){
  if(x[i]%%2==0)
    {y[i]=5}
  else {y[i]=12}
}
y

x = c(5,2,9,12)
ifelse(x>6, 2*x, 3*x)

#Extended Example: A measure of Association
#Comparing two vectors if they increase or decrease together
findud2 = function (u, v){
  m = length(u)
  n = length(v)
  k = min(m,n)
  score = NULL
  for(i in 1:(k-1)){
    if(((u[i+1]> u[i]) & (v[i+1] > v[i]))| ((u[i+1]< u[i]) & (v[i+1] <v[i])) | ((u[i+1] == u[i]) & (v[i+1] == v[i]))){
      score = c(score,1)
    }
    else score = c(score,0)
  }
  return(mean(score))
}

u = c(5,12,13,3,6,0 ,1,15,16,8,88)
v = c(4,2,3,23,6,10,11,12,6,3,2)
findud2(u,v)

#Books solution
# Its a great solution
# write a function fundud() that converts vector v to 1s and 0s representing an element increasing or decreasing relative to previous on

finddud = function(v){
  vud = v[-1] - v[-length(v)] # this interesting. v[-1] gives all element except first element and v[-length(v)] gives all elements except the last one
                              # eg. v = c(1,2,4,7) -> v[-1] - v[-length[v]] == c(2,4,7) - c(1,2,4) thus giving the difference between consecutive elements
  return(ifelse(vud>0,1,-1))

# This function is also interesting
udcorr = function(x,y){
    ud = lapply(list(x,y), finddud) #findud only takes a single vector input; but here input x, y are put into a list and sent it to the function without any issue
    return(mean(ud[[1]]==ud[[2]])) #1st column of the list belong to item-x and 2nd column belong to item-y
  }
}

#Another solution using sign() and diff()
w = c(1,5,3,4,1,6,9)
diff(w) #differnece of consecutive numbers
sign(diff(w)) #+ve or -ve sign represented as +1 or -1

udcorr2 = function(x,y) return(mean(sign(diff(x)) == sign(diff(y))))
udcorr2(u,v)


#Extended example: Recoding an abalone data set
# Due to vector nature of the arguments, ifelse canb nested
g = c("M","F","F","I","M","M","F")
ifelse(g == "M", 1, ifelse(g == "F",2,3))

# you can also creat subgroups
m = which(g == "M")
f = which(f =="F")
i = which(g == "I")

#Alternative to store the groups in a list
grps = list()
for(gen in c("M","F","I")) grps[[gen]] = which(g == gen)

which(g =="M")
grps2 = list()
grps2[["M"]] = c(1,5,6)
grps2[["F"]] = c(2,3,7)
grps2[["I"]] = c(4)
grps2

#Drawing graphs for abalone dataset

aba = read.csv("abalone.data", header = T, as.is = T)
colnames(aba) = c("Gender","Length", "Diameter","Height","WholeWt", "ShuckedWt","ViscWt","ShellWt","Rings")
grps = list()
for(gen in c("M","F")) grps[[gen]] = which(aba$Gender == gen)
abam = aba[grps$M,]
abaf = aba[grps$F,]
par(mfrow = c(1,1))
plot(abam$Length, abam$Diameter, col = "red")
plot(abaf$Length, abaf$Diameter, pch = "x", new = FALSE, col = "green")

# More compact code instead of 2 plot commands
pchvec = ifelse(aba$Gender == "M", "o","x")
plot(aba$Length, aba$Diameter, pch = pchvec)


# Testing Vector Equality
x = c(1:3)
y = c(1,3,4)
x == y #gives element wise equality
all(x==y) # gives if all elements of vectors are equal hence the vectors are equal
identical(x,y) #gives if vectors are equal

x = 1:2
y = c(1,2)
all(x==y)
identical(x,y)
# Oops! not same, this happens because vectors have different type of elements
typeof(x)
typeof(y)


#Vector Element Names
x = c(1,2,4)
x
names(x)
names(x)=c("ab","bc","cd")
x
x["ab"] # vectors can be called by their names

# Assing null to remove names
names(x) = NULL
x

#More on concatenate = c()
u = c(5,2,3)
typeof(u)
u = c(5,2,"abc")
typeof(u)
u = c(5,3, list(a = 1, b= 4))
typeof(u)
u
# if mix mode elements then all the elements are converted to be the lowest element type

# c() flattens the vectors
u = c(1:5, c(2:7))
u
