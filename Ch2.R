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

predb = function(x,k){
  n = length(x)
  k2 = k/2
  pred = vector(length = n-k)
  sm = sum(x[1:k])
  if(sm>=k2) pred[1] = 1 else pred[1] = 0
  
  
}
