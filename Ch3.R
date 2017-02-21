#3.1 Creating Matrices
# Internal storage of matrix  = column major order i.e Col1 stored then Col2 stored etc
y = matrix(c(1,2,3,4), nrow = 2, ncol = 2)
y[1,]
y[,1]

#elements can be indvidually specified
y = matrix(nrow =2, ncol = 2)
y[1,1] = 1
y[2,1] = 2
y[1,2] = 3
y[2,2] = 4

y

y = matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = T) #byrow ensures rows are filled first
y

#3.2 General Matrix Operations
#3.2.1 Performing Linear Algebra Operations on Matrices
y = matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = T)
y*y  #element wise multiplication
3*y #Multiplying by a scalar
y+y #Addition
y%*%y #Matrix multiplication

#3.3.2 Matrix Indexing:
z = matrix(c(1,1,1,2,1,0,3,0,1,4,0,0), nrow = 4, ncol =3, byrow = T)
z
z[1:2,]
z[,2:3]
z[c(1,3),]

# can also assign values to submatrix
z[c(1,3), ] = matrix(c(9,9,9,8,8,8), nrow = 2)
z

#Another way of reassigning; from another matrix
x = matrix(nrow = 3 , ncol = 3)
x
y = matrix(c(4,5,2,3), nrow = 2)
x[2:3,2:3] = y
x

#Negative subscripts are used to exclude elements
y
y[-1,] #exclude row 1
y[,-1] #exclude column 1

#Extended Example: Image manipulation
library(pixmap)
mtrush1 = read.pnm("mtrush1.pgm") #reading the image file
mtrush1

str(mtrush1) #Look at the type of content for the image
  # Key here is that the intensity matrix is at mtrush1@grey ( for these class of objects access is through @ instead of $)
mtrush1@grey[28,88] #One pixel value
plot(mtrush1)

locator() #Run locator; click on a point and enter escpate to get the coordinate
#Coordinate of range pixels of roosevelt rows: 85-163, columns = 135 to 177

mtrush2 = mtrush1
mtrush2@grey[84:163, 135:177] = 1
plot(mtrush2)

#will blot out roosevlet; User locator() function; When this function is called it waits for user to click on a point within
# the graph and returns cooridinates of the point

# How about disguising president roosevelt by adding randome noise
# Instructions from book:
  # Add random noise to image at range rows and cols of image and the
  # returned objects is of the class pizmap; paramater  controls the weight of the noise with result
  # being 1-q times the original image plus q times the random noise

blurimage = function(img, rows, cols, q){
  nrows = length(rows)
  ncols = length(cols)
  tot = nrows*ncols
  noise = rnorm(tot)
  noise = abs(noise)/max(noise)
  u = matrix(noise, nrow = nrows, ncol = ncols)
  img@grey[rows,cols] =q*img@grey[rows,cols]+ (1-q)*u[1:nrows,1:ncols]
  #img@grey[rows,cols] = u[1:nrows,1:ncols]
  return(img)
}

mtrush3 = blurimage(mtrush1, 84:163, 135:177, 0.4)
plot(mtrush3)

#BOOK SOLUTION
blurpart = function(img, rows, cols, q){
  lrows = length(rows)
  lcols = length(cols)
  newimg = img
  randomnoise = matrix(nrow = lrows, ncol = lcols, runif(lrows*lcols))
  newimg@grey[rows,cols] = (1-q)*img@grey[rows, cols] + q*randomnoise
  return(newimg)
}
mtrush3 = blurpart(mtrush1, 84:163, 135:177, 0.65)
plot(mtrush3)


#Filtering the matrices
#Careful with syntax for filtering
x = matrix(c(1,2,2,3,3,4), nrow = 3, byrow = T)
x
x[x[,2]>=3,]
#Dissecting it
j= x[,2]>=3
j
x[c(FALSE, TRUE,  TRUE)]
x[j,]

#Filtering criteria can be separate from the object (x in above example)
z = c(5,12,13)
x[z%%2 ==1, ]

x[x[,1] > 1 & x[,2] >2,]

m = matrix(c(1,4,2,5,3,6), nrow = 3, byrow = T)
m[m[,1]>2 & m[,2]>5] #Same example as above; key point here is that the output should have been 1 x 2 matrix, but we got a two element 
# vector
#solution here is to use drop argument which tell R to retain 2-d structure. Discussed bit later

#Applying vector operations on matrix
x = matrix(c(5,-1,2,10,9,11), nrow = 3, byrow = T)
x
which(x>2) 
# Ans give: 1 3 5 6
# INdexing is in column major form
# (1,1)-> 1 ; (2,1) ->2, (3,1)->3,(1,2)->4, (2,2)->5, (3,2) ->6


#Extended Example: Generating a co-variance matrix
# Example uses row() and col() functions; arguments are atrices
# row(a[u,v]) returns row number of element in the place a[u,v]; but should't the row number be 'u' and column number should be v
# it is useful some time apparently when using multivariate normal distribution
x = matrix(c(5,-1,2,10,9,11), nrow = 3, byrow = T)
#row(x[2,1])  not clear how row() works
x
row(x) #look carefully at the entries
col(x) #Look carefully at the entires

#Getting entries with same row and column number aka diagonal entries
row(x) == col(x)
#SUppose we are working with n-variate normal distribution. Matrix has nrows and ncolns and each of the n variables to have varince 1;
# with correlation rho between paris of variables. For n = 3 and rho =0,2 the example is:
#   1  0.2, 0.2
# 0.2  1    0.2
# 0.2, 0.2, 1

makecov = function(rho, n){
  m = matrix(nrow = n , ncol = n)
  m = ifelse(row(m)==col(m),1,rho) #Diagonal entries are 1; non diagonal are rho or sigma
  return(m)
}

u = makecov(0.5,5)
u

#3.3 Applying Functions to Matrix rows and Columns:
#apply() most used feature of R

#Using the apply() function
#general form = apply(m, dimcode, f, fargs)
# m - matrix, dimcode = 1 for applying to rows, 2 for applying to columns
# f = function; fargs = function argument

z = matrix(c(1,4,2,5,3,6), nrow = 3, byrow = T)
apply(z,2, mean) #mean along columns

#apply along rows
apply(z,1, mean)

# we can define our function
f = function(x) x/c(2,8)
apply(z,1,f) # This has to be applied by rows because each row has 2 elements
  # Note that the output gives a reordered matrix i.e the size of result is 2x3 and not 3x2
  # Each computation of apply() goes in the column of output of apply()
# Apply() behavior is that function applied retuns vector of k components, the the result of apply
# will have k rows. 
# Can use transpose t() to change it
t(apply(z,1,f))

f2 = function(x) x/c(2,4,6)
apply(z,2,f2)


#Suppose we hav matrix of 1s and 0s and want to create a vector as follows;
# For each row of hte matrix, the corresponding element of the vector will be either 1 or 0 dependeing
# wheteher the major of the first d elements in that row is 1 or 0, d  is the parameter to vary

#Think from perspective of using lapply
cp = function(inp,d){
  ifelse(sum(inp[1:d])/d > 0.5, return(1),return(0))
}

x = matrix(c(1,0,1,1,0,1,1,1,1,0,1,0,0,1,1,0,1,1,1,0), nrow = 4, byrow = T)
apply(x,1,cp,3)
apply(x,1,cp,2)

#Extended example: Finding Outliers
#Data matrix rs; Find the most deviant value here deviant = furthest from the median value

find.outlier = function(inp){
  out2 = function(vec){
    index = which.max(abs(median(vec)-vec))
    return(vec[index])
  }
  
  return(apply(inp,1,out2))
}
 a = matrix( c(1,5,2,2,3,8,1,2,9,7,2,2), nrow = 4, byrow = T)
 find.outlier(a)

#3.4 Adding and Deleting Matrix Rows and Columns
 #Matrixes are generally fixed length and dimensions so cannot add or delete rows columns
 # however matrices can be reassigned
 
 #appending vectorx
 x = c(12,5,13,1,8)
 x = c(x,20) #append 20
 x
 x = c(x[1:3],99,x[4:6]) #insert 99
 x
 x = x[-length(x)] #delete last element
 x
 x = x[-2:-4] #delete rnage of elements
 x
 
 
# Similarly for matrices: rbind and cbind can bind rows and column
 one = c(1,1,1,1)
 nine = c(99,99,99,99)
 z = matrix(c(1,1,1,2,1,0,3,0,1,4,0,0), nrow = 4, byrow = T)
 z
 z = cbind(z,one)
 z
 z = rbind(z,nine)
 z
 z = cbind(55,z) 
 z
 
 # quick way to create matrices:
 q = cbind(c(1,2),c(3,4))
 q

 #Extended Example: Finding the closest pair of vertices in the graph
 
  
#3.5 More on the Vect
#3.6 Avoiding Unintend
#3.7 Naming Matrix
#3.8 Higher Dimension