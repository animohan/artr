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
#3.3 Applying Functions
#3.4 Adding and Deleting
#3.5 More on the Vect
#3.6 Avoiding Unintend
#3.7 Naming Matrix
#3.8 Higher Dimension