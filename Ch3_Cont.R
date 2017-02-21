#3.4.2 Extended Example: Finding the closes pair of vertices in a graph
#Function input = distance matrix where (i,j) gives distance between city i and city j
# outputs minimum one-hop distance between cities and pairs of cities that achieves the minimum

dist = matrix(c(0,12,13,8,20,12,0,15,28,88,13,15,0,6,9,8,28,6,0,33,20,88,9,33,0), nrow = 5, byrow = T)

# Just the code to get the length of all shortest paths
shortdist = function(dist){
  lrows = dim(dist)[1]
  lcols = dim(dist)[2]
  for(i in (1:lrows)){
    for(j in (1:lcols)){
      curr_dist = dist[i,j]
      for(k in (1:lcols)){
        if(is.na(dist[i,k]) & !is.na(dist[j,k])){
          dist[i,k] = dist[i,j] + dist[i,k]
        }
        if(!is.na(dist[i,k])){
          ifelse(dist[i,k] >  dist[i,j]+dist[j,k], (dist[i,k] = dist[i,j] + dist[j,k]), (dist[i,k] = dist[i,k]) )
        }
      }
    }
  }
  return(dist)
}

short = shortdist(dist)
short

#Books question was: Just find the shortest one hop distance and out the pair of cities which has that distance

# Another try at the Answer
mindist = function(dist){
  lrows = dim(dist)[1]
  lcols = dim(dist)[2]
  minval = max(dist)
  minrow = NA
  mincol = NA
  for( i in (1:lrows)){
    for( j in (1:lcols)){
      if( (dist[i,j] < minval) & (i!=j) ){
        minval = dist[i,j]
        minrow = i
        mincol = j
      }
    }
  }
  return(c(minval, minrow, mincol))
}

mindist(dist)

#Book solution
imin = function(x){
  lx = length(x) #Length of input vector
  i = x[lx] #x[lx] contains the row number
  j = which.min(x[(i+1):(lx-1)]) # For each row passed find the min value from row value + 1 to 2nd last element of the vector
      # This works because:
      #  lets say for for row 1, min distance was between (1,2): That will be recorded. Now when we look at row 2, we can ignore (2,1) and (2,2); 
      # Since our eventual goal is just to find the min value for the entire matrix, it is not important to find the minimum for each row
      # Distance matrix is symmetric i.e distance from i to j is same as distance to j to i and hence we only look at elements from row i+1 to the end.
  
  k = i+j #Due to symmetry of the matrix we find the min value from row i+1, which means that which.min returns the index w.r.t to i+1.
          # Adding k = row number back gives back the correct index of the element w.r.t to the matrix.
  return(c(k,x[k]))
}

mind = function(d) {
  n = nrow(d) # number of rows of the input
  
  #add a coln to assign numbers to each row
  dd = cbind(d, 1:n) # Column bind and the row numbers is the last column of dd
  
  wmins = apply(dd[-n,],1, imin) # Find the min in each row
      # We ignore the last row because we already the dist matrix already has distance from all other entries to the last row.
      # don't need to ignore the column number because imin takes care of that
  
  #wmins will be 2xn (2 rows n columns) because apply put the return k, x[k] as different rows hence 1st row will be indices and 2nd row being values
  i = which.min(wmins[2,]) #end row has indices
  j = wmins[1,i] #first row has values
  return(c(d[i,j],i,j))
}
mind(dist)



#Another buggy way
min.elem = min(dist)
min.index = which(dist ==min.elem, arr.ind = T)
min.elem
min.index
#Doesnt work very well, but one thing to note here is that we can get minimum of the array and its array index in 1 call


# 3.5 More on Vector and Matrix Distinction
# closer look at matrices
z = matrix(1:8, nrow = 4)
z

#z is still a vector, even though arranged as matrix
length(z)
class(z)
attributes(z)

#3.6 Avoiding Unintended Dimension Reduction
r = z[2,]
r
# Note that r is not a matrix
attributes(z)
attributes(r)

#Here is a way to ensure submatrix is a matrix
r = z[2,, drop = F]
r
attributes(r)

#Another way to convert vector to matrix
 u = c(1,2,3)
 u
 v = as.matrix(u)
 v

#3.7 Naming Matrix Rows and Columns
 z
colnames(z) = c("a","b") 
z
colnames(z)
z[,"a"]

#row names
rownames(z) = c("ab","bc","cd","de")
z
z["cd",] 
#3.8 Higher Dimension
#higher dimensional matrices are called array
first = matrix(1:8, nrow = 4)
second = matrix(9:16, nrow = 4)
test = array(data = c(first, second), dim = c(4,2,2))
test

#test is an 3 d matrix with 3,2,2 dimensions
attributes(test)
test[1,1,2]

