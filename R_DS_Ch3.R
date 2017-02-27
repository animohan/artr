#Chapter 3 : Linked Lists


#Element Data types
# Factors
# Factor = vector of integer values labelled to corresponding set of uniques characters in a categorical vector
fact1 = factor(c("a","b","c","c","a","b"))
fact1
str(fact1)

#User can dfine levels per requirements
fact2 = factor(c("a","b","c","c","a","b"), labels = c(1,2,3), levels = c("c","a","b"))
fact2


#Matrix
mat1 = matrix(1:10, nrow = 5)
mat1
mode(mat1) #Check data type in the matrix

#Categorical matrix
mat2 = matrix(c("ID","Total",1,10,2,45,3,26,4,8), ncol = 2, byrow = T)
mat2
mode(mat2)


#Array
#n dimensional vector with homogenous content; content can be character, numeric, logical and complex
#2 dimensional array
arr1 = array(1:10, c(2,5))
arr1

#Three dimensional array
arr1 = array(1:18, c(3,2,3)) #c (3,2,3) column vector defines the dimension of the array such that lenght of col bector defines the dimension of the array
                             # and values of col vector define the grid size in (x,y,z) resp
arr1

#Dataframes:
# Dataframe is a 2D table with combination of multiple forms of vectors
# Has properties of list and matrix

int = c(1:5); char = letters[1:5]
log = c(T,F,F,T,F); comp = c(1i, 1+2i,5,8i,4)
data.frame(int, char, log, comp)


#List is a way of grouping all possible objects and assigning them to single objects
# 1Dimensional and can take heterogeneous objects
# can contain multiple lists within a list

list1 = list(age = c(1:5), 
             name = c("John","Jack","Jane","Jade", "Jet"),
             mat = matrix(1:9, nrow = 3),
             df = data.frame(name = c("John","Jack","Jane","Jade","Jet"), gender = c("M","M","F","F","M")),
             small.list = list(city = c("Austin","Mumbai","Seol","Tokyo")),
             country = c("USA","India", "Korea","Japan") )
list1

#Linked List

#create an empty environement
create_emptyenv = function(){
  emptyenv()
}

# An enviroment can hold collection of named objects and pointer to an enclosing environment.

#isEmpty function to check if the list is empty

isEmpty = function(llist){
  if(class(llist)!='linklist') warning("Not linked list class")
  identical(llist, create_emptyenv()) #Identical checks if the two objects are equal or not
}


#Define linked list

linkListNode = function(val, node = NULL){ #Link list node function has an element and next node.
  llist = new.env(parent = create_emptyenv())
  llist$element = val #Element field store item value
  llist$nextnode = node #node points to the next linked list node
  class(llist) = "linklist"
  llist
}

#create a linked list
LList = linkListNode(5, linkListNode(2,create_emptyenv()))

#Constructed list can changed by adding and deleting nodes.
#Elements of the node in link list can be accessed using following functions
setNextNode = function(llist){
  llist$nextnode
}

setNexElement = function(llist){
  llist$element
}
 
#Lenght of the linked list
sizeLinkList = function(llist, size = 0){ #sizeLinkList function starts from first position and keeps scannling the list node till empty env
  if(isEmpty(llist)){
    return(size)
  } else{
    size = size + 1L
    sizeLinkList (llist$nextnode, size)
  }
}


addElement = function(new, llist){
  if(isEmpty(llist)){
    llist = linkedlist(new)
  }else{
    llist = linkListNode(llist, new)
  }
  llist  
}