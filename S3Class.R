
#Good tutorial : https://www.programiz.com/r-programming/S3-class

s = list(name = "John", age = 21, GPA = 3.5)
class(s) = "student"
s

#Constructors: Good practice to use a function with the same name as class to create objects
student = function(name, age, gpa){
  #integrity check
  if(g>4 || g<0) stop("GPA must be between 0 and 4")
  
  #assign values
  value = list(name = name, age = age, GPA = gpa)
  
  #class can be set using class or attr() function
  attr(value, "class") = "student"
  #class(value) = "student"
  value
}

s = student("Paul", 26, 3.7)
s

#Methods and generic function
print.student = function(obj) {
  cat(obj$name, "\n")
  cat(obj$age, "years old","\n")
  cat("GPA:", obj$GPA,"\n")
}

print(s)

#writing your own new fucntion
grade = function (obj){  # This is a definition of a generic function
  UseMethod("grade") #
}

grade.default = function(obj){
  cat("Generic Function\n")
}

grade.student = function(obj){
  cat("Your grade is ",obj$GPA, "\n")
}

print(s)
grade(s)


##LINKED LIST IMPLEMENTATION:

node = list(val=0, nextone = NULL)
class(node) = "linklist"

linklist = function(inpval, nextelement){
  llist = list(val = inpval, nextone = nextelement)
  class(llist) = "linklist"
  llist
}

node1 = linklist(5,NULL)
node2 = linklist(10,NULL)
node3 = linklist(15, NULL)
node4 = linklist(20, NULL)
head = linklist(NULL, NULL)

node2$nextone = node1
node3$nextone = node2
node4$nextone = node3
head$nextone = node4


print.linklist = function(obj){
  while(!is.null(obj$nextone)){
    cat(obj$val,"\n")
    obj = obj$nextone
  }
  cat(obj$val,"\n")
}

print(head)

length.linklist = function(obj){
  len = 0
  while(!is.null(obj$nextone)){
    len = len+1
    obj = obj$nextone
  }
  return(len)
}

length(head)

insert.linklist = function(llist,obj,posn){
  head = linklist(NULL, NULL)
  head$nextone = llist$nextone

  if(length(llist)<posn){ return(-1)}
  else{
    for(i in (1:posn)){
      head = head$nextone
    }
    obj$nextone = head$nextone
    head$nextone = obj
  }
  return(llist)
}

node25 = linklist(199,NULL)
a = insert.linklist(head,node25,2)
print(a)



