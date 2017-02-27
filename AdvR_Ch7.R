#7.2 S3

# In S3, methods belong to functions called generic functions or generics for 
# short.
# S3 methods do not belong to objects or classes.

#Given a class, the job of an S3 generic is to call the right S3 method

#7.2.2 Defining classes and creating objects
#S3 is simple and has no formal definition of a class
# To make an objects an instnae of a class, just take an existing base object
# and set the class attribut.
# You can do that during creation with structure() or after the fact with class<-

#E.g
#1
#foo <- structur(list(), class = "foo")

#2
#foo <-list()
#class(foo)<- "foo"

#S3 objects are usually built on top of lists or atomic vectors with attributes
# Functions can be turned into S3 objects


#Most S3 classes provide a constructor function:
foo <- function(x){
  if(!is.numeric(x)) stop("X must be numeric")
  structure(list(x), class = "foo")
}


#7.2.3: Creating new methods and generics
# To add a new generic, create a function that calls UseMethod().
# UseMethod takes two arguments: The name of the generic function and argument
# to use for method dispatch
# If 2nd argument is ommitted, then it will dispatch on the first argument of 
# the function. 
# There is no need to pass any of the arguments of the generic to UseMethod()

f = function(x) UseMethod("f")

# A generic isn't useful without some methods. To add a methos, create a regula
# function with correct generic.class name:
f.a = function(c) "Class a"
a = structure(list(), class = "a")
class(a)
f(a)

#7.2.4 Method Dispatch
# S3 method dispath is relatively simple.
# UseMethod() creates a vector of function names and looks each in turn. 
# Default class makes it possible to set up a fall back method for unknown classes

f = function(x) UseMethod("f")
f.a = function(x) "Class a"
f.default =function(x) "Unknown Class"
f(structure(list(),class = "a"))
#Output = Class a ;

#No method for b class, so uses method for a class
f(structure(list(), class = c("b","a")))

#No method for c class, so falls back to default
f(structure(list(), class = "c"))


