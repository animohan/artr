#Reference Class
#https://www.programiz.com/r-programming/reference-class

#Defining a reference class
setRefClass("student", fields = list(name = "character",age = "numeric", GPA = "numeric"))

#Create reference objects
#Function setRefClass returns a generator function having same names as the class and can be used to create new objects. it acts as constructor
student = setRefClass("student", fields = list(name = "character", age = "numeric", GPA = "numeric"))
s = student(name = "Jade", age = 19, GPA = 3.9)
s

#Modify fields
s$name = "Jaden"
s

#Warning Note
# Generally in R, objects are copoed when assigned ot a new variable or passed by value
a = list(x = 1, y = 2)
b = a
b
a$x = 25
b
#Note values of b has not changed; ie. b is copied by value; NOT true for reference objects
newStudent = s
newStudent
s$GPA = 3.2
newStudent #Note that changing s$GPA changed newStudent GPA as well
#For copying explicity use copy function

s2 = s$copy()
s$GPA = 3.5
s2


#Refernece methods
#Methods are defined for a reference class and do not belong to generic functions like S3 and S4
# All reference class have some methods predefined because all classes are derived from superclass envRefClass

#We can create own method for the class
student = setRefClass("student", fields = list(name = "character", age = "numeric", GPA = "numeric"),
                      methods = list(
                        inc_age = function(x){ age <<- age + x}, # Have to use non-local assignement operator
                        dec_age = function(x){ age <<- age -x }
                        )
                      )

s = student(name = "John", age = 21, GPA = 3.5)
s
s$inc_age(5)
s
