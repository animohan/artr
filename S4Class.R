#S4 Classes:
#Good reference: https://www.programiz.com/r-programming/S4-class

#S4 class is defined using setClass function
#Member variables are called slots. While defining class, need to set the names and slots(member variables) along with class of slot

#Example
setClass("student", slots = list(name = "character", age = "numeric", GPA = "numeric"))

#Creating S4 objects

#Create an object using new()
# provide the class name and value for the slots

s = new("student", name = "John", age = 21, GPA = 3.5)
s

#Function setClass returns a generator function having same names as the class and can be used to create new objects. it acts as constructor
student = setClass("student", slots = list(name = "character", age = "numeric", GPA = "numeric"))
student

#Creating S4 objects from generator function
newStudent = student(name = "Kim", age = 22, GPA = 3.7)
newStudent

#Slots are accessed and modified using @ operator
newStudent@name
newStudent@GPA = 3.75
newStudent

#can also use slot function to modify
slot(newStudent, "name")
slot(newStudent, "name") = "Kimberly"
newStudent


#Methods and Generic Functions
# Specific method for show()
# show() already exists like print(); here we make a specific version for student

setMethod("show","student",
          function(object){
            cat(object@name, "\n")
            cat(object@age, "\n")
            cat("GPA:", object@GPA,"\n")
          }
        )
newStudent #Show is the equivalent of print 
