# 5 : Data Frames
# Dataframe is liek matrix
# each column may be a different entity
# technically, data frame is a list with components of the list being equal=length vectors

#5.1 Creating Data Frames
 kids = c("Jack", "Jill")
 ages = c(12,10)
 d = data.frame(kids, ages, stringsAsFactors = FALSE)
d
  #Default stringAsFactors is True hence the character vectors will be converted as Factor
 
# 5.1.1 Accessing Data Frames
# d is a list can be accessed via component indices or by component names
d[[1]]
d$kids

# Can also be treated like a matrix 
d[,1]

str(d)

# Saw three ways to access teh columns: d[[1]], d[,1], d$kids
# 3rd one ie d$kids is more clear
# however in writing some code or package, matrix like notation d[,1] is needed, especially if sub data frames are bieng extractored

# 5.1.2
#Extended Example: Regression Analysis of Exam Grades
# exam grades
a = c(2.0, 3.3, 4.0, 2.3, 2.3, 3.3)
b = c(3.3, 2.0, 4.0, 0.0, 1.0, 3.7)
c = c(4.0, 3.7, 4.0, 3.3, 3.3, 4.0)
exam = data.frame(a, b,c )
names(exam) = c("Exam 1", "Exam 2", "Quiz")
exam

# 5.2 Other Matrix-Like Operations
# 5.2.1 Extracting Subdata frames:
  # sub data frames can be extracted by rows and columns
exam[2:5,]
exam[2:5,2] # Gives a vector
class(exam[2:5,2]) #Class = Vector

exam[2:5,2, drop = F] # Gives a Dataframe
class(exam[2:5,2, drop = F]) # Class = Data.frame

exam[2:3,2:3]

#Can also do filtering
exam[exam$`Exam 1`> 2.3,] #Note the syntax after $; required when there is aspace

# 5.2.2: More on treatement of NA values
#if we want to ignore NA values use na.rm
x = c(2, 4, 6, NA)
mean(x)
mean(x, na.rm = T)

#Subset and filtering
exam[exam$Quiz>3.3,]
subset(exam, Quiz>3.3)

exam.na = exam
exam.na$Quiz[2] = NA

exam.na[exam.na$Quiz>3.3,] #Filtering keeps NA values
subset(exam.na, Quiz>3.3) #Note NA is ignored

# 5.3 Merging Data
# 5.4 Applying Function