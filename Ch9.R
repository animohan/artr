j = list(name = "Joe", salary = 55000, union = T)
class(j) = "employee"
attributes(j)
j #Default print method
#We will write a new print method for this class

print.employee = function(wrkr){
  cat(wrkr$name ,"\n")
  cat("salary", wrkr$salary,"\n")
  cat("union member", wrkr$union,"\n")
}

methods(,"employee")


#9.1.5 Using Inheritance:
k = list(name = "Kate", salary = "6800", union = F, hrsthismonth = 2)
class(k) = c("hrlyemployee", "employee")
k


#9.1.6 Class for storing upper triangular matrix
sum1toi = function(i) { return(i*(i+1)/2)}
