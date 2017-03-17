#4 Lists:
#Lists can have multiple objects

#4.1 Creating Lists
#Declaring list
j = list(name = "Joe", salary = 55000, union = T)
j
j$sal #names can be abbrieviated salary-> sal

#Lists can be created via vector()
z = vector(mode = "list")
z[["abc"]] = 1:5
z

#Another declaration
jalt = list("Joe",5500, T)
jalt

#4.2 General List Operations
# 4.2.1 List Indexing
j$salary
j[["salary"]]
j[[2]]

#single brackts can be used as well
j["salary"]
j[2]
#Using the single brackt returns a list
typeof(j[2])
typeof(j[[2]])

j[1:2] #first and 2nd element i.e name and salary

j2 = j[2] #subsetting and assigning
j2
class(j2) #j2 is a list
str(j2)

#Now lets try using the same thing with double brackts
#j[[1:2]] throws and error
j2a = j[[2]]
j2a
class(j2a) #this is of numeric class; above it was list

#4.2.2 Adding and Deleting List Elements
z = list(a = "abc", b = 12)
z
#add a c components
z$c = "sailing"
z

#Components can be added via vecot index
z[[4]] = 28
z[5:7] = c(FALSE, TRUE, TRUE)
z

#Components can be deleted by setting not null
z$b = NULL
z
z[[2]] #Also not indices have been updaed now $c = "sailing" has index =2 

#Another way of forming list by concatenatinb two lists
 c(list("Joe",5500,T), list (5))

# Getting the size of list
length(j)
j

#Extended example:
# Write a function findwords() that will determine which words are in a text file and compile a list of location of each word's occurence in the text
findwords = function(tf){
  txt = scan(tf,"")
  wl = list()
  for(i in 1:length(txt)){
    wrd = txt[i] #Get the word
    wl[[wrd]] = c(wl[[wrd]],i)
      #if the word alreayd exists in word list then i is concatenated as its location in the list
      # if the word doesn't exit its wl[[wrd]] will be null and it will be update with c(wl[[wrd]],i) i.e with the current location of word
  }
  return(wl)
}

wl = findwords("Testconcorda")
wl

#4.3 Accessing List Components and values
# if components of list have tags then they can be obtained via names
names(j)

#for values; use unlist function
ulj = unlist(j)
ulj
class(ulj)
#return value of unlist = vector of character strings
names(ulj) #element names of the vector comes from the list

z = list(a = 5, b = 12, c = 13)
y = unlist(z)
class(y) #In this case unlist gives vector of numbers

#mixed case
w = list(a = 5, b = "xyz")[1]
wu = unlist(w)
class(wu) # Vector of characters in this case; R chooses lowest common denomiator variable type
#Note that we can set the names of the vector to null
names(wu) = NULL

wu

#4.4 Applying Function to Lists:
# 4.4.1 Using the lapply() and sapply() Functions
# lapply = list apply works like matrix apply() function calling the specified function on each component of a list and returning another list
lapply(list(1:3,25:29), median)
# lapply returns a list, but it can be converted into vector or matrix, sapply does that
sapply(list(1:3, 25:29), median)

#Sort the wordlist
findwords2 = function(tf){
  txt = scan(tf,"")
  k = length(txt)
  wl = list()
  for(i in 1:k){
    word =  txt[i]
    wl[[word]] = c(wl[[word]], i)
  }
  
  namelist = names(wl)
  sorted.names = sort(namelist)
  return(wl[sorted.names])
}
findwords2("Testconcorda")

# Can order by frequency as well
freq.order = function(wordlist){
  freqs = sapply(wordlist, length)
  # freqs gives the length of indices for each word; like this:
  #the  here means  that first  item 
  #3     1     1     2     1     3 
  # Basically says, the -> 3 indices, here -> 1 index, means -> index etc..
  # order() gives the indices of the location of elments to be sorted. Hence order returns
  # 2,3, 5,13,14.. here it means that index 2 == "here" is the first element
  # index 3 == "means" is the 2nd element, index 5 = first,
  
  return(wordlist[order(freqs)]) # feed the sorted frequency to wordlist to reorder its elements
  
}


#Extended Example: Abalone data
# Sample example of getting indices of observations
g = c("M","F","F","I","M","M","F")
gender = function(inp){
  return(which(g == inp))
}


lapply(c("M","F","I"), function(gender) return(which(g == gender))) #position of indices in "g" vector

#4.5 Recursive Lists
#Lists can be recursive i.e having lists within lista
b = list( u = 5, v = 12)
c = list(w = 13)
a = list(b, c)
a
