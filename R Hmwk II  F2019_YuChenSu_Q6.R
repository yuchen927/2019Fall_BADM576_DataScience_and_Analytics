######################################################################
#
#Question 6
# 
# write a function to verify the check digit for ISBN13.
# You will get full credit as long as your functio works.
# I do not care about how you approach the problem in your code.
# you can get more ISBN13 numbers on amazon.com for testing.


# Here is the algorithm
# 
# 1.	Take the first 12 digits of the 13-digit ISBN
# 2.	Multiply each number in turn, from left to right by a number.
#     The first digit is multiplied by 1, the second by 3, the third by 1 gain, the fourth by 3 again, and so on to the eleventh which is multiplied by 1 and the twelfth by 3.
# 3.	Add all of the 12 answers.
# 4.	Do a modulo 10 division on the result from step 2. 
# 5.	Take that remainder result from step 4.
#     If it's a zero, then the check digit is zero. 
#     If the remainders isn't zero then subtract the remainder from 10. 
#     The answer to that is your check digit.
#     
# if the isbn13 number is ok, then retun 1 
# if the isbn13 number is not ok, then return 0
# 
# see for details.https://isbn-information.com/check-digit-for-the-13-digit-isbn.html    


ISBN13check <- function(isbn13){
  #  your code here
  
  
  testvector = c(1,2,3,4,5,6,7,8,9,10,11,12)
  isbn13Split = strsplit(isbn13, split = "")
  isbn13Split = isbn13Split[[1]]
  testDigits = as.numeric(isbn13Split[1:12])
  checkDigit = as.character(isbn13Split[13])
  testnumber = sum(testvector * testDigits) %% 14
  
  if ((testnumber == 13) && (checkDigit == 'X')){
    return('OK')
  } else {
    if (as.character(testnumber) == checkDigit){
      return ('OK')
    } else {
      return ('Invalid ISBN')
    }
  }
}

#To test your functions:

ISBN13check(9781491910399)   #should be true
ISBN13check(9781617291562)   #should be true
ISBN13check(9781491952967)   #should be False
#
#
#
##########################################################################



#########################################################################

# From Teacher's Solution. But it's for ISBN10, not ISBN13
# The difference is "number" vs. "string"
# I have tried "ToString()", but it doesn't work.

#########################################################################


#Data Science and Analytics
#Sung W Kim

CheckISBN = function (isbn10){
  
  # this test vector is used by the isbn 10 algorithm
  
  testvector = c(1,2,3,4,5,6,7,8,9)         
  
  # this splits the input string (isbn10) into a vector of characters.
  
  isbn10Split = strsplit(isbn10,split = "") 
  
  # the strplit() returns a list.  next lines changes it to a vector.
  isbn10Split = isbn10Split[[1]]
  
  # this takes the first nine numbers and makes it a vector of size 9
  
  testDigits = as.numeric(isbn10Split[1:9])
  
  # the last digit of the ISBN should be treated as a character.
  
  checkDigit = as.character(isbn10Split[10])
  
  
  
  # vector multiply example. try the code below to convince yourself how it works.
  
  #  a= c(1,2,3)
  #  b = c(4,5,6)
  #  a*b
  #  sum(a*b)
  #
  
  #test number = multiply the corresponding elements and add them up 
  # and divide by 11 and get remainder ( MODulus division)
  # if you divide by 11, the remainder will be between 0 and 10.
  
  testnumber = sum(testvector * testDigits) %% 11
  
  if ((testnumber == 10) && (checkDigit == 'X')){
    return('OK')
  } else {
    if (as.character(testnumber) == checkDigit){
      return ('OK')
    } else {
      return ('Invalid ISBN')
    }
  }
  
  
}

CheckISBN('038549565X')
