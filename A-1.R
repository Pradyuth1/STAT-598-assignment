#Name : Pradyuth Vangur

# Problem 1: The seq() function

# Question 1
seq(from = 10, to = 15, by = 0.5)
#from represents the starting point of the sequence of numbers to be generated.
#to represents the end point of the sequence.
#by represents the amount in which the consecutive number should differ from the previous.

seq(from = 10, to = 15, length.out = 6)
#length.out represents the length of the sequence and the numbers will differ
#uniformly from each other.

#Question 2
seq(10)
#This represents the number of digits to be sequenced.
#It assumes the starting number to be one and ends up with the number inputted.

seq(3,10)
#3 represents the starting number of the sequence.
#10 represents the end of the sequence. Each number differs by 1.

#Question 3
vec_1 <- c(1,10,4,6,7)
seq(vec_1)
#We observe that seq takes in the size of the vector and treats it like
#seq(n), where n is the size of vector and returns sequence of numbers
#starting from 1 and ending in n with a difference of 1

#Question 4
vec_2 <- c(4)
seq(vec_2)
#seq function is considering the first number of the vector as the size
#of the sequence.

#Question 5
seq_along(vec_1)
#We observe that result is same as seq(vec_1)
seq_along(vec_2)
#seq_along takes in vector and treats it as a single sized vector.

#Question 6
2 + seq_len(8)
#This returns a sequence from 3 to 10. This basically adds up 2
#to the vector created by seq_len(8)

#Question 7
#seq_len(-8)
#This provides an error because it cannot take in negative numbers.

-seq_len(8)
#Instead we will have to provide -seq_len(8) to obtain the negative
# sequenced numbers.

seq_len(8)
#This provides a sequence of length 8 with numbers from 1 to 8 with
#spacing of one.

#Problem 2: Vectors in R

#Question 1
letters
summary(letters)

#Question 2
class(letters)
#We observe that the datatype of letters is character. We make use
#of class() function.

#Question 3
length(letters)
#Length of letters is 26

#Question 4
letters[3]
#To chooose the nth element from letters, we wrap n inside [] and
#place it adjacent to letters as letters[n]

#Question 5
letters_back <- rev(letters)
letters_back
#We make use of the function rev() to reverse it.

#Question 6
letters_back_alt_odd <- letters_back[seq(1, length(letters), 2)]
letters_back_alt_odd
#This code takes in letters which are placed in odd positions.

letters_back_alt_even <- letters_back[seq(2, length(letters), 2)]
letters_back_alt_even
#This code takes in letters which are placed in even positions.

#Question 7
letters_matrix <- matrix(data = letters[seq(1,16)], nrow = 4, ncol = 4)
letters_matrix
#This generates matrix with the first 16 elements of letters.

#Question 8
month.abb
class(month.abb)
#This represents the abbreviation of the months in character form.

month.name
class(month.name)
#This represents the names of the months.

#Problem 3: Matrices in R

#Question 1
num <- runif(20, min = 0, max = 1)
num
num_mat <- matrix(data = num, nrow = 4, ncol = 5)
num_mat

#Question 2
num_mat[3, ]
#The above code will index the third row

class(num_mat[3, ])
#It returns numeric as the data-structure

dim(num_mat[3, ])

#Question 3
dim(num_mat[3, , drop = FALSE])
#Dimension is displayed here

#Question 4
sum(num_mat)

#Question 5
num_mat_tf <- num_mat/sum(num_mat)
num_mat_tf
sum(num_mat_tf)

#Question 6
row <- rowSums(num_mat)
row

col <- colSums(num_mat)
col

#Question 7
num_mat_row <- num_mat / row
num_mat_row
rowSums(num_mat_row)

#Question 8
t(num_mat)
#t() transposes the matrix

#Question 9
num_mat_col <- t(num_mat) / col
#Divide the transpose of num_mat with col

num_mat_colt <- t(num_mat_col)
#Transpose the matrix whose column sum is 1

num_mat_colt

colSums(num_mat_colt)
#We see that sum of each column is one

#Question 10
#Case 1: Looking at the numbers vertically in each columns
mat <- matrix(runif(18, min = 0, max = 5), nrow = 2, ncol = 9)
mat
mat_new1 <- matrix(data = mat[2,], nrow = 3, ncol = 3)
#mat[2, ] represents the alternate elements to be considered
mat_new1

#Case 2: Moving along columns of the matrix
mat_new_t2 <- t(mat)
mat_new2 <- matrix(data = mat_new_t2[,1], nrow = 3, ncol = 3)
#mat[,1] represents the alternate elements of the column to be considered
mat_new2

#Question 11
m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
#A matrix with two columns and 7 rows is generated

m
m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column
#Now, another column is being added to m matrix and positioning
#is done accordingly by [, c(1,3,2)]
m