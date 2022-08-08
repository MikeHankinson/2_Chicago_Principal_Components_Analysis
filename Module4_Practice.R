# =====================================================
# Module 4 Practice - Principal Component Analysis and
#                     Latent Class Analysis
# Mike Hankinson
# =====================================================

# Principal Component Analysis
# ******************************************

# Example 1
# PCA Module OUtputs and Connection to Data
# __________________________________________
# 1.	Load and Scale Data 
# 2.	Perform PCA 
# 3.	Create Covariance and Correlation Matrices
# 4.	Select the Number of Factors
# 5.	Recreate Data from PCA:
# 6.	Compute the EIGEN VALUES and VECTORS of the COVARIANCE MATRIX:

# Load and Scale Data
# ------------------------------------------
data("iris")
pca.dat <-iris[,1:4]
pca.dat <-scale(pca.dat)
dim(pca.dat)      # [1] 150   4
head(pca.dat)
#        Sepal.Length Sepal.Width Petal.Length Petal.Width
# [1,]   -0.8976739  1.01560199    -1.335752   -1.311052
# [2,]   -1.1392005 -0.13153881    -1.335752   -1.311052
# [3,]   -1.3807271  0.32731751    -1.392399   -1.311052
# [4,]   -1.5014904  0.09788935    -1.279104   -1.311052
# [5,]   -1.0184372  1.24503015    -1.335752   -1.311052
# [6,]   -0.5353840  1.93331463    -1.165809   -1.048667


# Create Covariance and Correlation Matrices
# ------------------------------------------
# Note: 
# - PCA Methodology rotates the axis of the data to **REPLACE VARIABLES with 
#   FACTORS/COMPONENTS (interchangeable)** 
#   that maximize the variation of the data projected onto that axis.
# 
# - LOADINGS are coefficients of FACTORS that link to the original data like the following:
#           ~ ORIGINAL DATA = FACTORS * transpose[LOADINGS]
#           ~ For matrix vectors A and X of size n, LAMBDA*A = LAMBDA*X
#           ~ LAMBDA = scalar and is EIGEN VALUE for both A and X 
#
# The EIGEN VECTOR associated with the 
# HIGHEST EIGEN VALUE = LOADING associated with the FIRST PRINCIPAL COMPONENT


# Looking at the covariance matrix of the data can indicate if there is a good fit for PCA.  

DO <-cov(pca.dat) # covariance matrix of scaled data. 
#                   The eigen vectors are equivalent to the 
#                   loading of the PCA solution

cor(pca.dat) 
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
# Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
# Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
# Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000


# Results show high correlation between: 
#   - Sepal.Length/Petal.Length, 
#   - Sepal.Length/Petal.Width 
#   - Petal.Width/Petal.Length  
# Meaning dimension reduction may be achievable. 

# Note: the covariance and correlation matrices are equivalent.
#       because they are scaled with unit standard deviation.  
#       correlation = covariance / standard deviation



# Perform PCA and Examine Results
# ------------------------------------------

iris.pca <- princomp(pca.dat)     #princomp() function performs PCA on the scaled data

dim(iris.pca$scores)    # [1] 150   4
head(iris.pca$scores)


dim(iris.pca$loadings)  # [1] 4 4
iris.pca$loadings[,1:4]


# Select the Number of Factors
# ------------------------------------------

individual_variation <- iris.pca$sdev^2/sum(iris.pca$sdev^2)
#Comp.1      Comp.2      Comp.3      Comp.4 
#0.729624454 0.228507618 0.036689219 0.005178709 


cumulative_variation_VAF <- cumsum(iris.pca$sdev^2/sum(iris.pca$sdev^2))  # VAF
#   Comp.1    Comp.2    Comp.3    Comp.4 
# 0.7296245 0.9581321 0.9948213 1.0000000 


# The variance accounted for by each component from both an individual 
# and cumulative perspective. As expected, each additional factor accounts for 
# a smaller proportion of the variation in the data, 
# and that when all 4 factors are considered, we account for 100% of the variation.



# --Elbow Plot--
# Use elbow plot to help decide number of factors to retain. 
# If a factor accounts for a significant amount of variation, 
# it will be worth increasing the complexity of the solution to 
# include it. If the gain in variation accounted for is small, the additional component is not necessary. 

plot(cumulative_variation_VAF, type="b", pch=16, xlab="# Components", ylab="VAF", main="PCA Elbow Plot", xaxt="n")
axis(1, at=1:4, labels=paste0("PC",1:4))

# Result: clear elbow at two principal components. By selecting these two components, 
# reduced dimensionality of the data in half, while still accounting for 
# 95.8% of the variation in the data. 



# Recreate Data: With FACTORS and LOADINGS
# -----------------------------------------
# Just to show that we can unwind to get identical data to cor(pca.dat) above.
# Look at how FACTORS and LOADINGS can relate to recreate the data

factors <- iris.pca$scores
dim(factors)      # [1] 150   4
head(factors)

loadings <- iris.pca$loadings
dim(t(loadings))  # [1] 4 4
head(loadings)

recreate4 <- factors%*%t(loadings)
dim(recreate4)    # [1] 150   4

cor(recreate4, pca.dat)
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
# Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
# Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
# Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000

# Result: Data is recreated perfectly, as evidenced by the diagonal of the correlation matrix
# between the recreated and the original data. Notice also that the off-diagonal 
# elements of this correlation matrix are identical to those from the initial correlation
# matrix of the data with itself-this is a result of the original data and recreated 
# data being identical.



# **Now: recreate data using only two principal components, 
#        which would be the optimal solution:
two.factors <- factors[,1:2]
dim(two.factors)      # [1] 150   2
head(two.factors)

two.loadings <- loadings[,1:2]
dim(t(two.loadings))  # [1] 4 4
head(two.loadings)

recreate2 <- two.factors%*%t(two.loadings)
dim(recreate2)    # [1] 150   4

cor(recreate2, pca.dat)
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length   0.96051998 -0.09483848    0.9277270   0.9183434
# Sepal.Width   -0.09151069  0.99544931   -0.4375792  -0.3893059
# Petal.Length   0.89843915 -0.43917527    0.9918316   0.9662209
# Petal.Width    0.91209611 -0.40071838    0.9909311   0.9670989

# Result: The data is recreated well, but not perfectly.



# Compare Eigen Vectors and Loadings
# ------------------------------------------
# Compute the eigen values and vectors of the previously saved covariance matrix of the data:
e <- eigen(DO)

e$values        # [1] 2.91849782 0.91403047 0.14675688 0.02071484
e$vectors
#         [,1]        [,2]       [,3]       [,4]
# [1,]  0.5210659 -0.37741762  0.7195664  0.2612863
# [2,] -0.2693474 -0.92329566 -0.2443818 -0.1235096
# [3,]  0.5804131 -0.02449161 -0.1421264 -0.8014492
# [4,]  0.5648565 -0.06694199 -0.6342727  0.5235971


# Remember: The covariance matrix of the data is a square matrix that shows the 
#           covariance shared between every pairwise combination of the variables. 
#           The eigen vectors of this square matrix are the loadings for the 
#           PCA solution. 
#           The eigen values of this covariance matrix demonstrate how the loadings 
#           should be ordered.

# Result: The first eigen value is the largest, and therefore the corresponding 
#         eigen vector should be the loading for the first principal component:

cbind(Eigen_Vector1=e$vectors[,1], Loading1=loadings[,1])
#               Eigen_Vector1   Loading1
# Sepal.Length     0.5210659  0.5210659
# Sepal.Width     -0.2693474 -0.2693474
# Petal.Length     0.5804131  0.5804131
# Petal.Width      0.5648565  0.5648565


# Skill Test:
# ------------------------------------------
# a. Recreate the (scaled) value for Sepal.Length for the first flower 
#    in the data set.

# Known:
# 1. ORIGINAL DATA = FACTORS * transpose[LOADINGS]

# Determine LOADINGS and FACTORS:
# 1. Loadings (EIGEN VECTOR) of the COVARIANCE MATRIX, DO: 0.521 -0.377  0.719  0.261
      loadings.first.flower <- e$vectors[1,1:4]  # [1]  0.5210659 -0.3774176  0.7195664  0.2612863
# 2. FACTORS (New VARIABLES)
      four.factors.first.flower <- factors[1,1:4] # -2.257  0.478  0.12727962  0.024  
#Answer: 
recreate4.first.flower <- factors%*%t(loadings.first.flower)
    # Did not work..."Error in factors %*% t(loadings.first.flower) : non-conformable arguments"
cor(recreate4.first.flower, pca.dat)

# b. Write out the linear combination of factor scores and loadings for all 
#    4 components that result in this value.

answer.to.skill.test.b <- -2.25*0.52 + 0.48*0.38 + 0.13*0.72 + 0.02 *0.26
print(answer.to.skill.test.b) # [1] -0.8888

# This linear combination = -0.89. 

# ___________________________________________________

# ___________________________________________________

# Example 2
# Analyze Principal Components
# ------------------------------------------
# 1.	Load and Scale Data 
# 2.	Perform PCA 
# 3.	Create Covariance and Correlation Matrices
# 4.	Select the Number of Factors
# 5.	a. Plotting Factors and Loadings
#     b. Extract Insights
#     c. Resource Allocation
# 6. Bi plot of loadings and component scores
# 7. Data Visualization


# 1.	Load and Scale Data 
dat <- read.csv("Students_PCA.csv")
dim(dat)      # [1] 100   6
head(dat)

# class.score SAT.score Activity.hours Entertainment.hours
# 1    78.54201  1236.883       9.768604            2.653054
# 2    85.95293  1353.543      13.645091            2.404317
# 
# Job.hours Median.income
# 1  7.551000      74383.06
# 2  3.459871      70929.37

pca.dat <-scale(dat)

# 2.	Perform PCA 
student.pca <- princomp(pca.dat)  

# 3.	Create Covariance and Correlation Matrices
cumulative_variation_VAF <- cumsum(student.pca$sdev^2/sum(student.pca$sdev^2))  # VAF
# Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6 
# 0.7579387 0.9010311 0.9600547 0.9887923 0.9981801 1.0000000 

# 4.	Select the Number of Factors
plot(cumulative_variation_VAF, type="b", pch=16, xlab="# Components", ylab="VAF", main="PCA Elbow Plot", xaxt="n")
axis(1, at=1:4, labels=paste0("PC",1:4))

#Select 2 Principal Components. 
#     - Dimension reduction from 6 to 2
#     - Still accounting for 90% of the variance


# 5.a. Plot Factors and Loadings
# ------------------------------------------
# The key to interpreting our principal components will often be found in the LOADINGS. 
# The LOADINGS (Eigen VALUES/VECTORS) are the values that explain how factors are connected to the original variables that we are attempting to analyze.
#     ORIGINAL DATA = FACTORS * transpose[LOADINGS]

# --Component 1 Loading Plot--
plot(student.pca$loadings[,1], type="b", xaxt="n", col="dodgerblue", pch=16, 
     xlab="Variable", ylab="Loadings", main="Loadings for Component 1")
axis(1,1:6, c("Class", "SAT", "Act.", "Ent.", "Job", "Median"))
abline(h=0, lty=2)  # abline() adds line to an existing graph

# Results of Graph:
#   Component 1 is positively correlated with variables 1, 2, 3, and 6 
#   Component 1 is negatively correlated with variables 4 and 5. 

#   An increase in the value of component will 
#     - increase test scores, extracurricular activity, and median household income 
#     - decrease hours spent working outside school, or watching television and playing video games. 

#   This appears to be an ideal solution to improve student experience and performance.


# 5.b. Extract Insights
# ------------------------------------------
# Look at the five students who have the lowest scores for COMPONENT 1.
five.lowest <- sort(student.pca$scores[,1], index.return=T)$ix[1:5]
dat[five.lowest,]

#     class.score SAT.score Activity.hours Entertainment.hours Job.hours Median.income
# 38    66.03757  936.9534      0.4776556           12.702055  6.256236      17490.45
# 23    67.88020  997.4920      0.4789626           14.486533  5.089863      20449.38
# 70    67.88180  975.1417      3.3434399            4.287297 12.202307      29687.03
# 62    70.65558 1021.8663      2.5293648            5.905658 12.549524      40933.83
# 90    70.96193 1051.5763      4.3536228            7.486376  8.442084      29828.43

# Result:  It is not a surprise to see that the students who have some of the 
# lowest test scores allocate their time outside of class poorly, 
# and come from families with a lower median income.


# 5.c. Resource Allocation
# ------------------------------------------
# use the scores for component 1 for these students to properly allocate resources 
# to help improve performance. If there are 80 hours of school resources to devote for 
# student assistance, divide these fairly to each student based on how they are 
# currently performing in terms of component 1.

proportions <- student.pca$scores[five.lowest, 1]/sum(student.pca$scores[five.lowest,1])
hours.allocation <- proportions*80    # [1] 19.47846 18.22708 15.26397 14.25399 12.77650

# Dedicate the hours displayed above to each of the 5 lowest students. 
# In addition, return to the values of the original variables to determine how best 
# to allocate this time. For example, if one student has particularly low SAT scores, 
# allocate a greater proportion of the available hours to tutoring for standardized testing.


# 6. Bi Plot of Loadings and Component Scores
# ------------------------------------------
# Generating a bi plot of loading values for each variable and factor scores 
# for observations allows to directly evaluate PCA results, and to understand 
# differences between particular students.

biplot(student.pca$scores[1:10, c(1,2)], student.pca$loadings[,c(1,2)])


# By looking at the left (vertical) and bottom (horizontal) axes, 
# we can see the component scores for individuals, represented within the graph by 
# bold black numbers.
# By looking at the right (vertical) and top (horizontal) axes, 
# we can observe the loadings of the two components we are using 
# for each variable, represented within the graph by bold red text and arrows.

# Compare Individual 3 with Individual 9
dat[c(3,9),]
#     class.score SAT.score Activity.hours Entertainment.hours Job.hours Median.income
# 3    79.24912  1221.162       9.249438            5.910693  5.241558      51087.18
# 9    75.64044  1154.146       7.985981            3.790240  7.039018      61542.54


# 7. Data Visualization
# ------------------------------------------
plot(student.pca$scores[,1], student.pca$scores[,2], xlab="PC1", ylab="PC2", 
     main="Component Scores for 100 Students", pch=32)
text(student.pca$scores[,1], student.pca$scores[,2], 1:100)



# ___________________________________________________

# ___________________________________________________


# Latent Class Analysis (LCA) -- For Categorical Variables
# ******************************************
# PCA algorithm is not able to handle categorical variables. 
# Latent Class Analysis (LCA) provides an excellent alternative
# for performing dimension reduction.



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Skipped LCA Section.  
# Return to learn about dimension reduction of categorical variables
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



#Functions
# ------------------------------------------
# - var(),cor() - var, cov and cor compute the variance of x and the covariance or 
#         correlation of x and y if these are vectors. If x and y are matrices
#         then the covariances (or correlations) between the columns of x and 
#         the columns of y are computed 

#?princomp() - princomp performs a principal components analysis on the given numeric 
#             data matrix and returns the results as an object of class princomp.

# cumsum() - Cumulative Sums
#            Returns a vector whose elements are the cumulative sums, 
#            products, minima or maxima of the elements of the argument.

# %*% - Matrix Multiplication

# t() - Matrix Transpose
# abline() # Add Straight Lines to a current Plot



#    rm(list = ls())      Removes global environment