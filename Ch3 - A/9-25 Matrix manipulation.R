
## Basic matrix manipulation
#matrix() takes data and puts it into an n x p matrix
x <- matrix(rnorm(12),4,3)

# matrix calculation requires %%
#t() is transpose function
x %*% t(x)

x = matrix(c(4,1,2,3),2,2)
y = solve(x)

# matrix times its inverse should result in identity matrix
x %*% y

