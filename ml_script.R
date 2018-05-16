library(rpart)
library(class)
#Reading Data
cars <- read.csv(file="./base.csv", header=TRUE, sep=",")

options(max.print=2100)

#Exploring
head(cars)
nrow(cars)
ncol(cars)

#renaming cols
colnames(cars) = letters[1:15]
c = cars
cars_knn = cars
#exploring col types
sapply(c, class)

# ---------- Preparing Data ----------
#Convert price coluns to string
c[,14] = gsub("[[:punct:]]","", as.character(cars[,14]))
c[,14] = strtoi(c[,14], base = 0L)
c[,14]
c[,15] = gsub("[[:punct:]]","", as.character(cars[,15]))
c[,15] = strtoi(c[,15], base = 0L)
c[,15]
c[,4] = gsub("[[:punct:]]","", as.character(cars[,4]))
c[,4] = strtoi(c[,4], base = 0L)
c[,4]


#Fitting variables into categories
my_seq = seq(0, 2000000, 1000)
my_str = as.character(my_seq)
my_str = setdiff(my_str, "1e+06")
#length(my_seq)
#length(my_str)

c[,4] = c[,4]+1
c[,4] = cut(as.double(c[,4]), breaks = my_seq, labels = my_str)
c[,14] = cut(as.double(c[,14]), breaks = my_seq, labels = my_str)
c[,15] = cut(as.double(c[,15]), breaks = my_seq, labels = my_str)

#drop unused factors
c[,c(4,14,15)] = droplevels(c[,c(4,14,15)])
c[,3] = factor(c[,3])

#Renaming levels
length(levels(c[,6]))
levels(c[,6]) = c(1:35)
length(levels(c[,11]))
levels(c[,11]) = c(1:12)

#checking factors
levels(c[,11])
levels(c[,14])
levels(c[,15])
levels(c[,6])
table(c[,4])
table(c[,14])
c[,c(4,14,15)]

#Separating test samples
test = sample(nrow(c), 10)

#Build train group
x_train = c[-test,c(1:14)]
y_train = c[-test,15]
ctrain <- cbind(x_train,y_train)

#fit <- rpart(y_train ~ ., data = ctrain, method="class")
#summary(fit)

#predicted = predict(fit, c[test,c(1:14)])

# -------------------- KNN ----------------
head(cars_knn)
sapply(cars_knn, class)

cars_knn[,15]

#Convert price coluns to string and numeric
cars_knn[,4] = gsub("[[:punct:]]","", as.character(cars[,4]))
cars_knn[,4] = strtoi(cars_knn[,4], base = 0L)
cars_knn[,4]
cars_knn[,14] = gsub("[[:punct:]]","", as.character(cars[,14]))
cars_knn[,14] = strtoi(cars_knn[,14], base = 0L)
cars_knn[,14]
cars_knn[,15] = gsub("[[:punct:]]","", as.character(cars[,15]))
cars_knn[,15] = strtoi(cars_knn[,15], base = 0L)
cars_knn[,15]

sapply(cars_knn, class)

#Separating test samples
test_knn = sample(nrow(cars_knn), 10)
cars_knn[test_knn,]

#checking which cols are numeric
sapply(cars_knn, is.numeric)
num_cols = c("c", "d", "n")

#Training group
knn_train = cars_knn[-test_knn, num_cols]
knn_class = cars_knn[-test_knn, "o"]

#Test group
knn_test = cars_knn[test_knn, num_cols]

head(cars_knn[,numeric_cols])

#Running knn
fit = knn(knn_train, knn_test, knn_class, k = 5, prob=TRUE)

#Comparing class to knn result
cars_knn[test_knn, "o"]
fit

head(cars_knn)

#Normalization
cars_knn[,c(4,14,15)] = (cars_knn[,c(4,14,15)]-min(cars_knn[,c(4,14,15)]))/(max(cars_knn[,c(4,14,15)])-min(cars_knn[,c(4,14,15)]))
