# R_Predictive_Analytics
"This semester, I am focusing on mastering R Predictive Analytics, and here is the code I’ve been working on."
#Unit - 1

a<-6#
6->a
a=6

a=TRUE
a<-TRUE


a<-5
a<-TRUE
class(a)


vector_char<-c("pink","orange","green")
class(vector)
vector_num<-c(1,2,3,4)
class(vector_num)
vector_com<-c(8+6i,3i-5)
class(vector_com)

seq(3,10)
a<-c(1,2, "Hello",3+2i)
seq(1,10, length.out=5)
a<-3:10
a[4:7]
a[-3]
a[c(TRUE,FALSE,TRUE,FALSE,FALSE)]
a[c(0,0,1,1)]

a<-3:10
b<-c(3,11)
a+b
a-b
a/b
a*b
a*b-a/b

data<-data.frame(name=c("x","y","z"))
roll<-c(1,2,3)
data<-cbind(data,roll)
a<-c("a",0)
rbind(data,a)

library(help="datasets")

View(mtcars)
View(sleep)

str(trees)
View(trees)

trees[20,2]  
trees[trees$Height>80,]
subset(trees,Height>80)

vect[5]

list1=list(str=c("x","y","z"), num=23, log=TRUE, com=2+3i, li=list(1,2,3))
names(list1)<-c("str","num","log","li")
print(list1)
list1[[2]][1]

vect<-c(1,2,3,c("hi","there","bye"))
vect[4]
list1


matrix:
  
  data<-c("Shubham","Rahul","Shubham","Rahul","Shubham","Rahul")
print(data)
print(is.factor(data))

fdata<-factor(data)
print(fdata)
print(is.factor(fdata))

#labels in r
print(levels(fdata))

#changing the labels
levels(fdata)<-c("Shubham","Rahul")
print(levels(fdata))   
print(fdata)

a<-c("n","s")
z<-is.factor(a)
z

z<-factor(c("North","East"))
d<-is.factor(z)
d
r<-factor((z),labels=c("d","x"))
r
c(1,2,3)
c("a","b","c")

Gender<-factor(c("Male","Female","Male","Male","Female","Male"), levels=c("Male","Female","Transgender"))
Gender[2]<-"Transgender"
Gender[3]<-"abc"
levels(Gender)<-c(levels(Gender),"abc")
Gender


students_df <- data.frame(
  Name = factor(c("Alice", "Bob", "Charlie")),
  RollNumber = factor(c(101, 102, 103)),
  Subject = factor(c("Math", "Science", "History"))
)

students_df


write.csv(students_df,file="dataframe.csv", row.names= FALSE)


#Data preprocessing

used_cars<-read.csv(file.choose())
View(used_cars)
str(used_cars)
summary(used_cars$price,color)
quantile(used_cars$price)
quantile(used_cars$price,seq(0,1,length.out=6))
table(used_cars$model)
prop<-prop.table(table(used_cars$model))
round(prop*100,digit=2)
range(used_cars$year)
diff(range(used_cars$year))
is.na(used_cars)
#Unit - 2
#------------------------------------------------------------
wscd<-read.csv(file.choose())
View(wscd)
str(wscd)
head(wscd)
wscd<-wscd[,-1]
head(wscd)
table(wscd$diagnosis)
prop.table(table(wscd$diagnosis))*100
round(prop.table(table(wscd$diagnosis))*100,digit=3)
factor(wscd$diagnosis,levels=c("B","M"),labels=c("Bengin","Malignant"))
summary(wscd)
summary(wscd[c("radius_mean","area_mean","smoothness_mean")])
summary(wscd[c("concave.points_mean","area_se")])
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wssc<-as.data.frame(lapply(wscd[2:31],normalize))
View(wssc)
wssc_train<-wssc[1:469,]
wssc_test<-wssc[470:569,]
wssc_train_label<-wscd[1:469,1]
wssc_test_label<-wscd[470:569,1]
library(class)
wdc_prep<-knn(train = wssc_train,test = wssc_test,cl=wssc_train_label,k=21)
wdc_prep
library(gmodels)
CrossTable(x=wdc_prep,y=wssc_test_label,prop.chisq = FALSE)
View(iris)
str(iris)
#sqldf
View(Sample_Superstore)
str(Sample_Superstore)
View(sqldf("select * from Sample_Superstore limit 10"))
View("sqldf * from Sample_Superstore where Profit>500")
View(sqldf("select * from Sample_Superstore where city like 'h%'"))
View(sqldf("select * from Sample_Superstore where city like 'a%'"))
View(sqldf("select * from Sample_Superstore where city like '%x%'"))
View(sqldf("select * from Sample_Superstore where [ship mode] = 'Standard Class'"))
View(sqldf("select sum(sales) as [Total Sales] from Sample_Superstore"))
View(sqldf("select Region, sum(sales) from Sample_Superstore group by [Region]"))
#TO Fetch Max Profit
View(sqldf("select max(Profit) from Sample_Superstore"))
#To Fectch max Profit in all using nested query
View(sqldf("select * from Sample_Superstore where profit = (select max(Profit) from Sample_Superstore)"))
#Show the segment and arrange it in terms of profit
View(sqldf("select  segment, Profit from smp order by Profit"))
#Find the total sum of sales where regions are central and west
View(sqldf("select sum(sales) from Sample_Superstore where Region in('Centarl','West')"))
#select all the cities where profit is minimum
#View(sqldf("select city from Sample_Superstore where sales = 'min(sales)'"))
#Find the region wise profit where sum of profit is > than 0
View(sqldf("select sum(profit),region from Sample_Superstore group by Region having sum(Profit)>0"))  
#Consider a data frame in which u have the values 
#1st vector in the data frame is name and the values are - "Abc",Null,xyz,Null,"pqr" ,
#2nd vector - Roll no. - 1, 2, NA, 4, 5 and 
#3rd vector - Sub - "R", "Java", "Python", "NA", "C++".
#SOl - 
#-->Firstly find the total no. of Null values 
#-->Next Is there any null values
#-->Next If yes remove all the rows and which contains null values 
#-->Next replace null values of roll no. column with 0 Name column with 'A',
#Subject column with 'S'
data<-data.frame(name=c("Abc",Null,"xyz",Null,"pqr"))
#19-9-24
library(e1071)
data <- data.frame(
  Day = c('D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9', 'D10', 'D11', 'D12', 'D13', 'D14'),
  Outlook = c('Sunny', 'Sunny', 'Overcast', 'Rain', 'Rain', 'Rain', 'Overcast', 'Sunny', 'Sunny', 'Rain', 'Sunny', 'Overcast', 'Overcast', 'Rain'),
  Temp = c('Hot', 'Hot', 'Hot', 'Mild', 'Cool', 'Cool', 'Cool', 'Mild', 'Cool', 'Mild', 'Mild', 'Mild', 'Hot', 'Mild'),
  Humidity = c('High', 'High', 'High', 'High', 'Normal', 'Normal', 'Normal', 'High', 'Normal', 'Normal', 'Normal', 'High', 'Normal', 'High'),
  Wind = c('Weak', 'Strong', 'Weak', 'Weak', 'Weak', 'Strong', 'Strong', 'Weak', 'Weak', 'Weak', 'Strong', 'Strong', 'Weak', 'Strong'),
  Play = c('No', 'No', 'Yes', 'Yes', 'Yes', 'No', 'Yes', 'No', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'No')
)
View(data)
#Remove the Day column as it's not needed for prediction 
data$Day <- NULL
View(data)
#Split the dataset into training and test data
set.seed(123) #For reproducibility
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

#Train the Naive Bayes Model

nb_model <- naiveBayes(Play ~ ., data = train_data)

#make a prediction on the test data
pred <- predict(nb_model, test_data)

#Display the Predicition 
pred

#Evaluate the model performance 
confusion_matrix <- table(Predicted = pred, Actual = test_data$Play)
confusion_matrix

#Calculate accuracy 
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy 

#Explanation of Naive bayes mathematimatically

#naive Bayes assumes that the feauters (Outlook , Temp, Humidity, wind)
#Given a new input, the probability of Play being 'Yes' or 'No' is 

#predict new input data
new_data <- data.frame(
  Outlook = 'Sunny',
  Temp = 'Cool',
  Humidity = 'High',
  wind = 'Strong'
)

#make a predicition for the new data
new_pred <- predict(nb_model, new_data)
print(new_pred)

#Outlook = rain
#Temp = mild
#humidity = normal
#wind = weak
data2 <- data.frame(
  Outlook = 'Rain',
  Temp = 'Mild',
  Humidity = 'Normal',
  wind = 'Weak'
)
pred2 <- predict(nb_model, data2)
print(pred2)
