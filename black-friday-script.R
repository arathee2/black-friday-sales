# input

		setwd("./competition/analyticsvidhya/black_friday")
		
		black_black_friday <- read.csv("train.csv")
		test <- read.csv("test-comb.csv")

# exploratory data analysis

		library(ggplot2)

		str(black_friday)
		summary(black_friday)

		plot1 <- ggplot(black_friday, aes(x = Stay_In_Current_City_Years, y = Purchase)) + geom_boxplot()
		plot1 # nothing special

		plot2 <- ggplot(black_friday, aes(x = Gender, y = Purchase)) + geom_boxplot()
		plot2 #nothing

		plot3 <- ggplot(black_friday, aes(x = City_Category, y = Purchase )) + geom_boxplot()
		plot3 # City "C" has slightly high IQR and median than other cities.

		plot4 <- ggplot(black_friday, aes(x = Occupation , y = Purchase )) + geom_boxplot()
		plot4 # again, nothing special.


# feature engineering

	# adding "Others" as a factor level to black_friday and test data set.
	# But first we create a backup of Product_ID. We''ll use it later.

		testProductId <- test$Product_ID

	# now the refactoring with "Others" as a new level.

		black_friday$Product_ID <- factor(black_friday$Product_ID, levels = c(levels(black_friday$Product_ID), "Others"))

		test$Product_ID <- factor(test$Product_ID, levels = c(levels(test$Product_ID), "Others"))

	# when new product id is encountered in test set it is factored as "Others"

		test$Product_ID[!(test$Product_ID %in% levels(black_friday$Product_ID))] <- "Others"


# splitting data

		library(caTools)

		set.seed(1)

		split <- sample.split(black_friday, SplitRatio = 0.6)
		train <- subset(black_friday, split == T)
		cv <- subset(black_friday, split == F)

# linear regression model

		linearModel <- lm(Purchase ~ ., data = train)
		summary(linearModel)

		linearPredictionCv <- predict(linearModel, cv)

		sse_lm <- sum((linearPredictionCv - cv$Purchase)^2)
		sse_lm
		rmse_lm <- sqrt(sse_lm/(nrow(cv)))
		rmse_lm # 4990 is a dissapointing rmse.

	# testing lm on test set

		linearPredictionTest <- predict(linearModel, test)
		lm_submit <- data.frame(test$User_ID, test$Product_ID, treePredictionTest)
		write.csv(lm_submit, file = "submit.csv", row.names = F)


# CART model

		library(rpart)
		library(rpart.plot)

	# removing Product category 2 and product category 3 from train.

		train$Product_Category_3 <- NULL
		train$Product_Category_2 <- NULL

		treeModel <- rpart(Purchase ~ ., data = train, minbucket = 50 )
		treePredictionCv <- predict(treeModel, cv)

		sse_tree <- sum((treePredictionCv - cv$Purchase)^2)
		rmse_tree <- sqrt(sse_tree/nrow(cv))
		rmse_tree # 2884 rmse. Much better than linear regression model. Over 40 % improvement.

	# testing cart on test set

		treePredictionTest <- predict(treeModel, test)


	# copying the backed up testProductId to the test set so that "Others" is removed in submisison file.

		test$Product_ID <- testProductId

# submitting CART solution as final solution

		cart_submit <- data.frame(test$User_ID, test$Product_ID, treePredictionTest)
		write.csv(cart_submit, file = "submit.csv", row.names = F)


### Achieved leaderboard rank #52