#set working directory
setwd("C:/Users/dom/Desktop/r files")

#read file
movies.df<-read.csv("IMDB_movies.csv", sep="\t")

#replace budget to numeric
movies.df$x1 <- str_replace(movies.df$budget, ' million', '000000')
movies.df$x2 <- str_replace(movies.df$x1, '\\$', '')
movies.df$x3 <- ifelse(str_detect(movies.df$x2, '\\.'),str_replace(movies.df$x2,'0$',''),movies.df$x2)
movies.df$x4 <- str_replace(movies.df$x3, '\\.', '')
movies.df$numericBudget <- str_replace_all(movies.df$x4, ',','')

View(movies.df)

class(movies.df$total.gross)
class(movies.df$numericBudget)
numericBudgetNum<-as.numeric(movies.df$numericBudget)
total.grossNum<-as.numeric(movies.df$total.gross)
class(numericBudgetNum)
class(total.grossNum)

#calculate correlation between total_gross&budet
total.gross1<-total.grossNum
budget1<-numericBudgetNum
corGrossBudget<-cor(total.gross1, budget1,
                    method = "pearson", use="complete.obs")
View(corGrossBudget)



# numericBudgetDummy
movies.df$numericBudgetDummy <- ifelse(str_detect(movies.df$numericBudget, 'N/A'),str_replace(movies.df$numericBudget,'N/A','0'),1)
numericBudgetDummy<-movies.df$numericBudgetDummy

#two sample t.test
t.test(total.gross1~numericBudgetDummy, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

#read file
players.df<-read.csv("IMDB_players.csv")

#print file to screen
players.df

#make pivot
n.actors.df<-
table(players.df$id, players.df$role)
head(n.actors.df)

