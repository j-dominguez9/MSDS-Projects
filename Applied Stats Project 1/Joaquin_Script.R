### Joaquin Script
##  why doesn't this work?

library(tidyverse)
cars <- read_csv('https://raw.githubusercontent.com/hmlam1/MSDS-Projects/main/Applied%20Stats%20Project%201/Vehicle_MSRP.csv', show_col_types=F)
head(msrp)
library(caret)
cars <- janitor::clean_names(cars)
View(msrp %>% count(make))
which.min(msrp$msrp)
cars <- msrp
head(cars)
colnames(cars)
msrp %>% distinct(transmission_type)
summary(cars)
hist(cars$engine_hp)
str(cars)
colSums(is.na(cars))
?colSums()
cars %>% is.na %>% count()

cars$engine_cylinders <- as.factor(cars$engine_cylinders)
str()
cars$transmission_type <- as.factor(cars$transmission_type)
count(cars$market_category)
is.factor(cars$market_category)
cars$market_category <- as.factor(cars$market_category)
cars$market_category
distinct(cars$market_category)
range(cars$market_category)
summary(cars$market_category)
str(cars)
summary(cars$engine_cylinders)
View(cars %>% select(engine_cylinders, make, model, engine_fuel_type) %>% filter(engine_fuel_type == 'electric'))
cars %>% select(engine_cylinders, make, model, engine_fuel_type) %>% filter(make == 'Tesla')
cars %>% select(engine_cylinders, engine_fuel_type) %>% filter(engine_fuel_type == 'electric')

cbind(cars, 1:n(cars))

cars %>% mutate(engine_cylinders = )

cars %>% replace(cars, )
?replace

# MV count
sapply(cars,function(x)all(any(is.na(x))))
sapply(cars,function(x)sum(is.na(x)))

install.packages('naniar')
library(naniar)
as_shadow(cars)
miss_var_summary(cars)
miss_case_summary(cars)
cars %>% select(engine_cylinders, engine_fuel_type) %>% group_by(engine_fuel_type) %>% miss_var_summary()
naniar::any_na(cars)


### replacing electric mv in cylinders
cars <- cars %>% mutate(engine_cylinders = ifelse(engine_fuel_type == 'electric', 0, engine_cylinders))

View(cars %>% filter(is.na(engine_cylinders)))

### replacing mv of Suzuki Verona (engine_cylinders, engine_fuel)
cars <- cars %>% mutate(engine_cylinders = ifelse(model == 'Verona', 6, engine_cylinders), engine_fuel_type = ifelse(model  == 'Verona', 'regular unleaded', engine_fuel_type))

## remove rotaryy engine mv
cars <- cars %>% filter(!is.na(engine_cylinders))

cars %>% filter(is.na(number_of_doors))
cars <- cars %>% mutate(number_of_doors = ifelse(make == 'Tesla' & model == 'Model S' & year == 2016,4, ifelse(make == 'Ferrari' & model == 'FF' & year == 2013,2,number_of_doors)))


View(cars %>% filter(is.na(engine_hp)))

cars <- cars %>% mutate(engine_hp = ifelse(model == '500e', 111, engine_hp))
cars <- cars %>% mutate(engine_hp = ifelse(model == 'Continental' & make == 'Lincoln', 350, engine_hp))
cars <- cars %>% mutate(engine_hp = ifelse(model == 'Escape' & make == 'Ford', 206, engine_hp))
cars <- cars %>% mutate(engine_hp = ifelse(model == 'Fit EV' & make == 'Honda', 120, engine_hp))
cars <- cars %>% mutate(engine_hp = ifelse(model == 'Focus' & make == 'Ford', 143, engine_hp))
cars <- cars %>% mutate(engine_hp = ifelse(model == 'Freestar' & make == 'Ford', 195, engine_hp))
cars <- cars %>% mutate(engine_hp = ifelse(model == 'Impala' & make == 'Chevrolet', 196, engine_hp))
cars <- cars %>% mutate(engine_hp = ifelse(model == 'Leaf' & make == 'Nissan', 107, engine_hp))
cars <- cars %>% mutate(engine_hp = ifelse(model == 'MKZ' & make == 'Lincoln', 188, engine_hp))
cars <- cars %>% mutate(engine_hp = ifelse(model == 'Model S' & make == 'Tesla', 400, engine_hp))
cars <- cars %>% mutate(engine_hp = ifelse(model == 'RAV4 EV' & make == 'Toyota', 154, engine_hp))
cars <- cars %>% mutate(engine_hp = ifelse(model == 'Soul EV' & make == 'Kia', 109, engine_hp))
cars <- cars %>% filter(!is.na(engine_hp))
miss_var_summary(cars)


summary(cars)


cars %>% select(msrp, year, make, model) %>% filter(msrp== 2000)

cars %>% select(msrp, year, make, model) %>% filter(msrp == 2000) %>% group_by(year) %>% count()
cars %>% filter(msrp<=10000) %>% group_by(make, model) %>% count()
cars %>% select(make, model, year, msrp) %>% filter(msrp>=500000)
cars <- cars %>% filter(!msrp>=500000)
cars
cars %>% select(year, msrp) %>% plot
cars <- cars %>% filter(year>=2001) %>% arrange(year)
summary(cars)

cars %>% select(msrp, make, model, year) %>% filter(msrp<=10000)
cars %>% select(msrp, make, model, year) %>% filter(msrp>150000 & msrp<200000)
cars <- cars %>% filter(msrp<150000)

fit <- lm(msrp~year, cars)
summary(fit)
library(olsrr)
olsrr::ols_plot_diagnostics(fit)
olsrr::ols_plot_dfbetas(fit)

cars <- cars %>% filter(!(year<=2010 & msrp>= 100000))

str(cars$market_category)
View(cars %>% select(market_category) %>% group_by(market_category) %>% count())
View(cars %>% filter(market_category=='N/A') %>% group_by(make, model, year) %>% count())

colnames(cars)
View(cars %>% select(vehicle_style) %>% group_by(highway) %>% count())
plot(cars$popularity)
cars %>% select(engine_hp) %>% hist(engine_hp)
is.numeric(cars$engine_hp)
hist(cars$popularity)
cars %>% filter(engine_hp>400) %>% group_by(make,model,engine_hp, year) %>% count() %>% print(n=100)

### fix obs error with  highway_mpg
cars <- cars %>% mutate(highway_mpg = ifelse(highway_mpg == 354, 34, highway_mpg))

cars %>% select(make,model,year,engine_cylinders) %>% filter(engine_cylinders==3)

cars %>% filter(city_mpg>60) %>% select(make,model, city_mpg, engine_fuel_type) %>% print(n=65)

cars %>% filter(popularity>5000) %>% select(make,model,year,msrp,popularity) %>% group_by(make, model) %>% count()

summary(cars$popularity)
cars %>% filter(make=='Ford') %>% select(popularity) %>% summary()

cars %>% filter(!make=='Ford')%>% select(popularity) %>% summarize(median(popularity))

cars %>% mutate(popularity = ifelse(make=='Ford', 1385, popularity))

cars %>% filter(popularity>=3000 & popularity<=4000) %>% select(make,model,popularity) %>% group_by(make, model, popularity) %>% count() %>% print(n=63)


cars %>% filter(make=='FIAT') %>% select(popularity) %>% summary()


hist(cars$msrp)
summary(cars)

factor_vars <- c("make", "model", "engine_fuel_type", "engine_cylinders", "transmission_type", "driven_wheels", "number_of_doors", "market_category", "vehicle_size", "vehicle_style")
cars[factor_vars] <- lapply(cars[factor_vars], factor)
str(cars)
cars %>% ggplot(aes(x = year, y = msrp, color = market_category)) + geom_point()
levels(cars$market_category)
summary(cars$market_category)


### setting ordered factor levels for market_category based on mean msrp of each category
mc_levels <- cars %>% select(msrp, market_category) %>%
  group_by(market_category) %>% summarize(mean(msrp)) %>% arrange(desc(`mean(msrp)`))


cars %>% filter(market_category=='Crossover,Factory Tuner,Luxury,High-Performance') %>% 
  select(make, model, msrp)



cars$market_category <- factor(cars$market_category, ordered = TRUE, levels = mc_levels$market_category)


cars %>% select(msrp, market_category) %>%
  ggplot(aes(x = msrp, fill=market_category))+
  geom_histogram()+theme(legend.position='none')


colnames(cars)


### 

### setting ordered levels for engine_cylinders
ec_levels <- cars %>% select(msrp, engine_cylinders) %>%
  group_by(engine_cylinders) %>% summarize(mean(msrp)) %>% arrange(desc(`mean(msrp)`))

cars$engine_cylinders <- factor(cars$engine_cylinders, ordered=TRUE, levels=ec_levels$engine_cylinders)


### ordering transmisssion_type
cars %>% ggplot(aes(y = msrp, fill = transmission_type))+geom_boxplot()

tt_levels <- cars %>% select(msrp, transmission_type) %>%
  group_by(transmission_type) %>% summarize(mean(msrp)) %>% arrange(desc(`mean(msrp)`))

cars$transmission_type <- factor(cars$transmission_type, ordered=TRUE, levels=tt_levels$transmission_type)

### ordering number_of_doors
cars %>% ggplot(aes(y = msrp, fill= number_of_doors))+geom_boxplot()
# not worth ordering

### ordering vehicle_style
cars %>% ggplot(aes(y = msrp, fill = vehicle_style))+geom_boxplot()

vs_levels <- cars %>% select(msrp, vehicle_style) %>%
  group_by(vehicle_style) %>% summarize(median(msrp)) %>% arrange(desc(`median(msrp)`))

cars$vehicle_style <- factor(cars$vehicle_style, ordered=TRUE, levels=vs_levels$vehicle_style)

# ordering vehicle_size

vsi_levels <- cars %>% select(msrp, vehicle_size) %>%
  group_by(vehicle_size) %>% summarize(median(msrp)) %>% arrange(desc(`median(msrp)`))

cars$vehicle_size <- factor(cars$vehicle_size, ordered=TRUE, levels=vsi_levels$vehicle_size)


median(cars$msrp)

install.packages('polycor')
library(polycor)



install.packages("Hmisc")
library(Hmisc)
res2 <- rcorr(as.matrix(cars))
cor(cars)
corrplot::cor.mtest(cars)
install.packages('correlation')
library(correlation)
correlation::correlation(cars)
plot(cars)

##plotting vars against msrp
cars %>% ggplot(aes(x = msrp, fill = make)) + geom_histogram()

corrplot(cars)
install.packages('ggcorrplot')
library(ggcorrplot)
model.matrix(~0+., data = cars) %>% cor(use='pairwise.complete.obs') %>% ggcorrplot(show.diag = F, type = 'lower', lab = TRUE, lab_size=2)


cor(cars[-16], cars$msrp)
i1 <- sapply(cars, is.numeric)
y1 <- 'msrp'
x1 <- setdiff(names(cars)[i1], y1)
num_corr <- cor(cars[x1], cars[[y1]])
cnames <- c('year', 'engine_hp', 'highway_mpg', 'city_mpg', 'popularity')
num_corr <- cbind(num_corr, cnames) %>% as.data.frame()
num_corr
num_corr %>% ggplot(aes(x = cnames, y = V1, color = cnames))+geom_point()+theme_classic()+ggtitle("MSRP correlation with numeric variables")+labs(x = "Numeric Variables", y = 'Correlation')+theme(legend.position='none')



cars %>% ggplot(aes(x = engine_fuel_type, y = msrp)) +geom_point()


hetcor(cars, )

cat1 <- sapply(cars, is.factor)
cat1_names <- setdiff(names(cars)[cat1], y1)
cat1_names
summary(aov(cars$msrp~cars$vehicle_style))

cor.test(x=cars$msrp, y=cars$transmission_type, method='spearman')

class(cars$engine_hp)

cars %>% ggplot(aes(x=msrp,  y=..density..))+geom_freqpoly(aes(color = transmission_type),  binwidth = 5000)

cars %>% ggplot(aes(x=reorder(vehicle_size, msrp, FUN=median), y = msrp))+geom_boxplot()
hetcor(cars, use='pairwise.complete.obs')
hetcor(cars$msrp, cars$engine_fuel_type, cars$model, cars$make, cars$engine_cylinders, cars$transmission_type, cars$driven_wheels, cars$number_of_doors, cars$market_category, cars$vehicle_size, cars$vehicle_style)


summary(cars$vehicle_size)




