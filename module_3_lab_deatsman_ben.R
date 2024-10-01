install.packages('tidyverse')
install.packages('completejourney')
library(tidyverse)
library(completejourney)

products
transactions <- transactions_sample
transactions

## PART 1
#EXERCISE 1

transactions <- transactions %>%
  mutate(
    regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
    loyalty_price = (sales_value + coupon_match_disc) / quantity,
    coupon_price = (sales_value - coupon_disc) / quantity
  )%>%
  select(regular_price, loyalty_price, coupon_price, product_id, everything())
transactions


# Identify the five households with the largest loyalty_price transactions. What is unique about the
#   transaction with the largest loyalty_price value?

transactions %>%
  slice_max(order_by = loyalty_price, n = 5)

#There are infinite values for my created variables. 


# Now filter for only those observations where quantity was greater than 0. Now which household(s)
#   have the largest loyalty_price transaction?

transactions %>%
  filter(quantity > 0) %>%
  slice_max(order_by = loyalty_price, n = 5)

# Q3. Using the first transaction in the result from #2, filter the `products` data based
#  on the `product_id` to find out which product the largest `loyalty_price` transaction
#   is associated with.

products %>%
  filter(product_id == '12484608')

# Electronic Gift Cards. 


#EXERCISE 2
# Products with a regular price of $1 or less

transactions %>%
  filter(regular_price <= 1)%>%
  select(product_id) %>%
  n_distinct()

# 2748

# Products with loyalty price $1 or less

transactions %>%
  filter(loyalty_price <= 1)%>%
  select(product_id) %>%
  n_distinct()

# 4648

# Products with coupon price $1 or less

transactions %>%
  filter(coupon_price <= 1)%>%
  select(product_id) %>%
  n_distinct()

# 4844


#EXCERCISE 3
# What proportion of baskets are over $10 in sales value? What proportion of baskets are over $20 in sales
#   value?

proportions <- transactions %>%
  group_by(basket_id) %>%
  mutate(
    basket_over_10 = sales_value > 10,
    basket_over_20 = sales_value > 20
  ) %>%
  ungroup() %>%
  summarize(
    proportion_over_10 = mean(basket_over_10),
    proportion_over_20 = mean(basket_over_20)
  )
proportions

# 3% of transactions are over $10 and 1% are over $20.


#EXCERCISE 4
# Which stores had the largest total sales value and largest average loyalty discount?

transactions %>%
  group_by(store_id) %>%
  summarize(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  arrange(desc(total_sales_value))

transactions %>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  group_by(store_id) %>%
  summarize(avg_pct_loyalty_disc = mean(pct_loyalty_disc, na.rm =TRUE)) %>%
  arrange(desc(avg_pct_loyalty_disc))

# Store 367 had the largest total sales value and store 224 had the highest average loyalty discount.


##PART 2
#EXCERCISE 1

library(readxl)
excel_sheets(path = "C:/Users/deats/OneDrive/Desktop/Data Wrangling/Data Wrangling Projects/mbta.xlsx")
mbta <- read_excel(path = "C:/Users/deats/OneDrive/Desktop/Data Wrangling/Data Wrangling Projects/mbta.xlsx", skip = 1, na = 'NA')
mbta

##EXCERCISE 2

str(mbta)
head(mbta, 6)
summary(mbta)
colSums(is.na(mbta))


##EXCERCISE 3
# Removing rows 1, 7, and 11, and column 1.

mbta <- mbta %>%
  slice(-c(1, 7, 11)) %>%
  select(-1)

dim(mbta)
# 8 rows, 59 columns


##EXCERCISE 4
# Making wide data longer. 

mbta <- mbta %>%
  pivot_longer(
    cols = -mode, 
    names_to = "date",  
    values_to = "thou_riders" 
  )
dim(mbta)
# 464 rows, 3 columns


##EXCERCISE 5
#Separating variables into year and month.

mbta <- mbta %>%
  separate(
    col = date,
    into = c("year", "month"),
    sep = "-" 
  )
head(mbta)


##EXCERCISE 6
##Replacing a singular value

mbta %>%
  filter(thou_riders == 40) 
mbta$thou_riders[3] <- 4


##EXCERCISE 7
# Compute average ridership per mode

mbta %>%
  group_by(mode) %>%
  summarize(avg_ridership = mean(thou_riders))

# Compute average ridership per mode in month of January.

mbta %>%
  filter(month == '01') %>%
  group_by(mode) %>%
  summarize(avg_ridership = mean(thou_riders))

# Which year had the largest total ridership for the boat mode?

mbta %>%
  filter(mode == 'Boat') %>%
  group_by(year) %>%
  summarize(avg_ridership = sum(thou_riders))
# Answer: 2007