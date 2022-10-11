# Data-Analysis-With-R


# Sales Data Analysis In R including visualisations

This Analysis forms my first project in  R which i will utilize the "tidyverse","dplyr","ggplot2" and "tidyr" libraries to manipulate data and produce some visuals.

## Step 1: Importing data data

The data is imported from an excel document into a data frame called foodsales."all libraries already installed"

```{r Load sales Data}
library(readxl)
foodsales <- read_excel("documents/sampledatafoodsales.xlsx")
```

## Step 2: Loading Libraries
Here we load the libraries(all liraries already installed)

```{r Load  libraries }
library(tidyverse)
library(dplyr)
library(ggplot2)
library(janitor)
library(dplyr)
library(tidyr)
library(lubridate)
options(dplyr.summarise.inform = FALSE)# this suppresses the summarise group warnings
```



## Step 2: Checking the Data and columns
Here we check the data to view the first few rows, chech the columns and a get a glimpse of the data
```{r Checking data}
head(foodsales)#Views the first 6 rows 
slice(foodsales,1:10) #Views the first 10 rows
colnames(foodsales)# Checks the column name headers
str(foodsales) #check also the data types for the data 
```

## Step 2: clean the column names and check if all datatypes are correct

Before Analysis we clean the column names and check data types for the data

```{r Cleaning columns}
my_sales_clean_data<-clean_names(foodsales) # Clean the data
#colnames(clean) # view the new column Names
my_sales_clean_data <- na.omit(my_sales_clean_data)#remove Nulls
slice(my_sales_clean_data,1:10) # View first 10 lines using the new columns

```

## Step 2: Analyse the data

### View Sales By Category

Here we do some analysis to check which categories had the most sales

```{r Sales By category}
 my_sales_clean_data %>% 
  select(city, category, product,order_date, quantity, unit_price, total_price) %>% 
  group_by(category) %>%
summarise( total_sales_by_category = sum(total_price)) %>% 
arrange(desc(total_sales_by_category))
```

### View Sales By Product

Here we do some analysis to check which Products had the most sales

```{r Sales By Products}
 my_sales_clean_data %>% 
  select(city, category, product,order_date, quantity, unit_price, total_price) %>% 
  group_by(product) %>%
summarise( total_sales_by_product = sum(total_price)) %>% 
arrange(desc(total_sales_by_product))
```

### View Sales By Order date
Here we do some analysis to check which Products had the most sales in a particular period"the library(lubridate)" is used to get the year and month pat

```{r Sales By Order date}
my_sales_clean_data %>% 
  mutate(month_name = month(order_date, label = TRUE)) %>% 
  mutate(saleyear = year(order_date)) %>%
 group_by(saleyear,month_name) %>% 
   summarise( total_sales_by_date = sum(total_price)) %>% 
   arrange(saleyear,month_name)
```

#### View Sales By Order date(2020 sales)

```{r 2020 sales}
my_sales_clean_data %>% 
  mutate(month_name = month(order_date, label = TRUE)) %>% 
  mutate(saleyear = year(order_date)) %>%
  filter(saleyear == 2020) %>% 
 group_by(month_name) %>% 
   summarise( total_sales_by_date = sum(total_price)) %>% 
    arrange(desc(total_sales_by_date))
```

#### View Sales By Order date(2021 sales)


```{r 2021 sales}
my_sales_clean_data %>% 
  mutate(month_name = month(order_date, label = TRUE)) %>% 
  mutate(saleyear = year(order_date)) %>%
  filter(saleyear == 2021) %>% 
 group_by(month_name) %>% 
   summarise( total_sales_by_date = sum(total_price)) %>% 
   arrange(desc(total_sales_by_date))
```


## Analysis By Visualisation(Plots)

this section we analyse by visualization 

### Sales By product
```{r Sales by Product}
 ggplot(data = my_sales_clean_data ,aes(x= product, y= total_price, fill =product))+
geom_col() +
   theme(axis.text.x = element_text(angle = 45,colour = "blue"))+
   labs(title ="Sales By Product", 
        subtitle = "Product sales",
        caption = "Sample sales data downloaded for test purposes",
        x="Product",
        y="Total Sales")
```

### Sales By City by category(wrap)

Here we get a view of all Sales done for each City

```{r sales By city}
 ggplot(data = my_sales_clean_data)+ geom_col(mapping = aes(x= category, y= total_price, fill =category))+
   facet_wrap(~city)+
  labs(title ="Sales By City by category", 
      #  subtitle = "Sales By City by category",
        caption = "Sample sales data downloaded for test purposes",
        x="Category",
        y="Total Sales")
```


### Sales By City by category(grid)


```{r Sales By City }
ggplot(data = my_sales_clean_data)+ geom_col(mapping = aes(x= category, y= total_price, fill =product))+
   facet_grid(~city)+
   theme(axis.text.x = element_text(angle = 45,colour = "blue"))+
   labs(title ="Sales By City By Category", 
        subtitle = "Products by Category",
        caption = "Sample sales data downloaded for test purposes",
 x="Product Category",
 y="Total Sales")
```


### Sales By order Date 

Total sales each month for all combined products each month

```{r}
 my_sales_clean_data %>% 
   mutate(month_name = month(order_date, label = TRUE)) %>% 
   mutate(saleyear = year(order_date)) %>%
   group_by(saleyear,month_name) %>% 
   summarise( total_sales_by_date = sum(total_price)) %>% 
 ggplot( mapping = aes(x= month_name, y= total_sales_by_date,color = factor(saleyear), group = factor(saleyear))) +
   geom_line()+
   theme(axis.text.x = element_text(angle = 45,colour = "blue"))+
   labs(title ="Monthly Sales(YOY)", 
        subtitle = "Sales by Order date",
        caption = "Sample sales data downloaded for test purposes",
        x="Product Category",
        y="Total Sales")
```

### Month Sales By Product(multiple lines) (2020)


```{r Sales by date 2020 by products}
my_sales_clean_data %>% 
   mutate(month_name = month(order_date, label = TRUE)) %>% 
   mutate(saleyear = year(order_date)) %>%
    filter(saleyear== 2020) %>% 
   group_by(saleyear,month_name,product) %>% 
   summarise( total_sales_by_date = sum(total_price)) %>% 
 ggplot( mapping = aes(x= month_name, y= total_sales_by_date,color = product, group = factor(product))) +
   geom_line()+
   theme(axis.text.x = element_text(angle = 45,colour = "blue"))+
   labs(title ="Monthly Sales(YOY)", 
        subtitle = "Sales by Order date",
        caption = "Sample sales data downloaded for test purposes",
        x="Product Category",
        y="Total Sales")
```


### Month Sales By Category(multiple lines) (2020)


```{r Sales by date 2020 by Category}
my_sales_clean_data %>% 
   mutate(month_name = month(order_date, label = TRUE)) %>% 
   mutate(saleyear = year(order_date)) %>%
    filter(saleyear== 2020) %>% 
   group_by(month_name,category ) %>% 
   summarise( total_sales_by_date = sum(total_price)) %>% 
 ggplot( mapping = aes(x= month_name, y= total_sales_by_date,color = category , group = factor(category ))) +
   geom_line()+
   theme(axis.text.x = element_text(angle = 45,colour = "blue"))+
   labs(title ="Monthly Sales(YOY)", 
        subtitle = "Sales by Order date",
        caption = "Sample sales data downloaded for test purposes",
        x="Product Category",
        y="Total Sales")
```


### Month Sales By Category (Pie) (2020)


```{r}
my_sales_clean_data %>%
 # mutate(month_name = month(order_date, label = TRUE)) %>% 
   mutate(saleyear = year(order_date)) %>%
    filter(saleyear== 2020) %>%
   group_by(category) %>%
   summarise( total_sales_by_cat = sum(total_price)) %>% 
   mutate(total_sales_by_cat_perc = `total_sales_by_cat` / sum(`total_sales_by_cat`)) %>% 
   mutate(labels = scales::percent(total_sales_by_cat_perc,accuracy = 1L)) %>% 
   ggplot( aes(x = "", y = total_sales_by_cat_perc, fill = category)) +
   geom_col(color = "black") +
   geom_label(aes(label =  labels),
              color = "white",
              position = position_stack(vjust = 0.5),
              show.legend = FALSE) +
   coord_polar(theta = "y")
```



## The End





