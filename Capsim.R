library(readr)
library(stringr)
library(magrittr)
library(dplyr)
library(pdftools)

summarize <- function(pdf_string){
  
  page_one <- pdf_string[[1]] %>%
    strsplit('\n') %>%
    unlist()
  
  page_two <- pdf_string[[2]] %>%
    strsplit('\n') %>%
    unlist()
  
  
  teams <- c("Andrews", "Baldwin", "Chester", "Digby", "Erie", "Ferris")
  
  numbers_string <- page_one %>%
    grep("[0-9]", ., value = TRUE)
  
  clean <- function(index){
    negatives_vector <- numbers_string[index] %>%
      strsplit(regex("[-(]")) %>%
      unlist() %>%
      gsub("[,$]", "", .)
    
    negatives <- numeric()
    for (i in negatives_vector[-1]){
      temp_negative_list <- strsplit(i, "[%)]") %>%
      unlist() %>%
      strsplit(., " ") %>%
      unlist() %>%
      as.numeric()
      negatives <- c(negatives, temp_negative_list[1])
    }
    
    numbers <- numbers_string[index] %>%
      str_split(., boundary("word")) %>%
      unlist() %>%
      grep("[0-9]", ., value = TRUE) %>%
      gsub(",", "", .) %>%
      as.numeric()
    
    counter <- 0
    for (i in numbers){
      counter <- counter + 1
      for (j in negatives){
        if (i == j){
          numbers[counter] <- -i
        }
      }
    }
    
    return(numbers)
  }
  
  ros <- clean(4)
  asset_turnover <- clean(5)
  roa <- clean(6)
  leverage <- clean(7)
  roe <- clean(8)
  emergency_loan <- clean(9)
  sales <- clean(10)
  ebit <- clean(11)
  profits <- clean(12)
  cumulative_profit <- clean(13)
  sga <- clean(14)
  contrib_margin <- clean(15)
  
  market_cap <- numeric()
  
  get_market_cap <- function(i){
    market_cap_string <- page_two[i] %>%
      str_split(., boundary("word")) %>%
      unlist() %>%
      grep("[0-9]", ., value = TRUE)
    return(as.numeric(market_cap_string[4]))
  }
  
  for (i in c(7:12)){
    market_cap <- c(market_cap, get_market_cap(i))
  }
  
  summary <- data.frame("Team" = teams,
                        "ROS" = ros,
                        "Asset Turnover" = asset_turnover,
                        "ROA" = roa,
                        "Leverage" = leverage,
                        "ROE" = roe,
                        "Emergency Loan" = emergency_loan,
                        "Sales" = sales,
                        "EBIT" = ebit,
                        "Profits" = profits,
                        "Cumulative Profit" = cumulative_profit,
                        "SGA" = sga,
                        "Contrib Margin" = contrib_margin,
                        "Market Cap" = market_cap)
  return(summary)
}

get_scores <- function(summaries){
  roa <- numeric()
  roe <- numeric()
  cumulative_profit <- numeric()
  market_cap <- numeric()
  for (summary in summaries){
    roa <- c(roa, summary["ROA"])
    roe <- c(roe, summary["ROE"])
    cumulative_profit <- c(cumulative_profit, summary["Cumulative.Profit"])
    market_cap <- c(market_cap, summary["Market.Cap"])
  }
  list_mean <- function(stat_list){
    sum_of_vectors <- c(0, 0, 0, 0, 0, 0)
    for (v in stat_list){
      sum_of_vectors <- sum_of_vectors + unlist(v)
    }
    return(sum_of_vectors/length(stat_list))
  }
  roa_mean <- list_mean(roa)
  roe_mean <- list_mean(roe)
  final_profit <- cumulative_profit %>%
                    tail(., n=1)
  final_market_cap <- market_cap %>%
                    tail(., n=1)
  
  scores <- 0.25* ((roa_mean/max(roa_mean)) +
                     (roe_mean/max(roe_mean)) +
                     (final_profit[[1]]/max(final_profit[[1]])) +
                     (final_market_cap[[1]]/max(final_market_cap[[1]])))
  
  counter <- 0
  for (score in scores){
    counter <- counter + 1
    if (score < 0){
      scores[counter] <- 0
    }
  }
  return(scores)
}

import_pdf <- function(pdf_title){
  pdf_title %>%
    pdf_text() %>%
    summarize() %>%
    return()
}

products <- function(pdf_title, c){
  if (c == "Low"){
    n = 5
  }
  if(c == "High"){
    n = 6
  }
  page <- pdf_text(pdf_title)[[n]] %>%
    str_split('\n') %>%
    unlist()
  
  separate <- function(i){
    separated <- page[i] %>%
      str_split(., boundary("word")) %>%
      unlist()
    return(separated)
  }
  
  numbers <- function(i){
    stat_list <- separate(i) %>%
      grep("[0-9]", ., value = TRUE) %>%
      gsub("[,$]", "", .) %>%
      as.numeric()
    return(stat_list)
  }
  
  final = length(page) - 2
  names = character()
  performance = numeric()
  size = numeric()
  price = numeric()
  mtbf = numeric()
  age = numeric()
  awareness = numeric()
  accessibility = numeric()
  
  for (i in c(18:final)){
    names = c(names, separate(i)[1])
    performance = c(performance, numbers(i)[6])
    size = c(size, numbers(i)[7])
    price = c(price, numbers(i)[8])
    mtbf = c(mtbf, numbers(i)[9])
    age = c(age, numbers(i)[10])
    awareness = c(awareness, numbers(i)[12]/100)
    accessibility = c(accessibility, numbers(i)[14]/100)
  }
  
  product_stats <- data.frame("Names" = names,
                              "Performance" = performance,
                              "Size" = size,
                              "Price" = price,
                              "MTBF" = mtbf,
                              "Age" = age,
                              "Awareness" = awareness,
                              "Accessibility" = accessibility)
  
  return(product_stats)
}

logit <- function(x){
  return(log(x/(1-x)))
}

add_survey_scores <- function(pdf_title, new_product, market_segment){
  #new_product is vector with elements price, performance, size, age, mtbf, awareness, accessibility in order
  if (market_segment == "Low"){
    page_number <- 5
    performance_and_size_index <- 13
    price_range <- c(15, 35)
    ideal_age <- 3
    age_sd <- 2.2
    mtbf_range <- c(14000, 20000)
  }
  if (market_segment == "High"){
    page_number <- 6
    performance_and_size_index <- 10
    price_range <- c(25, 45)
    ideal_age <- 0
    age_sd <- 1.9
    mtbf_range <- c(17000, 23000)
  }
  
  page <- pdf_text(pdf_title)[[page_number]] %>%
    str_split('\n') %>%
    unlist()
  
  performance_and_size <- page[performance_and_size_index] %>%
    strsplit(., " ") %>% 
    unlist() %>%
    grep("[0-9]", ., value=TRUE) %>%
    as.numeric()
  
  ideal_performance <- performance_and_size[2]
  ideal_size <- performance_and_size[3]
  
  price_score <- logit((price_range[2] - new_product[1]) / 20)
  performance_and_size_score <- logit((2.5 - sqrt((new_product[2] - ideal_performance) ^ 2 + (new_product[3] - ideal_size) ^ 2)) / 2.5)
  age_score <- logit(dnorm(new_product[4], ideal_age, age_sd) / dnorm(ideal_age, ideal_age, age_sd))
  mtbf_score <- logit((new_product[5] - 14000) / 6000)
  
  if (market_segment == "Low"){
    base_score <- 0.41 * price_score + 0.29 * age_score + 0.21 * mtbf_score + 0.09 * performance_and_size_score
  }
  if (market_segment == "High"){
    base_score <- 0.33 * performance_and_size_score + 0.29 * age_score + 0.25 * price_score + 0.13 * mtbf_score
  }
  adjusted_base_score <- (base_score/10) ^ 2
  final_score = adjusted_base_score * (1 - (1-new_product[6])/2) * (1 - (1 - new_product[7])/2)
 }

round_four_high <- products("Round 4.PDF", "High")
round_four_low <- products("Round 4.PDF", "Low")


round_one_summary <- import_pdf("Round 1.PDF")
round_two_summary <- import_pdf("Round 2.PDF")
round_three_summary <- import_pdf("Round 3.PDF")
round_four_summary <- import_pdf("Round 4.PDF")
round_five_summary <- import_pdf("Round 5.PDF")
round_six_summary <- import_pdf("Round 6.PDF")

summaries <- list(round_one_summary, round_two_summary, round_three_summary, round_four_summary, round_five_summary, round_six_summary)
get_scores(summaries)