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
  
  numbers <- page_one %>%
    grep("[0-9]", ., value = TRUE)
  
  clean <- function(index, k = 1){
    i_string <- numbers[index] %>%
      str_split(., boundary("word")) %>%
      unlist() %>%
      grep("[0-9]", ., value = TRUE) %>%
      gsub(",", "", .) %>%
      as.numeric()
    return(i_string)
  }
  
  ros <- clean(4)
  asset_turnover <- clean(5, 2)
  roa <- clean(6)
  leverage <- clean(7)
  roe <- clean(8)
  emergency_loan <- clean(9, 2)
  sales <- clean(10)
  ebit <- clean(11)
  profits <- clean(12)
  cumulative_profit <- clean(13, 2)
  sga <- clean(14, 3)
  contrib_margin <- clean(15, 2)
  
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
  return(scores)
}

import_pdf <- function(pdf_title){
  pdf_title %>%
    pdf_text() %>%
    summarize() %>%
    return()
}

round_one_summary <- import_pdf("Round 1.PDF")
round_two_summary <- import_pdf("Round 2.PDF")
round_three_summary <- import_pdf("Round 3.PDF")
round_four_summary <- import_pdf("Round 4.PDF")

summaries <- list(round_one_summary, round_two_summary, round_three_summary, round_four_summary)

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
  
  final = length(page_five) - 2
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

products("Round 2.PDF", "High")