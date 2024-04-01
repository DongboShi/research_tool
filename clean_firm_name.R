#! /usr/bin/env Rscript

library(readr)
library(argparse)
library(stringr)
suppressPackageStartupMessages(library(dplyr))

# 创建解析器
parser <- ArgumentParser(description="Clean Company Names from a CSV File")

# 添加文件名参数
parser$add_argument("filename", type="character", 
                    help="The CSV file containing company names to be cleaned. The column name of the company should be 'company_name'")
parser$add_argument("output_filename", type="character", 
                    help="The CSV file to write the cleaned company names")

# 解析命令行参数
args <- parser$parse_args()
input_filename <- args$filename
output_filename <- args$output_filename

# 清洗公司名的函数
clean_company_name <- function(company_name) {
  name_neat <- str_remove_all(company_name, "菲律宾|西班牙|维尔京群岛|汶莱|法国|澳洲|意大利|塞舌尔|安圭拉|埃及|马来西亚|加拿大|印尼|南非|南韩|巴拿马|文莱|毛里求斯|泰国|萨摩亚|株|澳大利亚|澳门|韩国|香港|荷兰|美国|香港|英国|英属|日本|德国|新加坡|台湾|印度")
  name_neat <- str_remove_all(name_neat, "省|市|总厂|工厂|总公司|有限公司|集团|厂|集团有限公司|有限责任公司|集团股份有限公司|股份有限公司|公司|研究所|研究院")
  name_neat <- str_remove_all(name_neat, "^\\s*[0-9]+\\s*")
  name_neat <- str_remove_all(name_neat, "[\\(\\)\\?\\-~_]|（|）|\\.|、")
  return(name_neat)
}


# 读入数据，清洗每个公司名称
clean_name <- function(input_filename, output_filename) {
  company_df <- read_csv(input_filename, show_col_types = FALSE)
  
  # 检查company_name列是否存在
  if (!"company_name" %in% colnames(company_df)) {
    stop("Error: 'company_name' column not found in the provided CSV file.")
  }
  
  company_df <- company_df %>%
    mutate(name_neat = sapply(company_name, clean_company_name))
  
  write_csv(company_df, output_filename)
}

# 使用tryCatch调用函数，并只显示错误消息
tryCatch({
  # 调用函数
  clean_name(args$filename, args$output_filename)
  print(paste0("Done! Cleaned data has been saved to ", args$output_filename, "."))
}, error = function(e) {
  cat(e$message) 
})
