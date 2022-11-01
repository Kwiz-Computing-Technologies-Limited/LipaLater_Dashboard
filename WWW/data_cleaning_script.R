# clean column names

clean_data = function(data) {
  ## - removing "Additional Loan Details -" from column names
  colnames(data) = gsub("Additional Loan Details -", "", colnames(data))
  
  colnames(data)[colnames(data) == " Item Type"] = " Item Type (Item 1)"
  colnames(data)[colnames(data) == " Item Brand"] = " Item Brand (Item 1)"
  colnames(data)[colnames(data) == " Item Purchase Price"] = " Item Purchase Price (Item 1)"
  colnames(data)[colnames(data) == " Partner Store"] = " Partner Store (Item 1)"
  
  
  ## - ensure all "item type" columns  are of the same data type (characters) and put all them into 1 column
  item_type = data |> select(starts_with(" Item Type")) |> colnames()
  data[, all_of(item_type)] = data[, all_of(item_type)] |>
    mutate_if(is.numeric, as.character)
  
  data = data |> 
    pivot_longer(cols = all_of(item_type),
                 names_to = c("index1", "index2"),
                 names_pattern = "^( Item Type)(.*)") |> 
    rename(`Item Type` = value) |>
    filter(!is.na(`Item Type`), `Item Type` != "0") |>
    select(-c(index1)) |> distinct() 
  
  
  ## - ensure all "item brand" columns  are of the same data type (characters) and put all them into 1 column
  item_brand = data |> select(starts_with(" Item Brand")) |> colnames()
  data[, all_of(item_brand)] = data[, all_of(item_brand)] |>
    mutate_if(is.numeric, as.character)
  
  data = data |> 
    pivot_longer(cols = all_of(item_brand),
                 names_to = c("index1", "index3"),
                 names_pattern = "^( Item Brand)(.*)") |> 
    rename(`Item Brand` = value) |>
    filter(index2 == index3) |>
    select(-c(index1, index3)) |> distinct() 
  
  
  ## - ensure all "item purchase price" columns  are of the same data type (numeric) and put all them into 1 column
  item_purchase_price = data |> select(starts_with(" Item Purchase Price")) |> colnames()
  data[, all_of(item_purchase_price)] = data[, all_of(item_purchase_price)] |>
    mutate_if(is.character, as.numeric)
  
  data = data |> 
    pivot_longer(cols = all_of(item_purchase_price),
                 names_to = c("index1", "index3"),
                 names_pattern = "^( Item Purchase Price)(.*)") |> 
    rename(`Item Purchase Price` = value) |>
    filter(index2 == index3) |>
    select(-c(index1, index3)) |> distinct() 
  
  
  ## - ensure all "Partner stores" columns  are of the same data type (characters) and put all them into 1 column
  partner_stores = data |> select(starts_with(" Partner Store")) |> colnames()
  data[, all_of(partner_stores)] = data[, all_of(partner_stores)] |>
    mutate_if(is.numeric, as.character)
  
  data = data |> 
    pivot_longer(cols = all_of(partner_stores),
                 names_to = c("index1", "index3"),
                 names_pattern = "^( Partner Store)(.*)") |> 
    rename(`Partner Store` = value) |>
    filter(index2 == index3) |>
    select(-c(index1, index3)) |> distinct() |>
    rename(`Client Location` = ` Place Of Origin (Location Of Client)`,
           `Referral Source` = ` Referral Source`,
           `Submitted On Date` = ` Submitted On Date`)
  
  return(data)
}


# 