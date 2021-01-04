df <- tibble(a = 1:5, b= letters[1:5], c=3:7)
df

mutate(df, great = if_else(c>4, "Yes", "No"))

df
