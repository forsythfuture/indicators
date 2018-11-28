# this script calculates percentage of people under 250% poverty

con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

# household valriable
house_vars <- c('TYPE', 'SERIALNO', 'PUMA', 'ST', 'TYPE', '')