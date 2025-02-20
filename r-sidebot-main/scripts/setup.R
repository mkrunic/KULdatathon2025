library(duckdb)
library(DBI)
library(here)

db_path <- here("climate.duckdb")

# Delete if exists
if (file.exists(db_path)) {
  unlink(db_path)
}

# Load tips.csv into a table named `tips`
conn <- dbConnect(duckdb(), dbdir = db_path)
duckdb_read_csv(conn, "climate", here("topic_scores_output.csv"), delim = ";")
dbDisconnect(conn)
