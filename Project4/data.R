project <- "https://data.world/natashapirani96/f-17-eda-project-4"
data.world::set_config(cfg_env("DW_API"))
bank <- data.world::query(
  data.world::qry_sql("select * from bank_additional"),
  dataset = project
)

attach(bank)
summary(bank)