project <- "https://data.world/jdramosf/f-17-eda-project-2"
data.world::set_config(cfg_env("DW_API"))
df <- data.world::query(data.world::qry_sql("SELECT * FROM data"), dataset = project)
attach(df)
summary(df)
