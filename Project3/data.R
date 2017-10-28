project <- "https://data.world/jdramosf/f-17-eda-project-3"
data.world::set_config(cfg_env("DW_API"))
cancer <- data.world::query(
  data.world::qry_sql("select * from cancer_reg"),
  dataset = project
)
attach(cancer)
summary(cancer)