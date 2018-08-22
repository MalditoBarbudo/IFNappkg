app <- ShinyDriver$new("../", seed = 25)
app$snapshotInit("mod_viz_basic")

# Input '`mod_mapUI-map_groups`' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(`mod_dataInput-mod_vizInput-color` = "bm",
              `mod_dataInput-mod_vizInput-inverse_pal` = TRUE,
              `mod_dataInput-mod_vizInput-tipo_grup_func` = "genere",
              `mod_dataInput-mod_vizInput-grup_func` = "Fagus",
              `mod_dataInput-mod_vizInput-statistic` = "_q95",
              wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
# Input '`mod_mapUI-map_click`' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(`mod_dataInput-agg_level` = "planifconif", wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
app$snapshot()
app$setInputs(`mod_dataInput-mod_vizInput-color` = "bm",
              `mod_dataInput-mod_vizInput-inverse_pal` = FALSE,
              `mod_dataInput-mod_vizInput-grup_func` = "Conífera",
              `mod_dataInput-mod_vizInput-statistic` = "_min",
              wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
app$snapshot()
app$setInputs(`mod_dataInput-viz_shape` = "parcela", wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
# Input '`mod_mapUI-map_groups`' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(`mod_dataInput-mod_vizInput-color` = "precipitacioanual",
              `mod_dataInput-mod_vizInput-inverse_pal` = TRUE,
              `mod_dataInput-mod_vizInput-mida` = "ab",
              `mod_dataInput-mod_vizInput-grup_func` = "Planifoli",
              wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
app$snapshot()
app$setInputs(`mod_dataInput-agg_level` = "parcela", wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
app$snapshot()
app$setInputs(`mod_dataInput-mod_vizInput-color` = "bat",
              `mod_dataInput-mod_vizInput-inverse_pal` = FALSE,
              `mod_dataInput-mod_vizInput-mida` = "cadesccon_dom_percdens",
              `mod_dataInput-mod_vizInput-tipo_grup_func` = "planifconif",
              `mod_dataInput-mod_vizInput-grup_func` = "Planifoli",
              wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
app$snapshot()
app$setInputs(`mod_dataInput-diameter_classes` = TRUE, wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
app$snapshot()
app$setInputs(`mod_dataInput-mod_vizInput-color` = "ab",
              `mod_dataInput-mod_vizInput-inverse_pal` = TRUE,
              `mod_dataInput-mod_vizInput-mida` = "temperaturamitjanaanual",
              `mod_dataInput-mod_vizInput-tipo_grup_func` = "especie",
              `mod_dataInput-mod_vizInput-grup_func` = "Salix",
              wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
app$snapshot()
app$setInputs(`mod_dataInput-agg_level` = "planifconif",
              `mod_dataInput-mod_vizInput-inverse_pal` = FALSE,
              wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
app$snapshot()
app$setInputs(`mod_dataInput-viz_shape` = "polygon",
              `mod_dataInput-mod_vizInput-inverse_pal` = TRUE,
              `mod_dataInput-mod_vizInput-statistic` = "_mean",
              wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
app$snapshot()
