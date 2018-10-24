app <- ShinyDriver$new("../", seed = 25)
app$snapshotInit("datos_inputs")

# Input '`mod_mapUI-map_groups`' was set, but doesn't have an input binding.
app$setInputs(
  `mod_dataInput-ifn` = "ifn2",
  `mod_dataInput-admin_div` = "provincia",
  `mod_dataInput-espai_tipus` = "enpes",
  `mod_dataInput-viz_shape` = "parcela"
)

Sys.sleep(15)

app$snapshot(list(output = "mod_mapUI-map"))
