
# source scripts
source('data-raw/input_dictionaries_creation.R')
source('data-raw/input_names_creation.R')

# use data
devtools::use_data(
dic_admin_div_choices, dic_admin_div_fil_choices, dic_agg_level_choices, dic_color_choices,
dic_espai_tipus_choices, dic_espai_tipus_fil_choices, dic_grup_func_choices, dic_ifn_choices,
dic_mida_choices, dic_statistic_choices, dic_tipo_grup_func_choices, dic_viz_shape_choices,
label_admin_div, label_admin_div_fil, label_agg_level, label_apply_filters,
label_diam_class, label_espai_tipus, label_espai_tipus_fil, label_grup_func,
label_ifn, label_mida, label_show_adv_fils, label_statistic,
label_tipo_grup_func, label_viz_shape, names_comarcas, names_municipios,
names_provincias, names_veguerias, polygons_comarques, polygons_dictionary,
polygons_enpe, polygons_municipis, polygons_pein, polygons_provincies,
polygons_vegueries, polygons_xn2000, label_tabpanel_visualization,
label_infopanel_plot, label_infopanel_variables, label_shape_click_info,
dic_col_vis_input, dic_adv_fil_filters,

internal = TRUE, overwrite = TRUE
)
