## objects_needed ####
source('data-raw/polygon_objects_creation.R')

## mod_data inputs ####

# ifn input
dic_ifn_choices <- list(

  ## catalan
  cat = c(
    'IFN 2' = 'ifn2',
    'IFN 3' = 'ifn3',
    'IFN 4' = 'ifn4',
    'IFN 3 respecte a IFN 2' = 'ifn3_ifn2',
    'IFN 4 respecte a IFN 3' = 'ifn3_ifn4'
  ),

  ## spanish
  esp = c(
    'IFN 2' = 'ifn2',
    'IFN 3' = 'ifn3',
    'IFN 4' = 'ifn4',
    'IFN 3 respecto a IFN 2' = 'ifn3_ifn2',
    'IFN 4 respecto a IFN 3' = 'ifn3_ifn4'
  ),

  ## english
  eng = c(
    'NFI 2' = 'ifn2',
    'NFI 3' = 'ifn3',
    'NFI 4' = 'ifn4',
    'NFI 3 compared to NFI 2' = 'ifn3_ifn2',
    'NFI 4 compared to NFI 3' = 'ifn3_ifn4'
  )

)

# viz_shape input
dic_viz_shape_choices <- list(

  ## catalan
  cat = c(
    'Parcel·les' = 'parcela',
    'Poligons' = 'polygon'
  ),

  ## spanish
  esp = c(
    'Parcelas' = 'parcela',
    'Polígonos' = 'polygon'
  ),

  ## english
  eng = c(
    'Plots' = 'parcela',
    'Polygons' = 'polygon'
  )

)

# admin_div input
dic_admin_div_choices <- list(

  ## catalan
  cat = c(
    Catalunya = '', Provincies = 'provincia', Vegueries = 'vegueria',
    Comarques = 'comarca', Municipis = 'municipi'
  ),

  ## spanish
  esp = c(
    Cataluña = '', Provincias = 'provincia', Veguerias = 'vegueria',
    Comarcas = 'comarca', Municipios = 'municipi'
  ),

  ## english
  eng = c(
    Catalonia = '', Provinces = 'provincia', Vegueries = 'vegueria',
    Region = 'comarca', Municipality = 'municipi'
  )

)

# espai_tipus input
dic_espai_tipus_choices <- list(

  ## catalan
  cat = c(
    'Nivell de protecció' = 'proteccio',
    "Espai d'interès Nacional" = 'nomein',
    "Espai de Protecció Especial" = 'enpes',
    "Xarxa Natura 2000" = 'nomxarxa2000'
  ),

  ## spanish
  esp = c(
    'Nivel de protección' = 'proteccio',
    "Espacio de Interes Nacional" = 'nomein',
    "Espacio de Protección Especial" = 'enpes',
    "Red Natura 2000" = 'nomxarxa2000'
  ),

  ## english
  eng = c(
    'Protection level' = 'proteccio',
    "National Space of Interest" = 'nomein',
    "Special Protection Space" = 'enpes',
    "Natura 2000 Net" = 'nomxarxa2000'
  )

)

# admin_div_fil input
dic_admin_div_fil_choices <- list(

  ## catalan
  cat = list(
    provincia = c('Totes' = '', names_provincias),
    vegueria = c('Totes' = '', names_veguerias),
    comarca = c('Totes' = '', names_comarcas),
    municipi = c('Tots' = '', names_municipios)
  ),

  ## spanish
  esp = list(
    provincia = c('Todas' = '', names_provincias),
    vegueria = c('Todas' = '', names_veguerias),
    comarca = c('Todas' = '', names_comarcas),
    municipi = c('Todos' = '', names_municipios)
  ),

  ## english
  eng = list(
    provincia = c('All' = '', names_provincias),
    vegueria = c('All' = '', names_veguerias),
    comarca = c('All' = '', names_comarcas),
    municipi = c('All' = '', names_municipios)
  )

)

# espai_tipus_fil input
dic_espai_tipus_fil_choices <- list(

  ##TODO general sections of the diferent lenguages and spaces must be revised
  ## to comply with the values of the variables to match (i.e. in the
  ## no_whatever)

  ## catalan
  cat = list(
    proteccio = list(
      general = c(
        "Tots" = '', "Només protegits" = 'only_protected',
        "Sense protecció" = 'no_protected'
      ),
      espais = c(
        "Paratge Natural d'Interès Nacional", "Parc Nacional", "Parc Natural",
        "Reserva Natural de Fauna Salvatge", "Reserva Natural Parcial",
        "Zona de Protecció"
      )
    ),
    nomein = list(
      general = c(
        "Tots" = '', "Només espais d'interès nacional" = 'only_protected',
        "Sense Pein" = 'no_protection'
      ),
      espais = c(
        "Aiguabarreig Segre-Noguera Pallaresa", "Aiguabarreig Segre-Noguera Ribagorçana",
        "Aiguamolls de l'Alt Empordà", "Aigüestortes", "Alta Garrotxa, l'",
        "Alt Pirineu, l'", "Ancosa-Montagut l'", "Artiga de Lin, Era", "Bessons, els",
        "Capçalera de la Noguera Ribagorçana", "Capçaleres del Ter i del Freser",
        "Cap de Creus", "Castell-Cap Roig", "Cingles de Bertí", "Collegats",
        "Collsacabra", "Conreria-Sant Mateu-Céllecs, la", "Costoja", "Estany de Banyoles",
        "Estany de Sils", "Eth Portilhon", "Faiada de Malpàs, la", "Foix, el", "Gallifa",
        "Gavarres, les", "Gelada", "Guilleries, les", "Marimanha", "Massís de l'Albera",
        "Massís de les Cadiretes", "Massís de les Salines", "Massís del Garraf",
        "Massís del Montseny", "Miracle, el", "Moianès, el", "Montanhes de Les e Bossòst",
        "Montesquiu", "Montgrí, el", "Montmell, el", "Montserrat", "Muntanyes de Begur",
        "Muntanyes de l'Ordal", "Muntanyes de Prades", "Muntanyes de Rocacorba",
        "Muntanyes de Tivissa-Vandellòs", "Naut Aran", "Obagues del Riu Corb", "Olèrdola",
        "Penya-segats de la Muga", "Pinya de Rosa", "Plana de Sant Jordi, la",
        "Ports, els", "Puig de la Banya del Boc", "Riba-roja", "Ribera Salada",
        "Riera de Merlès", "Riera de Navel", "Riera de Sorreigs", "Roques Blanques",
        "Saburella", "Sant Joan de Toran", "Sant Llorenç del Munt i l'Obac",
        "Savassona", "Serra Cavallera", "Serra d'Aubenç", "Serra de Bellmunt",
        "Serra de Boumort", "Serra de Carreu", "Serra de Castelltallat",
        "Serra de Collserola", "Serra del Catllaràs", "Serra de Llaberia",
        "Serra del Montsant", "Serra del Montsec", "Serra del Verd",
        "Serra de Miralles-Queralt", "Serra de Montgrony", "Serra de Montsià",
        "Serra d'Ensija-els Rasos de Peguera", "Serra de Picancel", "Serra de Queralt",
        "Serra de Sant Gervàs", "Serra de Turp", "Serra Llarga", "Serra Mitjana",
        "Serres de Busa-els Bastets-Lord", "Serres de Cardó-el Boix", "Serres del Cadí-el Moixeró",
        "Serres de Milany-Santa Magdalena i Puigsacalm-Bellmunt", "Serres de Montnegre-el Corredor",
        "Serres de Pàndols-Cavalls", "Serres de Pradell-l'Argentera",
        "Serres d'Odèn-Port del Comte", "Tossal Gros de Miramar", "Tossals d'Almatret",
        "Tossa Plana de Lles-Puigpedrós", "Tres Hereus, els", "Turons de Maçanet",
        "Turons de la Plana Ausetana", "Vall Alta de Serradell", "Vall del Riu Llobregós",
        "Volcà de la Crosa", "Zona Volcànica de la Garrotxa"
      )
    ),
    enpes = list(
      general = c(
        "Tots" = '', "Només espai de protecció especial" = 'only_protected',
        "Sense protecció" = 'no_protected'
      ),
      espais = c(
        "Paratge natural d'interès nacional de cap Gros-cap de Creus",
        "Paratge natural d'interès nacional de la vall del monestir de Poblet",
        "Paratge natural d'interès nacional de la Serra de Rodes",
        "Paratge natural d'interès nacional del massís de l'Albera",
        "Paratge natural d'interès nacional del Massís de Pedraforca",
        "Paratge natural d'interès nacional de Pinya de Rosa",
        "Parc nacional d'Aigüestortes i Estany de Sant Maurici",
        "Parc natural de Cap de Creus",
        "Parc natural de l'Alt Pirineu",
        "Parc natural de la Muntanya de Montserrat",
        "Parc natural de la Zona Volcànica de la Garrotxa",
        "Parc natural del Cadí-Moixeró",
        "Parc natural del Massís de Sant Llorenç del Munt i Serra de l'Obac",
        "Parc Natural del Montsant",
        "Parc natural dels Aiguamolls de l'Empordà",
        "Parc natural dels Ports",
        "Parc Natural Massís del Montseny",
        "Reserva Natural de Fauna Salvatge de l'Aiguabarreig Segre-Noguera Pallaresa",
        "Reserva natural parcial de Baish Aran",
        "Reserva natural parcial de la Capçalera de l'Orlina",
        "Reserva natural parcial de la Fageda de Jordà",
        "Reserva natural parcial de la Llosa",
        "Reserva natural parcial de l'Alt Àneu",
        "Reserva natural parcial de la Muntanya Montserrat",
        "Reserva natural parcial del Barranc de la Trinitat",
        "Reserva natural parcial del Barranc del Titllar",
        "Reserva natural parcial de les Fagedes dels Ports",
        "Reserva natural parcial  del volcà Aiguanegra",
        "Reserva natural parcial  del volcà  Croscat",
        "Reserva natural parcial  del volcà de Santa Margarida",
        "Reserva natural parcial  del volca Montolivet",
        "Reserva natural parcial  del volcà Puig Astrol",
        "Reserva natural parcial de Noguera Pallaresa-Bonaigua",
        "Reserva natural parcial de Riera de Merlès",
        "Reserva natural parcial de St Quirze de Colera",
        "Zona perifèrica de protecció del parc nacional",
        "Zona perifèrica de protecció del Parc natural de la Muntanya de Montserrat"
      )
    ),
    nomxarxa2000 = list(
      general = c(
        "Tots" = '', "Només en Xarxa Natura 2000" = 'only_protected',
        "Sense Xarxa Natura 2000" = 'no_protected'
      ),
      espais = c(
        "Aiguabarreig Segre- Noguera Pallaresa", "Aiguabarreig Segre-Noguera Ribagorçana",
        "Aiguamolls de l'Alt Empordà", "Aigüestortes",
        "Alta Garrotxa-Massís de les Salines", "Alt Pallars", "Baish Aran",
        "Barranc de Santes Creus", "Bellmunt-Almenara", "Beneïdor", "Capçaleres del Foix",
        "Capçaleres del Ter i del Freser", "Cap de Creus", "El Montgr-Les Medes-El Baix Ter",
        "El Montmell-Marmellar", "Els Bessons", "Era Artiga de Lin-Eth Portilhon",
        "Estany de Banyoles", "Estany de Sils-Riera de Santa Coloma",
        "Gallifa-Cingles de Bertí", "Garriga d'Empordà", "Granyena",
        "La Faiada de Malpàs i Cambatiri", "L'Albera", "Les Gavarres", "Les Guilleries",
        "Litoral del Baix Empordà", "Massís de Bonastre", "Massís de les Cadiretes",
        "Massís del Montseny", "Montgrony", "Montserrat-Roques Blanques- riu Llobregat",
        "Muntanyes de Prades", "Muntanyes de Rocacorba-Puig de la Baya del Boc",
        "Obagues de la riera de Madrona", "Obagues del riu Corb", "Prepirineu Central català",
        "Rasos de Tubau", "Ribera de l'Algars", "Ribera Salada", "Riberes de l'Alt Segre",
        "Riberes de l'Alt Ter", "Riberes del Baix Ter", "Riera de Clariana",
        "Riera de la Goda", "Riera de Merlès", "Riera de Sorreigs", "Riu Brugent",
        "Riu de la Llosa", "Riu Fluvià", "Riu Gaià", "Riu Llobregat d'Empordà-Riera de Torrelles",
        "Riu Siurana i planes del Priorat", "Sant Llorenç del Munt i l'Obac",
        "Secans de la Noguera", "Serra Cavallera",
        "Serra d'Aubenç i Roc de Cogul", "Serra de Boumort- Collegats",
        "Serra de Castelltallat", "Serra de Catllaràs", "Serra de Collserola",
        "Serra de Montsant-Pas de l'Ase", "Serra de Montsià", "Serra de Prada-Castellàs",
        "Serra de Turp i Mora Condal-Valldan", "Serres de Cardó - El Boix",
        "Serres del Litoral central", "Serres del litoral septentrional",
        "Serres del Montsec, Sant Mamet i Mitjana",
        "Serres de Queralt i Els Tossals-Aigua d'Ora", "Sistema prelitoral central",
        "Sistema prelitoral meridional", "Sistema transversal Català",
        "Tivissa-Vandellós-Llaberia", "Tossal de Montagut", "Tossals d'Almatret i Riba-roja",
        "Tossa Plana de Lles-Puigpedrós", "Vall Alta de Serradell - Serra de Sant Gervàs",
        "Vall la Vinaixa", "Valls de l'Anoia", "Valls del Sió-Llobregós",
        "Vessants de la Noguera Ribagorçana", "Zona exclosa", "Zona Volcànica de la Garrotxa"
      )
    )
  ),

  ## spanish
  esp = list(
    proteccio = list(
      general = c(
        "Todos" = '', "Solo protegidos" = 'only_protected',
        "Sin protección" = 'no_protected'
      ),
      espacios = c(
        "Paraje Natural de Interés Nacional" = "Paratge Natural d'Interès Nacional",
        "Parque Nacional" = "Parc Nacional",
        "Parque Natural" = "Parc Natural",
        "Reserva Natural de Fauna Salvaje" = "Reserva Natural de Fauna Salvatge",
        "Reserva Natural Parcial" = "Reserva Natural Parcial",
        "Zona de protección" = "Zona de Protecció"
      )
    ),
    nomein = list(
      general = c(
        "Todos" = '', "Solo espacios de interés nacional" = 'ony_protection',
        "Sin PEIN" = 'no_protection'
      ),
      espacios = c(
        "Aiguabarreig Segre-Noguera Pallaresa", "Aiguabarreig Segre-Noguera Ribagorçana",
        "Aiguamolls de l'Alt Empordà", "Aigüestortes", "Alta Garrotxa, l'",
        "Alt Pirineu, l'", "Ancosa-Montagut l'", "Artiga de Lin, Era", "Bessons, els",
        "Capçalera de la Noguera Ribagorçana", "Capçaleres del Ter i del Freser",
        "Cap de Creus", "Castell-Cap Roig", "Cingles de Bertí", "Collegats",
        "Collsacabra", "Conreria-Sant Mateu-Céllecs, la", "Costoja", "Estany de Banyoles",
        "Estany de Sils", "Eth Portilhon", "Faiada de Malpàs, la", "Foix, el", "Gallifa",
        "Gavarres, les", "Gelada", "Guilleries, les", "Marimanha", "Massís de l'Albera",
        "Massís de les Cadiretes", "Massís de les Salines", "Massís del Garraf",
        "Massís del Montseny", "Miracle, el", "Moianès, el", "Montanhes de Les e Bossòst",
        "Montesquiu", "Montgrí, el", "Montmell, el", "Montserrat", "Muntanyes de Begur",
        "Muntanyes de l'Ordal", "Muntanyes de Prades", "Muntanyes de Rocacorba",
        "Muntanyes de Tivissa-Vandellòs", "Naut Aran", "Obagues del Riu Corb", "Olèrdola",
        "Penya-segats de la Muga", "Pinya de Rosa", "Plana de Sant Jordi, la",
        "Ports, els", "Puig de la Banya del Boc", "Riba-roja", "Ribera Salada",
        "Riera de Merlès", "Riera de Navel", "Riera de Sorreigs", "Roques Blanques",
        "Saburella", "Sant Joan de Toran", "Sant Llorenç del Munt i l'Obac",
        "Savassona", "Serra Cavallera", "Serra d'Aubenç", "Serra de Bellmunt",
        "Serra de Boumort", "Serra de Carreu", "Serra de Castelltallat",
        "Serra de Collserola", "Serra del Catllaràs", "Serra de Llaberia",
        "Serra del Montsant", "Serra del Montsec", "Serra del Verd",
        "Serra de Miralles-Queralt", "Serra de Montgrony", "Serra de Montsià",
        "Serra d'Ensija-els Rasos de Peguera", "Serra de Picancel", "Serra de Queralt",
        "Serra de Sant Gervàs", "Serra de Turp", "Serra Llarga", "Serra Mitjana",
        "Serres de Busa-els Bastets-Lord", "Serres de Cardó-el Boix", "Serres del Cadí-el Moixeró",
        "Serres de Milany-Santa Magdalena i Puigsacalm-Bellmunt", "Serres de Montnegre-el Corredor",
        "Serres de Pàndols-Cavalls", "Serres de Pradell-l'Argentera",
        "Serres d'Odèn-Port del Comte", "Tossal Gros de Miramar", "Tossals d'Almatret",
        "Tossa Plana de Lles-Puigpedrós", "Tres Hereus, els", "Turons de Maçanet",
        "Turons de la Plana Ausetana", "Vall Alta de Serradell", "Vall del Riu Llobregós",
        "Volcà de la Crosa", "Zona Volcànica de la Garrotxa"
      )
    ),
    enpes = list(
      general = c(
        "Todos" = '', "Solo espacios con protección especial" = 'only_protected',
        "Sin ENPES" = 'no_protected'
      ),
      espais = c(
        "Paratge natural d'interès nacional de cap Gros-cap de Creus",
        "Paratge natural d'interès nacional de la vall del monestir de Poblet",
        "Paratge natural d'interès nacional de la Serra de Rodes",
        "Paratge natural d'interès nacional del massís de l'Albera",
        "Paratge natural d'interès nacional del Massís de Pedraforca",
        "Paratge natural d'interès nacional de Pinya de Rosa",
        "Parc nacional d'Aigüestortes i Estany de Sant Maurici",
        "Parc natural de Cap de Creus",
        "Parc natural de l'Alt Pirineu",
        "Parc natural de la Muntanya de Montserrat",
        "Parc natural de la Zona Volcànica de la Garrotxa",
        "Parc natural del Cadí-Moixeró",
        "Parc natural del Massís de Sant Llorenç del Munt i Serra de l'Obac",
        "Parc Natural del Montsant",
        "Parc natural dels Aiguamolls de l'Empordà",
        "Parc natural dels Ports",
        "Parc Natural Massís del Montseny",
        "Reserva Natural de Fauna Salvatge de l'Aiguabarreig Segre-Noguera Pallaresa",
        "Reserva natural parcial de Baish Aran",
        "Reserva natural parcial de la Capçalera de l'Orlina",
        "Reserva natural parcial de la Fageda de Jordà",
        "Reserva natural parcial de la Llosa",
        "Reserva natural parcial de l'Alt Àneu",
        "Reserva natural parcial de la Muntanya Montserrat",
        "Reserva natural parcial del Barranc de la Trinitat",
        "Reserva natural parcial del Barranc del Titllar",
        "Reserva natural parcial de les Fagedes dels Ports",
        "Reserva natural parcial  del volcà Aiguanegra",
        "Reserva natural parcial  del volcà  Croscat",
        "Reserva natural parcial  del volcà de Santa Margarida",
        "Reserva natural parcial  del volca Montolivet",
        "Reserva natural parcial  del volcà Puig Astrol",
        "Reserva natural parcial de Noguera Pallaresa-Bonaigua",
        "Reserva natural parcial de Riera de Merlès",
        "Reserva natural parcial de St Quirze de Colera",
        "Zona perifèrica de protecció del parc nacional",
        "Zona perifèrica de protecció del Parc natural de la Muntanya de Montserrat"
      )
    ),
    nomxarxa2000 = list(
      general = c(
        "Todos" = '', "Solo aquellos en la Red Natura 2000" = 'only_protected',
        "Sin Red Natura 2000" = 'no_protected'
      ),
      espais = c(
        "Aiguabarreig Segre- Noguera Pallaresa", "Aiguabarreig Segre-Noguera Ribagorçana",
        "Aiguamolls de l'Alt Empordà", "Aigüestortes",
        "Alta Garrotxa-Massís de les Salines", "Alt Pallars", "Baish Aran",
        "Barranc de Santes Creus", "Bellmunt-Almenara", "Beneïdor", "Capçaleres del Foix",
        "Capçaleres del Ter i del Freser", "Cap de Creus", "El Montgr-Les Medes-El Baix Ter",
        "El Montmell-Marmellar", "Els Bessons", "Era Artiga de Lin-Eth Portilhon",
        "Estany de Banyoles", "Estany de Sils-Riera de Santa Coloma",
        "Gallifa-Cingles de Bertí", "Garriga d'Empordà", "Granyena",
        "La Faiada de Malpàs i Cambatiri", "L'Albera", "Les Gavarres", "Les Guilleries",
        "Litoral del Baix Empordà", "Massís de Bonastre", "Massís de les Cadiretes",
        "Massís del Montseny", "Montgrony", "Montserrat-Roques Blanques- riu Llobregat",
        "Muntanyes de Prades", "Muntanyes de Rocacorba-Puig de la Baya del Boc",
        "Obagues de la riera de Madrona", "Obagues del riu Corb", "Prepirineu Central català",
        "Rasos de Tubau", "Ribera de l'Algars", "Ribera Salada", "Riberes de l'Alt Segre",
        "Riberes de l'Alt Ter", "Riberes del Baix Ter", "Riera de Clariana",
        "Riera de la Goda", "Riera de Merlès", "Riera de Sorreigs", "Riu Brugent",
        "Riu de la Llosa", "Riu Fluvià", "Riu Gaià", "Riu Llobregat d'Empordà-Riera de Torrelles",
        "Riu Siurana i planes del Priorat", "Sant Llorenç del Munt i l'Obac",
        "Secans de la Noguera", "Serra Cavallera",
        "Serra d'Aubenç i Roc de Cogul", "Serra de Boumort- Collegats",
        "Serra de Castelltallat", "Serra de Catllaràs", "Serra de Collserola",
        "Serra de Montsant-Pas de l'Ase", "Serra de Montsià", "Serra de Prada-Castellàs",
        "Serra de Turp i Mora Condal-Valldan", "Serres de Cardó - El Boix",
        "Serres del Litoral central", "Serres del litoral septentrional",
        "Serres del Montsec, Sant Mamet i Mitjana",
        "Serres de Queralt i Els Tossals-Aigua d'Ora", "Sistema prelitoral central",
        "Sistema prelitoral meridional", "Sistema transversal Català",
        "Tivissa-Vandellós-Llaberia", "Tossal de Montagut", "Tossals d'Almatret i Riba-roja",
        "Tossa Plana de Lles-Puigpedrós", "Vall Alta de Serradell - Serra de Sant Gervàs",
        "Vall la Vinaixa", "Valls de l'Anoia", "Valls del Sió-Llobregós",
        "Vessants de la Noguera Ribagorçana", "Zona exclosa", "Zona Volcànica de la Garrotxa"
      )
    )
  ),

  ## english
  eng = list(
    proteccio = list(
      general = c(
        "All" = '', "Only protected" = 'only_protected',
        "No protected" = 'no_protected'
      ),
      espacios = c(
        "Paraje Natural de Interés Nacional" = "Paratge Natural d'Interès Nacional",
        "Parque Nacional" = "Parc Nacional",
        "Parque Natural" = "Parc Natural",
        "Reserva Natural de Fauna Salvaje" = "Reserva Natural de Fauna Salvatge",
        "Reserva Natural Parcial" = "Reserva Natural Parcial",
        "Zona de protección" = "Zona de Protecció"
      )
    ),
    nomein = list(
      general = c(
        "All" = '', "Only national spaces of interest" = 'ony_protection',
        "No PEIN" = 'no_protection'
      ),
      espacios = c(
        "Aiguabarreig Segre-Noguera Pallaresa", "Aiguabarreig Segre-Noguera Ribagorçana",
        "Aiguamolls de l'Alt Empordà", "Aigüestortes", "Alta Garrotxa, l'",
        "Alt Pirineu, l'", "Ancosa-Montagut l'", "Artiga de Lin, Era", "Bessons, els",
        "Capçalera de la Noguera Ribagorçana", "Capçaleres del Ter i del Freser",
        "Cap de Creus", "Castell-Cap Roig", "Cingles de Bertí", "Collegats",
        "Collsacabra", "Conreria-Sant Mateu-Céllecs, la", "Costoja", "Estany de Banyoles",
        "Estany de Sils", "Eth Portilhon", "Faiada de Malpàs, la", "Foix, el", "Gallifa",
        "Gavarres, les", "Gelada", "Guilleries, les", "Marimanha", "Massís de l'Albera",
        "Massís de les Cadiretes", "Massís de les Salines", "Massís del Garraf",
        "Massís del Montseny", "Miracle, el", "Moianès, el", "Montanhes de Les e Bossòst",
        "Montesquiu", "Montgrí, el", "Montmell, el", "Montserrat", "Muntanyes de Begur",
        "Muntanyes de l'Ordal", "Muntanyes de Prades", "Muntanyes de Rocacorba",
        "Muntanyes de Tivissa-Vandellòs", "Naut Aran", "Obagues del Riu Corb", "Olèrdola",
        "Penya-segats de la Muga", "Pinya de Rosa", "Plana de Sant Jordi, la",
        "Ports, els", "Puig de la Banya del Boc", "Riba-roja", "Ribera Salada",
        "Riera de Merlès", "Riera de Navel", "Riera de Sorreigs", "Roques Blanques",
        "Saburella", "Sant Joan de Toran", "Sant Llorenç del Munt i l'Obac",
        "Savassona", "Serra Cavallera", "Serra d'Aubenç", "Serra de Bellmunt",
        "Serra de Boumort", "Serra de Carreu", "Serra de Castelltallat",
        "Serra de Collserola", "Serra del Catllaràs", "Serra de Llaberia",
        "Serra del Montsant", "Serra del Montsec", "Serra del Verd",
        "Serra de Miralles-Queralt", "Serra de Montgrony", "Serra de Montsià",
        "Serra d'Ensija-els Rasos de Peguera", "Serra de Picancel", "Serra de Queralt",
        "Serra de Sant Gervàs", "Serra de Turp", "Serra Llarga", "Serra Mitjana",
        "Serres de Busa-els Bastets-Lord", "Serres de Cardó-el Boix", "Serres del Cadí-el Moixeró",
        "Serres de Milany-Santa Magdalena i Puigsacalm-Bellmunt", "Serres de Montnegre-el Corredor",
        "Serres de Pàndols-Cavalls", "Serres de Pradell-l'Argentera",
        "Serres d'Odèn-Port del Comte", "Tossal Gros de Miramar", "Tossals d'Almatret",
        "Tossa Plana de Lles-Puigpedrós", "Tres Hereus, els", "Turons de Maçanet",
        "Turons de la Plana Ausetana", "Vall Alta de Serradell", "Vall del Riu Llobregós",
        "Volcà de la Crosa", "Zona Volcànica de la Garrotxa"
      )
    ),
    enpes = list(
      general = c(
        "All" = '', "Only spaces with special protection" = 'only_protected',
        "No ENPES" = 'no_protected'
      ),
      espais = c(
        "Paratge natural d'interès nacional de cap Gros-cap de Creus",
        "Paratge natural d'interès nacional de la vall del monestir de Poblet",
        "Paratge natural d'interès nacional de la Serra de Rodes",
        "Paratge natural d'interès nacional del massís de l'Albera",
        "Paratge natural d'interès nacional del Massís de Pedraforca",
        "Paratge natural d'interès nacional de Pinya de Rosa",
        "Parc nacional d'Aigüestortes i Estany de Sant Maurici",
        "Parc natural de Cap de Creus",
        "Parc natural de l'Alt Pirineu",
        "Parc natural de la Muntanya de Montserrat",
        "Parc natural de la Zona Volcànica de la Garrotxa",
        "Parc natural del Cadí-Moixeró",
        "Parc natural del Massís de Sant Llorenç del Munt i Serra de l'Obac",
        "Parc Natural del Montsant",
        "Parc natural dels Aiguamolls de l'Empordà",
        "Parc natural dels Ports",
        "Parc Natural Massís del Montseny",
        "Reserva Natural de Fauna Salvatge de l'Aiguabarreig Segre-Noguera Pallaresa",
        "Reserva natural parcial de Baish Aran",
        "Reserva natural parcial de la Capçalera de l'Orlina",
        "Reserva natural parcial de la Fageda de Jordà",
        "Reserva natural parcial de la Llosa",
        "Reserva natural parcial de l'Alt Àneu",
        "Reserva natural parcial de la Muntanya Montserrat",
        "Reserva natural parcial del Barranc de la Trinitat",
        "Reserva natural parcial del Barranc del Titllar",
        "Reserva natural parcial de les Fagedes dels Ports",
        "Reserva natural parcial  del volcà Aiguanegra",
        "Reserva natural parcial  del volcà  Croscat",
        "Reserva natural parcial  del volcà de Santa Margarida",
        "Reserva natural parcial  del volca Montolivet",
        "Reserva natural parcial  del volcà Puig Astrol",
        "Reserva natural parcial de Noguera Pallaresa-Bonaigua",
        "Reserva natural parcial de Riera de Merlès",
        "Reserva natural parcial de St Quirze de Colera",
        "Zona perifèrica de protecció del parc nacional",
        "Zona perifèrica de protecció del Parc natural de la Muntanya de Montserrat"
      )
    ),
    nomxarxa2000 = list(
      general = c(
        "All" = '', "Only those inside Natura 2000 Net" = 'only_protected',
        "No Natura 2000 Net" = 'no_protected'
      ),
      espais = c(
        "Aiguabarreig Segre- Noguera Pallaresa", "Aiguabarreig Segre-Noguera Ribagorçana",
        "Aiguamolls de l'Alt Empordà", "Aigüestortes",
        "Alta Garrotxa-Massís de les Salines", "Alt Pallars", "Baish Aran",
        "Barranc de Santes Creus", "Bellmunt-Almenara", "Beneïdor", "Capçaleres del Foix",
        "Capçaleres del Ter i del Freser", "Cap de Creus", "El Montgr-Les Medes-El Baix Ter",
        "El Montmell-Marmellar", "Els Bessons", "Era Artiga de Lin-Eth Portilhon",
        "Estany de Banyoles", "Estany de Sils-Riera de Santa Coloma",
        "Gallifa-Cingles de Bertí", "Garriga d'Empordà", "Granyena",
        "La Faiada de Malpàs i Cambatiri", "L'Albera", "Les Gavarres", "Les Guilleries",
        "Litoral del Baix Empordà", "Massís de Bonastre", "Massís de les Cadiretes",
        "Massís del Montseny", "Montgrony", "Montserrat-Roques Blanques- riu Llobregat",
        "Muntanyes de Prades", "Muntanyes de Rocacorba-Puig de la Baya del Boc",
        "Obagues de la riera de Madrona", "Obagues del riu Corb", "Prepirineu Central català",
        "Rasos de Tubau", "Ribera de l'Algars", "Ribera Salada", "Riberes de l'Alt Segre",
        "Riberes de l'Alt Ter", "Riberes del Baix Ter", "Riera de Clariana",
        "Riera de la Goda", "Riera de Merlès", "Riera de Sorreigs", "Riu Brugent",
        "Riu de la Llosa", "Riu Fluvià", "Riu Gaià", "Riu Llobregat d'Empordà-Riera de Torrelles",
        "Riu Siurana i planes del Priorat", "Sant Llorenç del Munt i l'Obac",
        "Secans de la Noguera", "Serra Cavallera",
        "Serra d'Aubenç i Roc de Cogul", "Serra de Boumort- Collegats",
        "Serra de Castelltallat", "Serra de Catllaràs", "Serra de Collserola",
        "Serra de Montsant-Pas de l'Ase", "Serra de Montsià", "Serra de Prada-Castellàs",
        "Serra de Turp i Mora Condal-Valldan", "Serres de Cardó - El Boix",
        "Serres del Litoral central", "Serres del litoral septentrional",
        "Serres del Montsec, Sant Mamet i Mitjana",
        "Serres de Queralt i Els Tossals-Aigua d'Ora", "Sistema prelitoral central",
        "Sistema prelitoral meridional", "Sistema transversal Català",
        "Tivissa-Vandellós-Llaberia", "Tossal de Montagut", "Tossals d'Almatret i Riba-roja",
        "Tossa Plana de Lles-Puigpedrós", "Vall Alta de Serradell - Serra de Sant Gervàs",
        "Vall la Vinaixa", "Valls de l'Anoia", "Valls del Sió-Llobregós",
        "Vessants de la Noguera Ribagorçana", "Zona exclosa", "Zona Volcànica de la Garrotxa"
      )
    )
  )

)



## mod_viz inputs ####

# Color input
dic_color_choices <- list(

  ## catalan
  cat = list(
    scenario1 = list(
      "Variables climàtiques" = c(
        'Radiació anual' = 'radiacioanual',
        'Temperatura mínima anual' = "temperaturaminimaanual",
        'Temperatura mitjana anual' = "temperaturamitjanaanual",
        'Temperatura màxima anual' = "temperaturamaximaanual",
        'Precipitació anual' = "precipitacioanual",
        'NPP_S' = "npp_s"
      ),
      "Variables IFN" = c(
        "ID parcel·la" = "idparcela",
        "ID classe" = "idclasse",
        "Caducifoli/Esclerofil/Conifera dominant per densitat" = "cadesccon_dom_percdens",
        "Percentatge Densitat Caducifoli/Esclerofil/Conifera dominant" = "cadesccon_dom_percdens_val",
        "Caducifoli/Esclerofil/Conifera dominant per àrea basal" = "cadesccon_dom_percab",
        "Percentatge Àrea Basal Caducifoli/Esclerofil/Conifera dominant" = "cadesccon_dom_percab_val",
        "Planifoli/Conifera dominant per densitat" = "planifconif_dom_percdens",
        "Percentatge Densitat Planifoli/Conifera dominant" = "planifconif_dom_percdens_val",
        "Planifoli/Conifera dominant per àrea basal" = "planifconif_dom_percab",
        "Percentatge Àrea Basal Planifoli/Conifera dominant" = "planifconif_dom_percab_val",
        "Gènere dominant per densitat" = "genere_dom_percdens",
        "Percentatge Densitat Gènere dominant" = "genere_dom_percdens_val",
        "Gènere dominant per àrea basal" = "genere_dom_percab",
        "Percentatge Àrea Basal Gènere dominant" = "genere_dom_percab_val",
        "Espècie simplificat dominant per densitat" = "especiesimp_dom_percdens",
        "Percentatge Densitat Espècie simplificat dominant" = "especiesimp_dom_percdens_val",
        "Espècie simplificat dominant per àrea basal" = "especiesimp_dom_percab",
        "Percentatge Àrea Basal Espècie simplificat dominant" = "especiesimp_dom_percab_val",
        "Espècie dominant per densitat" = "especie_dom_percdens",
        "Percentatge Densitat Espècie dominant" = "especie_dom_percdens_val",
        "Espècie dominant per àrea basal" = "especie_dom_percab",
        "Percentatge Àrea Basal Espècie dominant" = "especie_dom_percab_val",
        "Densitat total parcel·la" = "densitat",
        "Densitat total peus morts parcel·la" = "densitatmorts",
        "Àrea Basal total parcel·la" = "ab",
        "Àrea Basal total peus morts parcel·la" = "abmorts",
        "Diàmetre a l'altura del pit parcel·la" = "dbh",
        "Diàmetre a l'altura del pit peus morts parcel·la" = "dbhmorts",
        "rc" = "rc",
        "vcc" = "vcc",
        "vccmorts" = "vccmorts",
        "vsc" = "vsc",
        "vscmorts" = "vscmorts",
        "iavc" = "iavc",
        "iavc_creaf" = "iavc_creaf",
        "vle" = "vle",
        "bm" = "bm",
        "bc" = "bc",
        "br" = "br",
        "bh" = "bh",
        "bat" = "bat",
        "iaf" = "iaf",
        "ph" = "ph",
        "cm" = "cm",
        "cc" = "cc",
        "cr" = "cr",
        "ch" = "ch",
        "cat" = "cat",
        "cca" = "cca"
      )
    ),
    scenario2 = list(
      "Variables climàtiques" = c(
        'Radiació anual' = 'radiacioanual',
        'Temperatura mínima anual' = "temperaturaminimaanual",
        'Temperatura mitjana anual' = "temperaturamitjanaanual",
        'Temperatura màxima anual' = "temperaturamaximaanual",
        'Precipitació anual' = "precipitacioanual",
        'NPP_S' = "npp_s"
      ),
      "Variables IFN" = c(
        'ID parcel·la' = "idparcela",
        'ID classe' = "idclasse",
        'Ordre per Densitat' = "ordredens",
        'Ordre per Àrea Basal' = "ordreab",
        'Percentatge Densitat' = "percdens",
        'Percentatge Àrea Basal' = "percab",
        'Densitat' = "densitat",
        'Denstitat peus morts' = "densitatmorts",
        'Àrea Basal' = "ab",
        'Àrea Basal peus morts' = "abmorts",
        "Diàmetre a l'altura del pit" = "dbh",
        "Diàmetre a l'altura del pit peus morts" = "dbhmorts",
        "rc" = "rc",
        "vcc" = "vcc",
        "vccmorts" = "vccmorts",
        "vsc" = "vsc",
        "vscmorts" = "vscmorts",
        "iavc" = "iavc",
        "iavc_creaf" = "iavc_creaf",
        "vle" = "vle",
        "bm" = "bm",
        "bc" = "bc",
        "br" = "br",
        "bh" = "bh",
        "bat" = "bat",
        "iaf" = "iaf",
        "ph" = "ph",
        "cm" = "cm",
        "cc" = "cc",
        "cr" = "cr",
        "ch" = "ch",
        "cat" = "cat",
        "cca" = "cca"
      )
    ),
    scenario3 = c(
      "Percentatge Densitat Caducifoli/Esclerofil/Conifera dominant" = "cadesccon_dom_percdens_val",
      "Percentatge Àrea Basal Caducifoli/Esclerofil/Conifera dominant" = "cadesccon_dom_percab_val",
      "Percentatge Densitat Planifoli/Conifera dominant" = "planifconif_dom_percdens_val",
      "Percentatge Àrea Basal Planifoli/Conifera dominant" = "planifconif_dom_percab_val",
      "Percentatge Densitat Gènere dominant" = "genere_dom_percdens_val",
      "Percentatge Àrea Basal Gènere dominant" = "genere_dom_percab_val",
      "Percentatge Densitat Espècie dominant" = "especiesimp_dom_percdens_val",
      "Percentatge Àrea Basal Espècie dominant" = "especiesimp_dom_percab_val",
      "Percentatge Densitat Espècie simplificat dominant" = "especie_dom_percdens_val",
      "Percentatge Àrea Basal Espècie simplificat dominant" = "especie_dom_percab_val",
      "Densitat total parcel·la" = "densitat",
      "Densitat total peus morts parcel·la" = "densitatmorts",
      "Àrea Basal total parcel·la" = "ab",
      "Àrea Basal total peus morts parcel·la" = "abmorts",
      "Diàmetre a l'altura del pit parcel·la" = "dbh",
      "Diàmetre a l'altura del pit peus morts parcel·la" = "dbhmorts",
      "rc" = "rc",
      "vcc" = "vcc",
      "vccmorts" = "vccmorts",
      "vsc" = "vsc",
      "vscmorts" = "vscmorts",
      "iavc" = "iavc",
      "iavc_creaf" = "iavc_creaf",
      "vle" = "vle",
      "bm" = "bm",
      "bc" = "bc",
      "br" = "br",
      "bh" = "bh",
      "bat" = "bat",
      "iaf" = "iaf",
      "ph" = "ph",
      "cm" = "cm",
      "cc" = "cc",
      "cr" = "cr",
      "ch" = "ch",
      "cat" = "cat",
      "cca" = "cca"
    ),
    scenario4 = c(
      'Ordre per Densitat' = "ordredens",
      'Ordre per Àrea Basal' = "ordreab",
      'Percentatge Densitat' = "percdens",
      'Percentatge Àrea Basal' = "percab",
      'Densitat' = "densitat",
      'Denstitat peus morts' = "densitatmorts",
      'Àrea Basal' = "ab",
      'Àrea Basal peus morts' = "abmorts",
      "Diàmetre a l'altura del pit" = "dbh",
      "Diàmetre a l'altura del pit peus morts" = "dbhmorts",
      "rc" = "rc",
      "vcc" = "vcc",
      "vccmorts" = "vccmorts",
      "vsc" = "vsc",
      "vscmorts" = "vscmorts",
      "iavc" = "iavc",
      "iavc_creaf" = "iavc_creaf",
      "vle" = "vle",
      "bm" = "bm",
      "bc" = "bc",
      "br" = "br",
      "bh" = "bh",
      "bat" = "bat",
      "iaf" = "iaf",
      "ph" = "ph",
      "cm" = "cm",
      "cc" = "cc",
      "cr" = "cr",
      "ch" = "ch",
      "cat" = "cat",
      "cca" = "cca"
    )
  ),


  ## spanish
  esp = list(
    scenario1 = list(
      "Variables climáticas" = c(
        'Radiación anual' = 'radiacioanual',
        'Temperatura mínima anual' = "temperaturaminimaanual",
        'Temperatura media anual' = "temperaturamitjanaanual",
        'Temperatura máxima anual' = "temperaturamaximaanual",
        'Precipitación anual' = "precipitacioanual",
        'NPP_S' = "npp_s"
      ),
      "Variables IFN" = c(
        "ID parcela" = "idparcela",
        "ID clase" = "idclasse",
        "Caducifolia/Esclerofila/Conifera dominante por densidad" = "cadesccon_dom_percdens",
        "Porcentaje Densidad Caducifolia/Esclerofila/Conifera dominante" = "cadesccon_dom_percdens_val",
        "Caducifolia/Esclerofila/Conifera dominante por área basal" = "cadesccon_dom_percab",
        "Porcentaje Área Basal Caducifolia/Esclerofila/Conifera dominante" = "cadesccon_dom_percab_val",
        "Planifolia/Conifera dominante por densidad" = "planifconif_dom_percdens",
        "Porcentaje Densidad Planifolia/Conifera dominante" = "planifconif_dom_percdens_val",
        "Planifolia/Conifera dominante por área basal" = "planifconif_dom_percab",
        "Porcentaje Área Basal Planifolia/Conifera dominante" = "planifconif_dom_percab_val",
        ### voy por aqui
        "Gènere dominant per densitat" = "genere_dom_percdens",
        "Percentatge Densitat Gènere dominant" = "genere_dom_percdens_val",
        "Gènere dominant per àrea basal" = "genere_dom_percab",
        "Percentatge Àrea Basal Gènere dominant" = "genere_dom_percab_val",
        "Espècie simplificat dominant per densitat" = "especiesimp_dom_percdens",
        "Percentatge Densitat Espècie simplificat dominant" = "especiesimp_dom_percdens_val",
        "Espècie simplificat dominant per àrea basal" = "especiesimp_dom_percab",
        "Percentatge Àrea Basal Espècie simplificat dominant" = "especiesimp_dom_percab_val",
        "Espècie dominant per densitat" = "especie_dom_percdens",
        "Percentatge Densitat Espècie dominant" = "especie_dom_percdens_val",
        "Espècie dominant per àrea basal" = "especie_dom_percab",
        "Percentatge Àrea Basal Espècie dominant" = "especie_dom_percab_val",
        "Densitat total parcel·la" = "densitat",
        "Densitat total peus morts parcel·la" = "densitatmorts",
        "Àrea Basal total parcel·la" = "ab",
        "Àrea Basal total peus morts parcel·la" = "abmorts",
        "Diàmetre a l'altura del pit parcel·la" = "dbh",
        "Diàmetre a l'altura del pit peus morts parcel·la" = "dbhmorts",
        "rc" = "rc",
        "vcc" = "vcc",
        "vccmorts" = "vccmorts",
        "vsc" = "vsc",
        "vscmorts" = "vscmorts",
        "iavc" = "iavc",
        "iavc_creaf" = "iavc_creaf",
        "vle" = "vle",
        "bm" = "bm",
        "bc" = "bc",
        "br" = "br",
        "bh" = "bh",
        "bat" = "bat",
        "iaf" = "iaf",
        "ph" = "ph",
        "cm" = "cm",
        "cc" = "cc",
        "cr" = "cr",
        "ch" = "ch",
        "cat" = "cat",
        "cca" = "cca"
      )
    ),
    scenario2 = list(
      "Variables climàtiques" = c(
        'Radiació anual' = 'radiacioanual',
        'Temperatura mínima anual' = "temperaturaminimaanual",
        'Temperatura mitjana anual' = "temperaturamitjanaanual",
        'Temperatura màxima anual' = "temperaturamaximaanual",
        'Precipitació anual' = "precipitacioanual",
        'NPP_S' = "npp_s"
      ),
      "Variables IFN" = c(
        'ID parcel·la' = "idparcela",
        'ID classe' = "idclasse",
        'Ordre per Densitat' = "ordredens",
        'Ordre per Àrea Basal' = "ordreab",
        'Percentatge Densitat' = "percdens",
        'Percentatge Àrea Basal' = "percab",
        'Densitat' = "densitat",
        'Denstitat peus morts' = "densitatmorts",
        'Àrea Basal' = "ab",
        'Àrea Basal peus morts' = "abmorts",
        "Diàmetre a l'altura del pit" = "dbh",
        "Diàmetre a l'altura del pit peus morts" = "dbhmorts",
        "rc" = "rc",
        "vcc" = "vcc",
        "vccmorts" = "vccmorts",
        "vsc" = "vsc",
        "vscmorts" = "vscmorts",
        "iavc" = "iavc",
        "iavc_creaf" = "iavc_creaf",
        "vle" = "vle",
        "bm" = "bm",
        "bc" = "bc",
        "br" = "br",
        "bh" = "bh",
        "bat" = "bat",
        "iaf" = "iaf",
        "ph" = "ph",
        "cm" = "cm",
        "cc" = "cc",
        "cr" = "cr",
        "ch" = "ch",
        "cat" = "cat",
        "cca" = "cca"
      )
    ),
    scenario3 = c(
      "Percentatge Densitat Caducifoli/Esclerofil/Conifera dominant" = "cadesccon_dom_percdens_val",
      "Percentatge Àrea Basal Caducifoli/Esclerofil/Conifera dominant" = "cadesccon_dom_percab_val",
      "Percentatge Densitat Planifoli/Conifera dominant" = "planifconif_dom_percdens_val",
      "Percentatge Àrea Basal Planifoli/Conifera dominant" = "planifconif_dom_percab_val",
      "Percentatge Densitat Gènere dominant" = "genere_dom_percdens_val",
      "Percentatge Àrea Basal Gènere dominant" = "genere_dom_percab_val",
      "Percentatge Densitat Espècie dominant" = "especiesimp_dom_percdens_val",
      "Percentatge Àrea Basal Espècie dominant" = "especiesimp_dom_percab_val",
      "Percentatge Densitat Espècie simplificat dominant" = "especie_dom_percdens_val",
      "Percentatge Àrea Basal Espècie simplificat dominant" = "especie_dom_percab_val",
      "Densitat total parcel·la" = "densitat",
      "Densitat total peus morts parcel·la" = "densitatmorts",
      "Àrea Basal total parcel·la" = "ab",
      "Àrea Basal total peus morts parcel·la" = "abmorts",
      "Diàmetre a l'altura del pit parcel·la" = "dbh",
      "Diàmetre a l'altura del pit peus morts parcel·la" = "dbhmorts",
      "rc" = "rc",
      "vcc" = "vcc",
      "vccmorts" = "vccmorts",
      "vsc" = "vsc",
      "vscmorts" = "vscmorts",
      "iavc" = "iavc",
      "iavc_creaf" = "iavc_creaf",
      "vle" = "vle",
      "bm" = "bm",
      "bc" = "bc",
      "br" = "br",
      "bh" = "bh",
      "bat" = "bat",
      "iaf" = "iaf",
      "ph" = "ph",
      "cm" = "cm",
      "cc" = "cc",
      "cr" = "cr",
      "ch" = "ch",
      "cat" = "cat",
      "cca" = "cca"
    ),
    scenario4 = c(
      'Ordre per Densitat' = "ordredens",
      'Ordre per Àrea Basal' = "ordreab",
      'Percentatge Densitat' = "percdens",
      'Percentatge Àrea Basal' = "percab",
      'Densitat' = "densitat",
      'Denstitat peus morts' = "densitatmorts",
      'Àrea Basal' = "ab",
      'Àrea Basal peus morts' = "abmorts",
      "Diàmetre a l'altura del pit" = "dbh",
      "Diàmetre a l'altura del pit peus morts" = "dbhmorts",
      "rc" = "rc",
      "vcc" = "vcc",
      "vccmorts" = "vccmorts",
      "vsc" = "vsc",
      "vscmorts" = "vscmorts",
      "iavc" = "iavc",
      "iavc_creaf" = "iavc_creaf",
      "vle" = "vle",
      "bm" = "bm",
      "bc" = "bc",
      "br" = "br",
      "bh" = "bh",
      "bat" = "bat",
      "iaf" = "iaf",
      "ph" = "ph",
      "cm" = "cm",
      "cc" = "cc",
      "cr" = "cr",
      "ch" = "ch",
      "cat" = "cat",
      "cca" = "cca"
    )
  ),

  ## english
  eng =

)
