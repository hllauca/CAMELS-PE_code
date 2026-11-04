# Soil suborders, great groups and horizon textures (DSOLMap) for CAMELS-PE v2.0
# Fuente: camels_pe_tipo_suelo_03-11-2026.xlsx (porcentajes de área por cuenca calculados sobre
# Tipo_Suelo.tif de DSOLMap, ~230 m, con las cuencas corregidas).
# Salidas (1_data/soil/):
#   soil_types_dominant.csv  clase dominante y su % por nivel (suborden, gran grupo, textura h1-h3)
#   soil_classes.csv         % de área de cada clase por cuenca, formato largo
#   soil_classification.csv  leyenda: jerarquía orden > suborden > gran grupo y clases texturales
# By Harold Llauca
rm(list=ls())
cat('\f')

library(openxlsx)
library(data.table)
# Run this script from the root of the CAMELS-PE project folder (see README.md).
f <- './1_data/soil/camels_pe_tipo_suelo_03-11-2026.xlsx'

# Fila 1: nombre en español; fila 2: código de la variable (se usa como encabezado)
rd <- function(s) as.data.table(read.xlsx(f, sheet = s, startRow = 2))
num_cols <- function(d, excl) setdiff(grep('_perc$', names(d), value = TRUE), excl)

ord <- rd('Taxonomia_Orden')
sub <- rd('Taxonomia_Suborden')
grp <- rd('Taxonomia_Gran_Grupo')
hz  <- lapply(1:3, function(h) rd(paste0('Horizonte_', h)))
leg <- as.data.table(read.xlsx(f, sheet = 'Leyenda_Taxonomia'))
tex <- as.data.table(read.xlsx(f, sheet = 'Leyenda_Texturas'))
setnames(leg, c('order', 'order_es', 'suborder', 'great_group', 'great_group_code'))
setnames(tex, c('code', 'name_es', 'name_en'))

ids <- ord$gauge_id
stopifnot(length(ids) == 136, !anyDuplicated(ids),
          all(sapply(c(list(sub, grp), hz), function(d) identical(d$gauge_id, ids))))

# Porcentajes por nivel en formato largo (todas las clases presentes en alguna cuenca, 0 si ausente)
long <- function(d, level, excl, prefix = '') {
  cols <- num_cols(d, excl)
  x <- melt(d[, c('gauge_id', cols), with = FALSE], id.vars = 'gauge_id', variable.name = 'class', value.name = 'perc')
  x[, class := sub('_perc$', '', sub(paste0('^', prefix), '', as.character(class)))]
  x[is.na(perc), perc := 0]
  x[, level := level]
  x[]
}
cls <- rbind(
  long(sub, 'suborder', 'soil_suborder_dom_perc'),
  long(grp, 'great_group', 'soil_group_dom_perc'),
  long(hz[[1]], 'texture_h1', character(), 'h1_'),
  long(hz[[2]], 'texture_h2', character(), 'h2_'),
  long(hz[[3]], 'texture_h3', character(), 'h3_')
)
setcolorder(cls, c('gauge_id', 'level', 'class', 'perc'))

# Controles: cada nivel suma 100 % en todas las cuencas
s <- cls[, .(tot = sum(perc)), by = .(gauge_id, level)]
stopifnot(all(abs(s$tot - 100) < 0.01))

# Jerarquía: subórdenes y grandes grupos presentes, con su orden y suborden
leg[, suborder_code := tolower(suborder)]
sub_leg <- unique(leg[, .(level = 'suborder', class = suborder_code, name = suborder, order, suborder = NA_character_)])
grp_leg <- leg[, .(level = 'great_group', class = great_group_code, name = great_group, order, suborder)]
stopifnot(all(unique(cls[level == 'suborder', class]) %in% sub_leg$class),
          all(unique(cls[level == 'great_group', class]) %in% grp_leg$class))
tex_leg <- tex[, .(level = 'texture', class = code, name = name_en, order = NA_character_, suborder = NA_character_)]
classification <- rbind(sub_leg[class %in% cls[level == 'suborder', class]],
                        grp_leg[class %in% cls[level == 'great_group', class]],
                        tex_leg)

# Consistencia de la jerarquía: la suma de los subórdenes de un orden = % del orden;
# la suma de los grandes grupos de un suborden = % del suborden
ord_l <- melt(ord[, c('gauge_id', num_cols(ord, c('soil_order_dom_perc', 'soil_coverage_perc'))), with = FALSE],
              id.vars = 'gauge_id', variable.name = 'order', value.name = 'perc')
ord_l[, order := sub('_perc$', '', as.character(order))][is.na(perc), perc := 0]
a1 <- merge(cls[level == 'suborder'], sub_leg[, .(class, order = tolower(order))], by = 'class')[, .(p = sum(perc)), by = .(gauge_id, order)]
a1 <- merge(a1, ord_l, by = c('gauge_id', 'order'), all = TRUE); a1[is.na(p), p := 0]
a2 <- merge(cls[level == 'great_group'], grp_leg[, .(class, sub = tolower(suborder))], by = 'class')[, .(p = sum(perc)), by = .(gauge_id, sub)]
a2 <- merge(a2, cls[level == 'suborder', .(gauge_id, sub = class, perc)], by = c('gauge_id', 'sub'), all = TRUE); a2[is.na(p), p := 0]
cat('Máx. diferencia orden vs suma de subórdenes:', max(abs(a1$p - a1$perc)), '\n')
cat('Máx. diferencia suborden vs suma de grandes grupos:', max(abs(a2$p - a2$perc)), '\n')
stopifnot(max(abs(a1$p - a1$perc)) < 0.01, max(abs(a2$p - a2$perc)) < 0.01)

# Clase dominante por nivel (máximo; se comprueba con la hoja Resumen)
dom <- cls[order(-perc), .SD[1], by = .(gauge_id, level)]
dom <- dcast(dom, gauge_id ~ level, value.var = c('class', 'perc'))
dom <- dom[match(ids, gauge_id)]
res <- rd('Resumen')
stopifnot(identical(res$gauge_id, ids),
          all(dom$class_suborder == tolower(res$soil_suborder_dom_desc)),
          all(dom$class_great_group == res$soil_group_dom),
          all(abs(dom$perc_texture_h1 - res$h1_texture_dom_perc) < 1e-6),
          all(abs(dom$perc_texture_h3 - res$h3_texture_dom_perc) < 1e-6))
out_dom <- dom[, .(gauge_id,
                   soil_suborder_dominant_class    = class_suborder,
                   soil_suborder_dominant_perc     = perc_suborder,
                   soil_great_group_dominant_class = class_great_group,
                   soil_great_group_dominant_perc  = perc_great_group,
                   soil_texture_h1_dominant_class  = class_texture_h1,
                   soil_texture_h1_dominant_perc   = perc_texture_h1,
                   soil_texture_h2_dominant_class  = class_texture_h2,
                   soil_texture_h2_dominant_perc   = perc_texture_h2,
                   soil_texture_h3_dominant_class  = class_texture_h3,
                   soil_texture_h3_dominant_perc   = perc_texture_h3)]

# Orden de las filas: por cuenca y, dentro de cada nivel, según la leyenda
lv <- c('suborder', 'great_group', 'texture_h1', 'texture_h2', 'texture_h3')
cls[, lvl_cls := ifelse(grepl('^texture', level), 'texture', level)]
cls[, k := match(paste(lvl_cls, class), paste(classification$level, classification$class))]
stopifnot(!anyNA(cls$k))
cls <- cls[order(match(gauge_id, ids), match(level, lv), k), .(gauge_id, level, class, perc)]

fwrite(out_dom, './1_data/soil/soil_types_dominant.csv')
fwrite(cls, './1_data/soil/soil_classes.csv')
fwrite(classification, './1_data/soil/soil_classification.csv', na = 'NA')
cat('soil_classes:', nrow(cls), 'filas;', nrow(classification), 'clases en la leyenda\n')
print(table(cls$level) / length(ids))
