# 1) Mejorar "script".
# Lo mejor seria que tome directamente los argumetos que le hace falta de la 
# seccion "settings". Esto implica que el nombre de los objetos se llaman igual
# en el menu de seleccion que en el script.
# Voy a armar una funcion general para esto.
# Si despues hace falta casos particulare, lo hare luego. 
# Para poder detllar casos particulares tnego que hacer que el SELECTED_TOOLS entre 
# al modulo. Asi en un futuro puedo hacer un if condicional.

# Esta funcionalidad debe ser una funicion por fuera del modulo. Tal vez
# en un futuro cambio de idea del mdulo, pero no de la funcion.

# 2) Carpeta temporal.
# Debe ser de uso comun. De esa forma puedo guardar tambien todos los resultados 
# en un archivo .RData o RDS.
# Lo  mismo si quiero armar descargar como la "minibase_mod" en formato excel, o todos
# los ploteos en formato png, o todas las salidas en un txt.

# 3) Cada vez que hace clic en "PLAY" y esta todo OK, lo que hace es
# tomar la hora. Inmediatamente toma el TOOL_SELECTED, la hora y arma una nueva carpeta temporal.
# Colocar la hora de accion en la visual del usuario.

# 3) La obtencion de los resultados de R en este momento es un modulo...
# tal vez debiera ser una funcion. Si quiero ver ralgo de la salida lo armo luego.
# pero si quiero camibar de lugar la generacion de los objetos, tiene que ser una fn comun,
# y no un modulo.

# 
# Dia, Hora, Cronometro y alarma!!!!

# 5) Deberia ser posible generar la misma salida de shiny que en quarto.
# Lo detallado para shiny como listas dentro del codigo de output 
# deberia ser suceptible de ser tomado como un objeto para que quarto pueda
# generar unr eporte con las mismas pestañas.

# 6) Enviar el ip a una pagina web para analizar desde donde lo estan usando.
