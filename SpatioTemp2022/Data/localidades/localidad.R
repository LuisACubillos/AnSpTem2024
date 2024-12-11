#localidades
# 
localidad <- data.frame(matrix(c(
               -71.2542,-29.9077,"Coquimbo",
               -71.5833,-33,"Valparaíso",
               -71.5667,-33.6333,"San Antonio",
               -72.6,-35.39,"Constitución",
               -73.11684, -36.72494, "Talcahuano",
               -73.65356,-37.60825,"Lebu",
               -73.4,-39.81798931,"Corral",
               -73.6,-41.6167,"Maullín"),
               ncol=3,byrow = TRUE))
colnames(localidad) = c("Lon","Lat","localidad")

