MODULE ProductoModule

  !Estructura para almacenar la información de los productos
    TYPE :: Producto
      CHARACTER(50) :: nombre
      INTEGER :: cantidad_disponible
      REAL :: precio_unitario
    END TYPE Producto
  END MODULE ProductoModule