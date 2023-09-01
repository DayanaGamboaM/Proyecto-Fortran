MODULE ProductoModule
  TYPE :: Producto
    CHARACTER(50) :: nombre
    INTEGER :: cantidad_disponible
    REAL :: precio_unitario
  END TYPE Producto
END MODULE ProductoModule

PROGRAM GestionInventario
  USE ProductoModule

  INTEGER, PARAMETER :: MAX_PRODUCTOS = 100
  TYPE(Producto), DIMENSION(MAX_PRODUCTOS) :: inventario
  INTEGER :: num_productos = 0

 integer :: opcion

  DO
    
    WRITE(*,*) "Sistema de Gestión de Inventario"
    WRITE(*,*) "1. Registrar Producto"
    WRITE(*,*) "2. Actualizar Cantidad"
    WRITE(*,*) "3. Consultar Productos"
    WRITE(*,*) "4. Salir"
    WRITE(*,*) "Elija una opcion:"
    READ(*,*) opcion

    SELECT CASE(opcion)
      CASE(1)
        CALL RegistrarProducto(inventario, num_productos)
      CASE(2)
        CALL ActualizarCantidad(inventario, num_productos)
      CASE(3)
        CALL ConsultarProductos(inventario, num_productos)
      CASE(4)
        EXIT
      CASE DEFAULT
        WRITE(*,*) "Opción no válida. Intente nuevamente."
    END SELECT
  END DO

CONTAINS

  SUBROUTINE RegistrarProducto(inventario, num_productos)
    TYPE(Producto), DIMENSION(:) :: inventario
    INTEGER :: num_productos

    IF (num_productos < MAX_PRODUCTOS) THEN
      WRITE(*,*) "Ingrese el nombre del producto:"
      READ(*,*) inventario(num_productos+1)%nombre
      WRITE(*,*) "Ingrese la cantidad disponible:"
      READ(*,*) inventario(num_productos+1)%cantidad_disponible
      WRITE(*,*) "Ingrese el precio unitario:"
      READ(*,*) inventario(num_productos+1)%precio_unitario
      num_productos = num_productos + 1
      WRITE(*,*) "Producto registrado con éxito."
    ELSE
      WRITE(*,*) "El inventario está lleno. No se pueden registrar más productos."
    END IF
  END SUBROUTINE RegistrarProducto

  SUBROUTINE ActualizarCantidad(inventario, num_productos)
    TYPE(Producto), DIMENSION(:) :: inventario
    INTEGER :: num_productos
    CHARACTER(50) :: nombre_producto
    INTEGER :: cantidad_actualizar
    INTEGER :: i   ! Agregamos la declaración de 'i'

    WRITE(*,*) "Ingrese el nombre del producto para actualizar la cantidad:"
    READ(*,*) nombre_producto

    DO i = 1, num_productos
      IF (TRIM(ADJUSTL(nombre_producto)) == TRIM(ADJUSTL(inventario(i)%nombre))) THEN
        WRITE(*,*) "Ingrese la cantidad a agregar/restar:"
        READ(*,*) cantidad_actualizar
        inventario(i)%cantidad_disponible = inventario(i)%cantidad_disponible + cantidad_actualizar
        WRITE(*,*) "Cantidad actualizada con éxito."
        RETURN
      END IF
    END DO

    WRITE(*,*) "Producto no encontrado en el inventario."
  END SUBROUTINE ActualizarCantidad

  SUBROUTINE ConsultarProductos(inventario, num_productos)
    TYPE(Producto), DIMENSION(:) :: inventario
    INTEGER :: num_productos
    CHARACTER(50) :: nombre_producto
    REAL :: precio_min, precio_max
    
    integer :: opcion
    WRITE(*,*) "1. Consultar por nombre"
    WRITE(*,*) "2. Consultar por rango de precios"
    READ(*,*) opcion

    SELECT CASE(opcion)
      CASE(1)
        WRITE(*,*) "Ingrese el nombre del producto a consultar:"
        READ(*,*) nombre_producto

        DO i = 1, num_productos
          IF (TRIM(ADJUSTL(nombre_producto)) == TRIM(ADJUSTL(inventario(i)%nombre))) THEN
            WRITE(*,*) "Nombre:", inventario(i)%nombre
            WRITE(*,*) "Cantidad Disponible:", inventario(i)%cantidad_disponible
            WRITE(*,*) "Precio Unitario:", inventario(i)%precio_unitario
            RETURN
          END IF
        END DO

        WRITE(*,*) "Producto no encontrado en el inventario."

      CASE(2)
        WRITE(*,*) "Ingrese el precio mínimo:"
        READ(*,*) precio_min
        WRITE(*,*) "Ingrese el precio máximo:"
        READ(*,*) precio_max

        DO i = 1, num_productos
          IF (inventario(i)%precio_unitario >= precio_min .AND. inventario(i)%precio_unitario <= precio_max) THEN
            WRITE(*,*) "Nombre:", inventario(i)%nombre
            WRITE(*,*) "Cantidad Disponible:", inventario(i)%cantidad_disponible
            WRITE(*,*) "Precio Unitario:", inventario(i)%precio_unitario
          END IF
        END DO

      CASE DEFAULT
        WRITE(*,*) "Opción no válida."
    END SELECT
  END SUBROUTINE ConsultarProductos

END PROGRAM GestionInventario
