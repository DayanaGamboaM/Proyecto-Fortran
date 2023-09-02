PROGRAM GestionInventario
  !Se utiliza la estructura de producto creada
  USE ProductoModule

  INTEGER, PARAMETER :: MAX_PRODUCTOS = 100
  TYPE(Producto), DIMENSION(MAX_PRODUCTOS) :: inventario
  INTEGER :: num_productos = 0
  Integer :: opcion

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

  !Definicion de las subrutinas para las operaciones de registro, actualización y consulta de productos
  
  SUBROUTINE RegistrarProducto(inventario, num_productos)
    TYPE(Producto), DIMENSION(:) :: inventario
    INTEGER :: num_productos

    !Registro de producto, no se podrán registrar más de 100 productos
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
    !Actualización de producto, las opciones existentes son (agregar o disminuir stock)
    !opc(1) Agregar stock -> se realiza suma al inventario
    !opc(2) Disminuir stock -> se realiza resta al inventario
    !otra opc no es válida
    TYPE(Producto), DIMENSION(:) :: inventario
    INTEGER :: num_productos
    CHARACTER(50) :: nombre_producto
    INTEGER :: cantidad_actualizar
    INTEGER :: i  
    INTEGER :: opcion
    WRITE(*,*) "1. Agregar stock"
    WRITE(*,*) "2. Disminuir stock"
    READ(*,*) opcion


      SELECT CASE(opcion)
      CASE(1)
        WRITE(*,*) "Ingrese el nombre del producto a agregar al stock:"
        READ(*,*) nombre_producto

        DO i = 1, num_productos
          IF (TRIM(ADJUSTL(nombre_producto)) == TRIM(ADJUSTL(inventario(i)%nombre))) THEN
            WRITE(*,*) "Ingrese la cantidad a agregar:"
            READ(*,*) cantidad_actualizar
            inventario(i)%cantidad_disponible = inventario(i)%cantidad_disponible + cantidad_actualizar
            WRITE(*,*) "Cantidad actualizada con éxito."
            RETURN
          END IF
        END DO

        WRITE(*,*) "Producto agregado al inventario."

      CASE(2)
        WRITE(*,*) "Ingrese el nombre del producto a restar al stock:"
        READ(*,*) nombre_producto

        DO i = 1, num_productos
          IF (TRIM(ADJUSTL(nombre_producto)) == TRIM(ADJUSTL(inventario(i)%nombre))) THEN
            WRITE(*,*) "Ingrese la cantidad a restar:"
            READ(*,*) cantidad_actualizar
            inventario(i)%cantidad_disponible = inventario(i)%cantidad_disponible - cantidad_actualizar
            WRITE(*,*) "Cantidad actualizada con éxito."
            RETURN
          END IF
        END DO

      CASE DEFAULT
        WRITE(*,*) "Opción no válida."
    END SELECT

    WRITE(*,*) "Producto no encontrado en el inventario."
  END SUBROUTINE ActualizarCantidad

  SUBROUTINE ConsultarProductos(inventario, num_productos)
    !Consulta de productos,las opciones son (consulta por nombre o rango de precio)
    TYPE(Producto), DIMENSION(:) :: inventario
    INTEGER :: num_productos
    CHARACTER(50) :: nombre_producto
    REAL :: precio_min, precio_max
    CHARACTER :: input_precio_min, input_precio_max
    INTEGER :: iostat
    INTEGER :: opcion
    WRITE(*,*) "1. Consultar por nombre"
    WRITE(*,*) "2. Consultar por rango de precios"
    READ(*,*) opcion

    SELECT CASE(opcion)
      !Consulta por nombre de producto
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
        !Consulta por rango de precio
        WRITE(*,*) "Ingrese el precio mínimo:"
        READ(*,*) input_precio_min
        WRITE(*,*) "Ingrese el precio máximo:"
        READ(*,*) input_precio_max


!ARREGLAAAAAAAAAAAR do while
      !Verifica si los valores ingresados son numéricos y no negativos
      READ(input_precio_min, *, IOSTAT = iostat) precio_min

      IF (iostat /= 0) THEN
        WRITE(*,*) "El valor ingresado para precio mínimo no es numérico. Intente nuevamente."
      ELSE IF (precio_min < 0.0) THEN
        WRITE(*,*) "El valor ingresado para precio mínimo no puede ser negativo. Intente nuevamente."
      ELSE
        READ(input_precio_max, *, IOSTAT = iostat) precio_max
        IF (iostat /= 0) THEN
          WRITE(*,*) "El valor ingresado para precio máximo no es numérico. Intente nuevamente."
        ELSE IF (precio_max < 0.0) THEN
          WRITE(*,*) "El valor ingresado para precio máximo no puede ser negativo. Intente nuevamente."
        ELSE

          ! (Continúa con la lógica para consultar por rango de precios)
        END IF
      END IF

        DO i = 1, num_productos
          IF (inventario(i)%precio_unitario >= precio_min .AND. inventario(i)%precio_unitario <= precio_max) THEN
            WRITE(*,*) "Nombre:", inventario(i)%nombre
            WRITE(*,*) "Cantidad Disponible:", inventario(i)%cantidad_disponible
            WRITE(*,*) "Precio Unitario:", inventario(i)%precio_unitario
          END IF

          !IF (inventario(i)%precio_unitario = precio_min .AND. inventario(i)%precio_unitario = precio_max) THEN
          !  print *, 'No tenemos productos para este rango de precios' 
           
         ! END IF
        END DO

      CASE DEFAULT
        WRITE(*,*) "Opción no válida."
    END SELECT
  END SUBROUTINE ConsultarProductos


  

END PROGRAM GestionInventario
