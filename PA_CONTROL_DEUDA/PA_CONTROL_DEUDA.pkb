CREATE OR REPLACE PACKAGE BODY TCSTARTA.PA_CONTROL_DEUDA
IS
--CONTROL SI TIENE DEUDA
FUNCTION CONTROL_DEUDA (
                        P_TIPO_IMPONIBLE      IN    VARCHAR2,
                        P_IMPONIBLE           IN    VARCHAR2,
                        P_FECHA_CONSULTA      IN    DATE)
RETURN VARCHAR2 
AS
V_RESULTADO         BOOLEAN;
V_FECHA_CONSULTA    DATE;
V_ANIO_EXTRAIDO     VARCHAR2(4);
V_CLAVE_IMPONIBLE   VARCHAR2(40);
V_MENSAJE           VARCHAR(24);
BEGIN
    --DESCOMPONER LA FECHA
    V_ANIO_EXTRAIDO := EXTRACT(YEAR FROM P_FECHA_CONSULTA)-1;
    V_FECHA_CONSULTA := TO_DATE('31/12/'||V_ANIO_EXTRAIDO,'DD/MM/YYYY');
    --DBMS_OUTPUT.PUT_LINE(V_FECHA_CONSULTA);
    
    --BUSCA LA CLAVE DEL IMPONIBLE
    V_CLAVE_IMPONIBLE := PA_SIAC_BOLETAS_MULTIPLES.CLAVE_IMPONIBLE (P_IMPONIBLE,P_TIPO_IMPONIBLE);
    --DBMS_OUTPUT.PUT_LINE(V_CLAVE_IMPONIBLE);
    
    --TRAE TRUE SI EL IMPONIBLE TIENE DEUDAS
    V_RESULTADO:=TIENE_DEUDA (P_TIPO_IMPONIBLE,V_CLAVE_IMPONIBLE,V_FECHA_CONSULTA,V_FECHA_CONSULTA,NULL,NULL);
    --dbms_output.put_line(sys.diutil.bool_to_int(V_RESULTADO));
    --DEVUELVE SI TIENE DEUDA O NO
    IF V_RESULTADO = TRUE THEN
        V_MENSAJE := 'TIENE DEUDA ANTERIOR';
    ELSE 
        V_MENSAJE := 'NO TIENE DEUDA ANTERIOR';
    END IF;
    --DBMS_OUTPUT.PUT_LINE(V_MENSAJE);
    RETURN V_MENSAJE;
    
END;
--FUNCION QUE SE INCLUYE EN LA FUNCION CONTROL DEUDA 
FUNCTION tiene_deuda (
      p_tipo_imponible       VARCHAR2,
      p_clave_imponible      VARCHAR2,
      p_fecha_acreditacion   DATE,
      p_fecha_vencimiento    DATE,
      p_impuesto             VARCHAR2 := NULL,
      p_concepto             VARCHAR2 := NULL
   )
      RETURN BOOLEAN
   AS
      l_suma          NUMBER;
      v_deuda         BOOLEAN := TRUE ;
      v_estado_plan   BOOLEAN := TRUE ;
    -- SE MODIFICARA PARA QUE SOLO TRAIGA DESDE EL AÑO 2014
      CURSOR planes
      IS
         SELECT DISTINCT vorig.plan_facilidad_incluido
                    FROM tbl_imponibles_x_planes ixp, tbl_vencimientos vorig
                   WHERE vorig.tipo_vencimiento = 'O'
                     AND vorig.impuesto = NVL (p_impuesto, vorig.impuesto)
                     AND vorig.concepto_obligacion =
                                  NVL (p_concepto, vorig.concepto_obligacion)
                     AND vorig.tipo_imponible = ixp.tipo_imponible
                     AND vorig.clave_imponible = ixp.clave_imponible
                     AND vorig.ANIO >= 2014
                     AND ixp.fecha_extraccion IS NULL
                     AND ixp.tipo_imponible = p_tipo_imponible
                     AND ixp.clave_imponible = p_clave_imponible
                     AND GREATEST( vorig.FECHA_PRIMER_VENCIMIENTO, 
                                   vorig.FECHA_SEGUNDO_VENCIMIENTO,
                                   vorig.FECHA_TERCER_VENCIMIENTO,
                                   vorig.FECHA_CUARTO_VENCIMIENTO --FALTA VER CON FECHA DE PRESENTACION
                                    ) <= p_fecha_vencimiento;--AQUI SE CAMBIO PARA QUE TRAIGA CON LA FECHA CALCULADA DE V_FECHA CONSULTA
   BEGIN
      BEGIN
         SELECT NVL (SUM (importe), 0)
           INTO l_suma
           FROM tbl_cuentas_corrientes cc, tbl_vencimientos v
          WHERE cc.impuesto = v.impuesto
            AND cc.concepto_obligacion = v.concepto_obligacion
            AND cc.numero_obligacion_impuesto = v.numero_obligacion_impuesto
            AND cc.numero_rectificativa = 0
            AND cc.numero_cuota = v.numero_cuota
            AND (   SYSDATE >= TRUNC (cc.fecha_acreditacion)--AQUI SE CAMBIO PARA QUE TOME EL PAGO INCLUSO HASTA LA FECHA DEL DIA DE HOY
                 OR cc.fecha_acreditacion IS NULL
                )
            AND TRUNC (v.fecha_primer_vencimiento) <
                   NVL (
                      p_fecha_vencimiento,
                      TRUNC (v.fecha_primer_vencimiento + 1)
                   )
            AND v.tipo_vencimiento != 'O'
			AND v.CUOTA_CONTADO is null   ---- no suma la contado
			AND V.FECHA_RECTIFICATIVA IS NULL
            AND v.tipo_imponible = p_tipo_imponible
            AND v.clave_imponible = p_clave_imponible
            AND v.impuesto = NVL (p_impuesto, v.impuesto)
            AND v.concepto_obligacion =
                                      NVL (p_concepto, v.concepto_obligacion)
            AND v.ANIO >=2014;
        --DBMS_OUTPUT.PUT_LINE(l_suma);
         IF l_suma = 0 OR l_suma IS NULL
         THEN
            v_deuda := FALSE ;
         ELSE
            RETURN TRUE ;
         END IF;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            v_deuda := FALSE ;
      END;

      -- Usuario: CA - Agregado para Quiebras - Fecha modificación 01/03/2006
      IF NOT v_deuda THEN
        v_deuda := tiene_deuda_quiebras (p_tipo_imponible,
                                                     p_clave_imponible,
                                                     p_fecha_acreditacion,
                                                     p_fecha_vencimiento);
        --dbms_output.put_line(sys.diutil.bool_to_int(v_deuda));
      END IF;
      --

      IF NOT v_deuda
      THEN
         FOR c IN planes
         LOOP
            v_estado_plan :=
                  PA_CUENTAS_CORRIENTES.ESTADO_PLAN(--HABRIA QUE PENSAR SI HAY QUE CAMBIAR ESTO POR P_FECHA_VENCIMIENTO
                     c.plan_facilidad_incluido,--ME DEVUELVE 0
                     p_fecha_vencimiento
                  );
            
            IF NOT v_estado_plan
            THEN
               RETURN TRUE ;
            END IF;
--            dbms_output.put_line(sys.diutil.bool_to_int(v_estado_plan));
         END LOOP;

         RETURN FALSE ;
      ELSE
         RETURN TRUE ;
      END IF;
   END;

FUNCTION tiene_deuda_quiebras (
      p_tipo_imponible       VARCHAR2,
      p_clave_imponible      VARCHAR2,
      p_fecha_acreditacion   DATE,
      p_fecha_vencimiento    DATE
   )
      RETURN BOOLEAN
  AS
  
 Cursor  reg_imponibles is
  Select DISTINCT quiebra_id
    from tbl_imponibles_x_quiebras
   where tipo_imponible = p_tipo_imponible
     and clave_imponible = p_clave_imponible;
  --
  l_suma          NUMBER;
  --
begin
 --
 FOR ri IN reg_imponibles LOOP
     --
     BEGIN
         SELECT NVL (SUM (importe), 0)
           INTO l_suma
           FROM tbl_cuentas_corrientes cc, tbl_vencimientos v
          WHERE cc.impuesto = v.impuesto
            AND cc.concepto_obligacion = v.concepto_obligacion
            AND cc.numero_obligacion_impuesto = v.numero_obligacion_impuesto
            AND cc.numero_rectificativa = 0
            AND cc.numero_cuota = v.numero_cuota
            AND (p_fecha_acreditacion >= TRUNC (cc.fecha_acreditacion)
                 OR cc.fecha_acreditacion IS NULL
                )
            AND TRUNC (v.fecha_primer_vencimiento) <
                   NVL (
                      p_fecha_vencimiento,
                      TRUNC (v.fecha_primer_vencimiento + 1)
                   )
            AND v.cuota_contado is null
            AND v.tipo_vencimiento != 'O'
            AND v.quiebra_id = ri.quiebra_id
            AND v.ANIO >= 2014;
         --
         IF l_suma = 0 OR l_suma IS NULL THEN
            RETURN FALSE ;
         ELSE
           RETURN TRUE ;
         END IF;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RETURN FALSE ;
      END;
      --
  END LOOP;
  --
  IF l_suma = 0 OR l_suma IS NULL THEN
      RETURN FALSE ;
  END IF;
  --
END;
END;
/
