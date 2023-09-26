CREATE OR REPLACE PACKAGE BODY TCSSANLO.PA_SIAC_PLANES
AS
--FUNCION QUE TRAE TODOS LOS CONTRIBUYENTES CON PLANES
FUNCTION GET_CONTRIBUYENTE_PLAN
    RETURN T_CONTRIBUYENTE
AS
    V_CONTRIBUYENTE   T_CONTRIBUYENTE;

    CURSOR C_CONTRIBUYENTE IS
        SELECT          
                        P.NUMERO_DOCUMENTO,
                        P.CUIT,
                        P.RAZON_SOCIAL
          FROM TBL_PERSONAS  P
               INNER JOIN TBL_PLANES_FACILIDADES PF
                   ON PF.CONTRIBUYENTE = P.PERSONA_ID
         GROUP BY       
                        P.NUMERO_DOCUMENTO,
                        P.CUIT,
                        P.RAZON_SOCIAL;                         
BEGIN
    V_CONTRIBUYENTE := T_CONTRIBUYENTE ();
    
            FOR CONTRI IN C_CONTRIBUYENTE
            LOOP
                V_CONTRIBUYENTE.EXTEND;
                V_CONTRIBUYENTE (V_CONTRIBUYENTE.COUNT) :=
                    R_CONTRIBUYENTE (
                                     CONTRI.NUMERO_DOCUMENTO,
                                     CONTRI.CUIT,
                                     CONTRI.RAZON_SOCIAL);
            END LOOP;

    RETURN V_CONTRIBUYENTE;
END;
--TRAE UNA LISTA POR RAZON SOCIAL DE CONTRIUYENTES CON PLANES
FUNCTION GET_LISTA_RAZON_SOCIAL (P_RAZON_SOCIAL TBL_PERSONAS.RAZON_SOCIAL%TYPE)
RETURN T_CONTRIBUYENTE
IS 
V_CONTRIBUYENTE T_CONTRIBUYENTE;
CURSOR C_CONTRIBUYENTE IS
        SELECT   
                        P.NUMERO_DOCUMENTO,
                        P.CUIT,
                        P.RAZON_SOCIAL
          FROM TBL_PERSONAS  P
               INNER JOIN TBL_PLANES_FACILIDADES PF
                   ON PF.CONTRIBUYENTE = P.PERSONA_ID
               INNER JOIN TBL_IMPONIBLES I
                   ON I.CONTRIBUYENTE = P.PERSONA_ID 
         WHERE  P.RAZON_SOCIAL LIKE '%'||P_RAZON_SOCIAL||'%'
         GROUP BY P.NUMERO_DOCUMENTO,
                        P.CUIT,
                        P.RAZON_SOCIAL; 
BEGIN 
    V_CONTRIBUYENTE := T_CONTRIBUYENTE ();

    FOR CONTRI IN C_CONTRIBUYENTE
    LOOP
        V_CONTRIBUYENTE.EXTEND;
        V_CONTRIBUYENTE (V_CONTRIBUYENTE.COUNT) :=
            R_CONTRIBUYENTE (
                             CONTRI.NUMERO_DOCUMENTO,
                             CONTRI.CUIT,
                             CONTRI.RAZON_SOCIAL);
    END LOOP;

    RETURN V_CONTRIBUYENTE;
END;
--FUNCION QUE TRAE LOS PLANES QUE POSEE EL CONTRIBUYENTE ASI COMO SU ESTADO, LA CANTIDAD DE CUOTAS Y FECHA EN LA QUE SE GENERO 
FUNCTION GET_PLAN_X_CUIT (
    P_CUIT   IN TBL_PERSONAS.CUIT%TYPE,
    P_DNI    IN TBL_PERSONAS.NUMERO_DOCUMENTO%TYPE,
    P_RAZON_SOCIAL IN TBL_PERSONAS.RAZON_SOCIAL%TYPE)
    RETURN T_PLAN
AS
    V_PLAN   T_PLAN;

    CURSOR C_PLAN IS
        SELECT PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO,
               PA_CUENTAS_CORRIENTES.PLAN (SYSDATE, PF.PLAN_FACILIDAD)    ESTADO
          FROM TBL_PLANES_FACILIDADES  PF
               INNER JOIN TBL_PERSONAS P ON P.PERSONA_ID = PF.CONTRIBUYENTE
         WHERE P.RAZON_SOCIAL = P_RAZON_SOCIAL
         AND P.CUIT = P_CUIT 
         AND P.NUMERO_DOCUMENTO = P_DNI 
         GROUP BY PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO
         ORDER BY PF.FECHA_GENERACION;
      
    CURSOR C_PLAN2 IS
        SELECT PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO, 
               PA_CUENTAS_CORRIENTES.PLAN (SYSDATE, PF.PLAN_FACILIDAD)    ESTADO
          FROM TBL_PLANES_FACILIDADES  PF
               INNER JOIN TBL_PERSONAS P ON P.PERSONA_ID = PF.CONTRIBUYENTE
         WHERE P.RAZON_SOCIAL = P_RAZON_SOCIAL
         AND P.CUIT = P_CUIT 
         OR P.NUMERO_DOCUMENTO = P_DNI 
         GROUP BY PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO
         ORDER BY PF.FECHA_GENERACION;
    CURSOR C_PLAN3 IS
        SELECT PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO,
               PA_CUENTAS_CORRIENTES.PLAN (SYSDATE, PF.PLAN_FACILIDAD)    ESTADO
          FROM TBL_PLANES_FACILIDADES  PF
               INNER JOIN TBL_PERSONAS P ON P.PERSONA_ID = PF.CONTRIBUYENTE
         WHERE P.RAZON_SOCIAL = P_RAZON_SOCIAL 
         AND P.NUMERO_DOCUMENTO = P_DNI 
         OR P.CUIT = P_CUIT
         GROUP BY PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO
         ORDER BY PF.FECHA_GENERACION;
    CURSOR C_PLAN4 IS
        SELECT PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO,
               PA_CUENTAS_CORRIENTES.PLAN (SYSDATE, PF.PLAN_FACILIDAD)    ESTADO
          FROM TBL_PLANES_FACILIDADES  PF
               INNER JOIN TBL_PERSONAS P ON P.PERSONA_ID = PF.CONTRIBUYENTE
         WHERE P.RAZON_SOCIAL = P_RAZON_SOCIAL
         OR P.CUIT = P_CUIT 
         OR P.NUMERO_DOCUMENTO = P_DNI 
         GROUP BY PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO
         ORDER BY PF.FECHA_GENERACION;
    CURSOR C_PLAN5 IS
        SELECT PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO,
               PA_CUENTAS_CORRIENTES.PLAN (SYSDATE, PF.PLAN_FACILIDAD)    ESTADO
          FROM TBL_PLANES_FACILIDADES  PF
               INNER JOIN TBL_PERSONAS P ON P.PERSONA_ID = PF.CONTRIBUYENTE
         WHERE P.CUIT = P_CUIT 
         AND P.NUMERO_DOCUMENTO = P_DNI 
         GROUP BY PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO
         ORDER BY PF.FECHA_GENERACION;
    CURSOR C_PLAN6 IS
        SELECT PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO,
               PA_CUENTAS_CORRIENTES.PLAN (SYSDATE, PF.PLAN_FACILIDAD)    ESTADO
          FROM TBL_PLANES_FACILIDADES  PF
               INNER JOIN TBL_PERSONAS P ON P.PERSONA_ID = PF.CONTRIBUYENTE
         WHERE P.CUIT = P_CUIT 
         OR P.NUMERO_DOCUMENTO = P_DNI 
         GROUP BY PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO
         ORDER BY PF.FECHA_GENERACION;
    CURSOR C_PLAN7 IS
        SELECT PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO,
               PA_CUENTAS_CORRIENTES.PLAN (SYSDATE, PF.PLAN_FACILIDAD)    ESTADO
          FROM TBL_PLANES_FACILIDADES  PF
               INNER JOIN TBL_PERSONAS P ON P.PERSONA_ID = PF.CONTRIBUYENTE
         WHERE P.NUMERO_DOCUMENTO = P_DNI 
         GROUP BY PF.NUMERO_ACOGIMIENTO,
               PF.FECHA_GENERACION,
               PF.CUOTAS_GENERADAS,
               PF.TOTAL_DEUDA,
               PF.PLAN_FACILIDAD,
               PF.IMPUESTO
         ORDER BY PF.FECHA_GENERACION;
    V_RESULTADO VARCHAR2(50):=0;
    V_CONTROL NUMBER;
    V_ERROR EXCEPTION;
BEGIN
    V_PLAN := T_PLAN ();
    
    --DIFERENTES ESTADOS PARA LA CONSULTA DE LOS PLANES VIGENTES
    BEGIN
        CASE
        WHEN (P_RAZON_SOCIAL IS NOT NULL) AND (P_CUIT IS NOT NULL) AND (P_DNI IS NOT NULL) THEN  V_RESULTADO :='1';
        WHEN (P_RAZON_SOCIAL IS NOT NULL) AND (P_CUIT IS NOT NULL) AND (P_DNI IS NULL) THEN  V_RESULTADO :='2';
        WHEN (P_RAZON_SOCIAL IS NOT NULL) AND (P_CUIT IS NULL) AND (P_DNI IS NOT NULL) THEN  V_RESULTADO :='3';
        WHEN (P_RAZON_SOCIAL IS NOT NULL) AND (P_CUIT IS NULL) AND (P_DNI IS NULL) THEN  V_RESULTADO :='4';
        WHEN (P_RAZON_SOCIAL IS NULL) AND (P_CUIT IS NOT NULL) AND (P_DNI IS NOT NULL) THEN  V_RESULTADO :='5';
        WHEN (P_RAZON_SOCIAL IS NULL) AND (P_CUIT IS NOT NULL) AND (P_DNI IS NULL) THEN  V_RESULTADO :='6';
        WHEN (P_RAZON_SOCIAL IS NULL) AND (P_CUIT IS NULL) AND (P_DNI IS NOT NULL) THEN  V_RESULTADO :='7';
        WHEN (P_RAZON_SOCIAL IS NULL) AND (P_CUIT IS NULL) AND (P_DNI IS NULL) THEN  V_RESULTADO :='8';
        ELSE V_RESULTADO :='NO SE ENCUENTRA';
        END CASE;  
    END;
    BEGIN
        IF V_RESULTADO = 1 THEN
            FOR PLAN IN C_PLAN
            LOOP
                V_PLAN.EXTEND;
                V_PLAN (V_PLAN.COUNT) :=
                    R_PLAN (PLAN.NUMERO_ACOGIMIENTO,
                            PLAN.FECHA_GENERACION,
                            PLAN.CUOTAS_GENERADAS,
                            PLAN.TOTAL_DEUDA,
                            PLAN.PLAN_FACILIDAD,
                            PLAN.IMPUESTO,
                            PLAN.ESTADO);
            END LOOP;
        ELSE 
            IF V_RESULTADO = 2 THEN 
                FOR PLAN IN C_PLAN2
                LOOP
                    V_PLAN.EXTEND;
                    V_PLAN (V_PLAN.COUNT) :=
                        R_PLAN (PLAN.NUMERO_ACOGIMIENTO,
                                PLAN.FECHA_GENERACION,
                                PLAN.CUOTAS_GENERADAS,
                                PLAN.TOTAL_DEUDA,
                                PLAN.PLAN_FACILIDAD,
                                PLAN.IMPUESTO,
                                PLAN.ESTADO);
                END LOOP;
            ELSE 
                IF V_RESULTADO = 3 THEN
                    FOR PLAN IN C_PLAN3
                    LOOP
                        V_PLAN.EXTEND;
                        V_PLAN (V_PLAN.COUNT) :=
                            R_PLAN (PLAN.NUMERO_ACOGIMIENTO,
                                    PLAN.FECHA_GENERACION,
                                    PLAN.CUOTAS_GENERADAS,
                                    PLAN.TOTAL_DEUDA,
                                    PLAN.PLAN_FACILIDAD,
                                    PLAN.IMPUESTO,
                                    PLAN.ESTADO);
                    END LOOP;
                ELSE
                V_CONTROL := CONTROL(P_RAZON_SOCIAL);
                     IF V_RESULTADO = 4 AND V_CONTROL = 1 THEN
                        
                        FOR PLAN IN C_PLAN4
                        LOOP
                            V_PLAN.EXTEND;
                            V_PLAN (V_PLAN.COUNT) :=
                                R_PLAN (PLAN.NUMERO_ACOGIMIENTO,
                                        PLAN.FECHA_GENERACION,
                                        PLAN.CUOTAS_GENERADAS,
                                        PLAN.TOTAL_DEUDA,
                                        PLAN.PLAN_FACILIDAD,
                                        PLAN.IMPUESTO,
                                        PLAN.ESTADO);
                        END LOOP;
                     ELSE 
                        IF V_RESULTADO = 4 AND V_CONTROL > 1 THEN 
                        RAISE V_ERROR;   
                     ELSE
                        IF V_RESULTADO = 5 THEN
                            FOR PLAN IN C_PLAN5
                            LOOP
                                V_PLAN.EXTEND;
                                V_PLAN (V_PLAN.COUNT) :=
                                    R_PLAN (PLAN.NUMERO_ACOGIMIENTO,
                                            PLAN.FECHA_GENERACION,
                                            PLAN.CUOTAS_GENERADAS,
                                            PLAN.TOTAL_DEUDA,
                                            PLAN.PLAN_FACILIDAD,
                                            PLAN.IMPUESTO,
                                            PLAN.ESTADO);
                            END LOOP;
                        ELSE 
                            IF V_RESULTADO = 6 THEN
                                FOR PLAN IN C_PLAN6
                                LOOP
                                    V_PLAN.EXTEND;
                                    V_PLAN (V_PLAN.COUNT) :=
                                        R_PLAN (PLAN.NUMERO_ACOGIMIENTO,
                                                PLAN.FECHA_GENERACION,
                                                PLAN.CUOTAS_GENERADAS,
                                                PLAN.TOTAL_DEUDA,
                                                PLAN.PLAN_FACILIDAD,
                                                PLAN.IMPUESTO,
                                                PLAN.ESTADO);
                                END LOOP;
                            ELSE 
                                IF V_RESULTADO = 7 THEN
                                    FOR PLAN IN C_PLAN7
                                    LOOP
                                        V_PLAN.EXTEND;
                                        V_PLAN (V_PLAN.COUNT) :=
                                            R_PLAN (PLAN.NUMERO_ACOGIMIENTO,
                                                    PLAN.FECHA_GENERACION,
                                                    PLAN.CUOTAS_GENERADAS,
                                                    PLAN.TOTAL_DEUDA,
                                                    PLAN.PLAN_FACILIDAD,
                                                    PLAN.IMPUESTO,
                                                    PLAN.ESTADO);
                                    END LOOP;
                                ELSE 
                                    IF V_RESULTADO = 8 THEN
                                    V_RESULTADO := 'SIN REGISTRO';
                                    DBMS_OUTPUT.PUT_LINE(V_RESULTADO);
                                    END IF;
                                END IF;
                        END IF;
                    END IF;    
                    END IF;
                END IF;
            END IF;
        END IF;
    END IF;
    END;
    RETURN V_PLAN;
EXCEPTION
WHEN V_ERROR THEN DBMS_OUTPUT.PUT_LINE('POSEE MAS DE UNA RAZON SOCIAL');
END;
--FUNCION QUE TRAE LOS DETALLES DE UN PLAN EN ESPECIFICO
FUNCTION GET_DETALLE_PLAN (
    P_NUMERO_ACOGIMIENTO IN TBL_PLANES_FACILIDADES.NUMERO_ACOGIMIENTO%TYPE)
    RETURN T_DET_PLAN
AS
V_DETALLE   T_DET_PLAN;
CURSOR C_DEUDA IS
    SELECT  V.NUMERO_CUOTA, V.FECHA_PRIMER_VENCIMIENTO,
                       PA_CUENTAS_CORRIENTES.IMPORTE_ORIGINAL
                                             (V.IMPUESTO,
                                              V.CONCEPTO_OBLIGACION,
                                              V.NUMERO_OBLIGACION_IMPUESTO,
                                              V.NUMERO_RECTIFICATIVA,
                                              V.NUMERO_CUOTA
                                             ) IMPORTE,
                       PA_ACTUALIZACION.ACTUALIZAR_VENCIMIENTO
                                 (V.IMPUESTO,
                                  V.CONCEPTO_OBLIGACION,
                                  V.NUMERO_OBLIGACION_IMPUESTO,
                                  V.NUMERO_RECTIFICATIVA,
                                  V.NUMERO_CUOTA,
                                  SYSDATE
                                 ) IMPORTE_ACTUALIZADO,
                       PA_CUENTAS_CORRIENTES.PAGOS_VENCIMIENTO
                                      (V.IMPUESTO,
                                       V.CONCEPTO_OBLIGACION,
                                       V.NUMERO_OBLIGACION_IMPUESTO,
                                       V.NUMERO_RECTIFICATIVA,
                                       V.NUMERO_CUOTA,
                                       SYSDATE
                                      ) IMPORTE_PAGADO
                  FROM TBL_VENCIMIENTOS V
                  INNER JOIN VW_PERSONAS P
                  ON P.PERSONA_ID = V.CONTRIBUYENTE
                  INNER JOIN TBL_PLANES_FACILIDADES PF
                  ON PF.PLAN_FACILIDAD = V.PLAN_FACILIDAD
                 WHERE  PF.NUMERO_ACOGIMIENTO = P_NUMERO_ACOGIMIENTO
              ORDER BY V.FECHA_PRIMER_VENCIMIENTO;
           
BEGIN 
    
    V_DETALLE := T_DET_PLAN();
    FOR DETALLE IN C_DEUDA
        LOOP
            V_DETALLE.EXTEND;
            V_DETALLE (V_DETALLE.COUNT) :=
                R_DET_PLAN (
                        DETALLE.NUMERO_CUOTA,
                        DETALLE.FECHA_PRIMER_VENCIMIENTO,
                        DETALLE.IMPORTE,
                        DETALLE.IMPORTE_ACTUALIZADO,
                        DETALLE.IMPORTE_PAGADO);
        END LOOP;

    RETURN V_DETALLE;

END;
--FUNCION QUE TRAE TODOS LOS CONTRIBUYENTES CON PLANES POR MEDIO DEL CURSOR
FUNCTION GET_CONTRIBUYENTE_PLAN_CURSOR 
      RETURN SYS_REFCURSOR AS                                    
      V_PLAN   SYS_REFCURSOR;
      
   BEGIN
          OPEN V_PLAN
           FOR
              SELECT  P.NUMERO_DOCUMENTO,
                        P.CUIT,
                        P.RAZON_SOCIAL,
                        P.NOMBRE
          FROM TBL_PERSONAS  P
               INNER JOIN TBL_PLANES_FACILIDADES PF
                   ON PF.CONTRIBUYENTE = P.PERSONA_ID
         GROUP BY P.NUMERO_DOCUMENTO,
                        P.CUIT,
                        P.RAZON_SOCIAL,
                        P.NOMBRE;         

      RETURN V_PLAN;
END;
--FUNCION QUE TRAE LA CANTIDAD DE VECES QUE ESTA UNA RAZON SOCIAL EN LA TABLA PERSONAS Y QUE SERA UTILIZADA EN LA FUNCION PLAN_X_CUIT
FUNCTION CONTROL(P_RAZON_SOCIAL IN TBL_PERSONAS.RAZON_SOCIAL%TYPE)
RETURN NUMBER
AS
V_RESULTADO NUMBER;
BEGIN
        BEGIN
          SELECT          
          COUNT(P.RAZON_SOCIAL)
          INTO V_RESULTADO
          FROM TBL_PERSONAS  P 
          WHERE P.RAZON_SOCIAL = P_RAZON_SOCIAL; 
        END;
        
           
    RETURN V_RESULTADO;
END;

FUNCTION GENERADOR_BOLETA_PLANES (
      p_cuotas_multiples IN   VARCHAR2,
      p_plan_facilidad   IN   NUMBER,   
      p_fecha_emision    IN   DATE
   )
      RETURN
            --VARCHAR2 AS
            sys_refcursor
AS
      v_codigo_impresion_num   VARCHAR2 (20);
      v_contribuyente          tbl_imponibles.contribuyente%TYPE;
      v2_clave_imponible        tbl_imponibles.clave%TYPE;
      V2_TIPO_IMPONIBLE         TBL_IMPONIBLES.TIPO_IMPONIBLE%TYPE;
      V_IMPUESTO               TBL_IMPUESTOS.IMPUESTO%TYPE; 
      V_CONCEPTO               TBL_CONCEPTOS_OBLIGACIONES.CONCEPTO_OBLIGACION%TYPE;
      V_ANIO                   TBL_VENCIMIENTOS.ANIO%TYPE;  
      ERROR                    EXCEPTION; 
--Cursos
      rf_datos                 sys_refcursor;
     
BEGIN
    --PARA SABER LA CANTIDAD DE IMPONIBLES
    
        BEGIN
            SELECT IP.CLAVE_IMPONIBLE,IP.TIPO_IMPONIBLE
            INTO V2_CLAVE_IMPONIBLE,V2_TIPO_IMPONIBLE
            FROM TBL_IMPONIBLES_X_PLANES IP
            WHERE IP.PLAN_FACILIDAD = P_PLAN_FACILIDAD
            GROUP BY IP.CLAVE_IMPONIBLE,IP.TIPO_IMPONIBLE;
        EXCEPTION
        WHEN OTHERS THEN
            V2_CLAVE_IMPONIBLE:=NULL;
            V2_TIPO_IMPONIBLE:=NULL;
        END;
        DBMS_OUTPUT.PUT_LINE(V2_CLAVE_IMPONIBLE||V2_TIPO_IMPONIBLE);
    
     
        SELECT 
               IP.CONTRIBUYENTE, 
               PF.IMPUESTO, 
               PF.CONCEPTO_OBLIGACION, 
               V.ANIO
        INTO V_CONTRIBUYENTE,
         V_IMPUESTO, V_CONCEPTO, V_ANIO
        FROM TBL_IMPONIBLES_X_PLANES IP
        INNER JOIN TBL_PLANES_FACILIDADES PF
        ON PF.PLAN_FACILIDAD = IP.PLAN_FACILIDAD
        INNER JOIN TBL_VENCIMIENTOS V 
        ON V.PLAN_FACILIDAD = PF.PLAN_FACILIDAD
        WHERE IP.PLAN_FACILIDAD = P_PLAN_FACILIDAD
        AND ROWNUM = 1
        GROUP BY IP.CLAVE_IMPONIBLE, 
               IP.TIPO_IMPONIBLE, 
               IP.CONTRIBUYENTE, 
               PF.IMPUESTO, 
               PF.CONCEPTO_OBLIGACION, 
               V.ANIO;
    
         
             --V_IMPONIBLE := F_GET_IMPONIBLE(V2_CLAVE_IMPONIBLE, V2_TIPO_IMPONIBLE);
             
                v_codigo_impresion_num :=
                                     INSERTA_BOLETA_COD_IMPRESION (
                                                           V_IMPUESTO,
                                                           V_CONCEPTO,
                                                           V_ANIO,
                                                           p_cuotas_multiples,
                                                           NULL,
                                                           NULL,
                                                           P_PLAN_FACILIDAD,
                                                           p_fecha_emision
                                                          );
                 DBMS_OUTPUT.PUT_LINE (v_codigo_impresion_num);
        
     
        OPEN rf_datos FOR
               SELECT 0 ID, SYSDATE term_date
                 FROM DUAL;
                     
                        rf_datos :=
                             GENERA_CODIGO_BARRA (TO_NUMBER (v_codigo_impresion_num),
                                            p_fecha_emision,
                                            v_contribuyente,
                                            V2_TIPO_IMPONIBLE,
                                            V2_CLAVE_IMPONIBLE,
                                            p_plan_facilidad,
                                            V_ANIO,
                                            V_CONCEPTO,
                                            V_IMPUESTO
                                           );
                    
                        
        RETURN rf_datos;
EXCEPTION
      WHEN OTHERS
      THEN
         RETURN NULL;
END; 
FUNCTION F_GET_IMPONIBLE (
                                            P_CLAVE_IMPONIBLE   IN  TBL_IMPONIBLES.CLAVE%TYPE,
                                            P_TIPO_IMPONIBLE    IN  TBL_IMPONIBLES.TIPO_IMPONIBLE%TYPE)
RETURN VARCHAR2
AS
V_IMPONIBLE VARCHAR2(200);
BEGIN
    CASE
        WHEN P_TIPO_IMPONIBLE = '0001' THEN 
            SELECT I.COL07
            INTO V_IMPONIBLE
            FROM TBL_IMPONIBLES I
            WHERE I.TIPO_IMPONIBLE = P_TIPO_IMPONIBLE
            AND I.CLAVE = P_CLAVE_IMPONIBLE;
        WHEN P_TIPO_IMPONIBLE = '0002' THEN 
            SELECT I.COL02
            INTO V_IMPONIBLE
            FROM TBL_IMPONIBLES I
            WHERE I.TIPO_IMPONIBLE = P_TIPO_IMPONIBLE
            AND I.CLAVE = P_CLAVE_IMPONIBLE;
        WHEN P_TIPO_IMPONIBLE = '0003' THEN 
            SELECT I.COL01
            INTO V_IMPONIBLE
            FROM TBL_IMPONIBLES I
            WHERE I.TIPO_IMPONIBLE = P_TIPO_IMPONIBLE
            AND I.CLAVE = P_CLAVE_IMPONIBLE;
            IF V_IMPONIBLE IS NULL THEN 
                SELECT I.COL02
                INTO V_IMPONIBLE
                FROM TBL_IMPONIBLES I
                WHERE I.TIPO_IMPONIBLE = P_TIPO_IMPONIBLE
                AND I.CLAVE = P_CLAVE_IMPONIBLE;
            END IF;
        ELSE 
            V_IMPONIBLE := NULL;
    END CASE;
    RETURN V_IMPONIBLE;
END;

FUNCTION get_domicilio_contrib2(P_TIPO_IMPONIBLE   TBL_TIPOS_IMPONIBLES.TIPO_IMPONIBLE%TYPE
                              ,P_CLAVE_IMPONIBLE  TBL_IMPONIBLES.CLAVE%TYPE
                              ,P_CONTRIBUYENTE    TBL_PERSONAS.PERSONA_ID%TYPE
                              ,P_TIPO_DIRECCION   TBL_TIPOS_DIRECCIONES.TIPO_DIRECCION%TYPE)
                               RETURN VARCHAR2
IS
   V_DIRECCION      VARCHAR2(8000);
   V_DIRECCION_IMPO TBL_DIRECCIONES_X_IMPONIBLES%ROWTYPE ;
   V_DIRECCION_PERS TBL_DIRECCIONES_X_PERSONAS%ROWTYPE;

   V_TIPO_IMPONIBLE       TBL_DIRECCIONES_X_IMPONIBLES.TIPO_IMPONIBLE %TYPE ;
   V_CLAVE_IMPONIBLE      TBL_DIRECCIONES_X_IMPONIBLES.CLAVE_IMPONIBLE%TYPE ;
   V_PERSONA_ID           TBL_DIRECCIONES_X_PERSONAS.PERSONA_ID%TYPE        ;
   V_TIPO_DIRECCION       TBL_DIRECCIONES_X_IMPONIBLES.TIPO_DIRECCION%TYPE  ;
   V_MANZANA              TBL_DIRECCIONES_X_IMPONIBLES.MANZANA%TYPE         ;
   V_CASA                 TBL_DIRECCIONES_X_IMPONIBLES.CASA%TYPE              ;
   V_TORRE                TBL_DIRECCIONES_X_IMPONIBLES.TORRE%TYPE              ;
   V_PISO                 TBL_DIRECCIONES_X_IMPONIBLES.PISO%TYPE              ;
   V_DEPARTAMENTO         TBL_DIRECCIONES_X_IMPONIBLES.DEPARTAMENTO%TYPE    ;
   V_PUERTA               TBL_DIRECCIONES_X_IMPONIBLES.PUERTA%TYPE             ;
   V_CALLE                TBL_DIRECCIONES_X_IMPONIBLES.CALLE %TYPE            ;
   V_BARRIO               TBL_DIRECCIONES_X_IMPONIBLES.BARRIO%TYPE          ;
   V_LOCALIDAD            TBL_DIRECCIONES_X_IMPONIBLES.LOCALIDAD%TYPE       ;
   V_MUNICIPIO            TBL_DIRECCIONES_X_IMPONIBLES.MUNICIPIO%TYPE       ;
   V_PROVINCIA            TBL_DIRECCIONES_X_IMPONIBLES.PROVINCIA%TYPE       ;
   V_PAIS                 TBL_DIRECCIONES_X_IMPONIBLES.PAIS%TYPE            ;
   V_CODIGO_POSTAL        TBL_DIRECCIONES_X_IMPONIBLES.CODIGO_POSTAL%TYPE   ;
   V_PARAJE               TBL_DIRECCIONES_X_IMPONIBLES.PARAJE%TYPE          ;
   V_DOMICILIO            TBL_DIRECCIONES_X_IMPONIBLES.DOMICILIO%TYPE       ;

   V_PAIS_DESC            TBL_DIVISIONES_GEOGRAFICAS.DIVISION_GEOGRAFICA_ABREV%TYPE ;
   V_PROVINCIA_DESC       TBL_DIVISIONES_GEOGRAFICAS.DIVISION_GEOGRAFICA_ABREV%TYPE ;
   V_MUNICIPIO_DESC       TBL_DIVISIONES_GEOGRAFICAS.DIVISION_GEOGRAFICA_ABREV%TYPE ;
   V_LOCALIDAD_DESC       TBL_DIVISIONES_GEOGRAFICAS.DIVISION_GEOGRAFICA_ABREV%TYPE ;

   V_BARRIO_DESC          TBL_BARRIOS.BARRIO_DESCR%TYPE ;
   V_CALLE_DESC              TBL_CALLES.CALLE_DESCR%TYPE;

BEGIN

   V_DIRECCION_PERS := PA_PERSONA.GET_DIRECCION(P_CONTRIBUYENTE,P_TIPO_DIRECCION);

   -- Si el contribuyente no tiene cargada ninguna dirección entonces se recupea
   -- la dirección del imponible
   IF V_DIRECCION_PERS.TIPO_DIRECCION IS NULL THEN
       V_DIRECCION_IMPO  := PA_IMPONIBLE.GET_DIRECCION(P_TIPO_IMPONIBLE,P_CLAVE_IMPONIBLE,P_TIPO_DIRECCION);
   END IF;

   V_TIPO_DIRECCION :=     NVL(V_DIRECCION_PERS.TIPO_DIRECCION,V_DIRECCION_IMPO.TIPO_DIRECCION);
   V_MANZANA        :=     NVL(V_DIRECCION_PERS.MANZANA,V_DIRECCION_IMPO.MANZANA              );
   V_CASA           :=     NVL(V_DIRECCION_PERS.CASA,V_DIRECCION_IMPO.CASA                    );
   V_TORRE          :=     NVL(V_DIRECCION_PERS.TORRE,V_DIRECCION_IMPO.TORRE                  );
   V_PISO           :=     NVL(V_DIRECCION_PERS.PISO,V_DIRECCION_IMPO.PISO                    );
   V_DEPARTAMENTO   :=     NVL(V_DIRECCION_PERS.DEPARTAMENTO,V_DIRECCION_IMPO.DEPARTAMENTO    );
   V_PUERTA         :=     NVL(V_DIRECCION_PERS.PUERTA,V_DIRECCION_IMPO.PUERTA                );
   V_CALLE          :=     NVL(V_DIRECCION_PERS.CALLE,V_DIRECCION_IMPO.CALLE                  );
   V_BARRIO         :=     NVL(V_DIRECCION_PERS.BARRIO,V_DIRECCION_IMPO.BARRIO                );
   V_LOCALIDAD      :=     NVL(V_DIRECCION_PERS.LOCALIDAD,V_DIRECCION_IMPO.LOCALIDAD          );
   V_MUNICIPIO      :=     NVL(V_DIRECCION_PERS.MUNICIPIO,V_DIRECCION_IMPO.MUNICIPIO          );
   V_PROVINCIA      :=     NVL(V_DIRECCION_PERS.PROVINCIA,V_DIRECCION_IMPO.PROVINCIA          );
   V_PAIS           :=     NVL(V_DIRECCION_PERS.PAIS,V_DIRECCION_IMPO.PAIS                    );
   V_CODIGO_POSTAL  :=     NVL(V_DIRECCION_PERS.CODIGO_POSTAL,V_DIRECCION_IMPO.CODIGO_POSTAL  );
   V_PARAJE         :=     NVL(V_DIRECCION_PERS.PARAJE,V_DIRECCION_IMPO.PARAJE                );
   V_DOMICILIO      :=     NVL(V_DIRECCION_PERS.DOMICILIO,V_DIRECCION_IMPO.DOMICILIO          );

   IF V_TIPO_DIRECCION IS NOT NULL  THEN
      V_PAIS_DESC      :=  PA_DIV_GEOGRAFICA.GET_DIV_GEOGRAFICA_ABREV(V_PAIS);
      V_PROVINCIA_DESC := PA_DIV_GEOGRAFICA.GET_DIV_GEOGRAFICA_ABREV(V_PROVINCIA);
      V_MUNICIPIO_DESC := PA_DIV_GEOGRAFICA.GET_DIV_GEOGRAFICA_ABREV(V_MUNICIPIO);
      V_LOCALIDAD_DESC := PA_DIV_GEOGRAFICA.GET_DIV_GEOGRAFICA_ABREV(V_LOCALIDAD);

      V_BARRIO_DESC    := PA_DIV_GEOGRAFICA.GET_BARRIO_DESCR(V_BARRIO);
      V_CALLE_DESC     := PA_DIV_GEOGRAFICA.GET_CALLE_DESCR(V_CALLE);
   END IF;

   IF v_calle_desc IS NOT NULL THEN

      v_direccion := v_calle_desc;

      v_direccion := v_direccion||' Puerta: '||v_puerta;

      IF v_piso IS NOT NULL THEN
        v_direccion := v_direccion||' Piso: '||v_piso;
      END IF;
        
      IF v_departamento IS NOT NULL THEN
        v_direccion := v_direccion||' Depto: '||v_departamento;
      END IF;
      
      v_direccion := v_direccion||' - CP: '||v_codigo_postal;

      IF v_barrio IS NOT NULL THEN
         v_direccion := v_direccion||' - Barrio: '||rtrim(ltrim(v_barrio_desc));
      END IF;

      IF v_localidad IS NOT NULL THEN
         v_direccion := v_direccion||' - Localidad: '||rtrim(ltrim(v_localidad_desc));
      END IF;

      IF v_municipio_desc IS NOT NULL THEN
         v_direccion := v_direccion||' - Municipio: '||rtrim(ltrim(v_municipio_desc));
      END IF;

      IF v_provincia_desc IS NOT NULL THEN
         v_direccion := v_direccion||' - Provincia: '||rtrim(ltrim(v_provincia_desc));
      END IF;

   ELSE
      -- se agrega para que complete en caso de que existan los datos de barrio localidad y municipio -- RQUINTEROS 08/06/2011

      v_direccion := v_domicilio;
      
      IF v_puerta IS NOT NULL THEN 
        v_direccion := v_direccion||' Puerta: '||v_puerta;
      END IF;
      
      IF v_codigo_postal IS NOT NULL THEN
        v_direccion := v_direccion||' - CP: '||v_codigo_postal;
      END IF;
      -- agregado para boletas
      IF v_manzana IS NOT NULL THEN
         v_direccion := v_direccion||' - Manzana: '||rtrim(ltrim(v_manzana));
      END IF;
      IF v_casa IS NOT NULL THEN
         v_direccion := v_direccion||' - Casa: '||rtrim(ltrim(v_casa));
      END IF;
      -- fin agregados
      
      IF v_barrio IS NOT NULL THEN
         v_direccion := v_direccion||' - Barrio: '||rtrim(ltrim(v_barrio_desc));
      END IF;

      IF v_localidad IS NOT NULL THEN
         v_direccion := v_direccion||' - Localidad: '||rtrim(ltrim(v_localidad_desc));
      END IF;

      IF v_municipio_desc IS NOT NULL THEN
         v_direccion := v_direccion||' - Municipio: '||rtrim(ltrim(v_municipio_desc));
      END IF;

      IF v_provincia_desc IS NOT NULL THEN
         v_direccion := v_direccion||' - Provincia: '||rtrim(ltrim(v_provincia_desc));
      END IF;

   END IF;

   RETURN v_direccion;

EXCEPTION
   WHEN OTHERS THEN
      RETURN NULL;
END;

FUNCTION INSERTA_BOLETA_COD_IMPRESION (
      P_IMPUESTO         IN   VARCHAR2,
      P_CONCEPTO         IN   VARCHAR2,
      P_ANIO             IN   VARCHAR2,
      P_CUOTAS_MULTIPLES IN   VARCHAR2,
      P_TIPO_IMPONIBLE   IN   VARCHAR,
      P_IMPONIBLE        IN   VARCHAR,
      P_PLAN_FACILIDAD   IN   NUMBER,
      P_FECHA_EMISION    IN   DATE
   )
      RETURN VARCHAR2
   AS
      --RETURN SYS_REFCURSOR AS
      DOMICILIO                        OBJ_DOMICILIO
         := OBJ_DOMICILIO (NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL
                          );
--CURSOR
      RC_DATOS                         SYS_REFCURSOR;
      RC                               SYS_REFCURSOR;
      --V_CLAVE_IMPONIBLE                VARCHAR2 (20) := PA_SIAC_BOLETAS_MULTIPLES.CLAVE_IMPONIBLE (P_IMPONIBLE, P_TIPO_IMPONIBLE);
      
     
      CURSOR IMPRESION_MASIVA
      IS
         SELECT   ROWID
             FROM TBL_VENCIMIENTOS
            WHERE NUMERO_CUOTA IN (            
                                    SELECT   REGEXP_SUBSTR (CADENA, '[^,]+', 1, NUMEROFILA) SPLIT
                                        FROM (SELECT 1 ID, P_CUOTAS_MULTIPLES CADENA
                                                FROM DUAL)
                                             CROSS JOIN
                                             (SELECT     ROWNUM NUMEROFILA
                                                    FROM (SELECT   MAX (LENGTH (REGEXP_REPLACE (CADENA, '[^,]+')))
                                                                 + 1 MX
                                                            FROM (SELECT 1 ID, P_CUOTAS_MULTIPLES CADENA
                                                                    FROM DUAL))
                                              CONNECT BY LEVEL <= MX)
                                       WHERE REGEXP_SUBSTR (CADENA, '[^,]+', 1, NUMEROFILA) IS NOT NULL
                                    )
              AND ANIO = P_ANIO
              AND CONCEPTO_OBLIGACION = P_CONCEPTO
              AND IMPUESTO = P_IMPUESTO
              AND PLAN_FACILIDAD = P_PLAN_FACILIDAD--AQUI SE CAMBIO
         ORDER BY PA_BOLETA.F_ORDEN_BOLETA
                         (IMPUESTO,
                          CONCEPTO_OBLIGACION,
                          DOMICILIO.GET_DOMICILIO_IMPRESION (TIPO_IMPONIBLE,
                                                             CLAVE_IMPONIBLE,
                                                             CONTRIBUYENTE
                                                            ),
                          TIPO_IMPONIBLE,
                          CLAVE_IMPONIBLE
                         ),--SE MODIFICO
                  ANIO,
                  NUMERO_CUOTA;

      P_BOLETA                         PA_CUENTAS_CORRIENTES.T_VENCIMIENTOS;
      P_MENSAJE_ERROR                  VARCHAR2 (100);
      V_CONTRIBUYENTE                  PA_BOLETA.RF_DATOS_BOLETA;
      V_LOCALIDAD                      VARCHAR2 (10)                   := NULL;
      V_TIPO_IMPONIBLE                 VARCHAR2 (10);
      V_CODIGO_UNIFICADO               VARCHAR2 (2)                    := NULL;
      V_IMPUESTO                       VARCHAR2 (4);
      V_ANIO                           NUMBER;
      V_CUOTA                          NUMBER;
      V_NUMERO_RECTIFICATIVA           NUMBER;
      V_CODIGO_IMPRESION               VARCHAR (4000);
      V_CONTADOR                       NUMBER                             := 0;
      V_NUMERO_BOLETA                  NUMBER;
      V_ERR                            BOOLEAN;
      V_ERROR_ACT_BOLETA               BOOLEAN;
      V_MENSAJE_ERROR                  VARCHAR2 (100);
      E_EXISTEN_MULTIPLES_IMPONIBLES   EXCEPTION;
      V_CANT_IMPONIBLES                NUMBER;
      V_BARRIO                         VARCHAR2 (20);
      V_BLOQUE                         VARCHAR2 (20);
      V_MANZANA                        VARCHAR2 (20);
      V_IMPORTE                        NUMBER;
      V_ERROR_INS                      BOOLEAN;
      V_CODIGO_ORGANISMO               TBL_PARAMETROS.VALOR%TYPE;
      V_TIPO_IMPONIBLE_OLD             TBL_VENCIMIENTOS.TIPO_IMPONIBLE%TYPE;
      V_CLAVE_IMPONIBLE_OLD            TBL_VENCIMIENTOS.CLAVE_IMPONIBLE%TYPE;
      V_ANIO_OLD                       TBL_VENCIMIENTOS.ANIO%TYPE;
      V_NUMERO_CUOTA_OLD               TBL_VENCIMIENTOS.NUMERO_CUOTA%TYPE;
      V_PERTENECE_MUESTREO             TBL_IMPONIBLES.PERTENECE_MUESTREO%TYPE;
      V_ZONA                           VARCHAR2 (100);
      V_COL_ZONA                       TBL_PARAMETROS.VALOR%TYPE;
      V_TIPO_GRUPO_INF_INM             TBL_PARAMETROS.VALOR%TYPE;
      V_COL_REPARTIDOR                 TBL_PARAMETROS.VALOR%TYPE;
      V_SELECT                         VARCHAR2 (2000);
      V_REPARTIDOR                     TBL_IMPONIBLES.COL01%TYPE;
      V_VIGENCIA                       DATE;
      V_CLAUSULA_FROM                  TBL_FILTROS_DINAMICO_BOLETA.CLAUSULA_FROM%TYPE;
      V_CLAUSULA_WHERE                 TBL_FILTROS_DINAMICO_BOLETA.CLAUSULA_WHERE%TYPE;
      V_CLAUSULA_TIPO_IMPONIBLE        TBL_FILTROS_DINAMICO_BOLETA.TIPO_IMPONIBLE%TYPE;
      V_CLAUSULA_CLAVE_IMPONIBLE       TBL_FILTROS_DINAMICO_BOLETA.CLAVE_IMPONIBLE%TYPE;
      V_RESULTADO_CLAUSULA             VARCHAR2 (1);
      V_RESULTADO                      VARCHAR2 (30);
      V_CODIGO_IMPRESION_NUM           NUMBER;
      V_FLAG                           VARCHAR2 (1);
      V_IMP_BOLETA_INMOB               VARCHAR2 (150);
      V_IMP_BOLETA_AUTO                VARCHAR2 (150);
      V_TIPO_IMPONIBLE_RI              TBL_VENCIMIENTOS.TIPO_IMPONIBLE%TYPE;
      V_CLAVE_IMPONIBLE_RI             TBL_VENCIMIENTOS.CLAVE_IMPONIBLE%TYPE;
      V_IMPUESTO_RI                    TBL_VENCIMIENTOS.IMPUESTO%TYPE;
      V_CONCEPTO_OBLIGACION_RI         TBL_VENCIMIENTOS.CONCEPTO_OBLIGACION%TYPE;
      V_NRO_OBLIGACION_IMPUESTO_RI     TBL_VENCIMIENTOS.NUMERO_OBLIGACION_IMPUESTO%TYPE;
      V_ANIO_RI                        TBL_VENCIMIENTOS.ANIO%TYPE;
      V_NUMERO_CUOTA_RI                TBL_VENCIMIENTOS.NUMERO_CUOTA%TYPE;
      V_NUMERO_RECTIFICATIVA_RI        TBL_VENCIMIENTOS.NUMERO_RECTIFICATIVA%TYPE;
      V_CONTRIBUYENTE_RI               TBL_VENCIMIENTOS.CONTRIBUYENTE%TYPE;
      V_JUICIO_ID_RI                   TBL_VENCIMIENTOS.JUICIO_ID%TYPE;
      V_PLAN_FACILIDAD_INCLUIDO_RI     TBL_VENCIMIENTOS.PLAN_FACILIDAD_INCLUIDO%TYPE;
      V_TIPO_VENCIMIENTO_RI            TBL_VENCIMIENTOS.TIPO_VENCIMIENTO%TYPE;
      V_CUOTA_CONTADO_RI               TBL_VENCIMIENTOS.CUOTA_CONTADO%TYPE;
      V_FECHA_PRIMER_VENCIMIENTO_RI    TBL_VENCIMIENTOS.FECHA_PRIMER_VENCIMIENTO%TYPE;
      V_MUESTRA_DEUDA                  VARCHAR2 (1);
      V_EXISTE                         NUMBER;
      V_DOMICILIO                      TBL_CONTRIBUYENTE_X_BOLETA%ROWTYPE;
      V_INFO_ADICIONAL_INMOB           VARCHAR2 (2000);
      V_PARAMETRO_BUSQUEDA             VARCHAR2 (1);
      V_PROXIMA_FECHA_VTO              DATE;
      V_PROXIMA_CUOTA                  NUMBER;
      V_DETALLE_IMPONIBLE              VARCHAR2 (300);
      V_TIPO_CONTRIBUYENTE             TBL_CONTRIBUYENTES.TIPO_CONTRIBUYENTE%TYPE;
      V_BLOQUEADO                      VARCHAR2 (20);
   BEGIN
      V_PARAMETRO_BUSQUEDA :=
                    NVL (FN_RECUPERAR_PARAMETRO ('PROXIMO_VENCIMIENTO'), 'A');
      V_CODIGO_ORGANISMO := FN_RECUPERAR_PARAMETRO ('CODIGO ORGANISMO');
      V_COL_ZONA := FN_RECUPERAR_PARAMETRO ('COL_ZONA');
      V_TIPO_GRUPO_INF_INM := FN_RECUPERAR_PARAMETRO ('TIPO_GRUPO_INF_INM');
      V_COL_REPARTIDOR := FN_RECUPERAR_PARAMETRO ('COL_REPARTIDOR');
      V_CODIGO_IMPRESION_NUM := PA_BOLETA.GET_CODIGO_IMPRESION;
      V_CODIGO_IMPRESION := TO_CHAR (V_CODIGO_IMPRESION_NUM);
      V_MUESTRA_DEUDA := 'S';

      BEGIN
         /*CASE P_TIPO_IMPONIBLE
            WHEN '0001'
            THEN
               SELECT CLAVE
                 INTO V_CLAVE_IMPONIBLE
                 FROM TBL_IMPONIBLES I
                WHERE I.COL07 = P_IMPONIBLE;
            --V_CODIGO_UNIFICADO := 'N';
         WHEN '0002'
            THEN
               SELECT CLAVE
                 INTO V_CLAVE_IMPONIBLE
                 FROM TBL_IMPONIBLES I
                WHERE I.COL02 = P_IMPONIBLE;
            -- V_CODIGO_UNIFICADO := 'S';
         WHEN '0003'
            THEN
               SELECT CLAVE
                 INTO V_CLAVE_IMPONIBLE
                 FROM TBL_IMPONIBLES I
                WHERE I.COL01 = P_IMPONIBLE;

               IF V_CLAVE_IMPONIBLE = 'NULL'
               THEN
                  SELECT CLAVE
                    INTO V_CLAVE_IMPONIBLE
                    FROM TBL_IMPONIBLES I
                   WHERE I.COL02 = P_IMPONIBLE;
               END IF;
            --V_CODIGO_UNIFICADO := 'N';
         ELSE*/
               OPEN RC FOR
                  SELECT 0 ID, SYSDATE TERM_DATE
                    FROM DUAL;
         --V_CODIGO_UNIFICADO := 'N';
         --END CASE;
      EXCEPTION
         WHEN OTHERS
         THEN
            V_CLAUSULA_FROM := NULL;
            V_CLAUSULA_WHERE := NULL;
            V_CLAUSULA_TIPO_IMPONIBLE := NULL;
            V_CLAUSULA_CLAVE_IMPONIBLE := NULL;
      END;

      V_FLAG := 'N';
      FOR A IN IMPRESION_MASIVA
      LOOP
         BEGIN
            BEGIN--SE MODIFICO
               SELECT C.TIPO_IMPONIBLE, C.CLAVE_IMPONIBLE,
                      C.IMPUESTO, C.CONCEPTO_OBLIGACION,
                      C.NUMERO_OBLIGACION_IMPUESTO, C.ANIO,
                      C.NUMERO_CUOTA, C.NUMERO_RECTIFICATIVA,
                      C.CONTRIBUYENTE, C.JUICIO_ID,
                      C.PLAN_FACILIDAD_INCLUIDO, C.TIPO_VENCIMIENTO,
                      --B.COL10, B.COL21,
                      --B.PERTENECE_MUESTREO, 
                      C.CUOTA_CONTADO,
                      C.FECHA_PRIMER_VENCIMIENTO
                 INTO V_TIPO_IMPONIBLE_RI, V_CLAVE_IMPONIBLE_RI,
                      V_IMPUESTO_RI, V_CONCEPTO_OBLIGACION_RI,
                      V_NRO_OBLIGACION_IMPUESTO_RI, V_ANIO_RI,
                      V_NUMERO_CUOTA_RI, V_NUMERO_RECTIFICATIVA_RI,
                      V_CONTRIBUYENTE_RI, V_JUICIO_ID_RI,
                      V_PLAN_FACILIDAD_INCLUIDO_RI, V_TIPO_VENCIMIENTO_RI,
                      --V_IMP_BOLETA_INMOB, V_IMP_BOLETA_AUTO,
                      --V_PERTENECE_MUESTREO, 
                      V_CUOTA_CONTADO_RI,
                      V_FECHA_PRIMER_VENCIMIENTO_RI
                 FROM TBL_VENCIMIENTOS C, TBL_PLANES_FACILIDADES PF
                WHERE C.PLAN_FACILIDAD = PF.PLAN_FACILIDAD
                  AND C.IMPUESTO = PF.IMPUESTO
                  AND C.CONCEPTO_OBLIGACION = PF.CONCEPTO_OBLIGACION
                  AND PF.FECHA_BAJA IS NULL
                  AND PF.FECHA_REVERSION IS NULL
                  AND PF.PLAN_FACILIDAD = P_PLAN_FACILIDAD
                  AND C.ROWID = A.ROWID;
            EXCEPTION
               WHEN OTHERS
               THEN
                  NULL;
            END;

            IF NVL (FN_RECUPERAR_PARAMETRO ('EDESA_MASIVA'), 'N') = 'S'
            THEN
               IF PA_EDESA.F_EXISTE_BLOQUEO (V_IMPUESTO_RI,
                                             V_CONCEPTO_OBLIGACION_RI,
                                             V_NRO_OBLIGACION_IMPUESTO_RI,
                                             V_NUMERO_RECTIFICATIVA_RI,
                                             V_NUMERO_CUOTA_RI
                                            )
               THEN
                  RAISE E_EXISTEN_MULTIPLES_IMPONIBLES;
               END IF;
            END IF;

            IF    V_JUICIO_ID_RI IS NOT NULL
               OR V_PLAN_FACILIDAD_INCLUIDO_RI IS NOT NULL
               OR V_TIPO_VENCIMIENTO_RI = 'O'
            THEN
               RAISE E_EXISTEN_MULTIPLES_IMPONIBLES;
            END IF;

            IF P_IMPUESTO IS NOT NULL AND P_IMPUESTO != V_IMPUESTO_RI
            THEN
               RAISE E_EXISTEN_MULTIPLES_IMPONIBLES;
            END IF;

            IF V_CODIGO_UNIFICADO IS NOT NULL
            THEN
               BEGIN
                  SELECT 1
                    INTO V_EXISTE
                    FROM TBL_IMPUESTOS_ASOC_UNIF
                   WHERE IMPUESTO = V_IMPUESTO_RI
                     AND CODIGO_UNIFICADO = V_CODIGO_UNIFICADO;
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     RAISE E_EXISTEN_MULTIPLES_IMPONIBLES;
                  WHEN OTHERS
                  THEN
                     NULL;
               END;
            END IF;

            PA_DOMICILIO.GET_DOMICILIO_IMPRESION_MASIVA (V_TIPO_IMPONIBLE_RI,
                                                         V_CLAVE_IMPONIBLE_RI,
                                                         V_CONTRIBUYENTE_RI,
                                                         V_DOMICILIO
                                                        );
            V_LOCALIDAD := V_DOMICILIO.LOCALIDAD;
            V_BARRIO := V_DOMICILIO.BARRIO;
            V_BLOQUE := V_DOMICILIO.SECTOR_BLOQUE;
            V_MANZANA := V_DOMICILIO.MANZANA;

            BEGIN
               /*IF v_tipo_imponible_ri = '0002'
               THEN
                  IF NVL (v_imp_boleta_inmob, 'S') = 'N'
                  THEN
                     RAISE e_existen_multiples_imponibles;
                  END IF;
               END IF;

               IF v_tipo_imponible_ri = '0003'
               THEN
                  IF NVL (v_imp_boleta_auto, 'S') = 'N'
                  THEN
                     RAISE e_existen_multiples_imponibles;
                  END IF;
               END IF;*/

               V_BLOQUEADO :=
                  PA_IMPONIBLE.GET_IMPONIBLE_BLOQUEADO (V_TIPO_IMPONIBLE_RI,
                                                        V_CLAVE_IMPONIBLE_RI
                                                       );

               IF NVL (V_BLOQUEADO, 'N') = 'S'
               THEN
                  RAISE E_EXISTEN_MULTIPLES_IMPONIBLES;
               END IF;

               
               IF V_LOCALIDAD IS NULL
               THEN
                  V_LOCALIDAD := '@@@';
               END IF;

               IF V_BARRIO IS NULL
               THEN
                  V_BARRIO := '@@@';
               END IF;

               IF V_BLOQUE IS NULL
               THEN
                  V_BLOQUE := '@@@';
               END IF;

               IF V_MANZANA IS NULL
               THEN
                  V_MANZANA := '@@@';
               END IF;

               IF     V_LOCALIDAD = NVL (NULL, V_LOCALIDAD)
                  AND V_BARRIO = NVL (NULL, V_BARRIO)
                  AND V_BLOQUE = NVL (NULL, V_BLOQUE)
                  AND V_MANZANA = NVL (NULL, V_MANZANA)
               THEN
                  V_IMPORTE :=
                     PA_ACTUALIZACION.ACTUALIZAR_VENCIMIENTO
                                               (V_IMPUESTO_RI,
                                                V_CONCEPTO_OBLIGACION_RI,
                                                V_NRO_OBLIGACION_IMPUESTO_RI,
                                                V_NUMERO_RECTIFICATIVA_RI,
                                                V_NUMERO_CUOTA_RI,
                                                SYSDATE
                                               );
                    DBMS_OUTPUT.PUT_LINE (V_IMPORTE);
                  IF V_IMPORTE <= 0
                  THEN
                     RAISE E_EXISTEN_MULTIPLES_IMPONIBLES;
                  END IF;

                  IF V_FLAG = 'N'
                      THEN
                      IF V_CODIGO_UNIFICADO IS NOT NULL
                      THEN
                         IF     V_TIPO_IMPONIBLE_OLD = V_TIPO_IMPONIBLE_RI
                            AND V_CLAVE_IMPONIBLE_OLD = V_CLAVE_IMPONIBLE_RI
                            AND V_ANIO_OLD = V_ANIO_RI
                            AND V_NUMERO_CUOTA_OLD = V_NUMERO_CUOTA_RI
                         THEN
                            V_ERR := TRUE;
                         ELSE
                            V_NUMERO_BOLETA := PA_BOLETA.GET_NUMERO_BOLETA;
                            V_ERR :=
                               PA_BOLETA.INSERTA_BOLETA_MASIVA
                                                          (V_NUMERO_BOLETA,
                                                           P_FECHA_EMISION,
                                                           V_CODIGO_IMPRESION_NUM
                                                          );
                            V_FLAG := 'S';
                         END IF;
                      ELSE
                            V_NUMERO_BOLETA := PA_BOLETA.GET_NUMERO_BOLETA;
                            V_ERR := PA_SIAC_BOLETAS_MULTIPLES.GUARDAR_BOLETA(V_NUMERO_BOLETA, P_FECHA_EMISION, V_CODIGO_IMPRESION_NUM);
                            V_FLAG := 'S';
                      END IF;
                  END IF;
                  V_TIPO_IMPONIBLE_OLD := V_TIPO_IMPONIBLE_RI;
                  V_CLAVE_IMPONIBLE_OLD := V_CLAVE_IMPONIBLE_RI;
                  V_ANIO_OLD := V_ANIO_RI;
                  V_NUMERO_CUOTA_OLD := V_NUMERO_CUOTA_RI;

                  IF V_ERR
                  THEN
                     V_ERROR_INS := FALSE;
                     INSERTA_VENCIMIENTOS_X_BOLETA2
                                               (V_IMPUESTO_RI,
                                                V_CONCEPTO_OBLIGACION_RI,
                                                V_NRO_OBLIGACION_IMPUESTO_RI,
                                                V_NUMERO_RECTIFICATIVA_RI,
                                                V_NUMERO_CUOTA_RI,
                                                V_NUMERO_BOLETA
                                               );

                     IF V_ERROR_INS
                     THEN
                        --RETURN NULL;
                        RETURN    'INSERTA_VENCIMIENTOS_X_BOLETA2: '
                               || TO_CHAR (SQLCODE)
                               || '584';
                     END IF;
                     
                  ELSE
                        RETURN    'INSERTA_BOLETA_MASIVA: '
                               || TO_CHAR (SQLCODE);
                  END IF;

                  IF V_ERR
                  THEN
                     V_ERROR_ACT_BOLETA := FALSE;
                     ACT_NUMERO_BOLETA (V_IMPUESTO_RI,
                                        V_CONCEPTO_OBLIGACION_RI,
                                        V_NRO_OBLIGACION_IMPUESTO_RI,
                                        V_NUMERO_RECTIFICATIVA_RI,
                                        V_NUMERO_CUOTA_RI,
                                        V_NUMERO_BOLETA,
                                        V_ERROR_ACT_BOLETA,
                                        V_MENSAJE_ERROR
                                       );

                     IF V_ERROR_ACT_BOLETA
                     THEN
                        --RETURN NULL;
                        RETURN    'ACT_NUMERO_BOLETA: '
                               || V_MENSAJE_ERROR
                               || '613';
                     END IF;
                  END IF;

                  V_INFO_ADICIONAL_INMOB := NULL;

                  BEGIN
                     V_INFO_ADICIONAL_INMOB :=
                        SUBSTR
                           (PA_SIAC_INTERFACE.F_INFO_ADICIONAL_INMOB
                                                        (V_CLAVE_IMPONIBLE_RI,
                                                         V_TIPO_IMPONIBLE_RI,
                                                         'S',
                                                         V_DOMICILIO
                                                        ),
                            1,
                            2000
                           );
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        RETURN    'PA_SIAC_INTERFACE.F_INFO_ADICIONAL_INMOB: '
                               || TO_CHAR (SQLCODE)
                               || '627';
                  END;

                  V_TIPO_CONTRIBUYENTE :=
                     PA_CONTRIBUYENTE.GET_TIPO_CONTRIBUYENTE
                                                          (V_CONTRIBUYENTE_RI,
                                                           SYSDATE
                                                          );
                  V_PROXIMA_FECHA_VTO := NULL;
                  V_PROXIMA_CUOTA := NULL;

                  BEGIN
                     PA_SIAC_BOLETAS_MULTIPLES.P_PROXIMO_VTO_MASIVA (V_IMPUESTO_RI,
                                           V_CONCEPTO_OBLIGACION_RI,
                                           V_ANIO_RI,
                                           V_NRO_OBLIGACION_IMPUESTO_RI,
                                           V_NUMERO_RECTIFICATIVA_RI,
                                           V_NUMERO_CUOTA_RI,
                                           V_CUOTA_CONTADO_RI,
                                           V_FECHA_PRIMER_VENCIMIENTO_RI,
                                           V_CLAVE_IMPONIBLE_RI,
                                           V_TIPO_IMPONIBLE_RI,
                                           V_PARAMETRO_BUSQUEDA,
                                           V_CONTRIBUYENTE_RI,
                                           V_TIPO_CONTRIBUYENTE,
                                           V_PROXIMA_FECHA_VTO,
                                           V_PROXIMA_CUOTA
                                          );
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        V_PROXIMA_FECHA_VTO := NULL;
                        V_PROXIMA_CUOTA := NULL;
                        RETURN    '1545 P_PROXIMO_VTO_MASIVA: '
                               || TO_CHAR (SQLCODE);
                  END;

                  V_DETALLE_IMPONIBLE := NULL;

                  BEGIN
                     V_DETALLE_IMPONIBLE :=
                        F_DETALLE_IMPONIBLE (V_CONTRIBUYENTE_RI,
                                             V_TIPO_CONTRIBUYENTE,
                                             V_TIPO_IMPONIBLE_RI,
                                             V_CLAVE_IMPONIBLE_RI,
                                             V_IMPUESTO_RI,
                                             V_CONCEPTO_OBLIGACION_RI,
                                             V_ANIO_RI,
                                             V_NUMERO_CUOTA_RI,
                                             V_FECHA_PRIMER_VENCIMIENTO_RI,
                                             'S',
                                             'S',
                                             NULL
                                            );
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        V_DETALLE_IMPONIBLE := NULL;
                        RETURN TO_CHAR (SQLCODE) || '685';
                  END;

                  BEGIN
                     PA_SIAC_INTERFACE.GENERA_CONTRIBUYENTE_BOLETA
                                                     (V_CODIGO_IMPRESION_NUM,
                                                      SYSDATE,
                                                      V_CONTRIBUYENTE_RI,
                                                      V_TIPO_IMPONIBLE_RI,
                                                      V_CLAVE_IMPONIBLE_RI,
                                                      NULL,
                                                      NULL,
                                                      A.ROWID,
                                                      V_DOMICILIO,
                                                      V_INFO_ADICIONAL_INMOB,
                                                      V_PROXIMA_FECHA_VTO,
                                                      V_PROXIMA_CUOTA,
                                                      V_DETALLE_IMPONIBLE,
                                                      'S'
                                                     );
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        RETURN TO_CHAR (SQLCODE) || '707';
                  END;

                  BEGIN
                     PA_SIAC_INTERFACE.GENERA_CONCEPTOS_BOLETA
                                               (V_IMPUESTO_RI,
                                                V_CONCEPTO_OBLIGACION_RI,
                                                V_NRO_OBLIGACION_IMPUESTO_RI,
                                                0,
                                                V_NUMERO_CUOTA_RI,
                                                V_CODIGO_IMPRESION_NUM,
                                                V_NUMERO_BOLETA,
                                                TRUNC (SYSDATE),
                                                'S'
                                               );
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        RETURN TO_CHAR (SQLCODE || '724');
                  END;

                  IF UPPER (NVL (V_MUESTRA_DEUDA, 'N')) = 'S'
                  THEN
                     BEGIN
                        PA_SIAC_INTERFACE.GENERA_CUOTAS_ADEUDADAS_BOLETA
                                               (V_TIPO_IMPONIBLE_RI,
                                                V_CLAVE_IMPONIBLE_RI,
                                                V_CONTRIBUYENTE_RI,
                                                V_IMPUESTO_RI,
                                                V_CONCEPTO_OBLIGACION_RI,
                                                V_NRO_OBLIGACION_IMPUESTO_RI,
                                                0,
                                                V_NUMERO_CUOTA_RI,
                                                V_NUMERO_BOLETA,
                                                V_CODIGO_UNIFICADO,
                                                'S'
                                               );
                     EXCEPTION
                        WHEN OTHERS
                        THEN
                           
                           RETURN '1 ERROR: ' || TO_CHAR (SQLCODE);
                     END;
                  END IF;
               END IF;
            EXCEPTION
               WHEN OTHERS
               THEN
               
                  RETURN '2 ERROR: ' ||TO_CHAR (SQLCODE);
            END;
         EXCEPTION
            WHEN E_EXISTEN_MULTIPLES_IMPONIBLES
            THEN
               RETURN '3 ERROR: ' ||TO_CHAR (SQLCODE);
         END;
      END LOOP;

      RETURN V_CODIGO_IMPRESION_NUM;
   
   EXCEPTION
      WHEN OTHERS
      THEN
         RETURN NULL;
   END;
   

FUNCTION GENERA_CODIGO_BARRA (
      p_codigo_impresion    IN   NUMBER,
      p_fecha_vencimiento   IN   DATE,
      p_contribuyente       IN   NUMBER,  
      p_tipo_imponible      IN   VARCHAR2,
      p_clave_imponible     IN   VARCHAR2,
      p_plan_facilidad      IN   NUMBER,
      V_ANIO                IN   NUMBER,
      V_CONCEPTO            IN   VARCHAR2,
      V_IMPUESTO            IN   VARCHAR2
   )
      RETURN sys_refcursor
   AS
      --RETURN VARCHAR2 AS
--Cursos
      rf_datos               sys_refcursor;
--Variables
      v_razon_social         VARCHAR2 (100);
      v_cuit                 VARCHAR2 (13);
      v_cuit_numerico        NUMBER;
      v_tipo_documento       VARCHAR2 (4);
      v_numero_documento     VARCHAR2 (10);
      v_contribuyente        NUMBER;
      v_tipo_contribuyente   tbl_contribuyentes.tipo_contribuyente%TYPE;
      v_multa_numero         NUMBER;
      v_aux                  VARCHAR2 (1);
      v_tipo_imponible       tbl_imponibles.tipo_imponible%TYPE;
      v_clave_imponible      tbl_imponibles.clave%TYPE;
      /*v2_tipo_imponible       tbl_imponibles.tipo_imponible%TYPE;
      v2_clave_imponible      tbl_imponibles.clave%TYPE;
      v2_contribuyente        NUMBER;--se agrego 3 lineas*/
      v_plan_facilidad       tbl_planes_facilidades.plan_facilidad%TYPE;
      v_ident_larga          tbl_imponibles.identificacion_larga%TYPE;
      v_muestra_condomino    tbl_parametros.valor%TYPE;
      v_concatena_rs         VARCHAR2 (30);
      v_condomino_desc       VARCHAR2 (30);
      v_dias                 NUMBER;
      v_fecha                DATE                                  := SYSDATE;
      v_codigo_barra         VARCHAR (100);
      v_tipo_codigo_barra    VARCHAR (10)                             := 'BS';
      v_numero_boleta        tbl_boletas.numero_boleta%TYPE;
      v_domicilio            VARCHAR2 (5000);
      DP_TIPO_DIRECCION      TBL_DIRECCIONES_X_PERSONAS.TIPO_DIRECCION%TYPE;   
   BEGIN--SE AGREGO EL TIPO DE DIRECCION DEL CONTRIBUYENTE
        begin
        SELECT DP.TIPO_DIRECCION
         INTO DP_TIPO_DIRECCION
         FROM TBL_DIRECCIONES_X_PERSONAS DP
        WHERE DP.PERSONA_ID = P_CONTRIBUYENTE;
         EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
        end;
      BEGIN
         SELECT multa_numero
           INTO v_multa_numero
           FROM tbl_vencimientos v, tbl_boletas b
          WHERE multa_numero IS NOT NULL
            AND b.numero_boleta = v.numero_boleta
            AND b.codigo_impresion = p_codigo_impresion
            AND ROWNUM = 1;
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      IF p_clave_imponible IS NULL
      THEN
         BEGIN
            SELECT co.tipo_contribuyente
              INTO v_tipo_contribuyente
              FROM tbl_contribuyentes co
             WHERE co.contribuyente = p_contribuyente;

            SELECT tipo_imponible, clave_imponible, plan_facilidad
              INTO v_tipo_imponible, v_clave_imponible, v_plan_facilidad
              FROM tbl_vencimientos
             WHERE numero_boleta IN (
                      SELECT numero_boleta
                        FROM tbl_boletas
                       WHERE codigo_impresion = p_codigo_impresion
                         AND ROWNUM = 1)
               AND ROWNUM = 1;
         EXCEPTION
            WHEN OTHERS
            THEN
               NULL;
         END;

         IF v_plan_facilidad IS NOT NULL
         THEN
            BEGIN
               SELECT clave_imponible, tipo_imponible
                 INTO v_clave_imponible, v_tipo_imponible
                 FROM tbl_imponibles_x_planes
                WHERE plan_facilidad = v_plan_facilidad;
            EXCEPTION
               WHEN OTHERS
               THEN
                  v_clave_imponible := NULL;
                  v_tipo_imponible := NULL;
            END;
         END IF;
      ELSE
         v_clave_imponible := p_clave_imponible;
         v_tipo_imponible := p_tipo_imponible;
      END IF;

      v_domicilio :=
         pa_domicilio.get_domicilio_impresion (v_tipo_imponible,
                                               v_clave_imponible,
                                               p_contribuyente
                                              );
      pa_boleta_mi.set_domicilio (v_domicilio);
      v_ident_larga :=
         pa_imponible.get_identificacion_larga (v_tipo_imponible,
                                                v_clave_imponible
                                               );
      v_muestra_condomino :=
                          UPPER (fn_recuperar_parametro ('MUESTRA_CONDOMINO'));

      IF p_contribuyente IS NOT NULL
      THEN
         v_contribuyente := p_contribuyente;
         pa_contribuyente.validar_contribuyentes (v_tipo_documento,
                                                  v_numero_documento,
                                                  v_cuit_numerico,
                                                  v_razon_social,
                                                  v_contribuyente
                                                 );
         v_cuit :=
               SUBSTR (TO_CHAR (v_cuit_numerico), 1, 2)
            || '-'
            || SUBSTR (TO_CHAR (v_cuit_numerico), 3, 8)
            || '-'
            || SUBSTR (TO_CHAR (v_cuit_numerico), 11, 1);
         --PA_BOLETA_MI.SET_DOMICILIO(PA_DOMICILIO.GET_DOMICILIO_IMPONIBLE(V_TIPO_IMPONIBLE,V_CLAVE_IMPONIBLE,P_CONTRIBUYENTE));
         pa_boleta_mi.set_domicilio (v_domicilio);

         IF     NVL (v_muestra_condomino, 'N') = 'S'
            AND v_clave_imponible IS NOT NULL
            AND v_tipo_imponible IS NOT NULL
         THEN
            v_condomino_desc :=
               SUBSTR
                  (pa_boleta_mi.get_contrib_condomino_desc
                                                          (v_tipo_imponible,
                                                           v_clave_imponible,
                                                           NULL,
                                                           p_fecha_vencimiento
                                                          ),
                   1,
                   30
                  );

            IF v_condomino_desc IS NOT NULL
            THEN
               v_concatena_rs := SUBSTR ('-' || v_condomino_desc, 1, 30);
            ELSE
               v_concatena_rs := NULL;
            END IF;
         END IF;

         --generador de codigo de barras
            -- calculo de dias
         v_dias := -1;
         v_fecha := TRUNC (SYSDATE);

         BEGIN
            IF p_fecha_vencimiento >= v_fecha
            THEN
               SELECT p_fecha_vencimiento - v_fecha AS diferencia
                 INTO v_dias
                 FROM DUAL;
            ELSIF v_fecha > p_fecha_vencimiento
            THEN
               SELECT v_fecha - p_fecha_vencimiento AS diferencia
                 INTO v_dias
                 FROM DUAL;
            END IF;

            -- fin calculo de dias
            IF v_dias >= 0
            THEN
               SELECT pa_siac_codigo_barras.codigo_barras_boleta_multiple(v.numero_boleta,
                                                                          p_fecha_vencimiento,
                                                                          SUM(pa_boleta.get_importe_primer_vto(v.impuesto,
                                                                                                           v.concepto_obligacion,
                                                                                                           v.numero_obligacion_impuesto,
                                                                                                           v.numero_rectificativa,
                                                                                                           v.numero_cuota,
                                                                                                           v.fecha_primer_vencimiento,
                                                                                                           v.fecha_segundo_vencimiento,
                                                                                                           b.fecha_emision)),
                                                                          v_dias,
                                                                          SUM(get_importe_segundo_vto(v.impuesto,
                                                                                                  v.concepto_obligacion,
                                                                                                  v.numero_obligacion_impuesto,
                                                                                                  v.numero_rectificativa,
                                                                                                  v.numero_cuota,
                                                                                                  v.fecha_primer_vencimiento,
                                                                                                  v.fecha_segundo_vencimiento,
                                                                                                  p_fecha_vencimiento))
                      ) AS codigo_barra
                 INTO v_codigo_barra
                 FROM tbl_vencimientos v, tbl_boletas b
                WHERE b.numero_boleta = v.numero_boleta
                  --AND v.tipo_imponible IS NOT NULL
                  --AND v.clave_imponible IS NOT NULL
                  AND v.contribuyente = p_contribuyente
                  AND b.codigo_impresion = p_codigo_impresion
                  AND v.PLAN_FACILIDAD = P_PLAN_FACILIDAD
                GROUP BY v.numero_boleta,
                         p_fecha_vencimiento,
                         v_dias;

               v_numero_boleta := pa_siac_codigo_barras.f_numero_boleta (p_codigo_impresion);
               pa_siac_codigo_barras.p_agregar_barra (v_numero_boleta, v_codigo_barra, v_tipo_codigo_barra);
            END IF;
         END;

         --fin generador de codigo de barras
         IF p_plan_facilidad IS NULL
         THEN
            OPEN rf_datos FOR
               SELECT v.tipo_imponible, v.clave_imponible, v.impuesto,
                      DECODE
                         (v.multa_numero,
                          NULL, pa_vector_fiscal.get_impuesto_descr
                                                                   (v.impuesto),
                          'Multa'
                         ) impuesto_desc,
                      v.concepto_obligacion, v.numero_obligacion_impuesto,
                      v.anio, v.numero_cuota, v.numero_rectificativa,
                      p_contribuyente contribuyente,
                      pa_domicilio.busca_domicilio
                                   ('TIPO_DIRECCION',
                                    pa_boleta_mi.get_domicilio
                                   ) tipo_direccion,
                      NVL
                         (pa_domicilio.busca_domicilio
                                                   ('CALLE_DESC',
                                                    pa_boleta_mi.get_domicilio
                                                   ),
                          pa_domicilio.get_domicilio_imponible
                                                           (p_tipo_imponible,
                                                            p_clave_imponible,
                                                            v_contribuyente
                                                           )
                         ) domicilio,
                      
                      -- PA_DOMICILIO.BUSCA_DOMICILIO('DOMICILIO',PA_BOLETA_MI.GET_DOMICILIO)) DOMICILIO,
                      DECODE
                         (v.multa_numero,
                          NULL, pa_vector_fiscal.get_impuesto_abrev
                                                                   (v.impuesto),
                          'Detalle Imponible Multa'
                         ) impuesto_abrev,
                      v.numero_factura comprobante,
                      SUBSTR (v_razon_social || v_concatena_rs,
                              1,
                              60
                             ) razon_social,
                      v_cuit cuit,
                         v_tipo_documento
                      || ' '
                      || v_numero_documento numero_documento,
                         v_ident_larga
                      || ' '|| 
                      pa_siac_interface.detalle_imponible_boleta
                            (v.contribuyente,
                             v_tipo_contribuyente,
                             v.tipo_imponible,
                             v.clave_imponible,
                             v.impuesto,
                             v.concepto_obligacion,
                             v.anio,
                             v.numero_cuota,
                             get_fecha_primer_vto
                                                 (v.fecha_primer_vencimiento,
                                                  v.fecha_segundo_vencimiento,
                                                  p_fecha_vencimiento
                                                 ),
                             NULL
                            ) imponibles_det,
                      pa_cuentas_corrientes.deuda_vencimiento_sb
                                 (v.impuesto,
                                  v.concepto_obligacion,
                                  v.numero_obligacion_impuesto,
                                  v.numero_rectificativa,
                                  v.numero_cuota,
                                  SYSDATE
                                 ) importe_deuda,
                                 p_fecha_vencimiento fecha_primer_vencimiento,
                      pa_actualizacion.actualizar_vencimiento
                         (v.impuesto,
                          v.concepto_obligacion,
                          v.numero_obligacion_impuesto,
                          v.numero_rectificativa,
                          v.numero_cuota,
                          SYSDATE
                         ) importe_primer_vencimiento,
                      pa_actualizacion.calcular_intereses_venc
                               (v.impuesto,
                                v.concepto_obligacion,
                                v.numero_obligacion_impuesto,
                                v.numero_rectificativa,
                                v.numero_cuota,
                                SYSDATE,
                                'N'
                               ) importe_interes,
                      get_fecha_segundo_vto
                         (v.fecha_primer_vencimiento,
                          v.fecha_segundo_vencimiento,
                          p_fecha_vencimiento
                         ) fecha_segundo_vencimiento,
                      get_importe_segundo_vto
                         (v.impuesto,
                          v.concepto_obligacion,
                          v.numero_obligacion_impuesto,
                          v.numero_rectificativa,
                          v.numero_cuota,
                          v.fecha_primer_vencimiento,
                          v.fecha_segundo_vencimiento,
                          p_fecha_vencimiento
                         ) importe_segundo_vencimiento,
                      '' importe_seg_vto_sin_cond, v.numero_boleta,
                      PA_DOMICILIO.get_domicilio_contrib(v.TIPO_IMPONIBLE,v.CLAVE_IMPONIBLE,v.CONTRIBUYENTE,null) domicilio_det,
                      pa_domicilio.busca_domicilio
                                          ('MANZANA',
                                           pa_boleta_mi.get_domicilio
                                          ) manzana,
                      pa_domicilio.busca_domicilio
                                             ('CASA',
                                              pa_boleta_mi.get_domicilio
                                             ) casa,
                      pa_domicilio.busca_domicilio
                                            ('TORRE',
                                             pa_boleta_mi.get_domicilio
                                            ) torre,
                      pa_domicilio.busca_domicilio
                                             ('PISO',
                                              pa_boleta_mi.get_domicilio
                                             ) piso,
                      pa_domicilio.busca_domicilio
                                     ('DEPARTAMENTO',
                                      pa_boleta_mi.get_domicilio
                                     ) departamento,
                      pa_domicilio.busca_domicilio
                                           ('PUERTA',
                                            pa_boleta_mi.get_domicilio
                                           ) puerta,
                      pa_domicilio.busca_domicilio ('CALLE',
                                                    'CALLEJON') calle,
                                         --PA_BOLETA_MI.GET_DOMICILIO) CALLE,
                      pa_domicilio.busca_domicilio
                                           ('BARRIO',
                                            pa_boleta_mi.get_domicilio
                                           ) barrio,
                      pa_domicilio.busca_domicilio
                                        ('LOCALIDAD',
                                         pa_boleta_mi.get_domicilio
                                        ) localidad,
                      pa_domicilio.busca_domicilio
                                        ('MUNICIPIO',
                                         pa_boleta_mi.get_domicilio
                                        ) municipio,
                      pa_domicilio.busca_domicilio
                                        ('PROVINCIA',
                                         pa_boleta_mi.get_domicilio
                                        ) provincia,
                      pa_domicilio.busca_domicilio
                                             ('PAIS',
                                              pa_boleta_mi.get_domicilio
                                             ) pais,
                      pa_domicilio.busca_domicilio
                                    ('CODIGO_POSTAL',
                                     pa_boleta_mi.get_domicilio
                                    ) codigo_postal,
                      pa_domicilio.busca_domicilio
                                           ('PARAJE',
                                            pa_boleta_mi.get_domicilio
                                           ) paraje,
                      pa_domicilio.busca_domicilio
                                        ('PAIS_DESC',
                                         pa_boleta_mi.get_domicilio
                                        ) pais_desc,
                      pa_domicilio.busca_domicilio
                                   ('PROVINCIA_DESC',
                                    pa_boleta_mi.get_domicilio
                                   ) provincia_desc,
                      pa_domicilio.busca_domicilio
                                   ('MUNICIPIO_DESC',
                                    pa_boleta_mi.get_domicilio
                                   ) municipio_desc,
                      pa_domicilio.busca_domicilio
                                   ('LOCALIDAD_DESC',
                                    pa_boleta_mi.get_domicilio
                                   ) localidad_desc,
                      pa_domicilio.busca_domicilio
                                      ('BARRIO_DESC',
                                       pa_boleta_mi.get_domicilio
                                      ) barrio_desc,
                      pa_domicilio.busca_domicilio ('CALLE_DESC',
                                                    'OPCION A'
                                                   ) calle_desc,
                                   -- PA_BOLETA_MI.GET_DOMICILIO) CALLE_DESC,
                      v.fecha_primer_vencimiento fecha_vencimiento_cuota,
                      v.plan_vivienda_id plan_vivienda_id, v.plan_facilidad,
                      ' ' orden, NULL, b.codigo_barra codigo_barra
                 FROM tbl_vencimientos v, tbl_boletas b
                WHERE b.numero_boleta = v.numero_boleta
                  AND v.tipo_imponible IS NOT NULL
                  AND v.clave_imponible IS NOT NULL
                
                  AND v.contribuyente = p_contribuyente
                  AND b.codigo_impresion = p_codigo_impresion
               UNION
               SELECT v.tipo_imponible, v.clave_imponible, v.impuesto,
                      DECODE
                         (v.multa_numero,
                          NULL, pa_vector_fiscal.get_impuesto_descr
                                                                   (v.impuesto),
                          'Multa'
                         ) impuesto_desc,
                      v.concepto_obligacion, v.numero_obligacion_impuesto,
                      v.anio, v.numero_cuota, v.numero_rectificativa,
                      p_contribuyente contribuyente,
                      pa_domicilio.busca_domicilio
                                   ('TIPO_DIRECCION',
                                    pa_boleta_mi.get_domicilio
                                   ) tipo_direccion,
                      NVL
                         (pa_domicilio.busca_domicilio
                                                   ('CALLE_DESC',
                                                    pa_boleta_mi.get_domicilio
                                                   ),
                          pa_domicilio.busca_domicilio
                                                   ('DOMICILIO',
                                                    pa_boleta_mi.get_domicilio
                                                   )
                         ) domicilio,
                      DECODE
                         (v.multa_numero,
                          NULL, pa_vector_fiscal.get_impuesto_abrev
                                                                   (v.impuesto),
                          'Detalle Imponible Multa'
                         ) impuesto_abrev,
                      v.numero_factura comprobante,
                      SUBSTR
                         (   v_razon_social
                          || DECODE
                                (v_muestra_condomino,
                                 'S', '-'
                                  || pa_boleta_mi.get_contrib_condomino_desc
                                                          (v.tipo_imponible,
                                                           v.clave_imponible,
                                                           v.plan_facilidad,
                                                           p_fecha_vencimiento
                                                          ),
                                 NULL
                                ),
                          1,
                          60
                         ) razon_social,
                      v_cuit cuit,
                         v_tipo_documento
                      || ' '
                      || v_numero_documento numero_documento,
                      DECODE
                         (v.plan_facilidad,
                          NULL, v_ident_larga,
                             'N? Acogimiento : '
                          || pa_plan_facilidad.f_get_nro_acogimiento
                                                             (v.plan_facilidad)
                         ) imponibles_det,
                      pa_boleta.get_importe_primer_vto
                                 (v.impuesto,
                                  v.concepto_obligacion,
                                  v.numero_obligacion_impuesto,
                                  v.numero_rectificativa,
                                  v.numero_cuota,
                                  v.fecha_primer_vencimiento,
                                  v.fecha_segundo_vencimiento,
                                  b.fecha_emision
                                 ) importe_deuda,
                      DECODE
                         (v.plan_facilidad,
                          NULL, get_fecha_primer_vto
                                                 (v.fecha_primer_vencimiento,
                                                  v.fecha_segundo_vencimiento,
                                                  p_fecha_vencimiento
                                                 ),
                          DECODE (SIGN (  v.fecha_primer_vencimiento
                                        - p_fecha_vencimiento
                                       ),
                                  1, v.fecha_primer_vencimiento,
                                  p_fecha_vencimiento
                                 )
                         ) fecha_primer_vencimiento,
                      pa_actualizacion.actualizar_vencimiento
                         (v.impuesto,
                          v.concepto_obligacion,
                          v.numero_obligacion_impuesto,
                          v.numero_rectificativa,
                          v.numero_cuota,
                          p_fecha_vencimiento
                         ) importe_primer_vencimiento,
                      NULL importe_vencimiento_sin_cond,
                      NULL fecha_segundo_vencimiento,
                      NULL importe_segundo_vencimiento,
                      NULL importe_seg_vto_sin_cond, v.numero_boleta,
                      PA_DOMICILIO.get_domicilio_contrib(v.TIPO_IMPONIBLE,v.CLAVE_IMPONIBLE,V.CONTRIBUYENTE,null) domicilio_det,
                      pa_domicilio.busca_domicilio
                                          ('MANZANA',
                                           pa_boleta_mi.get_domicilio
                                          ) manzana,
                      pa_domicilio.busca_domicilio
                                             ('CASA',
                                              pa_boleta_mi.get_domicilio
                                             ) casa,
                      pa_domicilio.busca_domicilio
                                            ('TORRE',
                                             pa_boleta_mi.get_domicilio
                                            ) torre,
                      pa_domicilio.busca_domicilio
                                             ('PISO',
                                              pa_boleta_mi.get_domicilio
                                             ) piso,
                      pa_domicilio.busca_domicilio
                                     ('DEPARTAMENTO',
                                      pa_boleta_mi.get_domicilio
                                     ) departamento,
                      pa_domicilio.busca_domicilio
                                           ('PUERTA',
                                            pa_boleta_mi.get_domicilio
                                           ) puerta,
                      pa_domicilio.busca_domicilio
                                            ('CALLE',
                                             pa_boleta_mi.get_domicilio
                                            ) calle,
                      pa_domicilio.busca_domicilio
                                           ('BARRIO',
                                            pa_boleta_mi.get_domicilio
                                           ) barrio,
                      pa_domicilio.busca_domicilio
                                        ('LOCALIDAD',
                                         pa_boleta_mi.get_domicilio
                                        ) localidad,
                      pa_domicilio.busca_domicilio
                                        ('MUNICIPIO',
                                         pa_boleta_mi.get_domicilio
                                        ) municipio,
                      pa_domicilio.busca_domicilio
                                        ('PROVINCIA',
                                         pa_boleta_mi.get_domicilio
                                        ) provincia,
                      pa_domicilio.busca_domicilio
                                             ('PAIS',
                                              pa_boleta_mi.get_domicilio
                                             ) pais,
                      pa_domicilio.busca_domicilio
                                    ('CODIGO_POSTAL',
                                     pa_boleta_mi.get_domicilio
                                    ) codigo_postal,
                      pa_domicilio.busca_domicilio
                                           ('PARAJE',
                                            pa_boleta_mi.get_domicilio
                                           ) paraje,
                      pa_domicilio.busca_domicilio
                                        ('PAIS_DESC',
                                         pa_boleta_mi.get_domicilio
                                        ) pais_desc,
                      pa_domicilio.busca_domicilio
                                   ('PROVINCIA_DESC',
                                    pa_boleta_mi.get_domicilio
                                   ) provincia_desc,
                      pa_domicilio.busca_domicilio
                                   ('MUNICIPIO_DESC',
                                    pa_boleta_mi.get_domicilio
                                   ) municipio_desc,
                      pa_domicilio.busca_domicilio
                                   ('LOCALIDAD_DESC',
                                    pa_boleta_mi.get_domicilio
                                   ) localidad_desc,
                      pa_domicilio.busca_domicilio
                                      ('BARRIO_DESC',
                                       pa_boleta_mi.get_domicilio
                                      ) barrio_desc,
                      pa_domicilio.busca_domicilio
                                       ('CALLE_DESC',
                                        pa_boleta_mi.get_domicilio
                                       ) calle_desc,
                      v.fecha_primer_vencimiento fecha_vencimiento_cuota,
                      v.plan_vivienda_id plan_vivienda_id, v.plan_facilidad,
                      ' ' orden, NULL, '1' codigo_barra
                 FROM tbl_vencimientos v, tbl_boletas b
                WHERE b.numero_boleta = v.numero_boleta
                  AND v.tipo_imponible IS NULL
                  AND v.clave_imponible IS NULL
                  AND v.contribuyente = p_contribuyente
                  AND b.codigo_impresion = p_codigo_impresion;
         ELSE
            OPEN rf_datos FOR
               SELECT v.tipo_imponible, v.clave_imponible, v.impuesto,
                      pa_plan_facilidad.get_tipo_plan_desc
                                        (NVL (p_plan_facilidad,
                                              v.plan_facilidad
                                             )
                                        ) impuesto_desc,
                      v.concepto_obligacion, v.numero_obligacion_impuesto,
                      v.anio, v.numero_cuota, v.numero_rectificativa,
                      p_contribuyente contribuyente,
                      pa_domicilio.busca_domicilio
                                   ('TIPO_DIRECCION',
                                    pa_boleta_mi.get_domicilio
                                   ) tipo_direccion,
                      NVL
                         (pa_domicilio.busca_domicilio
                                                   ('CALLE_DESC',
                                                    pa_boleta_mi.get_domicilio
                                                   ),
                          pa_domicilio.busca_domicilio
                                                   ('DOMICILIO',
                                                    pa_boleta_mi.get_domicilio
                                                   )
                         ) domicilio,
                      pa_vector_fiscal.get_impuesto_abrev
                                                   (v.impuesto)
                                                              impuesto_abrev,
                      v.numero_factura comprobante,
                      SUBSTR
                         (   v_razon_social
                          || DECODE
                                (v_muestra_condomino,
                                 'S', '-'
                                  || pa_boleta_mi.get_contrib_condomino_desc
                                                          (v.tipo_imponible,
                                                           v.clave_imponible,
                                                           v.plan_facilidad,
                                                           p_fecha_vencimiento
                                                          ),
                                 NULL
                                ),
                          1,
                          60
                         ) razon_social,
                      v_cuit cuit,
                         v_tipo_documento
                      || ' '
                      || v_numero_documento numero_documento,
                      --IMPONIBLE_DET
                      pa_siac_interface.detalle_imponible_boleta
                            (P_CONTRIBUYENTE,
                             NULL,
                             P_TIPO_IMPONIBLE,
                             P_CLAVE_IMPONIBLE,
                             V_IMPUESTO,
                             V_CONCEPTO,
                             V_ANIO,
                             NULL,
                             NULL,
                             NULL
                            ) imponibles_det,--AQUI SE AGREGO
                         'Acogimiento : '
                      || pa_plan_facilidad.f_get_nro_acogimiento
                                                       (NVL (p_plan_facilidad,
                                                             v.plan_facilidad
                                                            )
                                                       )N_ACOGIMIENTO
                      /*|| CHR (10)
                      || v_ident_larga N_ACOGIMIENTO*/,
                      pa_boleta.get_importe_primer_vto
                                 (v.impuesto,
                                  v.concepto_obligacion,
                                  v.numero_obligacion_impuesto,
                                  v.numero_rectificativa,
                                  v.numero_cuota,
                                  v.fecha_primer_vencimiento,
                                  v.fecha_segundo_vencimiento,
                                  b.fecha_emision
                                 ) importe_ACTUALIZADO,--AQUI TAMBIEN SE CAMBIO
                      DECODE
                         (SIGN (  v.fecha_primer_vencimiento
                                - p_fecha_vencimiento
                               ),
                          1, v.fecha_primer_vencimiento,
                          p_fecha_vencimiento
                         ) fecha_primer_vencimiento,
                      pa_actualizacion.actualizar_vencimiento
                         (v.impuesto,
                          v.concepto_obligacion,
                          v.numero_obligacion_impuesto,
                          v.numero_rectificativa,
                          v.numero_cuota,
                          v.FECHA_PRIMER_VENCIMIENTO
                         ) IMPORTE_ORIGINAL,--AQUI SE CAMBIO
                         pa_actualizacion.calcular_intereses_venc
                               (v.impuesto,
                                v.concepto_obligacion,
                                v.numero_obligacion_impuesto,
                                v.numero_rectificativa,
                                v.numero_cuota,
                                SYSDATE,
                                'N'
                               ) importe_interes,
                      NULL importe_vencimiento_sin_cond,
                      NULL fecha_segundo_vencimiento,
                      NULL importe_segundo_vencimiento,
                      NULL importe_seg_vto_sin_cond, v.numero_boleta,
                      'Calle: '||get_domicilio_contrib2(p_tipo_imponible,p_clave_imponible,P_CONTRIBUYENTE,NULL) domicilio_det,--AQUI SE AGREGO
                      pa_domicilio.busca_domicilio
                                          ('MANZANA',
                                           pa_boleta_mi.get_domicilio
                                          ) manzana,
                      pa_domicilio.busca_domicilio
                                             ('CASA',
                                              pa_boleta_mi.get_domicilio
                                             ) casa,
                      pa_domicilio.busca_domicilio
                                            ('TORRE',
                                             pa_boleta_mi.get_domicilio
                                            ) torre,
                      pa_domicilio.busca_domicilio
                                             ('PISO',
                                              pa_boleta_mi.get_domicilio
                                             ) piso,
                      pa_domicilio.busca_domicilio
                                     ('DEPARTAMENTO',
                                      pa_boleta_mi.get_domicilio
                                     ) departamento,
                      pa_domicilio.busca_domicilio
                                           ('PUERTA',
                                            pa_boleta_mi.get_domicilio
                                           ) puerta,
                      pa_domicilio.busca_domicilio
                                            ('CALLE',
                                             pa_boleta_mi.get_domicilio
                                            ) calle,
                      pa_domicilio.busca_domicilio
                                           ('BARRIO',
                                            pa_boleta_mi.get_domicilio
                                           ) barrio,
                      pa_domicilio.busca_domicilio
                                        ('LOCALIDAD',
                                         pa_boleta_mi.get_domicilio
                                        ) localidad,
                      pa_domicilio.busca_domicilio
                                        ('MUNICIPIO',
                                         pa_boleta_mi.get_domicilio
                                        ) municipio,
                      pa_domicilio.busca_domicilio
                                        ('PROVINCIA',
                                         pa_boleta_mi.get_domicilio
                                        ) provincia,
                      pa_domicilio.busca_domicilio
                                             ('PAIS',
                                              pa_boleta_mi.get_domicilio
                                             ) pais,
                      pa_domicilio.busca_domicilio
                                    ('CODIGO_POSTAL',
                                     pa_boleta_mi.get_domicilio
                                    ) codigo_postal,
                      pa_domicilio.busca_domicilio
                                           ('PARAJE',
                                            pa_boleta_mi.get_domicilio
                                           ) paraje,
                      pa_domicilio.busca_domicilio
                                        ('PAIS_DESC',
                                         pa_boleta_mi.get_domicilio
                                        ) pais_desc,
                      pa_domicilio.busca_domicilio
                                   ('PROVINCIA_DESC',
                                    pa_boleta_mi.get_domicilio
                                   ) provincia_desc,
                      pa_domicilio.busca_domicilio
                                   ('MUNICIPIO_DESC',
                                    pa_boleta_mi.get_domicilio
                                   ) municipio_desc,
                      pa_domicilio.busca_domicilio
                                   ('LOCALIDAD_DESC',
                                    pa_boleta_mi.get_domicilio
                                   ) localidad_desc,
                      pa_domicilio.busca_domicilio
                                      ('BARRIO_DESC',
                                       pa_boleta_mi.get_domicilio
                                      ) barrio_desc,
                      pa_domicilio.busca_domicilio
                                       ('CALLE_DESC',
                                        pa_boleta_mi.get_domicilio
                                       ) calle_desc,
                      v.fecha_primer_vencimiento fecha_vencimiento_cuota,
                      v.plan_vivienda_id plan_vivienda_id, v.plan_facilidad,
                      '' orden, NULL,   B.codigo_barra--AQUI CAMBIO
                 FROM tbl_vencimientos v, tbl_boletas b
                WHERE b.numero_boleta = v.numero_boleta
                  --AND IP.CLAVE_IMPONIBLE = V_CLAVE_IMPONIBLE
                  --AND IP.TIPO_IMPONIBLE = V_TIPO_IMPONIBLE--AQUI SE AGREGO
                  AND v.PLAN_FACILIDAD = P_PLAN_FACILIDAD
                  AND b.codigo_impresion = p_codigo_impresion;
         END IF;
      ELSE
         OPEN rf_datos FOR
            SELECT   tipo_imponible, clave_imponible, impuesto,
                     impuesto_desc, concepto_obligacion,
                     numero_obligacion_impuesto, anio, numero_cuota,
                     numero_rectificativa, contribuyente, tipo_direccion,
                     domicilio, impuesto_abrev, comprobante, razon_social,
                     cuit, numero_documento, imponibles_det, importe_deuda,
                     TRUNC (fecha_primer_vencimiento),
                     importe_primer_vencimiento,
                     importe_vencimiento_sin_cond,
                     TRUNC (fecha_segundo_vencimiento),
                     importe_segundo_vencimiento, importe_seg_vto_sin_cond,
                     numero_boleta, manzana, casa, torre, piso, departamento,
                     puerta, calle, barrio, localidad, municipio, provincia,
                     pais, codigo_postal, paraje, pais_desc, provincia_desc,
                     municipio_desc, localidad_desc, barrio_desc, calle_desc,
                     fecha_vencimiento_cuota, plan_vivienda_id,
                     plan_facilidad, NULL, numero_boleta, '3' codigo_barra
                FROM tbl_contribuyente_x_boleta
               WHERE codigo_impresion = p_codigo_impresion
            ORDER BY 51;
      END IF;
      
      RETURN rf_datos;
   END;
   


END PA_SIAC_PLANES;
/
