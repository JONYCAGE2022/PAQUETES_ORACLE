CREATE OR REPLACE PACKAGE BODY TCSGUEM.PA_SIAC_MULTA
IS
--TRAE LA CLAVE DEL IMPONIBLE(NO SE LO USA)
    FUNCTION GET_CLAVE_IMPONIBLE (
    P_TIPO_IMPONIBLE    IN VARCHAR2,
    P_IMPONIBLE   IN VARCHAR2)
    RETURN VARCHAR2
IS
    V_CLAVE   TBL_IMPONIBLES.CLAVE%TYPE;
    E_ERROR   EXCEPTION;
BEGIN
    BEGIN
        IF  P_TIPO_IMPONIBLE = '0001'
        THEN
            SELECT I.CLAVE
              INTO V_CLAVE
              FROM TBL_IMPONIBLES I
             WHERE    I.COL07 = P_IMPONIBLE
             AND I.TIPO_IMPONIBLE = P_TIPO_IMPONIBLE
             AND I.FECHA_BAJA IS NULL;
        ELSE
            IF P_TIPO_IMPONIBLE = '0002'
            THEN
                SELECT I.CLAVE
                  INTO V_CLAVE
                  FROM TBL_IMPONIBLES I
                 WHERE    I.COL02 = P_IMPONIBLE
                 AND I.TIPO_IMPONIBLE = P_TIPO_IMPONIBLE
                 AND I.FECHA_BAJA IS NULL;
            ELSE 
                IF P_TIPO_IMPONIBLE = '0003'
                THEN 
                    SELECT I.CLAVE
                      INTO V_CLAVE
                      FROM TBL_IMPONIBLES I
                     WHERE    I.COL01 = P_IMPONIBLE
                     AND I.TIPO_IMPONIBLE = P_TIPO_IMPONIBLE
                     AND I.FECHA_BAJA IS NULL;
                ELSE 
                    RAISE E_ERROR;
                END IF;
            END IF;
        END IF;
        RETURN V_CLAVE;
    EXCEPTION
        WHEN E_ERROR
        THEN
            DBMS_OUTPUT.PUT_LINE ('NO SE ENCONTRO CLAVE');
    END;
END;
    --TRAE LAS MULTAS POR IMPONIBLE
    FUNCTION GET_MULTAS_X_IMPONIBLE (
        P_IMPONIBLE        IN TBL_IMPONIBLES.CLAVE%TYPE,
        P_TIPO_IMPONIBLE   IN TBL_IMPONIBLES.TIPO_IMPONIBLE%TYPE,
        P_ANIO            IN NUMBER)
        RETURN SYS_REFCURSOR
    AS
        V_MULTA             SYS_REFCURSOR;
        V_CLAVE_IMPONIBLE   VARCHAR2 (150);
        --V_ESQUEMA           VARCHAR2 (150);
    BEGIN
        --OBTENER CLAVE
        V_CLAVE_IMPONIBLE :=
            PA_SIAC_BOLETAS_MULTIPLES.clave_imponible (P_IMPONIBLE,P_TIPO_IMPONIBLE);

        --BUSCAR MULTA EN TBL_MULTAS
        IF P_ANIO IS NULL THEN
            OPEN V_MULTA FOR
                SELECT V.MULTA_NUMERO,
                        PA_MULTA.GET_TIPO_MULTA(V.MULTA_NUMERO) AS TIPO_MULTA,
                        PA_MULTA.GET_TIPO_MULTA_DESCR(PA_MULTA.GET_TIPO_MULTA(V.MULTA_NUMERO)) AS MULTA_DESCRIPCION,
                        M.FECHA_MULTA,
                        M.FECHA_ANULACION,
                        M.FECHA_CONDONACION,
                        M.FECHA_NOTIFICACION,
                        M.FECHA_VTO_BASE,
                        V.ANIO,
                        V.IMPUESTO,
                        V.CONCEPTO_OBLIGACION,
                        V.TIPO_IMPONIBLE,
                        V.CLAVE_IMPONIBLE,
                        V.NUMERO_CUOTA,
                        PA_MULTA.F_GET_ORIGEN(V.MULTA_NUMERO)AS ORIGEN,
                        PA_MULTA.GET_DESCR_ORIGEN(PA_MULTA.F_GET_ORIGEN(V.MULTA_NUMERO))AS ORIGEN_DESCRIPCION,
                        PA_MULTA.GET_NUMERO_FACTURA(V.MULTA_NUMERO)AS NUMERO_FACTURA,
                        M.NUMERO_ACTA,
                        PA_MULTA.GET_ESTADO_MULTA(V.MULTA_NUMERO,SYSDATE)AS ESTADO_MULTA,
                        M.IMPORTE_MULTA,
                        PA_MULTA.IMPORTE_ACTUALIZADO_MULTA(V.MULTA_NUMERO,SYSDATE)AS IMPORTE_ACTUALIZADO,
                        M.RESOLUCION,
                        M.OBSERVACIONES
                  FROM TBL_VENCIMIENTOS V 
                  JOIN TBL_MULTAS M
                    ON V.MULTA_NUMERO = M.MULTA_NUMERO
                 WHERE V.TIPO_IMPONIBLE = P_TIPO_IMPONIBLE
                 AND V.CLAVE_IMPONIBLE = V_CLAVE_IMPONIBLE
                 AND V.CONCEPTO_OBLIGACION = '0004';
        ELSE
            OPEN V_MULTA FOR
                SELECT V.MULTA_NUMERO,
                        PA_MULTA.GET_TIPO_MULTA(V.MULTA_NUMERO) AS TIPO_MULTA,
                        PA_MULTA.GET_TIPO_MULTA_DESCR(PA_MULTA.GET_TIPO_MULTA(V.MULTA_NUMERO)) AS MULTA_DESCRIPCION,
                        M.FECHA_MULTA,
                        M.FECHA_ANULACION,
                        M.FECHA_CONDONACION,
                        M.FECHA_NOTIFICACION,
                        M.FECHA_VTO_BASE,
                        V.ANIO,
                        V.IMPUESTO,
                        V.CONCEPTO_OBLIGACION,
                        V.TIPO_IMPONIBLE,
                        V.CLAVE_IMPONIBLE,
                        V.NUMERO_CUOTA,
                        PA_MULTA.F_GET_ORIGEN(V.MULTA_NUMERO)AS ORIGEN,
                        PA_MULTA.GET_DESCR_ORIGEN(PA_MULTA.F_GET_ORIGEN(V.MULTA_NUMERO))AS ORIGEN_DESCRIPCION,
                        PA_MULTA.GET_NUMERO_FACTURA(V.MULTA_NUMERO)AS NUMERO_FACTURA,
                        M.NUMERO_ACTA,
                        PA_MULTA.GET_ESTADO_MULTA(V.MULTA_NUMERO,SYSDATE)AS ESTADO_MULTA,
                        M.IMPORTE_MULTA,
                        PA_MULTA.IMPORTE_ACTUALIZADO_MULTA(V.MULTA_NUMERO,SYSDATE)AS IMPORTE_ACTUALIZADO,
                        M.RESOLUCION,
                        M.OBSERVACIONES
                  FROM TBL_VENCIMIENTOS V 
                  JOIN TBL_MULTAS M
                    ON V.MULTA_NUMERO = M.MULTA_NUMERO
                 WHERE V.TIPO_IMPONIBLE = P_TIPO_IMPONIBLE
                 AND V.CLAVE_IMPONIBLE = V_CLAVE_IMPONIBLE
                 AND V.CONCEPTO_OBLIGACION = '0004'
                 AND V.ANIO = P_ANIO;
        END IF;

        RETURN V_MULTA;
    EXCEPTION
        WHEN NO_DATA_FOUND
        THEN
            CLOSE V_MULTA;

            RETURN NULL;
    END;

    --DETALLE DE MULTAS O TASA Y SELLO (NO SE LO UTILIZA)
    FUNCTION GET_MULTA (P_MULTA          IN TBL_MULTAS.MULTA_NUMERO%TYPE,
                        P_TASA_Y_SELLO   IN TBL_TASAS_Y_SELLOS.TASA_SELLO_ID%TYPE)
        RETURN SYS_REFCURSOR
    AS
        V_DETALLE_MULTA   SYS_REFCURSOR;
    BEGIN
        BEGIN
            IF P_MULTA IS NOT NULL AND P_TASA_Y_SELLO IS NULL
            THEN
                OPEN V_DETALLE_MULTA FOR SELECT M.MULTA_NUMERO,
                                                M.FECHA_MULTA,
                                                V.NUMERO_FACTURA,
                                                M.NUMERO_ACTA,
                                                M.FECHA_ANULACION,
                                                M.FECHA_CONDONACION,
                                                M.TIPO_MULTA,
                                                TM.TIPO_MULTA_DESCR,
                                                TM.IMPUESTO,
                                                TM.CONCEPTO_OBLIGACION,
                                                M.MONEDA,
                                                M.ORIGEN,
                                                M.FECHA_NOTIFICACION,
                                                M.FECHA_VTO_BASE,
                                                M.IMPORTE_MULTA,
                                                M.RESOLUCION,
                                                M.OBSERVACIONES
                                           FROM TBL_MULTAS M
                                           JOIN TBL_VENCIMIENTOS V
                                           ON V.MULTA_NUMERO = M.MULTA_NUMERO
                                           JOIN TBL_TIPOS_MULTAS TM
                                           ON TM.TIPO_MULTA = M.TIPO_MULTA
                                          WHERE M.MULTA_NUMERO = P_MULTA;
            ELSE
                IF P_MULTA IS NULL AND P_TASA_Y_SELLO IS NOT NULL
                THEN
                    OPEN V_DETALLE_MULTA FOR
                        SELECT TS.TASA_SELLO_ID,
                           TS.TIPO_TASA,
                           TT.TIPO_TASA_DESCR,
                           TS.FECHA_OBLIGACION_PAGO,
                           TS.FECHA_VENCIMIENTO,
                           TS.FECHA_EMISION,
                           TS.OBSERVACIONES,
                           TS.USUARIO_ANULACION,
                           TS.FECHA_ANULACION,
                           DT.SUBTIPO_TASA,
                           ST.SUBTIPO_TASA_DESCR,
                           DT.MONTO_OBRA,
                           DT.IMPORTE,
                           TS.IMPORTE_INTERES,
                           TS.TIPO_DESCUENTO,
                           (
                           SELECT TD.TIPO_DESCUENTO_DESCR
                           FROM TBL_TIPOS_DESCUENTOS TD
                           JOIN TBL_TASAS_Y_SELLOS TS2
                           ON TS2.TIPO_DESCUENTO = TD.TIPO_DESCUENTO
                           WHERE TS2.TASA_SELLO_ID = P_TASA_Y_SELLO
                           )TIPO_DESC_DESCR,
                           TS.IMPORTE_DESCUENTO,
                           TS.IMPORTE_TOTAL
                      FROM TBL_TASAS_Y_SELLOS  TS, TBL_TIPOS_TASAS TT, TBL_DETALLE_TASAS_Y_SELLOS DT, TBL_SUBTIPOS_TASAS ST
                     WHERE TS.TASA_SELLO_ID = P_TASA_Y_SELLO
                     AND TT.TIPO_TASA = TS.TIPO_TASA
                     AND TS.TASA_SELLO_ID = DT.TASA_SELLO_ID
                     AND DT.SUBTIPO_TASA = ST.SUBTIPO_TASA;

                ELSE
                    V_DETALLE_MULTA:=NULL;
                END IF;
            END IF;
        END;

        RETURN V_DETALLE_MULTA;
    END;
--GENERA LA BOLETA
FUNCTION generador_boleta (
      p_impuesto         IN   VARCHAR2,
      p_concepto         IN   VARCHAR2,
      --p_anio             IN   VARCHAR2,
--      p_cuotas_multiples IN   VARCHAR2,
      p_tipo_imponible   IN   VARCHAR,
      p_imponible        IN   VARCHAR,
      P_MULTA            IN   VARCHAR2,  
      p_fecha_emision    IN   DATE
   )
      RETURN
            --VARCHAR2 AS
            sys_refcursor
   AS
      v_codigo_impresion_num   VARCHAR2 (20);
      v_contribuyente          tbl_imponibles.contribuyente%TYPE;
      v_clave_imponible        tbl_imponibles.clave%TYPE;
      v_codigo_unificado       VARCHAR2 (2);
--Cursos
      rf_datos                 sys_refcursor;
   BEGIN
      CASE p_tipo_imponible
         WHEN '0001'
         THEN
            SELECT clave, contribuyente
              INTO v_clave_imponible, v_contribuyente
              FROM tbl_imponibles i
             WHERE i.col07 = p_imponible
               AND i.tipo_imponible = p_tipo_imponible
               AND i.fecha_baja IS NULL;

            v_codigo_unificado := 'N';
         WHEN '0002'
         THEN
            SELECT clave, contribuyente
              INTO v_clave_imponible, v_contribuyente
              FROM tbl_imponibles i
             WHERE i.col02 = p_imponible
               AND i.tipo_imponible = p_tipo_imponible
               AND i.fecha_baja IS NULL;

            v_codigo_unificado := 'S';
         WHEN '0003'
         THEN
            SELECT clave, contribuyente
              INTO v_clave_imponible, v_contribuyente
              FROM tbl_imponibles i
             WHERE i.col01 = p_imponible
               AND i.tipo_imponible = p_tipo_imponible
               AND i.fecha_baja IS NULL;

            IF v_clave_imponible = 'NULL'
            THEN
               SELECT clave, contribuyente
                 INTO v_clave_imponible, v_contribuyente
                 FROM tbl_imponibles i
                WHERE i.col02 = p_imponible
                  AND i.tipo_imponible = p_tipo_imponible
                  AND i.fecha_baja IS NULL;
            END IF;

            v_codigo_unificado := 'N';
         ELSE
            OPEN rf_datos FOR
               SELECT 0 ID, SYSDATE term_date
                 FROM DUAL;

            v_codigo_unificado := 'N';
      END CASE;

--V_CLAVE_IMPONIBLE := CLAVE_IMPONIBLE (P_IMPONIBLE,P_TIPO_IMPONIBLE);
      v_codigo_impresion_num :=
         boleta_simple_masiva (p_impuesto,
                               p_concepto,
                               --p_anio,
--                               p_cuotas_multiples,
--                               p_tipo_imponible,
--                               p_imponible,
                               P_MULTA,
                               p_fecha_emision
                              );
      -- COMMIT;
      rf_datos :=
         boleta_simple (TO_NUMBER (v_codigo_impresion_num),
                        p_fecha_emision,
                        v_contribuyente,
                        NULL,
                        NULL,
                        NULL
                       );
      -- COMMIT;
      RETURN rf_datos;
   EXCEPTION
      WHEN OTHERS
      THEN
         RETURN NULL;
   END;
--MUESTRA LA BOLETA  
FUNCTION boleta_simple (
      p_codigo_impresion    IN   NUMBER,
      p_fecha_vencimiento   IN   DATE,
      p_contribuyente       IN   NUMBER,
      p_tipo_imponible      IN   VARCHAR2,
      p_clave_imponible     IN   VARCHAR2,
      p_plan_facilidad      IN   NUMBER
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
   BEGIN
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
                  AND v.tipo_imponible IS NOT NULL
                  AND v.clave_imponible IS NOT NULL
                  AND v.contribuyente = p_contribuyente
                  AND b.codigo_impresion = p_codigo_impresion
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
                         PA_MULTA.GET_TIPO_MULTA(v.MULTA_NUMERO) TIPO_MULTA,
                         PA_MULTA.GET_TIPO_MULTA_DESCR(PA_MULTA.GET_TIPO_MULTA(v.MULTA_NUMERO)) TIPO_MULTA_DESCR,
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
                      --   v_ident_larga
                      --|| ' '|| 
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
                         PA_MULTA.GET_TIPO_MULTA(v.MULTA_NUMERO) TIPO_MULTA,
                         PA_MULTA.GET_TIPO_MULTA_DESCR(PA_MULTA.GET_TIPO_MULTA(v.MULTA_NUMERO)) TIPO_MULTA_DESCR,
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
                         'N? Acogimiento : '
                      || pa_plan_facilidad.f_get_nro_acogimiento
                                                       (NVL (p_plan_facilidad,
                                                             v.plan_facilidad
                                                            )
                                                       )
                      || CHR (10)
                      || v_ident_larga imponibles_det,
                      0 importe_deuda,
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
                          p_fecha_vencimiento
                         ) importe_primer_vencimiento,
                      NULL importe_vencimiento_sin_cond,
                      NULL fecha_segundo_vencimiento,
                      NULL importe_segundo_vencimiento,
                      NULL importe_seg_vto_sin_cond, v.numero_boleta,
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
                      '' orden, NULL, '2' codigo_barra
                 FROM tbl_vencimientos v, tbl_boletas b
                WHERE b.numero_boleta = v.numero_boleta
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
--INSERTA LA BOLETA Y GENERA EL CODIGO DE IMPRESIÓN  
FUNCTION boleta_simple_masiva (
      p_impuesto         IN   VARCHAR2,
      p_concepto         IN   VARCHAR2,
      --p_anio             IN   VARCHAR2,
--      p_cuotas_multiples IN   VARCHAR2,
--      p_tipo_imponible   IN   VARCHAR,
--      p_imponible        IN   VARCHAR,
      P_MULTA            IN   VARCHAR2,
      p_fecha_emision    IN   DATE
   )
      RETURN VARCHAR2
   AS
      --RETURN SYS_REFCURSOR AS
      domicilio                        obj_domicilio
         := obj_domicilio (NULL,
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
      rc_datos                         sys_refcursor;
      rc                               sys_refcursor;
--      v_clave_imponible                VARCHAR2 (20) := PA_SIAC_BOLETAS_MULTIPLES.CLAVE_IMPONIBLE (p_imponible, p_tipo_imponible);
      
     
      CURSOR impresion_masiva
      IS
         SELECT   impuesto, concepto_obligacion, numero_obligacion_impuesto, max(numero_rectificativa) as numero_rectificativa, MULTA_NUMERO --ROWID
             FROM tbl_vencimientos
            WHERE MULTA_NUMERO IN (            
                                    SELECT   REGEXP_SUBSTR (cadena, '[^,]+', 1, numeroFila) SPLIT
                                        FROM (SELECT 1 ID, P_MULTA cadena
                                                FROM DUAL)
                                             CROSS JOIN
                                             (SELECT     ROWNUM numeroFila
                                                    FROM (SELECT   MAX (LENGTH (REGEXP_REPLACE (cadena, '[^,]+')))
                                                                 + 1 mx
                                                            FROM (SELECT 1 ID, P_MULTA cadena
                                                                    FROM DUAL))
                                              CONNECT BY LEVEL <= mx)
                                       WHERE REGEXP_SUBSTR (cadena, '[^,]+', 1, numeroFila) IS NOT NULL
                                    )
              --AND anio = p_anio
              AND concepto_obligacion = p_concepto
              AND impuesto = p_impuesto
--              AND tipo_imponible = p_tipo_imponible
--              AND clave_imponible = v_clave_imponible
              --AND MULTA_NUMERO = P_MULTA
         group by impuesto, concepto_obligacion, numero_obligacion_impuesto, MULTA_NUMERO
         /*ORDER BY pa_boleta.f_orden_boleta
                         (impuesto,
                          concepto_obligacion,
                          domicilio.get_domicilio_impresion (tipo_imponible,
                                                             clave_imponible,
                                                             contribuyente
                                                            ),
                          tipo_imponible,
                          clave_imponible
                         ),
                  tipo_imponible,
                  clave_imponible,
                  anio,
                  numero_cuota*/;

      p_boleta                         pa_cuentas_corrientes.t_vencimientos;
      p_mensaje_error                  VARCHAR2 (100);
      v_contribuyente                  pa_boleta.rf_datos_boleta;
      v_localidad                      VARCHAR2 (10)                   := NULL;
      v_tipo_imponible                 VARCHAR2 (10);
      v_codigo_unificado               VARCHAR2 (2)                    := NULL;
      v_impuesto                       VARCHAR2 (4);
      v_anio                           NUMBER;
      v_cuota                          NUMBER;
      v_numero_rectificativa           NUMBER;
      v_codigo_impresion               VARCHAR (4000);
      v_contador                       NUMBER                             := 0;
      v_numero_boleta                  NUMBER;
      v_err                            BOOLEAN;
      v_error_act_boleta               BOOLEAN;
      v_mensaje_error                  VARCHAR2 (100);
      e_existen_multiples_imponibles   EXCEPTION;
      v_cant_imponibles                NUMBER;
      v_barrio                         VARCHAR2 (20);
      v_bloque                         VARCHAR2 (20);
      v_manzana                        VARCHAR2 (20);
      v_importe                        NUMBER;
      v_error_ins                      BOOLEAN;
      v_codigo_organismo               tbl_parametros.valor%TYPE;
      v_tipo_imponible_old             tbl_vencimientos.tipo_imponible%TYPE;
      v_clave_imponible_old            tbl_vencimientos.clave_imponible%TYPE;
      v_anio_old                       tbl_vencimientos.anio%TYPE;
      v_numero_cuota_old               tbl_vencimientos.numero_cuota%TYPE;
      v_pertenece_muestreo             tbl_imponibles.pertenece_muestreo%TYPE;
      v_zona                           VARCHAR2 (100);
      v_col_zona                       tbl_parametros.valor%TYPE;
      v_tipo_grupo_inf_inm             tbl_parametros.valor%TYPE;
      v_col_repartidor                 tbl_parametros.valor%TYPE;
      v_select                         VARCHAR2 (2000);
      v_repartidor                     tbl_imponibles.col01%TYPE;
      v_vigencia                       DATE;
      v_clausula_from                  tbl_filtros_dinamico_boleta.clausula_from%TYPE;
      v_clausula_where                 tbl_filtros_dinamico_boleta.clausula_where%TYPE;
      v_clausula_tipo_imponible        tbl_filtros_dinamico_boleta.tipo_imponible%TYPE;
      v_clausula_clave_imponible       tbl_filtros_dinamico_boleta.clave_imponible%TYPE;
      v_resultado_clausula             VARCHAR2 (1);
      v_resultado                      VARCHAR2 (30);
      v_codigo_impresion_num           NUMBER;
      v_flag                           VARCHAR2 (1);
      v_imp_boleta_inmob               VARCHAR2 (150);
      v_imp_boleta_auto                VARCHAR2 (150);
      v_tipo_imponible_ri              tbl_vencimientos.tipo_imponible%TYPE;
      v_clave_imponible_ri             tbl_vencimientos.clave_imponible%TYPE;
      v_impuesto_ri                    tbl_vencimientos.impuesto%TYPE;
      v_concepto_obligacion_ri         tbl_vencimientos.concepto_obligacion%TYPE;
      v_nro_obligacion_impuesto_ri     tbl_vencimientos.numero_obligacion_impuesto%TYPE;
      v_anio_ri                        tbl_vencimientos.anio%TYPE;
      v_numero_cuota_ri                tbl_vencimientos.numero_cuota%TYPE;
      v_numero_rectificativa_ri        tbl_vencimientos.numero_rectificativa%TYPE;
      v_contribuyente_ri               tbl_vencimientos.contribuyente%TYPE;
      v_juicio_id_ri                   tbl_vencimientos.juicio_id%TYPE;
      v_plan_facilidad_incluido_ri     tbl_vencimientos.plan_facilidad_incluido%TYPE;
      v_tipo_vencimiento_ri            tbl_vencimientos.tipo_vencimiento%TYPE;
      v_cuota_contado_ri               tbl_vencimientos.cuota_contado%TYPE;
      v_fecha_primer_vencimiento_ri    tbl_vencimientos.fecha_primer_vencimiento%TYPE;
      v_muestra_deuda                  VARCHAR2 (1);
      v_existe                         NUMBER;
      v_domicilio                      tbl_contribuyente_x_boleta%ROWTYPE;
      v_info_adicional_inmob           VARCHAR2 (2000);
      v_parametro_busqueda             VARCHAR2 (1);
      v_proxima_fecha_vto              DATE;
      v_proxima_cuota                  NUMBER;
      v_detalle_imponible              VARCHAR2 (300);
      v_tipo_contribuyente             tbl_contribuyentes.tipo_contribuyente%TYPE;
      v_bloqueado                      VARCHAR2 (20);
   BEGIN
      v_parametro_busqueda :=
                    NVL (fn_recuperar_parametro ('PROXIMO_VENCIMIENTO'), 'A');
      v_codigo_organismo := fn_recuperar_parametro ('CODIGO ORGANISMO');
      v_col_zona := fn_recuperar_parametro ('COL_ZONA');
      v_tipo_grupo_inf_inm := fn_recuperar_parametro ('TIPO_GRUPO_INF_INM');
      v_col_repartidor := fn_recuperar_parametro ('COL_REPARTIDOR');
      v_codigo_impresion_num := pa_boleta.get_codigo_impresion;
      v_codigo_impresion := TO_CHAR (v_codigo_impresion_num);
      v_muestra_deuda := 'S';

--      BEGIN
--         CASE p_tipo_imponible
--            WHEN '0001'
--            THEN
--               SELECT clave
--                 INTO v_clave_imponible
--                 FROM tbl_imponibles i
--                WHERE i.col07 = p_imponible;
--            --V_CODIGO_UNIFICADO := 'N';
--         WHEN '0002'
--            THEN
--               SELECT clave
--                 INTO v_clave_imponible
--                 FROM tbl_imponibles i
--                WHERE i.col02 = p_imponible;
--            -- V_CODIGO_UNIFICADO := 'S';
--         WHEN '0003'
--            THEN
--               SELECT clave
--                 INTO v_clave_imponible
--                 FROM tbl_imponibles i
--                WHERE i.col01 = p_imponible;
--
--               IF v_clave_imponible = 'NULL'
--               THEN
--                  SELECT clave
--                    INTO v_clave_imponible
--                    FROM tbl_imponibles i
--                   WHERE i.col02 = p_imponible;
--               END IF;
--            --V_CODIGO_UNIFICADO := 'N';
--         ELSE
--               OPEN rc FOR
--                  SELECT 0 ID, SYSDATE term_date
--                    FROM DUAL;
--         --V_CODIGO_UNIFICADO := 'N';
--         END CASE;
--      EXCEPTION
--         WHEN OTHERS
--         THEN
--            v_clausula_from := NULL;
--            v_clausula_where := NULL;
--            v_clausula_tipo_imponible := NULL;
--            v_clausula_clave_imponible := NULL;
--      END;

      v_flag := 'N';
      FOR a IN impresion_masiva
      LOOP
         BEGIN
            BEGIN
               SELECT c.tipo_imponible, c.clave_imponible,
                      c.impuesto, c.concepto_obligacion,
                      c.numero_obligacion_impuesto, c.anio,
                      c.numero_cuota, c.numero_rectificativa,
                      c.contribuyente, c.juicio_id,
                      c.plan_facilidad_incluido, c.tipo_vencimiento,
                      b.col10, b.col21,
                      b.pertenece_muestreo, cuota_contado,
                      fecha_primer_vencimiento
                 INTO v_tipo_imponible_ri, v_clave_imponible_ri,
                      v_impuesto_ri, v_concepto_obligacion_ri,
                      v_nro_obligacion_impuesto_ri, v_anio_ri,
                      v_numero_cuota_ri, v_numero_rectificativa_ri,
                      v_contribuyente_ri, v_juicio_id_ri,
                      v_plan_facilidad_incluido_ri, v_tipo_vencimiento_ri,
                      v_imp_boleta_inmob, v_imp_boleta_auto,
                      v_pertenece_muestreo, v_cuota_contado_ri,
                      v_fecha_primer_vencimiento_ri
                 FROM tbl_vencimientos c, tbl_imponibles b
                WHERE c.clave_imponible = b.clave
                  AND c.tipo_imponible = b.tipo_imponible
--                  AND b.clave = v_clave_imponible     SE ANULO LA CLAVE Y TIPO 
--                  AND b.tipo_imponible = p_tipo_imponible
                  --AND c.ROWID = a.ROWID;
                  --impuesto, concepto_obligacion, numero_obligacion_impuesto, max(numero_rectificativa) as numero_rectificativa, numero_cuota
                  AND c.impuesto = a.impuesto
                  AND c.concepto_obligacion = a.concepto_obligacion
                  AND c.numero_obligacion_impuesto = a.numero_obligacion_impuesto
                  AND c.numero_rectificativa = a.numero_rectificativa
                  AND c.MULTA_NUMERO = a.MULTA_NUMERO;--SE CAMBIO NUMERO DE CUOTA POR NUMERO DE MULTA
                  
                  
                  
                  
                  
            EXCEPTION
               WHEN OTHERS
               THEN
                  NULL;
            END;

            IF NVL (fn_recuperar_parametro ('EDESA_MASIVA'), 'N') = 'S'
            THEN
               IF pa_edesa.f_existe_bloqueo (v_impuesto_ri,
                                             v_concepto_obligacion_ri,
                                             v_nro_obligacion_impuesto_ri,
                                             v_numero_rectificativa_ri,
                                             v_numero_cuota_ri
                                            )
               THEN
                  RAISE e_existen_multiples_imponibles;
               END IF;
            END IF;

            IF    v_juicio_id_ri IS NOT NULL
               OR v_plan_facilidad_incluido_ri IS NOT NULL
               OR v_tipo_vencimiento_ri = 'O'
            THEN
               RAISE e_existen_multiples_imponibles;
            END IF;

            IF p_impuesto IS NOT NULL AND p_impuesto != v_impuesto_ri
            THEN
               RAISE e_existen_multiples_imponibles;
            END IF;

            IF v_codigo_unificado IS NOT NULL
            THEN
               BEGIN
                  SELECT 1
                    INTO v_existe
                    FROM tbl_impuestos_asoc_unif
                   WHERE impuesto = v_impuesto_ri
                     AND codigo_unificado = v_codigo_unificado;
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     RAISE e_existen_multiples_imponibles;
                  WHEN OTHERS
                  THEN
                     NULL;
               END;
            END IF;

            pa_domicilio.get_domicilio_impresion_masiva (v_tipo_imponible_ri,
                                                         v_clave_imponible_ri,
                                                         v_contribuyente_ri,
                                                         v_domicilio
                                                        );
            v_localidad := v_domicilio.localidad;
            v_barrio := v_domicilio.barrio;
            v_bloque := v_domicilio.sector_bloque;
            v_manzana := v_domicilio.manzana;

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

               v_bloqueado :=
                  pa_imponible.get_imponible_bloqueado (v_tipo_imponible_ri,
                                                        v_clave_imponible_ri
                                                       );

               IF NVL (v_bloqueado, 'N') = 'S'
               THEN
                  RAISE e_existen_multiples_imponibles;
               END IF;

               
               IF v_localidad IS NULL
               THEN
                  v_localidad := '@@@';
               END IF;

               IF v_barrio IS NULL
               THEN
                  v_barrio := '@@@';
               END IF;

               IF v_bloque IS NULL
               THEN
                  v_bloque := '@@@';
               END IF;

               IF v_manzana IS NULL
               THEN
                  v_manzana := '@@@';
               END IF;

               IF     v_localidad = NVL (NULL, v_localidad)
                  AND v_barrio = NVL (NULL, v_barrio)
                  AND v_bloque = NVL (NULL, v_bloque)
                  AND v_manzana = NVL (NULL, v_manzana)
               THEN
                  v_importe :=
                     pa_actualizacion.actualizar_vencimiento
                                               (v_impuesto_ri,
                                                v_concepto_obligacion_ri,
                                                v_nro_obligacion_impuesto_ri,
                                                v_numero_rectificativa_ri,
                                                v_numero_cuota_ri,
                                                SYSDATE
                                               );

                  IF v_importe <= 0
                  THEN
                     RAISE e_existen_multiples_imponibles;
                  END IF;

                  IF v_flag = 'N'
                      THEN
                      IF v_codigo_unificado IS NOT NULL
                      THEN
                         IF     v_tipo_imponible_old = v_tipo_imponible_ri
                            AND v_clave_imponible_old = v_clave_imponible_ri
                            AND v_anio_old = v_anio_ri
                            AND v_numero_cuota_old = v_numero_cuota_ri
                         THEN
                            v_err := TRUE;
                         ELSE
                            v_numero_boleta := pa_boleta.get_numero_boleta;
                            v_err :=
                               pa_boleta.inserta_boleta_masiva
                                                          (v_numero_boleta,
                                                           p_fecha_emision,
                                                           v_codigo_impresion_num
                                                          );
                            v_flag := 'S';
                         END IF;
                      ELSE
                            v_numero_boleta := pa_boleta.get_numero_boleta;
                            v_err := PA_SIAC_BOLETAS_MULTIPLES.guardar_boleta(v_numero_boleta, p_fecha_emision, v_codigo_impresion_num);
                            v_flag := 'S';
                      END IF;
                  END IF;
                  v_tipo_imponible_old := v_tipo_imponible_ri;
                  v_clave_imponible_old := v_clave_imponible_ri;
                  v_anio_old := v_anio_ri;
                  v_numero_cuota_old := v_numero_cuota_ri;

                  IF v_err
                  THEN
                     v_error_ins := FALSE;
                     inserta_vencimientos_x_boleta2
                                               (v_impuesto_ri,
                                                v_concepto_obligacion_ri,
                                                v_nro_obligacion_impuesto_ri,
                                                v_numero_rectificativa_ri,
                                                v_numero_cuota_ri,
                                                v_numero_boleta
                                               );

                     IF v_error_ins
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

                  IF v_err
                  THEN
                     v_error_act_boleta := false;
                     act_numero_boleta (v_impuesto_ri,
                                        v_concepto_obligacion_ri,
                                        v_nro_obligacion_impuesto_ri,
                                        v_numero_rectificativa_ri,
                                        v_numero_cuota_ri,
                                        v_numero_boleta,
                                        v_error_act_boleta,
                                        v_mensaje_error
                                       );

                     IF v_error_act_boleta
                     THEN
                        --RETURN NULL;
                        RETURN    'ACT_NUMERO_BOLETA: '
                               || v_mensaje_error
                               || '613';
                     END IF;
                  END IF;

                  v_info_adicional_inmob := NULL;

                  BEGIN
                     v_info_adicional_inmob :=
                        SUBSTR
                           (pa_siac_interface.f_info_adicional_inmob
                                                        (v_clave_imponible_ri,
                                                         v_tipo_imponible_ri,
                                                         'S',
                                                         v_domicilio
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

                  v_tipo_contribuyente :=
                     pa_contribuyente.get_tipo_contribuyente
                                                          (v_contribuyente_ri,
                                                           SYSDATE
                                                          );
                  v_proxima_fecha_vto := NULL;
                  v_proxima_cuota := NULL;

                  BEGIN
                     PA_SIAC_BOLETAS_MULTIPLES.p_proximo_vto_masiva (v_impuesto_ri,
                                           v_concepto_obligacion_ri,
                                           v_anio_ri,
                                           v_nro_obligacion_impuesto_ri,
                                           v_numero_rectificativa_ri,
                                           v_numero_cuota_ri,
                                           v_cuota_contado_ri,
                                           v_fecha_primer_vencimiento_ri,
                                           v_clave_imponible_ri,
                                           v_tipo_imponible_ri,
                                           v_parametro_busqueda,
                                           v_contribuyente_ri,
                                           v_tipo_contribuyente,
                                           v_proxima_fecha_vto,
                                           v_proxima_cuota
                                          );
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        v_proxima_fecha_vto := NULL;
                        v_proxima_cuota := NULL;
                        RETURN    '1545 P_PROXIMO_VTO_MASIVA: '
                               || TO_CHAR (SQLCODE);
                  END;

                  v_detalle_imponible := NULL;

                  BEGIN
                     v_detalle_imponible :=
                        f_detalle_imponible (v_contribuyente_ri,
                                             v_tipo_contribuyente,
                                             v_tipo_imponible_ri,
                                             v_clave_imponible_ri,
                                             v_impuesto_ri,
                                             v_concepto_obligacion_ri,
                                             v_anio_ri,
                                             v_numero_cuota_ri,
                                             v_fecha_primer_vencimiento_ri,
                                             'S',
                                             'S',
                                             NULL
                                            );
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        v_detalle_imponible := NULL;
                        RETURN TO_CHAR (SQLCODE) || '685';
                  END;

                  BEGIN
                     genera_contribuyente_boleta2
                                                     (v_codigo_impresion_num,
                                                      SYSDATE,
                                                      v_contribuyente_ri,
                                                      v_tipo_imponible_ri,
                                                      v_clave_imponible_ri,
                                                      NULL,
                                                      NULL,
                                                     
                                                      a.impuesto,
                                                      a.concepto_obligacion,
                                                      a.numero_obligacion_impuesto,
                                                      a.numero_rectificativa,
                                                      a.MULTA_NUMERO,
                                                      
                                                      
                                                      --a.ROWID,
                                                      v_domicilio,
                                                      v_info_adicional_inmob,
                                                      v_proxima_fecha_vto,
                                                      v_proxima_cuota,
                                                      v_detalle_imponible,
                                                      'S'
                                                     );
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        RETURN TO_CHAR (SQLCODE) || '707';
                  END;

                  BEGIN
                     pa_siac_interface.genera_conceptos_boleta
                                               (v_impuesto_ri,
                                                v_concepto_obligacion_ri,
                                                v_nro_obligacion_impuesto_ri,
                                                0,
                                                v_numero_cuota_ri,
                                                v_codigo_impresion_num,
                                                v_numero_boleta,
                                                TRUNC (SYSDATE),
                                                'S'
                                               );
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        RETURN TO_CHAR (SQLCODE || '724');
                  END;

                  IF UPPER (NVL (v_muestra_deuda, 'N')) = 'S'
                  THEN
                     BEGIN
                        pa_siac_interface.genera_cuotas_adeudadas_boleta
                                               (v_tipo_imponible_ri,
                                                v_clave_imponible_ri,
                                                v_contribuyente_ri,
                                                v_impuesto_ri,
                                                v_concepto_obligacion_ri,
                                                v_nro_obligacion_impuesto_ri,
                                                0,
                                                v_numero_cuota_ri,
                                                v_numero_boleta,
                                                v_codigo_unificado,
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
            WHEN e_existen_multiples_imponibles
            THEN
               RETURN '3 ERROR: ' ||TO_CHAR (SQLCODE);
         END;
      END LOOP;

      RETURN v_codigo_impresion_num;
   
   EXCEPTION
      WHEN OTHERS
      THEN
         RETURN NULL;
   END;

PROCEDURE GENERA_CONTRIBUYENTE_BOLETA2( P_CODIGO_IMPRESION     IN   NUMBER
                                      ,P_FECHA_VENCIMIENTO    IN   DATE
                                      ,P_CONTRIBUYENTE        IN   NUMBER
                                      ,P_TIPO_IMPONIBLE       IN   VARCHAR2
                                      ,P_CLAVE_IMPONIBLE      IN   VARCHAR2
                                      ,P_PLAN_FACILIDAD       IN   NUMBER
                                      ,P_IMPUESTO_UNIFICADO   IN   TBL_IMPUESTOS_ASOC_UNIF.CODIGO_UNIFICADO%TYPE
                                      ,P_IMPUESTO                       IN  TBL_VENCIMIENTOS.IMPUESTO%TYPE
                                      ,P_CONCEPTO_OBLIGACION            IN  TBL_VENCIMIENTOS.CONCEPTO_OBLIGACION%TYPE
                                      ,P_NUMERO_OBLIGACION_IMPUESTO     IN  TBL_VENCIMIENTOS.NUMERO_OBLIGACION_IMPUESTO%TYPE
                                      ,P_NUMERO_RECTIFICATIVA           IN  TBL_VENCIMIENTOS.NUMERO_RECTIFICATIVA%TYPE
                                      ,P_NUMERO_CUOTA                   IN  TBL_VENCIMIENTOS.NUMERO_CUOTA%TYPE
                                      --,P_ROWID                IN   ROWID
                                      ,P_DOMICILIO            IN   TBL_CONTRIBUYENTE_X_BOLETA%ROWTYPE
                                      ,P_INFO_ADICIONAL_INMOB IN   VARCHAR2
                                      ,P_PROXIMA_FECHA_VTO    IN   DATE
                                      ,P_PROXIMA_CUOTA        IN   NUMBER
                                      ,P_DETALLE_IMPONIBLE    IN   VARCHAR2
                                      ,P_MASIVA               IN   VARCHAR2 := 'N') IS

 -- RF_DATOS_CONTRIBUYENTE   PA_SIAC_BOLETAS_MULTIPLES.RF_DATOS_BOLETA;¿PREGUNTAR?
  V_RAZON_SOCIAL           VARCHAR2(100);
  V_CUIT_NUMERICO          NUMBER;
  V_CONTRIBUYENTE          NUMBER;
  V_TIPO_IMPONIBLE         TBL_IMPONIBLES.TIPO_IMPONIBLE%TYPE;
  V_CLAVE_IMPONIBLE        TBL_IMPONIBLES.CLAVE%TYPE;
  V_PLAN_FACILIDAD         TBL_PLANES_FACILIDADES.PLAN_FACILIDAD%TYPE;
  V_IDENT_LARGA            TBL_IMPONIBLES.IDENTIFICACION_LARGA%TYPE;
  V_MUESTRA_CONDOMINO      TBL_PARAMETROS.VALOR%TYPE;
  V_CONCATENA_RS           VARCHAR2(30);
  V_CONDOMINO_DESC         VARCHAR2(30);

  V_CUIT                   VARCHAR2(13);
  V_TIPO_DOCUMENTO         TBL_PERSONAS.TIPO_DOCUMENTO%TYPE;
  V_DOCUMENTO              TBL_PERSONAS.NUMERO_DOCUMENTO%TYPE;

  V_IMPUESTO_DESCR  TBL_IMPUESTOS.IMPUESTO_DESCR%TYPE;
  V_IMPUESTO_ABREV  TBL_IMPUESTOS.IMPUESTO_ABREV%TYPE;
  

  V_RS_CONCATENADA       VARCHAR2(5000);
  V_RAZON_SOCIAL_COTIT   VARCHAR2(500);


  CURSOR C_OCUPANTES ( P_CLAVE_IMPONIBLE IN TBL_IMPONIBLES.CLAVE%TYPE
                      ,P_TIPO_IMPONIBLE  IN TBL_IMPONIBLES.TIPO_IMPONIBLE%TYPE
                       ) IS
    SELECT PERSONA_ID
      FROM TBL_OCUPANTES
     WHERE TIPO_IMPONIBLE = P_TIPO_IMPONIBLE
       AND CLAVE_IMPONIBLE = P_CLAVE_IMPONIBLE
       AND TRUNC(SYSDATE) BETWEEN FECHA_OCUPACION AND NVL(FECHA_DESOCUPACION, TRUNC(SYSDATE));

  V_CONTRIBUYENTE_X_BOLETA TBL_CONTRIBUYENTE_X_BOLETA%ROWTYPE;

  CURSOR CONTRIBUYENTE_BOLETA ( P_DOMICILIO  TBL_CONTRIBUYENTE_X_BOLETA%ROWTYPE) IS
             SELECT  V.TIPO_IMPONIBLE
                    ,V.CLAVE_IMPONIBLE
                    ,V.IMPUESTO
                    ,NULL  IMPUESTO_DESC
                    ,V.CONCEPTO_OBLIGACION
                    ,V.NUMERO_OBLIGACION_IMPUESTO
                    ,V.ANIO
                    ,V.NUMERO_CUOTA
                    ,V.NUMERO_RECTIFICATIVA
                    ,V.CONTRIBUYENTE                CONTRIBUYENTE
                    ,P_DOMICILIO.TIPO_DIRECCION     TIPO_DIRECCION
                    ,NVL (P_DOMICILIO.CALLE_DESC,P_DOMICILIO.DOMICILIO) DOMICILIO
                    ,NULL   IMPUESTO_ABREV
                    ,V.NUMERO_FACTURA  COMPROBANTE
                    ,NULL 







 RAZON_SOCIAL

                    ,NULL  CUIT
                    ,NULL  NUMERO_DOCUMENTO
                    ,PA_IMPONIBLE.GET_IDENTIFICACION_LARGA(V.TIPO_IMPONIBLE,V.CLAVE_IMPONIBLE) IMPONIBLES_DET
                    ,0 IMPORTE_DEUDA
                    ,GET_FECHA_PRIMER_VTO (V.FECHA_PRIMER_VENCIMIENTO,
                                          V.FECHA_SEGUNDO_VENCIMIENTO,
                                          P_FECHA_VENCIMIENTO)  FECHA_PRIMER_VENCIMIENTO
                    ,NULL IMPORTE_PRIMER_VENCIMIENTO
                    ,NULL IMPORTE_VENCIMIENTO_SIN_COND
                    ,GET_FECHA_SEGUNDO_VTO (V.FECHA_PRIMER_VENCIMIENTO,
                                           V.FECHA_SEGUNDO_VENCIMIENTO,
                                           P_FECHA_VENCIMIENTO)  FECHA_SEGUNDO_VENCIMIENTO
                    
                    ,GET_IMPORTE_SEGUNDO_VTO (V.IMPUESTO,
                                             V.CONCEPTO_OBLIGACION,
                                             V.NUMERO_OBLIGACION_IMPUESTO,
                                             V.NUMERO_RECTIFICATIVA,
                                             V.NUMERO_CUOTA,
                                             V.FECHA_PRIMER_VENCIMIENTO,
                                             V.FECHA_SEGUNDO_VENCIMIENTO,
                                             P_FECHA_VENCIMIENTO,
                                             P_IMPUESTO_UNIFICADO
) IMPORTE_SEGUNDO_VENCIMIENTO
                    
                    ,NULL IMPORTE_SEG_VTO_SIN_COND
                    ,P_DOMICILIO.MANZANA            MANZANA
                    ,P_DOMICILIO.CASA               CASA
                    ,P_DOMICILIO.TORRE              TORRE
                    ,P_DOMICILIO.PISO               PISO
                    ,P_DOMICILIO.DEPARTAMENTO       DEPARTAMENTO
                    ,P_DOMICILIO.PUERTA             PUERTA
                    ,P_DOMICILIO.CALLE              CALLE
                    ,P_DOMICILIO.BARRIO             BARRIO
                    ,P_DOMICILIO.LOCALIDAD          LOCALIDAD
                    ,P_DOMICILIO.MUNICIPIO          MUNICIPIO
                    ,P_DOMICILIO.PROVINCIA          PROVINCIA
                    ,P_DOMICILIO.PAIS               PAIS
                    ,P_DOMICILIO.CODIGO_POSTAL      CODIGO_POSTAL
                    ,P_DOMICILIO.PARAJE             PARAJE
                    ,P_DOMICILIO.PAIS_DESC          PAIS_DESC
                    ,P_DOMICILIO.PROVINCIA_DESC     PROVINCIA_DESC
                    ,P_DOMICILIO.MUNICIPIO_DESC     MUNICIPIO_DESC
                    ,P_DOMICILIO.LOCALIDAD_DESC     LOCALIDAD_DESC
                    ,P_DOMICILIO.BARRIO_DESC        BARRIO_DESC
                    ,P_DOMICILIO.CALLE_DESC         CALLE_DESC
                    ,V.FECHA_PRIMER_VENCIMIENTO     FECHA_VENCIMIENTO_CUOTA
                    ,V.PLAN_VIVIENDA_ID             PLAN_VIVIENDA_ID
                    ,V.PLAN_FACILIDAD
                    ,V.NUMERO_BOLETA
               FROM TBL_VENCIMIENTOS V
              WHERE P_IMPUESTO = V.IMPUESTO
                    AND P_CONCEPTO_OBLIGACION = V.CONCEPTO_OBLIGACION
                    AND P_NUMERO_OBLIGACION_IMPUESTO = V.NUMERO_OBLIGACION_IMPUESTO
                    AND P_NUMERO_RECTIFICATIVA = V.NUMERO_RECTIFICATIVA
                    AND P_NUMERO_CUOTA = V.NUMERO_CUOTA;
                                      --ROWID = P_ROWID ;

BEGIN


  V_MUESTRA_CONDOMINO := UPPER(FN_RECUPERAR_PARAMETRO('MUESTRA_CONDOMINO'));

  FOR C IN CONTRIBUYENTE_BOLETA (P_DOMICILIO) LOOP
      BEGIN

        V_RAZON_SOCIAL    := NULL ;
        V_CUIT            := NULL ;
        V_TIPO_DOCUMENTO  := NULL ;
        V_DOCUMENTO       := NULL ;

        V_IMPUESTO_DESCR  := NULL ;
        V_IMPUESTO_ABREV  := NULL ;

        BEGIN
          SELECT DECODE(P.PERSONERIA,
                              'J', P.RAZON_SOCIAL || DECODE(P.NOMBRE_FANTASIA, NULL, '', ' ('||P.NOMBRE_FANTASIA||')'),
                        'F', P.RAZON_SOCIAL || DECODE(P.NOMBRE, NULL, '', ', '||P.NOMBRE || DECODE(P.NOMBRE_FANTASIA, NULL, '', ' ('||P.NOMBRE_FANTASIA||')'))
                        ) , SUBSTR(P.CUIT,1,2) || '-' ||
                 SUBSTR(P.CUIT, 3, 8) || '-' ||
                 SUBSTR(P.CUIT, 11, 1),  TIPO_DOCUMENTO,NUMERO_DOCUMENTO

            INTO V_RAZON_SOCIAL, V_CUIT, V_TIPO_DOCUMENTO, V_DOCUMENTO
            FROM TBL_PERSONAS P
           WHERE P.PERSONA_ID = C.CONTRIBUYENTE;

          SELECT  IMPUESTO_ABREV,IMPUESTO_DESCR
            INTO    V_IMPUESTO_ABREV,V_IMPUESTO_DESCR
            FROM    TBL_IMPUESTOS
           WHERE   IMPUESTO = C.IMPUESTO;

        EXCEPTION
          WHEN OTHERS THEN NULL ;
        END ;

        IF V_MUESTRA_CONDOMINO = 'S' THEN
           BEGIN
              V_RS_CONCATENADA := NULL;

              FOR I IN C_OCUPANTES ( P_CLAVE_IMPONIBLE
                                    ,P_TIPO_IMPONIBLE) LOOP

                V_RAZON_SOCIAL_COTIT    := NULL;
                BEGIN
                  SELECT PERSONA_DESCR
                     INTO V_RAZON_SOCIAL_COTIT
                  FROM VW_PERSONAS
                  WHERE PERSONA_ID = I.PERSONA_ID ;
                EXCEPTION
                  WHEN OTHERS THEN NULL ;
                END ;

                IF V_RS_CONCATENADA IS NULL THEN
                   V_RS_CONCATENADA := V_RAZON_SOCIAL_COTIT;
                ELSE
                  V_RS_CONCATENADA := V_RS_CONCATENADA||'-'||V_RAZON_SOCIAL_COTIT;
                END IF;

              END LOOP;

                V_RAZON_SOCIAL := SUBSTR(V_RAZON_SOCIAL || '-'|| V_RS_CONCATENADA,1,60);

          EXCEPTION
              WHEN OTHERS THEN NULL ;
          END ;

        ELSE

          V_RAZON_SOCIAL := SUBSTR(V_RAZON_SOCIAL,1,60);

        END IF ;

        V_CONTRIBUYENTE_X_BOLETA.NUMERO_BOLETA                   := C.NUMERO_BOLETA                  ;
        V_CONTRIBUYENTE_X_BOLETA.TIPO_IMPONIBLE                  := C.TIPO_IMPONIBLE                 ;
        V_CONTRIBUYENTE_X_BOLETA.CLAVE_IMPONIBLE                 := C.CLAVE_IMPONIBLE                ;
        V_CONTRIBUYENTE_X_BOLETA.IMPUESTO                        := C.IMPUESTO                       ;

        
        V_CONTRIBUYENTE_X_BOLETA.IMPUESTO_DESC                   := V_IMPUESTO_DESCR; 

        V_CONTRIBUYENTE_X_BOLETA.CONCEPTO_OBLIGACION             := C.CONCEPTO_OBLIGACION            ;
        V_CONTRIBUYENTE_X_BOLETA.NUMERO_OBLIGACION_IMPUESTO      := C.NUMERO_OBLIGACION_IMPUESTO     ;
        V_CONTRIBUYENTE_X_BOLETA.ANIO                            := C.ANIO                           ;
        V_CONTRIBUYENTE_X_BOLETA.NUMERO_CUOTA                    := C.NUMERO_CUOTA                   ;
        V_CONTRIBUYENTE_X_BOLETA.NUMERO_RECTIFICATIVA            := C.NUMERO_RECTIFICATIVA           ;
        V_CONTRIBUYENTE_X_BOLETA.CONTRIBUYENTE                   := C.CONTRIBUYENTE                  ;
        V_CONTRIBUYENTE_X_BOLETA.TIPO_DIRECCION                  := C.TIPO_DIRECCION                 ;
        V_CONTRIBUYENTE_X_BOLETA.DOMICILIO                       := C.DOMICILIO                      ;

        
        V_CONTRIBUYENTE_X_BOLETA.IMPUESTO_ABREV                  := V_IMPUESTO_ABREV; 

        V_CONTRIBUYENTE_X_BOLETA.COMPROBANTE                     := C.COMPROBANTE                    ;

        
        V_CONTRIBUYENTE_X_BOLETA.RAZON_SOCIAL                    := V_RAZON_SOCIAL; 

        
        V_CONTRIBUYENTE_X_BOLETA.CUIT                            := V_CUIT; 

        
        V_CONTRIBUYENTE_X_BOLETA.NUMERO_DOCUMENTO                := V_TIPO_DOCUMENTO ||' '|| V_DOCUMENTO; 

        V_CONTRIBUYENTE_X_BOLETA.IMPONIBLES_DET                  := C.IMPONIBLES_DET                 ;
        V_CONTRIBUYENTE_X_BOLETA.IMPORTE_DEUDA                   := C.IMPORTE_DEUDA                  ;
        V_CONTRIBUYENTE_X_BOLETA.FECHA_PRIMER_VENCIMIENTO        := TRUNC(C.FECHA_PRIMER_VENCIMIENTO);
        V_CONTRIBUYENTE_X_BOLETA.IMPORTE_PRIMER_VENCIMIENTO      := C.IMPORTE_PRIMER_VENCIMIENTO     ;
        V_CONTRIBUYENTE_X_BOLETA.IMPORTE_VENCIMIENTO_SIN_COND    := C.IMPORTE_VENCIMIENTO_SIN_COND   ;
        V_CONTRIBUYENTE_X_BOLETA.FECHA_SEGUNDO_VENCIMIENTO       := C.FECHA_SEGUNDO_VENCIMIENTO      ;
        V_CONTRIBUYENTE_X_BOLETA.IMPORTE_SEGUNDO_VENCIMIENTO     := C.IMPORTE_SEGUNDO_VENCIMIENTO    ;
        V_CONTRIBUYENTE_X_BOLETA.IMPORTE_SEG_VTO_SIN_COND        := C.IMPORTE_SEG_VTO_SIN_COND       ;
        V_CONTRIBUYENTE_X_BOLETA.MANZANA                         := C.MANZANA                        ;
        V_CONTRIBUYENTE_X_BOLETA.CASA                            := C.CASA                           ;
        V_CONTRIBUYENTE_X_BOLETA.TORRE                           := C.TORRE                          ;
        V_CONTRIBUYENTE_X_BOLETA.PISO                            := C.PISO                           ;
        V_CONTRIBUYENTE_X_BOLETA.DEPARTAMENTO                    := C.DEPARTAMENTO                   ;
        V_CONTRIBUYENTE_X_BOLETA.PUERTA                          := C.PUERTA                         ;
        V_CONTRIBUYENTE_X_BOLETA.CALLE                           := C.CALLE                          ;
        V_CONTRIBUYENTE_X_BOLETA.BARRIO                          := C.BARRIO                         ;
        V_CONTRIBUYENTE_X_BOLETA.LOCALIDAD                       := C.LOCALIDAD                      ;
        V_CONTRIBUYENTE_X_BOLETA.MUNICIPIO                       := C.MUNICIPIO                      ;
        V_CONTRIBUYENTE_X_BOLETA.PROVINCIA                       := C.PROVINCIA                      ;
        V_CONTRIBUYENTE_X_BOLETA.PAIS                            := C.PAIS                           ;
        V_CONTRIBUYENTE_X_BOLETA.CODIGO_POSTAL                   := C.CODIGO_POSTAL                  ;
        V_CONTRIBUYENTE_X_BOLETA.PARAJE                          := C.PARAJE                         ;
        V_CONTRIBUYENTE_X_BOLETA.PAIS_DESC                       := C.PAIS_DESC                      ;
        V_CONTRIBUYENTE_X_BOLETA.PROVINCIA_DESC                  := C.PROVINCIA_DESC                 ;
        V_CONTRIBUYENTE_X_BOLETA.MUNICIPIO_DESC                  := C.MUNICIPIO_DESC                 ;
        V_CONTRIBUYENTE_X_BOLETA.LOCALIDAD_DESC                  := C.LOCALIDAD_DESC                 ;
        V_CONTRIBUYENTE_X_BOLETA.BARRIO_DESC                     := C.BARRIO_DESC                    ;
        V_CONTRIBUYENTE_X_BOLETA.CALLE_DESC                      := C.CALLE_DESC                     ;
        V_CONTRIBUYENTE_X_BOLETA.FECHA_VENCIMIENTO_CUOTA         := C.FECHA_VENCIMIENTO_CUOTA        ;
        V_CONTRIBUYENTE_X_BOLETA.PLAN_VIVIENDA_ID                := C.PLAN_VIVIENDA_ID               ;
        V_CONTRIBUYENTE_X_BOLETA.PLAN_FACILIDAD                  := C.PLAN_FACILIDAD                 ;
        V_CONTRIBUYENTE_X_BOLETA.ORDEN                           := NULL ; 
        V_CONTRIBUYENTE_X_BOLETA.ORDEN_NUM                       := NULL ; 
        V_CONTRIBUYENTE_X_BOLETA.CODIGO_IMPRESION                := P_CODIGO_IMPRESION ;
        V_CONTRIBUYENTE_X_BOLETA.INFO_ADICIONAL_INMOB            := P_INFO_ADICIONAL_INMOB ;
        V_CONTRIBUYENTE_X_BOLETA.PROXIMA_FECHA_VTO               := P_PROXIMA_FECHA_VTO ;
        V_CONTRIBUYENTE_X_BOLETA.PROXIMA_CUOTA                   := P_PROXIMA_CUOTA ;
        V_CONTRIBUYENTE_X_BOLETA.DETALLE_IMPONIBLE               := P_DETALLE_IMPONIBLE ;

        PA_BOLETA.INSERTA_CONTRIBUYENTE_X_BOLETA(V_CONTRIBUYENTE_X_BOLETA ,P_MASIVA ) ;

      END ;
  END LOOP ;
END ;
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
