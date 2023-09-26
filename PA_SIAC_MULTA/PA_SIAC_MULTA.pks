CREATE OR REPLACE PACKAGE TCSGUEM.PA_SIAC_MULTA
AS
    /**********************************************************************************************************************
       NAME:      SIAC MULTA

       REVISIONS:
       Ver        Date        Author           Description
       ---------  ----------  ---------------  ---------------------------------------------------------------------------------------------------------------------------
       1.0        26/04/2023    JCHAUQUE         -CONTIENE GET MULTAS (TRAE LAS MULTAS)
       1.1        02/05/2023    JCHAUQUE         -TRAER LAS MULTAS DE TASA Y SELLO
                                                 -TRAE DETALLES DE LA MULTA
       1.2        02/05/2023    JCHAUQUE         -SE ANULO LA FUNCIÓN GET_MULTA, SE RENOMBRO GET_DETALLE_MULTA POR GET_MULTA  
       1.3        12/05/2023    JCHAUQUE         -SE AGREGO LA MULTIBOLETA 
       1.4        06/05/2023    JCHAUQUE         -SE MODIFICO LA MULTIBOLETA PARA AGREGAR VARIAS MULTAS A LA VEZ 
       1.5        10/07/2023    JCHAUQUE         -SE AGREGO EL CONTROL DE DEUDAS PARA MULTAS
       1.6        13/07/2023    JCHAUQUE         -SE CORRIGIO EL CONTROL DE DEUDA PARA QUE TRAIGA SOLO DEUDA DESDE 2014  
    ************************************************************************************************************************/
    --TRAE LA CLAVE DEL IMPONIBLE
    FUNCTION GET_CLAVE_IMPONIBLE (
    P_TIPO_IMPONIBLE    IN VARCHAR2,
    P_IMPONIBLE   IN VARCHAR2)
    RETURN VARCHAR2;
    --TRAE LA MULTA DEL IMPONIBLE
    FUNCTION GET_MULTAS_X_IMPONIBLE (P_IMPONIBLE IN TBL_IMPONIBLES.CLAVE%TYPE,
                         P_TIPO_IMPONIBLE IN TBL_IMPONIBLES.TIPO_IMPONIBLE%TYPE,
                         P_ANIO            IN NUMBER)
    RETURN SYS_REFCURSOR;
    --TRAE DETALLES DE MULTA
    FUNCTION GET_MULTA (P_MULTA IN TBL_MULTAS.MULTA_NUMERO%TYPE,
                        P_TASA_Y_SELLO   IN TBL_TASAS_Y_SELLOS.TASA_SELLO_ID%TYPE)
    RETURN SYS_REFCURSOR;
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
            sys_refcursor;
--MUESTRA LA BOLETA  
    FUNCTION boleta_simple (
      p_codigo_impresion    IN   NUMBER,
      p_fecha_vencimiento   IN   DATE,
      p_contribuyente       IN   NUMBER,
      p_tipo_imponible      IN   VARCHAR2,
      p_clave_imponible     IN   VARCHAR2,
      p_plan_facilidad      IN   NUMBER
   )
      RETURN sys_refcursor;
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
      RETURN VARCHAR2;
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
                                      ,P_MASIVA               IN   VARCHAR2 := 'N');
--CONTROL DE DEUDA 
FUNCTION CONTROL_DEUDA (
                        P_TIPO_IMPONIBLE      IN    VARCHAR2,
                        P_IMPONIBLE           IN    VARCHAR2,
                        P_FECHA_CONSULTA      IN    DATE)
RETURN VARCHAR2 ;
--FUNCION QUE SE INCLUYE EN CONTROL_DEUDA
FUNCTION tiene_deuda (
      p_tipo_imponible       VARCHAR2,
      p_clave_imponible      VARCHAR2,
      p_fecha_acreditacion   DATE,
      p_fecha_vencimiento    DATE,
      p_impuesto             VARCHAR2 := NULL,
      p_concepto             VARCHAR2 := NULL
   )
      RETURN BOOLEAN;
--FUNCION QUE SE INCLUYE EN LA FUNCION CONTROL DEUDA 
FUNCTION tiene_deuda_quiebras (
      p_tipo_imponible       VARCHAR2,
      p_clave_imponible      VARCHAR2,
      p_fecha_acreditacion   DATE,
      p_fecha_vencimiento    DATE
   )
      RETURN BOOLEAN;
END;
/
