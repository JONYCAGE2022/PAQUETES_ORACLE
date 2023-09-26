CREATE OR REPLACE PACKAGE TCSSANLO.PA_SIAC_PLANES
AS
    /******************************************************************************
       NAME:      PAQUETE PLANES
       PORPUSE:   PLANES PARA SIAC

       REVISIONS:
       Ver        Date        Author           Description
       ---------  ----------  ---------------  ------------------------------------
       1.6        14/12/2022    JCHAUQUE         -CONTIENE CONTRIBUYENTES CON PLANES,
                                                 CANTIDAD DE PLANES Y DETALLE .
       Ver        Date        Author           Description
       ---------  ----------  ---------------  ------------------------------------
       1.7        14/02/2023    JCHAUQUE         -SE AGREGO LA FUNCION PARA GENERAR BOLETA
       
       Ver        Date        Author           Description
       ---------  ----------  ---------------  ------------------------------------
       1.8        1/03/2023    JCHAUQUE         -SE CORRIGIO LA EMISION DE BOLETA
    ******************************************************************************/
--FUNCION QUE TRAE LOS DATOS DEL CONTRIBUYENTE SI ES QUE TIENE UN PLAN
FUNCTION GET_CONTRIBUYENTE_PLAN
    RETURN T_CONTRIBUYENTE;    
-- FUNCION QUE TRAE LOS DATOS DEL CONTRIBUYENTE QUE TIENE PLAN POR MEDIO DE LA RAZON SOCIAL
FUNCTION GET_LISTA_RAZON_SOCIAL (P_RAZON_SOCIAL TBL_PERSONAS.RAZON_SOCIAL%TYPE)
RETURN T_CONTRIBUYENTE;
-- FUNCION QUE TRAE LOS PLANES QUE POSEE EL CONTRIBUYENTE 
FUNCTION GET_PLAN_X_CUIT (
    P_CUIT   IN TBL_PERSONAS.CUIT%TYPE,
    P_DNI    IN TBL_PERSONAS.NUMERO_DOCUMENTO%TYPE,
    P_RAZON_SOCIAL IN TBL_PERSONAS.RAZON_SOCIAL%TYPE)
    RETURN T_PLAN;
-- FUNCION QUE TRAE CON DETALLES UN PLAN (AUN EN PROCESO)
FUNCTION GET_DETALLE_PLAN (
    P_NUMERO_ACOGIMIENTO IN TBL_PLANES_FACILIDADES.NUMERO_ACOGIMIENTO%TYPE)
    RETURN T_DET_PLAN;
--CONTRIBUYENTES CON PLANES PERO CON RETORNO EN CURSOR
FUNCTION GET_CONTRIBUYENTE_PLAN_CURSOR 
      RETURN SYS_REFCURSOR;
--CONTROL PARA RAZON SOCIAL 
FUNCTION CONTROL(P_RAZON_SOCIAL IN TBL_PERSONAS.RAZON_SOCIAL%TYPE)
RETURN NUMBER;
--GENERA BOLETA PARA PLANES 
FUNCTION GENERADOR_BOLETA_PLANES (
      p_cuotas_multiples IN   VARCHAR2,
      p_plan_facilidad   IN   NUMBER,   
      p_fecha_emision    IN   DATE
   )
      RETURN sys_refcursor;
--FUNCION QUE TRAE LA CLAVE DEL IMPONIBLE
FUNCTION F_GET_IMPONIBLE (
                                            P_CLAVE_IMPONIBLE   IN  TBL_IMPONIBLES.CLAVE%TYPE,
                                            P_TIPO_IMPONIBLE    IN  TBL_IMPONIBLES.TIPO_IMPONIBLE%TYPE)
RETURN VARCHAR2;

--BUSCA DOMICLIO PARA BOLETA
FUNCTION get_domicilio_contrib2(P_TIPO_IMPONIBLE   TBL_TIPOS_IMPONIBLES.TIPO_IMPONIBLE%TYPE
                              ,P_CLAVE_IMPONIBLE  TBL_IMPONIBLES.CLAVE%TYPE
                              ,P_CONTRIBUYENTE    TBL_PERSONAS.PERSONA_ID%TYPE
                              ,P_TIPO_DIRECCION   TBL_TIPOS_DIRECCIONES.TIPO_DIRECCION%TYPE)
                               RETURN VARCHAR2;
--INSERTA BOLETA Y GENERA CODIGO DE IMPRESION
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
      RETURN VARCHAR2;
--GENERA CODIGO DE BARRA
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
      RETURN sys_refcursor;
END;
/
