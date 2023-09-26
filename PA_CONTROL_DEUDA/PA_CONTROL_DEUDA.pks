CREATE OR REPLACE PACKAGE TCSTARTA.PA_CONTROL_DEUDA 
AS
 /*******************************************************************************************************************************************************
       NAME:      SIAC MULTA

       REVISIONS:
       Ver        Date        Author           Description
       ---------  ----------  ---------------  ---------------------------------------------------------------------------------------------------------------------------
       1.0        25/07/2023    JCHAUQUE       -SE TRASLADO DE PA_SIAC_MULTA EL CONTROL DE DEUDAS, PARA QUE QUEDE DE FORMA INDEPENDIENTE AL RESTO  
    ****************************************************************************************************************************************************/
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
