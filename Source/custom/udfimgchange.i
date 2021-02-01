PROCEDURE udf-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-udf AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   v-udf = CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = {&internal-tables}.rec_key). 

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'udfmsg-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN udf-image IN WIDGET-HANDLE(char-hdl) (INPUT v-udf).
END PROCEDURE.
