PROCEDURE udf-image :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-log AS LOG NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      IF NOT ip-log THEN
         UDF:LOAD-IMAGE("Graphics/32x32/window_dialog.png").
      ELSE
         UDF:LOAD-IMAGE("Graphics/32x32/window_dialog_star.png").
   END.
END PROCEDURE.
