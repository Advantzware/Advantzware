/* getID.i - used in prompts/ID.w and config.w */

/*------------------------------------------------------------------------------
  Purpose:     get installation identifier dir names
  Parameters:  starting directory, return directory name
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipSearchDir AS CHARACTER NO-UNDO.

  DEFINE OUTPUT PARAMETER opListItems AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE fileName AS CHARACTER FORMAT 'X(30)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.

  INPUT FROM OS-DIR(ipSearchDir) NO-ECHO.
  REPEAT:
    SET fileName ^ attrList.
    IF attrList EQ 'f' OR fileName BEGINS '.' THEN
    NEXT.
    opListItems = IF opListItems EQ '' THEN fileName
                  ELSE opListItems + ',' +  fileName.
  END.
  INPUT CLOSE.
