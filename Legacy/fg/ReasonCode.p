

/* **********************  Internal Procedures  *********************** */


PROCEDURE pBuildReasonCode:
    DEFINE INPUT PARAMETER ipCodeType AS CHARACTER.
    DEFINE OUTPUT PARAMETER opComboList AS CHARACTER.
   
  
 FOR EACH rejct-cd NO-LOCK WHERE rejct-cd.TYPE EQ ipCodeType BREAK BY rejct-cd.TYPE .
       IF NOT LAST(rejct-cd.TYPE) THEN
           ASSIGN opComboList = opComboList
           + rejct-cd.CODE + " - "
           + rejct-cd.dscr + ","
           + rejct-cd.CODE + "," .
       ELSE
           ASSIGN opComboList = opComboList
           + rejct-cd.CODE + " - "
           + rejct-cd.dscr + ","
           + rejct-cd.CODE  .
   END.
        
END PROCEDURE.
