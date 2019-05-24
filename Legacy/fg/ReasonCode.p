/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildReasonCode:
    DEFINE INPUT  PARAMETER ipCodeType  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opComboList AS CHARACTER NO-UNDO.   
    DEFINE VARIABLE cInitVal AS CHARACTER NO-UNDO.
  
     /* ensure blank option avaiable */
     ASSIGN cInitVal = ","
            opComboList = cInitVal.
     FOR EACH rejct-cd NO-LOCK
         WHERE rejct-cd.type EQ ipCodeType
         BREAK BY rejct-cd.type
         :
         opComboList = opComboList
                     + rejct-cd.code + " - "
                     + rejct-cd.dscr + ","
                     + rejct-cd.code + ","
                     .
    END. /* each rejct-cd */
    IF opComboList NE cInitVal THEN 
      opComboList = TRIM(opComboList,",").
        
END PROCEDURE.
