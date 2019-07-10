
/*------------------------------------------------------------------------
    File        : ProgramMaster.p
    Purpose     : Program Maintenance

    Syntax      :

    Description : Return a Dataset of all Program

    Author(s)   : kuldeep
    Created     : 2 oct 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ProgramMaster.i}
DEFINE INPUT PARAMETER prmAction  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER programName    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vTitle   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER viewId    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER addId     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER updateId    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER deleteId    AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsProg.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

IF prmItem = ? THEN ASSIGN prmItem = "".
IF programName    = ? THEN ASSIGN programName    = "".
IF vTitle   = ? THEN ASSIGN vTitle   = "".
IF viewId    = ? THEN ASSIGN viewId    = "".
IF addId     = ? THEN ASSIGN addId     = "".
IF updateId    = ? THEN ASSIGN updateId    = "".
IF deleteId    = ? THEN ASSIGN deleteId    = "".



DEFINE STREAM s1.
/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
when "search" then do:
  FOR EACH prgrms WHERE (prgrms.prgmname begins programName or programName = "" )
                    and (prgrms.prgtitle begins vTitle or vTitle = "" )
                    and (prgrms.can_run  = viewId or viewId  = "" )
                    and (prgrms.can_create  = addId or addId = "" )
                    and (prgrms.can_update  = updateId or updateId = "" )
                    and (prgrms.can_delete  = deleteId or deleteId = "" )
                    no-lock:
                    create ttProg .
                     BUFFER-COPY prgrms TO ttProg .
 End.
End. 
  
    WHEN "select" THEN DO:
        
        RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).
        
        ASSIGN
            v-qry-handle = QUERY q-ProgQuery:HANDLE.
                
        v-qry-handle:QUERY-PREPARE(v-qry-string).
        
        DATASET dsProg:FILL().
    END.
    WHEN "delete" THEN DO:
        OUTPUT STREAM s1 TO catalog.txt.
        FOR EACH beforeProg TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeProg:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
        END.
        IF NOT v-return-value THEN 
	    DO:
	    	EXPORT STREAM s1 ERROR-STATUS:GET-NUMBER(1) ":" ERROR-STATUS:GET-MESSAGE(1).
	    END.
	    ELSE DO:
		    EXPORT STREAM s1 "Deleted.".    
        END.
        OUTPUT STREAM s1 CLOSE.
    END.
    WHEN "update" THEN DO:
        FOR EACH beforeProg TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeProg:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
               
        END.
        
    END.
    WHEN "insert" THEN DO:
        FOR EACH beforeProg TRANSACTION: 
            ASSIGN v-return-value = BUFFER beforeProg:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
        END. 
    END.
END CASE.

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    IF prmItem = "" THEN
        ASSIGN
            prm-query = "FOR EACH prgrms NO-LOCK".
    ELSE
        ASSIGN
             prm-query = "FOR EACH prgrms WHERE prgrms.prgmname = '" + prmItem + "' NO-LOCK".    
END PROCEDURE.
