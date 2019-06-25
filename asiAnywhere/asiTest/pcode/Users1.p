
/*------------------------------------------------------------------------
    File        : User.p
    Purpose     : Users Maintenance

    Syntax      :

    Description : Return a Dataset of all Users

    Author(s)   : kuldeep
    Created     : 28 sep 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{Users1.i}
DEFINE INPUT PARAMETER prmAction  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem    AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUsers.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

DEFINE STREAM s1.
/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "select" THEN DO:
        
        RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).
        
        ASSIGN
            v-qry-handle = QUERY q-UsersQuery:HANDLE.
                
        v-qry-handle:QUERY-PREPARE(v-qry-string).
        
        DATASET dsUsers:FILL().
    END.
    WHEN "delete" THEN DO:
        OUTPUT STREAM s1 TO catalog.txt.
        FOR EACH beforeUsers TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeUsers:SAVE-ROW-CHANGES("users") NO-ERROR.
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
        FOR EACH beforeUsers TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeUsers:SAVE-ROW-CHANGES("users") NO-ERROR.
               
        END.
        
    END.
    WHEN "insert" THEN DO:
        FOR EACH beforeUsers TRANSACTION: 
            ASSIGN v-return-value = BUFFER beforeUsers:SAVE-ROW-CHANGES("users") NO-ERROR.
        END. 
    END.
END CASE.

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    IF prmItem = "" THEN
        ASSIGN
            prm-query = "FOR EACH users NO-LOCK".
    ELSE
        ASSIGN
             prm-query = "FOR EACH users WHERE users.user_id = '" + prmItem + "' NO-LOCK".    
END PROCEDURE.
