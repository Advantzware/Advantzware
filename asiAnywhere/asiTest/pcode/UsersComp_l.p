
/*------------------------------------------------------------------------
    File        : UserComp_l.p
    Purpose     : UserComp_l Maintenance

    Syntax      :

    Description : Return a Dataset of all UserComp_l

    Author(s)   : kuldeep
    Created     : 28 sep 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{UsersComp_l.i}

DEFINE INPUT PARAMETER prmAction  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vComp_l    AS CHARACTER  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUserComp_l.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

DEFINE STREAM s1.
/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "select" THEN DO:
        RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).        
        q-UserComp_lQuery:QUERY-PREPARE(v-qry-string).        
        DATASET dsUserComp_l:FILL().
    END.
    WHEN "delete" THEN DO:
        OUTPUT STREAM s1 TO catalog.txt.
        FOR EACH beforeUserComp_l TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeUserComp_l:SAVE-ROW-CHANGES("usercomp") NO-ERROR.
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
        FOR EACH beforeUserComp_l TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeUserComp_l:SAVE-ROW-CHANGES("usercomp") NO-ERROR.
               
        END.
        
    END.
    WHEN "insert" THEN DO:
        FOR EACH beforeUserComp_l TRANSACTION: 
            ASSIGN v-return-value = BUFFER beforeUserComp_l:SAVE-ROW-CHANGES("usercomp") NO-ERROR.
        END. 
    END.
END CASE.

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    
    IF prmItem = "" THEN
        ASSIGN
            prm-query = "FOR EACH usercomp NO-LOCK".
   ELSE 
      ASSIGN prm-query = " PRESELECT EACH users NO-LOCK WHERE users.user_id = '" + prmItem + "'  "
             prm-query = prm-query +  " , EACH usercomp OF users where usercomp.loc NE '' and usercomp.company = '" + vComp_l + "' no-lock".

END PROCEDURE.
