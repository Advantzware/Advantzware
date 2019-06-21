
/*------------------------------------------------------------------------
    File        : UserVend.p
    Purpose     : UserVend Maintenance

    Syntax      :

    Description : Return a Dataset of all UserVend

    Author(s)   : kuldeep
    Created     : 28 sep 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{UserVend.i}

DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmId      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUserVend.

define var vComp as char.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

DEFINE STREAM s1.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "select" THEN DO:
        RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).        
        q-UserVendQuery:QUERY-PREPARE(v-qry-string).        
        DATASET dsUserVend:FILL().
    END.
    WHEN "delete" THEN DO:
        OUTPUT STREAM s1 TO catalog.txt.
        FOR EACH beforeUserVend TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeUserVend:SAVE-ROW-CHANGES("uservend") NO-ERROR.
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
        FOR EACH beforeUserVend TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeUserVend:SAVE-ROW-CHANGES("uservend") NO-ERROR.
               
        END.
        
    END.
    WHEN "insert" THEN DO:
        FOR EACH beforeUserVend TRANSACTION: 
            ASSIGN v-return-value = BUFFER beforeUserVend:SAVE-ROW-CHANGES("uservend") NO-ERROR.
        END. 
    END.
END CASE.

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
   DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
   
   IF vComp <> "" THEN
      ASSIGN prm-query = " PRESELECT EACH users NO-LOCK WHERE users.user_id = '" + prmId + "'  "
             prm-query = prm-query +  " , EACH UserVend OF users where uservend.vend_no = "
                       + QUOTER(vComp)
                       + " AND uservend.company eq " + QUOTER(prmComp)
                       + " no-lock".
   ELSE 
      ASSIGN prm-query = " PRESELECT EACH users NO-LOCK WHERE users.user_id = "
                       + QUOTER(prmId) + "  "
             prm-query = prm-query +  " , EACH uservend OF users WHERE uservend.company EQ "
                       + QUOTER(prmComp)
                       + " no-lock".
             
END PROCEDURE.
