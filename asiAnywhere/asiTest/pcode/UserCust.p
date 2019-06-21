
/*------------------------------------------------------------------------
    File        : UserCust.p
    Purpose     : UserCust Maintenance

    Syntax      :

    Description : Return a Dataset of all UserCust

    Author(s)   : kuldeep
    Created     : 28 sep 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{UserCust.i}

DEFINE INPUT PARAMETER prmAction  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmId      AS CHARACTER  NO-UNDO.
/*DEFINE INPUT PARAMETER vComp      AS CHARACTER  NO-UNDO.*/
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUserCust.

define var vComp as char.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

DEFINE STREAM s1.
/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "select" THEN DO:
        RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).        
        q-UserCustQuery:QUERY-PREPARE(v-qry-string).        
        DATASET dsUserCust:FILL().

        
    END.
    WHEN "delete" THEN DO:
        OUTPUT STREAM s1 TO catalog.txt.
        FOR EACH beforeUserCust TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeUserCust:SAVE-ROW-CHANGES("usercust") NO-ERROR.
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
        FOR EACH beforeUserCust TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeUserCust:SAVE-ROW-CHANGES("usercust") NO-ERROR.
               
        END.
        
    END.
    WHEN "insert" THEN DO:
        FOR EACH beforeUserCust TRANSACTION: 
            ASSIGN v-return-value = BUFFER beforeUserCust:SAVE-ROW-CHANGES("usercust") NO-ERROR.
        END. 
    END.
END CASE.

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    
 

     

    IF  vComp <> "" THEN do:
        ASSIGN prm-query = " PRESELECT EACH users NO-LOCK WHERE users.user_id = '" + prmId + "'  ".
        assign prm-query = prm-query +  " , EACH usercust OF users where usercust.cust_no = '" + vComp + "' no-lock".


    end. 
   ELSE do:
   
        ASSIGN prm-query = " PRESELECT EACH users NO-LOCK WHERE users.user_id = '" + prmId + "'  ".
        assign prm-query = prm-query +  " , EACH usercust OF users  no-lock".

   END.
  
             
             
END PROCEDURE.
