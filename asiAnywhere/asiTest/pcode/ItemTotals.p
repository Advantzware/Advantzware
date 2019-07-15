
/*------------------------------------------------------------------------
    File        : ItemTotal.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ItemTotal.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  AS Character no-undo.
DEFINE INPUT PARAMETER prmItemNum   AS Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemTotal.

DEFINE VARIABLE v-return-value AS LOGICAL NO-UNDO.
Define STREAM s1.
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmAction     = ? THEN ASSIGN prmAction     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmItemNum  = ? THEN ASSIGN prmItemNum  = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "select" THEN DO:
        
        RUN build-qry .
                
        DATASET dsItemTotal:FILL().
    END.
    WHEN "delete" THEN DO:
        OUTPUT STREAM s1 TO ItemTotal.txt.
        FOR EACH beforeItemTotal TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeItemTotal:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
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
        FOR EACH beforeItemTotal TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeItemTotal:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
               
        END.
        
    END.
    WHEN "insert" THEN DO:
        FOR EACH beforeItemTotal TRANSACTION: 
            ASSIGN v-return-value = BUFFER beforeItemTotal:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
        END. 
    END.
END CASE.

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
MESSAGE "test" prmAction prmComp prmItemNum.
PROCEDURE build-qry:
/*FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
    oe-ordl.ord-no = int(prmOrderNum) AND
    oe-ordl.LINE = int(prmItemNum)
    NO-LOCK,*/
    
   FIND FIRST itemfg where
          itemfg.company EQ prmComp AND
          itemfg.i-no = prmItemNum
          no-lock NO-ERROR.
   IF AVAIL itemfg THEN DO:
 
    create ttItemTotal.
    assign 
        ttItemTotal.i-no        = itemfg.i-no
        ttItemTotal.i-name      = itemfg.i-name
        ttItemTotal.q-ptd       = itemfg.q-ptd
        ttItemTotal.q-ord-ytd   = itemfg.q-ord-ytd
        ttItemTotal.u-ord       = itemfg.u-ord
        ttItemTotal.q-prod-ptd  = itemfg.q-prod-ptd
        ttItemTotal.q-prod-ytd  = itemfg.q-prod-ytd
        ttItemTotal.u-prod      = itemfg.u-prod
        ttItemTotal.q-ship-ptd  = itemfg.q-ship-ptd
        ttItemTotal.q-ship-ytd  = itemfg.q-ship-ytd
        ttItemTotal.u-ship      = itemfg.u-ship
        ttItemTotal.q-inv-ptd   = itemfg.q-inv-ptd
        ttItemTotal.q-inv-ytd   = itemfg.q-inv-ytd
        ttItemTotal.u-inv       = itemfg.u-inv
        ttItemTotal.ptd-msf     = itemfg.ptd-msf[1]
        ttItemTotal.ytd-msf     = itemfg.ytd-msf
        ttItemTotal.lyytd-msf   = itemfg.lyytd-msf
        ttItemTotal.l-score     = itemfg.l-score[50]
        ttItemTotal.t-len       = itemfg.t-len    
        ttItemTotal.w-score     = itemfg.w-score[50]
        ttItemTotal.t-wid       = itemfg.t-wid
        ttItemTotal.d-score     = itemfg.d-score[50]
        ttItemTotal.t-sqin      = itemfg.t-sqin
        ttItemTotal.t-sqft      = itemfg.t-sqft
        ttItemTotal.cust-no     = itemfg.cust-no
        ttItemTotal.part-no     = itemfg.part-no.

    FIND FIRST reftable WHERE reftable.reftable EQ "FACTORED"
                        AND reftable.company  EQ itemfg.company
                        AND reftable.loc      EQ ""
                        AND reftable.code     EQ itemfg.i-no
                        NO-LOCK NO-ERROR.

    IF AVAIL reftable THEN
       ttItemTotal.factor = reftable.code2 .
  END.  /* IF AVAIL itemfg THEN DO:**/
 
 END PROCEDURE.


