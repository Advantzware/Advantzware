
/*------------------------------------------------------------------------
    File        : bankrecon.p
    Purpose     : 
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttBankReconciliation NO-UNDO
    FIELD number      AS CHAR
    FIELD vdate       AS CHAR
    FIELD amt         AS DEC
    FIELD bank        AS CHAR
    FIELD vend        AS CHAR
    FIELD vendname    AS CHAR
    FIELD cleared     AS CHAR
    FIELD reckey      AS CHAR
    FIELD extra       AS CHAR 
   .

DEFINE DATASET dsBankReconciliation FOR ttBankReconciliation.
    

DEFINE INPUT PARAMETER prmAction     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmnumber     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvdate      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmamt        AS DEC NO-UNDO.
DEFINE INPUT PARAMETER prmbank       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmvend       AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmvendname   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcleared    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmreckey     AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBankReconciliation .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     

IF prmAction       = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp         = ?  THEN ASSIGN prmComp       = "".
IF prmUser         = ?  THEN ASSIGN prmUser       = "".
IF prmnumber       = ?  THEN ASSIGN prmnumber     = "".
IF prmvdate        = ?  THEN ASSIGN prmvdate      = "".
IF prmamt          = ?  THEN ASSIGN prmamt        = 0. 
IF prmbank         = ?  THEN ASSIGN prmbank       = 0. 
IF prmvend         = ?  THEN ASSIGN prmvend       = "".
IF prmvendname     = ?  THEN ASSIGN prmvendname   = "".
IF prmcleared      = ?  THEN ASSIGN prmcleared    = "".
IF prmreckey       = ?  THEN ASSIGN prmreckey     = "".


DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.

DEFINE BUFFER b-prgrms FOR prgrms.
DEF BUFFER b-ap-pay FOR ap-pay.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE v-dirname LIKE b-prgrms.DIR_group NO-UNDO.
DEFINE VARIABLE Audit_File AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.


     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

ASSIGN
    cocode = prmComp
    g_company = prmComp
    g_user    = prmUser  .


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode .

{ap/reconcil.i NEW SHARED}
DEF VAR v-can-update AS LOG NO-UNDO.
DEF VAR lv-bank LIKE ar-cash.bank-code NO-UNDO.

 RUN ap/reconcil.p.

 
IF prmAction = "Update" THEN DO:

     FIND FIRST reconcile WHERE tt-rowid = to-rowid(prmreckey) NO-LOCK NO-ERROR.

       IF AVAIL reconcile THEN DO:
                FIND CURRENT reconcile NO-LOCK NO-ERROR.
                tt-cleared = prmcleared EQ "Yes".
                IF tt-type EQ 1 THEN DO /*TRANSACTION*/ :
                    RELEASE b-ap-pay.
                    
                    FIND ap-pay WHERE ROWID(ap-pay) EQ tt-rowid NO-LOCK NO-ERROR.
                    
                    IF AVAIL ap-pay THEN
                        IF ap-pay.d-no NE 0                                              AND
                        NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN DO:
                            FIND FIRST b-ap-pay
                                WHERE b-ap-pay.company   EQ ap-pay.company
                                AND b-ap-pay.check-act EQ ap-pay.check-act
                                AND b-ap-pay.check-no  EQ ap-pay.d-no
                                EXCLUSIVE-LOCK NO-ERROR.
                        END.

                       ELSE FIND b-ap-pay WHERE ROWID(b-ap-pay) EQ ROWID(ap-pay) EXCLUSIVE-LOCK NO-ERROR.
                       
                       IF AVAIL b-ap-pay THEN DO:
                           b-ap-pay.cleared = tt-cleared.
                           
                           FOR EACH ap-pay
                               WHERE ap-pay.company EQ b-ap-pay.company
                               AND ap-pay.d-no    EQ b-ap-pay.check-no
                               AND NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
                               EXCLUSIVE-LOCK
                               USE-INDEX d-no:
                               ap-pay.cleared = b-ap-pay.cleared.
                           END.
                       END.

                  END. /*tt-type eq 1*/
                  ELSE
                      IF tt-type EQ 2 THEN DO:
                          FIND ar-cash WHERE ROWID(ar-cash) EQ tt-rowid NO-LOCK NO-ERROR.
                          IF AVAIL ar-cash THEN DO:
                              lv-bank = ar-cash.bank-code.
                              RELEASE ar-cash.
                              FOR EACH tt-cash
                                  WHERE tt-trnum EQ INT(SUBSTR(tt-number,4,10))
                                  USE-INDEX tt-trnum,

                                  FIRST ar-cash EXCLUSIVE-LOCK
                                  WHERE ROWID(ar-cash)     EQ tt-cash.row-id
                                  AND ar-cash.reconciled EQ NO
                                  AND ar-cash.posted     EQ YES
                                  AND ar-cash.memo       EQ NO
                                  AND ar-cash.bank-code  EQ lv-bank,

                                  FIRST bank NO-LOCK
                                  WHERE bank.company   EQ ar-cash.company
                                  AND bank.bank-code EQ ar-cash.bank-code
                                  USE-INDEX bank
                                  /*TRANSACTION*/ :
                                  ar-cash.cleared = tt-cleared.
                              END.
                          END.
                      END.

                      ELSE
                          IF tt-type EQ 3 THEN DO /*TRANSACTION*/ :
                              FIND gl-jrn WHERE ROWID(gl-jrn) EQ tt-rowid EXCLUSIVE-LOCK NO-ERROR.
                              IF AVAIL gl-jrn THEN gl-jrn.cleared = tt-cleared.
                          END.
                          ELSE
                              IF tt-type EQ 4 THEN
                                  FOR EACH ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-rowid,
                                  FIRST ar-mcash-ref
                                  WHERE ar-mcash-ref.rec_key  EQ ar-mcash.rec_key
                                  AND ar-mcash-ref.reftable EQ "ar-mcash-ref"
                                  AND ar-mcash-ref.company  EQ "ar-mcash"
                                  USE-INDEX rec_key
                                  /*TRANSACTION*/ EXCLUSIVE-LOCK:
                                  ar-mcash-ref.val[2] = INT(tt-cleared).
                              END.
            END.
 


/*RUN get-security.*/
ASSIGN prmAction = "View" .
 
END. /*IF prmAction = "update" THEN DO:*/



 IF prmAction = "Search" THEN do:
  FOR EACH reconcile WHERE (tt-vend = prmvend OR prmvend = "") AND
                           (tt-bank = prmvendname OR prmvendname = "") AND
                           (tt-number = prmnumber OR prmnumber = "")    NO-LOCK BY tt-bank BY tt-number:

  CREATE ttBankReconciliation.
           ASSIGN 
                 ttBankReconciliation.number       = tt-number
                 ttBankReconciliation.vdate        = string(tt-date)
                 ttBankReconciliation.amt          = tt-amt
                 ttBankReconciliation.bank         = tt-bank
                 ttBankReconciliation.vend         = tt-vend
                 ttBankReconciliation.vendname     = tt-name
                 ttBankReconciliation.cleared      = string(tt-cleared)

                 ttBankReconciliation.reckey       = string(tt-rowid)
                 ttBankReconciliation.extra        = string(tt-type)   
                     .

  END. /* end for loop*/
 END. /* end search*/ 


 IF prmAction = "View" THEN do:
  FIND FIRST reconcile WHERE tt-rowid = TO-ROWID(prmreckey) NO-LOCK NO-ERROR.
   IF AVAIL reconcile  THEN do:
  CREATE ttBankReconciliation.
           ASSIGN 
                 ttBankReconciliation.number       = tt-number
                 ttBankReconciliation.vdate        = string(tt-date)
                 ttBankReconciliation.amt          = tt-amt
                 ttBankReconciliation.bank         = tt-bank
                 ttBankReconciliation.vend         = tt-vend
                 ttBankReconciliation.vendname     = tt-name
                 ttBankReconciliation.cleared      = string(tt-cleared)

                 ttBankReconciliation.reckey       = string(tt-rowid)
                 ttBankReconciliation.extra        = string(tt-type)   
                     .
   
  END. /* avail*/
 END. /* end search*/ 
