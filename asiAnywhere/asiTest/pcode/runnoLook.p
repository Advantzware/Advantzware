/*------------------------------------------------------------------------
    File        : runnolook.p
    Purpose     : Run#

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Feb 11,2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttRunNumberLookRun NO-UNDO 
    FIELD runno       AS INT 
    FIELD jrnl        AS CHAR
    FIELD vdate       AS CHAR     
    FIELD dscr        AS CHAR.

DEFINE DATASET dsRunNumberLookRun FOR ttRunNumberLookRun.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRunNumberLookRun.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEF VAR jrnl-list AS CHAR NO-UNDO.
DEF VAR dscr-list AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-table FIELD tt-run-no LIKE gltrans.trnum
                        FIELD tt-date LIKE gltrans.tr-date
                        FIELD tt-jrnl LIKE gltrans.jrnl
                        FIELD tt-dscr AS CHAR FORMAT "x(50)"
                        INDEX tt-run-no tt-run-no
                        INDEX tt-jrnl   tt-jrnl tt-run-no
                        INDEX tt-dscr   tt-dscr tt-run-no.

ASSIGN
   jrnl-list = "CASHR,CRDIS,GENERAL,MCSHREC,APMEM,ACPAY,AP-PURCH,APCKR," +
               "ARINV,CDISB,CRMEM,DBMEM,APVOIDCK,OEINV,ADJUST,JCOST"
   dscr-list = "Cash Disbursements,Cash Disbursements,General Journal Entries," +
               "Misc Cash Receipts,Accounts Payable Memo,Accounts Payable," +
               "Accounts Payable Purchases,Accounts Payable Check Register," +
               "Accounts Receivable Invoice,Cash Disbursements," +
               "Accounts Receivable Memo,Accounts Receivable Memo," +
               "Accounts Payable Void Check,Order Entry Invoice," +
               "FG Adjustments,Job Cost Posting Register".

  RUN build-table.


    if prmAction <> "search" then do:
        FOR EACH tt-table NO-LOCK :
            create ttRunNumberLookRun.
                assign                                        
                   ttRunNumberLookRun.runno   =  tt-table.tt-run-no
                   ttRunNumberLookRun.jrnl    =  tt-table.tt-jrnl
                   ttRunNumberLookRun.vdate   =  string(tt-table.tt-date) 
                   ttRunNumberLookRun.dscr    =  tt-table.tt-dscr  .
        END.	 /* FOR EACH item */     

    END.  /*if prmAction <> "search" then do*/ 


    IF prmAction = "search" then do:

        if prmField = "run"  then do:
            if prmCondition = "EQUAL" then do:
                  FOR EACH tt-table WHERE tt-table.tt-run-no = int(prmText)  NO-LOCK :
                      create ttRunNumberLookRun.
                      assign                                        
                          ttRunNumberLookRun.runno   =  tt-table.tt-run-no
                          ttRunNumberLookRun.jrnl    =  tt-table.tt-jrnl
                          ttRunNumberLookRun.vdate   =  string(tt-table.tt-date) 
                          ttRunNumberLookRun.dscr    =  tt-table.tt-dscr  .
                  END. /*for each*/
              END. /*if prmCondition = EQUAL */
         end.  /* if prmField = est  */
           IF prmField = "jrnl" then do:
             if prmCondition = "EQUAL" then do:
                 FOR EACH tt-table WHERE tt-table.tt-jrnl = prmText  NO-LOCK :
                      create ttRunNumberLookRun.
                      assign                                        
                          ttRunNumberLookRun.runno   =  tt-table.tt-run-no
                          ttRunNumberLookRun.jrnl    =  tt-table.tt-jrnl
                          ttRunNumberLookRun.vdate   =  string(tt-table.tt-date) 
                          ttRunNumberLookRun.dscr    =  tt-table.tt-dscr  .
                  END. /*for each*/
             END. /*if prmCondition = EQUAL*/
             IF prmCondition = "BEGIN" then do:
                  FOR EACH tt-table WHERE tt-table.tt-jrnl BEGINS prmText  NO-LOCK :
                      create ttRunNumberLookRun.
                      assign                                        
                          ttRunNumberLookRun.runno   =  tt-table.tt-run-no
                          ttRunNumberLookRun.jrnl    =  tt-table.tt-jrnl
                          ttRunNumberLookRun.vdate   =  string(tt-table.tt-date) 
                          ttRunNumberLookRun.dscr    =  tt-table.tt-dscr  .
                  END. /*for each*/
             END.  /*if prmCondition = BEGIN*/
         END.  /*IF prmField = i-no */

    END.  /* IF prmAction = search then do: */

    PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH glhist WHERE glhist.company eq prmComp NO-LOCK
      BREAK BY glhist.tr-num:

    IF FIRST-OF(glhist.tr-num) THEN DO:
      CREATE tt-table.
      ASSIGN
       tt-run-no = glhist.tr-num
       tt-jrnl   = glhist.jrnl
       tt-date   = glhist.tr-date
       tt-dscr   = glhist.tr-dscr.

      IF LOOKUP(tt-jrnl,jrnl-list) GT 0 THEN
        tt-dscr = ENTRY(LOOKUP(tt-jrnl,jrnl-list),dscr-list).   
    END.
  END. 

  FOR EACH gltrans WHERE gltrans.company eq prmComp NO-LOCK
      BREAK BY gltrans.trnum:

    IF FIRST-OF(gltrans.trnum) THEN DO:
      CREATE tt-table.
      ASSIGN
       tt-run-no = gltrans.trnum
       tt-jrnl   = gltrans.jrnl
       tt-date   = gltrans.tr-date
       tt-dscr   = gltrans.tr-dscr.

      IF LOOKUP(tt-jrnl,jrnl-list) GT 0 THEN
        tt-dscr = ENTRY(LOOKUP(tt-jrnl,jrnl-list),dscr-list).   
    END.
  END.

END PROCEDURE.
