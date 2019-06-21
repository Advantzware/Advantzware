
/*------------------------------------------------------------------------
    File        : salestax_list.p
    Purpose     : Sales Tax Code
    Main File   : 
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttDebitCreditItem NO-UNDO
    FIELD invno       AS CHAR
    FIELD duedate     AS CHAR
    FIELD amtdue      AS DEC
    FIELD amtpaid     AS DEC
    FIELD amtdisc     AS DEC
    FIELD actnum       AS CHAR
    FIELD actdscr      AS CHAR
    FIELD reckey      AS CHAR
    FIELD memo          AS CHAR 
     
    .
  
DEFINE DATASET dsDebitCreditItem FOR ttDebitCreditItem.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prminvno      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmduedate    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmamtdue     AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmamtpaid    AS DEC   NO-UNDO.
DEFINE INPUT PARAMETER prmamtdisc    AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmactnum     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmactdscr    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmreckey     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmmemo       AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmapmain     AS CHAR   NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsDebitCreditItem .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     

IF prmAction          = ?  THEN ASSIGN prmAction      = "Select".
IF prmComp            = ?  THEN ASSIGN prmComp        = "".
IF prmUser            = ?  THEN ASSIGN prmUser        = "".
IF prminvno           = ?  THEN ASSIGN prminvno       = "".
IF prmduedate         = ?  THEN ASSIGN prmduedate     = "".
IF prmamtdue          = ?  THEN ASSIGN prmamtdue      = 0. 
IF prmamtpaid         = ?  THEN ASSIGN prmamtpaid     = 0. 
IF prmamtdisc         = ?  THEN ASSIGN prmamtdisc     = 0.
IF prmactnum          = ?  THEN ASSIGN prmactnum      = "".
IF prmactdscr         = ?  THEN ASSIGN prmactdscr     = "".
IF prmreckey          = ?  THEN ASSIGN prmreckey      = "".
IF prmmemo            = ?  THEN ASSIGN prmmemo        = "".
IF prmapmain          = ?  THEN ASSIGN prmapmain      = "".

DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.

DEF VAR lv-actnum AS cha NO-UNDO.
DEF VAR act_dscr AS cha FORM "x(30)" LABEL "Account Description" NO-UNDO.
DEF BUFFER bf-payl FOR ap-payl.
DEF BUFFER bf-vend FOR vend.
DEF VAR lv-inv-displayed AS LOG NO-UNDO.
DEF VAR ll-inquiry AS LOG NO-UNDO.
DEF VAR v-vend-act AS cha NO-UNDO.
  DEF VAR X AS INT NO-UNDO.        
     
 IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.
   ASSIGN  g_company = prmComp
           cocode  = prmComp .



IF prmAction = "GridSelect" THEN DO:
    
  FIND FIRST ap-pay WHERE ap-pay.rec_key = prmapmain AND 
      ap-pay.company = cocode NO-LOCK NO-ERROR.
    FOR EACH ap-payl OF ap-pay  NO-LOCK :
    
        CREATE ttDebitCreditItem.
        ASSIGN
            ttDebitCreditItem.invno   = string(ap-payl.inv-no)
            ttDebitCreditItem.duedate = string(ap-payl.due-date)
            ttDebitCreditItem.amtdue  = ap-payl.amt-due 
            ttDebitCreditItem.amtpaid = ap-payl.amt-paid
            ttDebitCreditItem.amtdisc = ap-payl.amt-disc
            ttDebitCreditItem.actnum  = ap-payl.actnum     
            ttDebitCreditItem.reckey  = ap-payl.rec_key 
            .

         FIND FIRST account WHERE account.company = cocode
                            AND account.actnum = ap-payl.actnum  NO-LOCK NO-ERROR.         
       IF AVAIL account THEN ASSIGN
           ttDebitCreditItem.actdscr = account.dscr.
    END.
                                        
                                        
     
END. /*IF prmAction = "Select" THEN DO:*/



 IF prmAction = "UpdateValidate" THEN DO:
     FIND FIRST ap-pay WHERE ap-pay.rec_key = prmapmain AND 
      ap-pay.company = cocode NO-LOCK NO-ERROR.

     FIND FIRST ap-payl WHERE ap-payl.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR. 

     FIND FIRST ap-inv
        WHERE ap-inv.company EQ g_company 
          AND ap-inv.posted  EQ YES
          AND ap-inv.vend-no EQ ap-pay.vend-no
          AND ap-inv.inv-no  EQ prminvno        NO-LOCK NO-ERROR.
    IF NOT AVAIL ap-inv THEN DO:
      cError = "Invalid Invoice Number..." .           
     RETURN .
    END.     

  IF DEC(prmamtdisc) EQ 0 AND DEC(prmamtpaid) EQ 0 THEN DO:
      cError = "You MUST enter a Credit OR Debit value..." .
      RETURN .
  END.

   FIND FIRST account
        WHERE account.company EQ g_company
          AND account.actnum  EQ prmactnum
          AND account.type    NE "T"
        NO-LOCK NO-ERROR.
    IF NOT AVAIL account THEN DO:
      cError = "Invalid GL Account Number" .
      RETURN .
    END.   

  FIND FIRST bf-payl
        WHERE bf-payl.c-no   EQ ap-pay.c-no
          AND bf-payl.inv-no EQ prminvno
          AND bf-payl.actnum EQ prmactnum
          AND ROWID(bf-payl) NE ROWID(ap-payl)
        NO-LOCK NO-ERROR.

    IF AVAIL bf-payl THEN DO:
      cError = "Invoice/Acct# already on Cash Receipt," + 
              "re-enter Invoice or Account .... " .
          RETURN .
    END.


 END. /* validate update */

 IF prmAction = "Update" THEN DO:
   
     FIND FIRST ap-payl WHERE ap-payl.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL ap-payl THEN
     ASSIGN
            ap-payl.inv-no           = prminvno    
            ap-payl.due-date         = date(prmduedate)
            ap-payl.amt-due          = prmamtdue   
            ap-payl.amt-paid         = prmamtpaid  
            ap-payl.amt-disc         = prmamtdisc   
            ap-payl.actnum           = prmactnum   
           .
            ASSIGN
                prmAction = "View" .


 END.

 IF prmAction = "AddnewRecValidate" THEN DO:
     FIND FIRST ap-pay WHERE ap-pay.rec_key = prmapmain AND 
      ap-pay.company = cocode NO-LOCK NO-ERROR.

     IF ap-pay.posted THEN do:
     cError = "This Memo has been posted. No addings are allowed!"  .
     RETURN.
     END.

 END. /* validate Add*/

 IF prmAction = "Add" THEN DO:

      FIND FIRST ap-pay WHERE ap-pay.rec_key = prmapmain AND 
      ap-pay.company = cocode NO-LOCK NO-ERROR.

      FOR EACH bf-payl OF ap-pay NO-LOCK BY bf-payl.LINE DESCENDING:
      X = bf-payl.LINE.
      LEAVE.
      END.

     CREATE ap-payl.

        FIND FIRST vend WHERE vend.company = ap-pay.company
                    AND vend.vend-no = ap-pay.vend-no
                    NO-LOCK NO-ERROR.
        ASSIGN ap-payl.c-no = ap-pay.c-no
            ap-payl.check-no = ap-pay.check-no
            ap-payl.LINE = X + 1
            ap-payl.memo = TRUE
            ap-payl.vend-no = ap-pay.vend-no
            ap-payl.actnum = IF AVAIL vend THEN vend.actnum ELSE "".

         ASSIGN
                prmReckey = ap-payl.rec_key
                prmAction = "View" .

 END.


 IF prmAction = "Delete" THEN DO:

    FIND FIRST ap-pay WHERE ap-pay.rec_key = prmapmain AND 
      ap-pay.company = cocode NO-LOCK NO-ERROR.

     IF ap-pay.posted THEN do:
     cError = "This Cash Receipt has been posted. No deletion allowed!" .
     RETURN.
  END.
       
 END. /*validate delete */


 IF prmAction = "Delete" THEN DO:
     FIND FIRST ap-pay WHERE ap-pay.rec_key = prmapmain AND 
      ap-pay.company = cocode NO-LOCK NO-ERROR.
     FIND FIRST ap-payl WHERE ap-payl.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL ap-payl THEN
         DELETE ap-payl.

     FIND LAST ap-payl WHERE ap-payl.c-no = ap-pay.c-no 
            AND ap-payl.check-no = ap-pay.check-no
              NO-LOCK NO-ERROR.
     IF AVAIL ap-payl THEN
         ASSIGN
         prmAction = "View"
         prmReckey = ap-payl.rec_key.

 END.

  IF prmAction = "CancelDelete" THEN DO:
     FIND FIRST ap-payl WHERE ap-payl.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL ap-payl THEN
         DELETE ap-payl.

   /*  FIND LAST ap-payl  NO-LOCK NO-ERROR.
     IF AVAIL ap-payl THEN
         ASSIGN
         prmAction = "View"
         prmReckey = ap-payl.rec_key.*/

 END.

 IF prmAction = "View" THEN DO:
     
     FIND FIRST ap-payl WHERE ap-payl.rec_key = prmReckey NO-LOCK NO-ERROR.

     CREATE ttDebitCreditItem.
        ASSIGN
            ttDebitCreditItem.invno   = string(ap-payl.inv-no)
            ttDebitCreditItem.duedate = string(ap-payl.due-date)
            ttDebitCreditItem.amtdue  = ap-payl.amt-due 
            ttDebitCreditItem.amtpaid = ap-payl.amt-paid
            ttDebitCreditItem.amtdisc = ap-payl.amt-disc
            ttDebitCreditItem.actnum  = ap-payl.actnum     
            ttDebitCreditItem.reckey  = ap-payl.rec_key  
             . 
      FIND FIRST account WHERE account.company = cocode
                            AND account.actnum = ap-payl.actnum  NO-LOCK NO-ERROR.         
       IF AVAIL account THEN ASSIGN
           ttDebitCreditItem.actdscr = account.dscr.

 END.


