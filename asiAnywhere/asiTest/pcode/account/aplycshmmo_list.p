
/*------------------------------------------------------------------------
    File        : aplycshmmo_list.p
    Purpose     : Account Receivable
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttApplyReapplyCashMemo NO-UNDO
    FIELD chkno       AS INT
    FIELD custno      AS CHAR         
    FIELD custname    AS CHAR     
    FIELD custdt      AS CHAR 
    FIELD inv         AS INT        
    FIELD baldue      AS DECIMAL  
    FIELD disc        AS DECIMAL
    FIELD paid        AS DECIMAL
    FIELD memo        AS CHAR
    FIELD aplyamt     AS DEC
    FIELD chkamt      AS DEC
    FIELD typ         AS CHAR
    FIELD bal         AS DEC
    FIELD reckey      AS CHAR
    .

DEFINE DATASET dsApplyReapplyCashMemo FOR ttApplyReapplyCashMemo.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPosted   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmunPosted   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmchkno       AS INT      NO-UNDO.
DEFINE INPUT PARAMETER prmcustno      AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER prmcustname    AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER prmcustdt      AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER prminv         AS INT      NO-UNDO.
DEFINE INPUT PARAMETER prmbaldue      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmdisc        AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmpaid        AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmmemo        AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER prmaplyamt     AS DEC      NO-UNDO.
DEFINE INPUT PARAMETER prmchkamt      AS DEC      NO-UNDO.
DEFINE INPUT PARAMETER prmtyp         AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER prmreckey      AS CHAR     NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsApplyReapplyCashMemo .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-cust FOR cust.

     FOR EACH ttApplyReapplyCashMemo:
        DELETE ttApplyReapplyCashMemo .
    END.

IF prmAction        = ?  THEN ASSIGN prmAction    = "Select".
IF prmComp          = ?  THEN ASSIGN prmComp      = "".
IF prmUser          = ?  THEN ASSIGN prmUser      = "".
IF prmPosted        = ?  THEN ASSIGN prmPosted    = "".
IF prmunPosted      = ?  THEN ASSIGN prmunPosted  = "".
IF prmchkno         = ?  THEN ASSIGN prmchkno     = 0. 
IF prmcustno        = ?  THEN ASSIGN prmcustno    = "". 
IF prmcustname      = ?  THEN ASSIGN prmcustname  = "".
IF prmcustdt        = ?  THEN ASSIGN prmcustdt    = "".
IF prminv           = ?  THEN ASSIGN prminv       = 0.
IF prmbaldue        = ?  THEN ASSIGN prmbaldue    = 0.
IF prmdisc          = ?  THEN ASSIGN prmdisc      = 0.
IF prmpaid          = ?  THEN ASSIGN prmpaid      = 0.
IF prmmemo          = ?  THEN ASSIGN prmmemo      = "".
IF prmaplyamt       = ?  THEN ASSIGN prmaplyamt   = 0.
IF prmchkamt        = ?  THEN ASSIGN prmchkamt    = 0.
IF prmtyp           = ?  THEN ASSIGN prmtyp       = "".




DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
  DEF BUFFER bARInvl FOR ap-invl. 
   DEF VAR X AS INT NO-UNDO.
  DEF VAR Y AS INT NO-UNDO.
DEF BUFFER bf-cashl FOR ar-cashl.
  
         DEF BUFFER b-ap-inv FOR ap-inv.
         DEF BUFFER b-ap-invl FOR ap-invl.
         DEF BUFFER bf-inv FOR ap-inv.
         DEF VAR lv-msg AS CHAR NO-UNDO.
         DEFINE VAR custcount AS CHAR NO-UNDO.
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

  {sys/inc/apsecure.i}


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "cust-no"  NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Customer#"  NO-UNDO.
DEF VAR ll-sort-asc AS LOG INIT YES NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR phandle AS HANDLE NO-UNDO.
DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-frst-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-last-rowid2 AS ROWID NO-UNDO.



IF prmAction = "Search" THEN DO:
    
     FOR EACH ar-cashl                                      
        WHERE  ar-cashl.c-no > 0
         AND  ar-cashl.company EQ cocode
         AND (NOT ar-cashl.memo OR ar-cashl.amt-disc EQ 0)
         AND (ar-cashl.cust-no BEGINS prmcustno OR prmcustno EQ "")              
         AND (ar-cashl.inv-no  EQ prminv OR prminv EQ 0) 
         AND ( (ar-cashl.posted EQ YES AND prmPosted = "True" AND ar-cashl.on-account EQ NO) OR    
               (prmunPosted = "True" AND (ar-cashl.on-account EQ YES OR ar-cashl.inv-no EQ 0) ))  NO-LOCK,
         EACH ar-cash OF ar-cashl WHERE
           (ar-cash.check-no EQ prmchkno OR prmchkno EQ 0) NO-LOCK : 
  
        CREATE ttApplyReapplyCashMemo.
           ASSIGN 
                 ttApplyReapplyCashMemo.chkno        = ar-cash.check-no 
                 ttApplyReapplyCashMemo.custno       = ar-cashl.cust-no
                 ttApplyReapplyCashMemo.custdt       = string(ar-cash.check-date)
                 ttApplyReapplyCashMemo.inv          = ar-cashl.inv-no
                 ttApplyReapplyCashMemo.baldue       = ar-cashl.amt-due
                 ttApplyReapplyCashMemo.disc         = ar-cashl.amt-disc
                 ttApplyReapplyCashMemo.paid         = ar-cashl.amt-paid
                 ttApplyReapplyCashMemo.memo         = string(ar-cashl.memo)    
                 ttApplyReapplyCashMemo.reckey       = ar-cashl.rec_key    .
            
    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/


       
/*******************************View************************************/


IF prmAction = "View" THEN DO:
    
     FOR EACH ar-cashl                                      
        WHERE  /*ar-cashl.c-no < 0 */
         ar-cashl.company EQ cocode
         AND ar-cashl.rec_key = prmReckey
         /*AND (NOT ar-cashl.memo OR ar-cashl.amt-disc EQ 0)*/  NO-LOCK,
        EACH ar-cash OF ar-cashl NO-LOCK:

        CREATE ttApplyReapplyCashMemo.
           ASSIGN 
                 ttApplyReapplyCashMemo.custno     = ar-cash.cust-no 
                 ttApplyReapplyCashMemo.chkno      = ar-cash.check-no
                 ttApplyReapplyCashMemo.chkamt     = ar-cash.check-amt
                 ttApplyReapplyCashMemo.reckey     = ar-cash.rec_key  
                 .
        
           IF AVAIL ar-cashl THEN DO:
               IF ar-cashl.memo THEN DO:
                   FIND bf-cashl WHERE RECID(bf-cashl) = RECID(ar-cashl).
                   IF ar-cashl.amt-paid LT 0 THEN
                       ASSIGN
                        ttApplyReapplyCashMemo.typ = "Credit Memo"
                       bf-cashl.dscr = "Credit" + " - " + bf-cashl.dscr.
                   ELSE
                       ASSIGN
                            ttApplyReapplyCashMemo.typ = "Debit Memo"
                           bf-cashl.dscr = "Debit" + " - " + bf-cashl.dscr.
               END.
               ELSE  ttApplyReapplyCashMemo.typ = "Check".
           END.

           IF AVAIL ar-cash THEN DO:
               FIND FIRST cust WHERE  cust.company = ar-cash.company
                   AND cust.cust-no = ar-cash.cust-no NO-LOCK NO-ERROR.
               IF AVAIL cust THEN 
                   ASSIGN 
                   ttApplyReapplyCashMemo.custname = cust.NAME
                   ttApplyReapplyCashMemo.bal = cust.on-account .

                ttApplyReapplyCashMemo.aplyamt = 0.
              /* FOR EACH ar-cashl OF ar-cash NO-LOCK: */
                IF AVAIL ar-cashl THEN DO:
                    ttApplyReapplyCashMemo.aplyamt = ttApplyReapplyCashMemo.aplyamt + ar-cashl.amt-paid.
                END. 
           END.
           
     END.
END. /*IF prmAction = "Select" THEN DO:*/
