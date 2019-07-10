
/*------------------------------------------------------------------------
    File        : cshrcpt_list.p
    Purpose     : Accounts Receivable
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttEnterEditCashReceipts NO-UNDO
    FIELD custno       AS CHAR
    FIELD custname     AS CHAR
    FIELD chkno        AS INT         
    FIELD chkdt        AS CHAR     
    FIELD amt          AS DEC 
    FIELD bnkcd        AS CHAR  
    FIELD bnknam       AS CHAR
    FIELD cur_cod      AS CHAR  
    FIELD ex_rate      AS DECIMAL
    FIELD notapp       AS DEC
    
    FIELD exrate       AS CHAR
    FIELD reckey       AS CHAR

    .

DEFINE DATASET dsEnterEditCashReceipts FOR ttEnterEditCashReceipts.
    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcustno       AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmcustname     AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmchkno        AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmchkdt        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmamt          AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmbnkcd        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmbnknam        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmcur_cod      AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmex_rate      AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER prmnotapp       AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmout          AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHAR       NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEnterEditCashReceipts .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


IF prmAction         = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp           = ?  THEN ASSIGN prmComp       = "".
IF prmUser           = ?  THEN ASSIGN prmUser       = "".
IF prmcustno         = ?  THEN ASSIGN prmcustno     = "".
IF prmcustname       = ?  THEN ASSIGN prmcustname   = "".
IF prmchkno          = ?  THEN ASSIGN prmchkno      = 0.
IF prmchkdt          = ?  THEN ASSIGN prmchkdt      = "". 
IF prmamt            = ?  THEN ASSIGN prmamt        = 0. 
IF prmbnkcd          = ?  THEN ASSIGN prmbnkcd      = "".
IF prmbnknam         = ?  THEN ASSIGN prmbnknam     = "".
IF prmcur_cod        = ?  THEN ASSIGN prmcur_cod    = "".
IF prmex_rate        = ?  THEN ASSIGN prmex_rate    = 0.
IF prmnotapp         = ?  THEN ASSIGN prmnotapp     = 0.
IF prmout            = ?  THEN ASSIGN prmout        = "".
IF prmReckey         = ?  THEN ASSIGN prmReckey     = "".

DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
  
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


DEF BUFFER bf-cash FOR ar-cash.
DEF BUFFER bf-cashl FOR ar-cashl.
DEF BUFFER ar-c-memo FOR reftable.

DEF VAR lv-old-cust LIKE ar-cash.cust-no NO-UNDO.
DEF VAR ll-warned AS LOG NO-UNDO.

&SCOPED-DEFINE enable-arcash proc-enable
&SCOPED-DEFINE create-more ar/c-arcash
&SCOPED-DEFINE where-ar-c-memo                                      ~
        WHERE ar-c-memo.reftable EQ "ar-cashl.ar-cashl"             ~
          AND ar-c-memo.company  EQ ar-cash.company                 ~
          AND ar-c-memo.loc      EQ ""




IF prmAction = "Search" THEN DO:
    
     FOR EACH ar-cash WHERE ar-cash.company = cocode 
         AND NOT ar-cash.memo
          AND (ar-cash.cust-no BEGINS prmcustno OR prmcustno = "")
         AND ((ar-cash.posted EQ NO  AND prmout = "False") OR    
               (ar-cash.posted EQ YES AND prmout = "True")) NO-LOCK:

        CREATE ttEnterEditCashReceipts.
           ASSIGN 
                 ttEnterEditCashReceipts.custno     = ar-cash.cust-no
                 ttEnterEditCashReceipts.chkno      = ar-cash.check-no
                 ttEnterEditCashReceipts.chkdt      = string(ar-cash.check-date)
                 ttEnterEditCashReceipts.amt        = ar-cash.check-amt
                 ttEnterEditCashReceipts.reckey     = ar-cash.rec_key    .

            
    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "ValidateAdd" THEN DO:
 FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey  NO-LOCK NO-ERROR.
  /* IF prmcustno THEN DO:*/
         IF NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ cocode 
                      AND LOOKUP(cust.active,"A,E") > 0
                      AND cust.cust-no = prmcustno)
                            THEN DO:
           cError = "Invalid Customer. Try Help" .
           RETURN.
        END.
       

        FIND FIRST bank WHERE bank.company = cocode AND
                            bank.bank-code = prmbnkcd NO-LOCK NO-ERROR.
      IF NOT AVAIL bank THEN DO:
         cError = "Invalid Bank Code. Try Help.".
         RETURN.
      END.
      prmbnknam = bank.bank-NAME.

      IF int(prmchkno) = 0 THEN DO:
        cError = "Check number must be entered...".
        RETURN.
      END.

      IF DEC(prmamt) EQ 0 THEN DO:
          cError = "Check Amount cannot be 0.".
          RETURN.
      END.

   
       IF INT(prmchkno) >= 90000000 AND
          INT(prmchkno) <= 99999999
       THEN DO:
          cError = "This number reserved for CR/DB memos.".
          RETURN.
       END.
    

    FIND FIRST bf-cash WHERE bf-cash.company = cocode
                          AND bf-cash.cust-no = prmcustno
                          AND bf-cash.check-no = int(prmchkno)
                          AND RECID(bf-cash) <> RECID(ar-cash)
                          NO-LOCK NO-ERROR.
    IF AVAIL bf-cash THEN DO:
        cError = "Cash Receipt Already Exists for Customer and Check".
        RETURN.
    END.
    IF NOT CAN-FIND(FIRST period
                    WHERE period.company EQ cocode
                      AND period.pstat   EQ YES
                      AND period.pst     LE DATE(prmchkdt)
                      AND period.pend    GE DATE(prmchkdt)) THEN DO:
      cError = "Check Date is not within an open period." .
    END.
          
  /*   END.*/

 END.

/********************************************************************/

IF prmAction = "Addnew" THEN DO:
    /* FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey  NO-LOCK NO-ERROR.*/
     DEF VAR li-next-cno AS INT NO-UNDO.

  FIND last bf-cash USE-INDEX c-no NO-LOCK NO-ERROR.
  li-next-cno = IF AVAIL bf-cash THEN bf-cash.c-no + 1 ELSE 1.

  
  FIND FIRST ar-ctrl WHERE ar-ctrl.company = cocode NO-LOCK NO-ERROR.
  
  FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
  FIND FIRST currency WHERE currency.company = cocode AND
                            currency.c-code = company.curr-code
                            NO-LOCK NO-ERROR.
  FIND FIRST bank WHERE bank.company = cocode AND
                        bank.actnum = ar-ctrl.cash-act NO-LOCK NO-ERROR.
  CREATE ar-cash .

  IF AVAIL bank THEN 
      ASSIGN ar-cash.bank-code = bank.bank-code. 

  
  
  ASSIGN ar-cash.company = cocode
         ar-cash.c-no = li-next-cno
         ar-cash.check-date = TODAY
         ar-cash.curr-code[1] = IF AVAIL company THEN company.curr-code ELSE ""
         ar-cash.ex-rate = IF AVAIL currency THEN currency.ex-rate ELSE 0
         .

    ASSIGN prmAction = "View" 
           prmReckey = ar-cash.rec_key.

END.

IF prmAction = "Add" THEN DO:
    
     FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN
            ar-cash.check-date  = DATE(prmchkdt)
            ar-cash.check-no    = prmchkno 
            ar-cash.cust-no     = prmcustno
            ar-cash.bank-code   = prmbnkcd
            ar-cash.check-amt   = prmamt.
        
        ASSIGN prmAction = "View" .

END.  



/**************Update *************************************************/

IF prmAction = "ValidateUpdate" THEN DO:
       
      IF NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ cocode 
                      AND LOOKUP(cust.active,"A,E") > 0
                      AND cust.cust-no = prmcustno)
                            THEN DO:
           cError = "Invalid Customer. Try Help" .
           RETURN.
        END.
       

        FIND FIRST bank WHERE bank.company = cocode AND
                            bank.bank-code = prmbnkcd NO-LOCK NO-ERROR.
      IF NOT AVAIL bank THEN DO:
         cError = "Invalid Bank Code. Try Help.".
         RETURN.
      END.
      prmbnknam = bank.bank-NAME.

      IF int(prmchkno) = 0 THEN DO:
        cError = "Check number must be entered...".
        RETURN.
      END.

      IF DEC(prmamt) EQ 0 THEN DO:
          cError = "Check Amount cannot be 0.".
          RETURN.
      END.

   
       IF INT(prmchkno) >= 90000000 AND
          INT(prmchkno) <= 99999999
       THEN DO:
          cError = "This number reserved for CR/DB memos.".
          RETURN.
       END.
    

    FIND FIRST bf-cash WHERE bf-cash.company = cocode
                          AND bf-cash.cust-no = prmcustno
                          AND bf-cash.check-no = int(prmchkno)
                          AND RECID(bf-cash) <> RECID(ar-cash)
                          NO-LOCK NO-ERROR.
    IF AVAIL bf-cash THEN DO:
        cError = "Cash Receipt Already Exists for Customer and Check".
        RETURN.
    END.
    IF NOT CAN-FIND(FIRST period
                    WHERE period.company EQ cocode
                      AND period.pstat   EQ YES
                      AND period.pst     LE DATE(prmchkdt)
                      AND period.pend    GE DATE(prmchkdt)) THEN DO:
      cError = "Check Date is not within an open period." .
    END.
   


END.

IF prmAction = "Update" THEN DO:
     FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

         ASSIGN
            ar-cash.check-date  = DATE(prmchkdt)
            ar-cash.check-no    = prmchkno 
            ar-cash.bank-code   = prmbnkcd
            ar-cash.check-amt   = prmamt.
        
        ASSIGN prmAction = "View" .

END.  

/*********************************delete ******************************/

IF prmAction = "DataDelete"  THEN DO:

    FIND FIRST ar-cash WHERE  ar-cash.company = cocode 
        AND ar-cash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.
   
    IF AVAIL ar-cash THEN DELETE ar-cash .
    
    FOR EACH ar-cash WHERE ar-cash.company = prmComp 
        NO-LOCK:
   /* IF AVAIL ar-cash THEN*/
        ASSIGN
        prmReckey = ar-cash.rec_key .
            LEAVE.
    END.
        prmAction = "View" .

END.


/*******************************View************************************/


IF prmAction = "View" THEN DO:
    
     FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey   NO-LOCK NO-ERROR.
     
        CREATE ttEnterEditCashReceipts.
           ASSIGN 
                 ttEnterEditCashReceipts.custno      = ar-cash.cust-no
                 ttEnterEditCashReceipts.chkno       = ar-cash.check-no
                 ttEnterEditCashReceipts.chkdt       = string(ar-cash.check-date)
                 ttEnterEditCashReceipts.amt         = ar-cash.check-amt
                 ttEnterEditCashReceipts.bnkcd       = ar-cash.bank-code 
                 ttEnterEditCashReceipts.cur_cod     = ar-cash.curr-code[1] 
                 ttEnterEditCashReceipts.ex_rate     = ar-cash.ex-rate
                 ttEnterEditCashReceipts.reckey      = ar-cash.rec_key  .

           FIND FIRST cust WHERE cust.company = cocode AND
               LOOKUP(cust.active,"A,E") > 0  AND
               cust.cust-no = ar-cash.cust-no
                            NO-LOCK NO-ERROR.
        IF AVAIL cust THEN
            ttEnterEditCashReceipts.custname = cust.NAME.

        FIND FIRST bank WHERE bank.company = cocode AND
                            bank.bank-code = ar-cash.bank-code NO-LOCK NO-ERROR.
        IF AVAIL bank THEN
            ttEnterEditCashReceipts.bnknam = bank.bank-NAME.

        ttEnterEditCashReceipts.notapp = DEC(ar-cash.check-amt).
        FOR EACH bf-cashl OF ar-cash NO-LOCK
            WHERE NOT CAN-FIND(FIRST ar-c-memo
                               {&where-ar-c-memo}
                               AND ar-c-memo.code EQ bf-cashl.rec_key):
            ttEnterEditCashReceipts.notapp = ttEnterEditCashReceipts.notapp - (bf-cashl.amt-paid - bf-cashl.amt-disc).
        END.



END. /*IF prmAction = "Select" THEN DO:*/





/*****************************procedure**********************************/
