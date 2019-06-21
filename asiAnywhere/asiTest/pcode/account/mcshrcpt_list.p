
/*------------------------------------------------------------------------
    File        : mcshrcpt_list.p
    Purpose     : Accounts Receivable
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttMiscellaneousCashReceiptList NO-UNDO
    FIELD chkno       AS CHAR
    FIELD payr        AS CHAR
    FIELD chkamt      AS DEC         
    FIELD chkdt       AS CHAR     
    FIELD bnkcd       AS CHAR 
    FIELD dscr         AS CHAR        
    FIELD cur_cod      AS CHAR  
    FIELD ex_rate      AS DECIMAL
    FIELD rcrd         AS INT
    FIELD pstd         AS CHAR
    
    FIELD exrate       AS CHAR
    FIELD reckey       AS CHAR

    .

DEFINE DATASET dsMiscellaneousCashReceiptList FOR ttMiscellaneousCashReceiptList.
    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmchkno        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmpayr         AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmchkamt       AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmchkdt        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmbnkcd        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmdscr         AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmcur_cod      AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmex_rate      AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER prmrcrd         AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmpstd         AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmpost         AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmout          AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHAR       NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMiscellaneousCashReceiptList .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


IF prmAction        = ?  THEN ASSIGN prmAction    = "Select".
IF prmComp          = ?  THEN ASSIGN prmComp      = "".
IF prmUser          = ?  THEN ASSIGN prmUser      = "".
IF prmchkno         = ?  THEN ASSIGN prmchkno     = "".
IF prmpayr          = ?  THEN ASSIGN prmpayr      = "".
IF prmchkamt        = ?  THEN ASSIGN prmchkamt    = 0.
IF prmchkdt         = ?  THEN ASSIGN prmchkdt     = "". 
IF prmbnkcd         = ?  THEN ASSIGN prmbnkcd     = "". 
IF prmdscr          = ?  THEN ASSIGN prmdscr      = "".
IF prmcur_cod       = ?  THEN ASSIGN prmcur_cod   = "".
IF prmex_rate       = ?  THEN ASSIGN prmex_rate   = 0.
IF prmrcrd          = ?  THEN ASSIGN prmrcrd      = 0.
IF prmpstd          = ?  THEN ASSIGN prmpstd      = "".
IF prmout           = ?  THEN ASSIGN prmout       = "".

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

  {sys/inc/varasgn.i}
  
DEF BUFFER b-ar-mcash FOR ar-mcash.
DEF BUFFER bf-reftable FOR reftable.
DEF BUFFER bf2-reftable FOR reftable.
DEF VAR v-posted AS LOG NO-UNDO.
DEF VAR v-check-no AS CHAR NO-UNDO.

&SCOPED-DEFINE browse2 ar/j-mcash.i

DEF VAR v-checkno AS CHAR FORMAT "9999999999" NO-UNDO.
DEFINE TEMP-TABLE tt-mcash NO-UNDO LIKE ar-mcash
       field check-no as char.


 
     IF prmOut = "True" THEN
        v-posted = YES.
     ELSE
        v-posted = NO.

     FOR EACH b-ar-mcash WHERE 
         b-ar-mcash.company = cocode AND
         b-ar-mcash.posted EQ v-posted /*(IF prmout = "Yes" THEN TRUE ELSE FALSE)*/
       /*  b-ar-mcash.payer BEGINS auto_find */ 
         NO-LOCK
         BY b-ar-mcash.payer:
    
         FIND FIRST bf-reftable WHERE
              bf-reftable.reftable = "AR-MCASH" AND
              bf-reftable.company  = b-ar-mcash.company AND
              bf-reftable.loc      = STRING(b-ar-mcash.m-no,">>>>>>9") AND
              bf-reftable.code     = b-ar-mcash.rec_key
              NO-LOCK NO-ERROR.

         IF AVAIL bf-reftable THEN
            v-check-no = bf-reftable.code2.
         ELSE
            v-check-no = FILL("0",10).

         FIND FIRST tt-mcash WHERE
              tt-mcash.company = b-ar-mcash.company AND
              tt-mcash.posted = b-ar-mcash.posted AND 
              tt-mcash.payer = b-ar-mcash.payer AND
              tt-mcash.check-date = b-ar-mcash.check-date AND
              tt-mcash.bank-code = b-ar-mcash.bank-code AND
              tt-mcash.curr-code[1] = b-ar-mcash.curr-code[1] AND
              tt-mcash.check-no = v-check-no
              NO-ERROR.
         
         IF NOT AVAIL tt-mcash THEN
         DO:
            CREATE tt-mcash.
            BUFFER-COPY b-ar-mcash EXCEPT check-amt TO tt-mcash
               ASSIGN tt-mcash.check-no = v-check-no.
         END.
    
         tt-mcash.check-amt = tt-mcash.check-amt + b-ar-mcash.check-amt.
    
         RELEASE tt-mcash.
     END. 



IF prmAction = "Search" THEN DO:
    
     FOR EACH tt-mcash  NO-LOCK,
         FIRST ar-mcash WHERE ar-mcash.m-no = tt-mcash.m-no
                        AND  (ar-mcash.payer = prmpayr OR  prmpayr = "") NO-LOCK,
         FIRST reftable WHERE TRUE 
         AND reftable.reftable = "AR-MCASH" 
         and reftable.company  = tt-mcash.company 
         AND reftable.loc      = STRING(tt-mcash.m-no,">>>>>>9") 
         AND reftable.code     = tt-mcash.rec_key                  
         AND reftable.code2    = tt-mcash.check-no NO-LOCK:

        CREATE ttMiscellaneousCashReceiptList.
           ASSIGN 
                 ttMiscellaneousCashReceiptList.chkno      = tt-mcash.check-no
                 ttMiscellaneousCashReceiptList.payr       = tt-mcash.payer     
                 ttMiscellaneousCashReceiptList.chkamt     = tt-mcash.check-amt 
                 ttMiscellaneousCashReceiptList.chkdt      = STRING(tt-mcash.check-date)
                 ttMiscellaneousCashReceiptList.bnkcd      = tt-mcash.bank-code
                 ttMiscellaneousCashReceiptList.reckey     = tt-mcash.rec_key    .

            
    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "ValidateAdd" THEN DO:

  IF NOT CAN-FIND(FIRST bank
                    WHERE bank.company   EQ cocode
                      AND bank.bank-code EQ prmbnkcd)
    THEN DO:
      cError = "Invalid bank code, try help..." .
      RETURN.
  END.
  IF NOT CAN-FIND(FIRST currency
                    WHERE currency.company EQ cocode 
                      AND currency.c-code  EQ prmcur_cod) THEN DO:
      cError = "Invalid currency code, try help...".
      RETURN.
  END.

 END.

/********************************************************************/

IF prmAction = "Addnew" THEN DO:

  DEF BUFFER bf-mcash FOR ar-mcash.
  DEF VAR li-next-mno AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND LAST bf-mcash USE-INDEX m-no NO-LOCK NO-ERROR.
  li-next-mno = IF AVAIL bf-mcash THEN bf-mcash.m-no + 1 ELSE 1.

  /* gdm - */
  ASSIGN prmchkno = "" .
         
  CREATE ar-mcash .
  
  FIND FIRST ar-ctrl WHERE ar-ctrl.company = cocode NO-LOCK NO-ERROR.
  IF AVAIL ar-ctrl THEN DO:
     FIND FIRST bank WHERE bank.company = cocode AND
                           bank.actnum = ar-ctrl.cash-act NO-LOCK NO-ERROR.
     IF NOT AVAIL bank THEN FIND FIRST bank WHERE bank.company = cocode NO-LOCK NO-ERROR.
     IF AVAIL bank THEN ASSIGN ar-mcash.bank-code = bank.bank-code.
  END.
  FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
  FIND FIRST currency WHERE currency.company = cocode AND
                            currency.c-code = company.curr-code
                            NO-LOCK NO-ERROR.
  

  ASSIGN ar-mcash.company = cocode
         ar-mcash.actnum = ar-ctrl.sales
         ar-mcash.m-no = li-next-mno
         ar-mcash.check-date = TODAY
         ar-mcash.curr-code[1] = IF AVAIL company THEN company.curr-code ELSE ""
         ar-mcash.ex-rate = IF AVAIL currency THEN currency.ex-rate ELSE 0
         .   

    ASSIGN prmAction = "View" 
           prmReckey = ar-mcash.rec_key.

END.

IF prmAction = "Add" THEN DO:
    
     FIND FIRST ar-mcash WHERE  ar-mcash.company = cocode AND ar-mcash.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN
            ar-mcash.payer          = prmpayr
            ar-mcash.dscr           = prmdscr 
            ar-mcash.check-date     = date(prmchkdt)
            ar-mcash.bank-code      = prmbnkcd
            ar-mcash.curr-code[1]   = prmcur_cod.

     FIND FIRST reftable NO-LOCK
      WHERE reftable.reftable = "AR-MCASH"       
        AND reftable.company  = ar-mcash.company
        AND reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9")
        AND reftable.code     = ar-mcash.rec_key NO-ERROR.
    
     IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
       reftable.reftable = "AR-MCASH"
       reftable.company  = ar-mcash.company
       reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9")
       reftable.code     = ar-mcash.rec_key.
      END.                                                

      IF AVAIL reftable THEN
          reftable.code2 = STRING(prmchkno,"9999999999").

    RELEASE reftable.
           
        
        ASSIGN prmAction = "View" .

END.  



/**************Update *************************************************/

IF prmAction = "ValidateUpdate" THEN DO:
    
    

    FIND FIRST ar-mcash WHERE  ar-mcash.company = cocode AND
         ar-mcash.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
    
    IF ar-mcash.posted = YES THEN  DO:
     cError = "Cannot Update Posted Misc. Cash Receipt.." .
      RETURN.
    END.

    IF NOT CAN-FIND(FIRST bank
                    WHERE bank.company   EQ cocode
                      AND bank.bank-code EQ prmbnkcd)
    THEN DO:
      cError = "Invalid bank code, try help..." .
      RETURN.
  END.
  IF NOT CAN-FIND(FIRST currency
                    WHERE currency.company EQ cocode 
                      AND currency.c-code  EQ prmcur_cod) THEN DO:
      cError = "Invalid currency code, try help...".
      RETURN.
  END.

END.

IF prmAction = "Update" THEN DO:

  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR ll-new AS LOG NO-UNDO.
  DEF VAR old-payer AS CHAR NO-UNDO.
  DEF VAR old-desc AS CHAR NO-UNDO.
  DEF VAR old-date AS DATE NO-UNDO.
  DEF VAR old-bank-code AS CHAR NO-UNDO.
  DEF VAR old-curr-code AS CHAR NO-UNDO.
  DEF VAR old-posted AS LOG NO-UNDO.
  DEF VAR old-check-no AS CHAR NO-UNDO.

  DEF BUFFER bf-ar-mcash FOR ar-mcash.
  DEF BUFFER b-reftable FOR reftable. 

   FIND FIRST ar-mcash WHERE  ar-mcash.company = cocode AND ar-mcash.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

   ASSIGN
       old-payer = ar-mcash.payer
       old-desc  = ar-mcash.dscr
       old-date  = ar-mcash.check-date
       old-bank-code = ar-mcash.bank-code
       old-curr-code = ar-mcash.curr-code[1]
       old-posted    = ar-mcash.posted.

    FIND FIRST b-reftable WHERE
         b-reftable.reftable = "AR-MCASH" AND
         b-reftable.company  = ar-mcash.company AND
         b-reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9") AND
         b-reftable.code     = ar-mcash.rec_key
         NO-LOCK NO-ERROR.

    IF AVAIL b-reftable THEN
       old-check-no = b-reftable.code2.

    FOR EACH bf-ar-mcash WHERE
         bf-ar-mcash.company      EQ cocode AND
         bf-ar-mcash.posted       EQ old-posted AND
         bf-ar-mcash.payer        EQ old-payer AND
         bf-ar-mcash.check-date   EQ old-date AND
         bf-ar-mcash.bank-code    EQ old-bank-code AND
         bf-ar-mcash.curr-code[1] EQ old-curr-code AND
         bf-ar-mcash.m-no         NE ar-mcash.m-no
         EXCLUSIVE-LOCK:

         ASSIGN
           bf-ar-mcash.payer        = prmpayr
           bf-ar-mcash.dscr         = prmdscr
           bf-ar-mcash.check-date   = DATE(prmchkdt)
           bf-ar-mcash.bank-code    = prmbnkcd
           bf-ar-mcash.curr-code[1] = prmcur_cod.

         FIND FIRST b-reftable WHERE
              b-reftable.reftable = "AR-MCASH" AND
              b-reftable.company  = bf-ar-mcash.company AND
              b-reftable.loc      = STRING(bf-ar-mcash.m-no,">>>>>>9") AND
              b-reftable.code     = bf-ar-mcash.rec_key AND
              b-reftable.code2    = prmchkno
              EXCLUSIVE-LOCK NO-ERROR.

         IF NOT AVAIL b-reftable THEN
         DO:
            CREATE b-reftable.
            ASSIGN
               b-reftable.reftable = "AR-MCASH"
               b-reftable.company  = bf-ar-mcash.company
               b-reftable.loc      = STRING(bf-ar-mcash.m-no,">>>>>>9")
               b-reftable.code     = bf-ar-mcash.rec_key.
         END.

         b-reftable.code2 = prmchkno.
         RELEASE b-reftable.
     END.


         ASSIGN
            ar-mcash.payer          = prmpayr
            ar-mcash.dscr           = prmdscr 
            ar-mcash.check-date     = date(prmchkdt)
            ar-mcash.bank-code      = prmbnkcd
            ar-mcash.curr-code[1]   = prmcur_cod .
        
        ASSIGN prmAction = "View" .

END.  

/*********************************delete ******************************/
IF prmAction = "ValidateDelete" THEN DO:

    FIND FIRST ar-mcash WHERE  ar-mcash.company = cocode 
        AND ar-mcash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    IF ar-mcash.posted = YES THEN  DO:
     cError = "Cannot Delete Posted Misc. Cash Receipt.." .
      RETURN.
    END.

END.

IF prmAction = "DataDelete"  THEN DO:

    FIND FIRST ar-mcash WHERE  ar-mcash.company = cocode 
        AND ar-mcash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    
   
    FIND FIRST reftable WHERE
          reftable.reftable = "AR-MCASH" AND
          reftable.company  = ar-mcash.company AND
          reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9") AND
          reftable.code     = ar-mcash.rec_key NO-LOCK NO-ERROR.
          
    IF AVAIL reftable THEN DO:
        v-check-no = reftable.code2.
    END.

FOR EACH bf-mcash EXCLUSIVE-LOCK
      WHERE bf-mcash.company      EQ cocode
        AND bf-mcash.posted       EQ ar-mcash.posted
        AND bf-mcash.payer        EQ ar-mcash.payer
        AND bf-mcash.check-date   EQ ar-mcash.check-date
        AND bf-mcash.bank-code    EQ ar-mcash.bank-code
        AND bf-mcash.curr-code[1] EQ ar-mcash.curr-code[1],
    FIRST b-reftable WHERE
          b-reftable.reftable = "AR-MCASH" AND
          b-reftable.company  = bf-mcash.company AND
          b-reftable.loc      = STRING(bf-mcash.m-no,">>>>>>9") AND
          b-reftable.code     = bf-mcash.rec_key AND
          b-reftable.code2    = v-check-no
          EXCLUSIVE-LOCK:
         
            DELETE bf-mcash .
      END.
    
    IF AVAIL  ar-mcash THEN
    DELETE ar-mcash .
    
    


    FOR EACH tt-mcash  NO-LOCK,
         FIRST ar-mcash WHERE ar-mcash.m-no = tt-mcash.m-no NO-LOCK,
         FIRST reftable WHERE TRUE 
         AND reftable.reftable = "AR-MCASH" 
         and reftable.company  = tt-mcash.company 
         /*AND reftable.loc      = STRING(tt-mcash.m-no,">>>>>>9") 
         AND reftable.code     = tt-mcash.rec_key */                 
         AND reftable.code2    = tt-mcash.check-no NO-LOCK:

        ASSIGN  prmReckey = ar-mcash.rec_key
                prmAction = "View".
        LEAVE.
    END.
    

END.


/*******************************View************************************/


IF prmAction = "View" THEN DO:
    
     FIND FIRST ar-mcash WHERE  ar-mcash.company = cocode AND ar-mcash.rec_key = prmReckey  NO-LOCK NO-ERROR.
    
        FIND FIRST reftable WHERE 
             reftable.reftable = "AR-MCASH" 
         and reftable.company  = ar-mcash.company 
         AND reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9") 
         AND reftable.code     = ar-mcash.rec_key 
          NO-LOCK NO-ERROR. 


     
        CREATE ttMiscellaneousCashReceiptList.
        IF AVAIL ar-mcash THEN
           ASSIGN 
                 ttMiscellaneousCashReceiptList.chkno      = v-check-no
                 ttMiscellaneousCashReceiptList.payr       = ar-mcash.payer
                 ttMiscellaneousCashReceiptList.dscr       = ar-mcash.dscr
                 ttMiscellaneousCashReceiptList.chkdt      = string(ar-mcash.check-date)
                 ttMiscellaneousCashReceiptList.bnkcd      = ar-mcash.bank-code         
                 ttMiscellaneousCashReceiptList.cur_cod    = ar-mcash.curr-code[1]
                 ttMiscellaneousCashReceiptList.ex_rate    = ar-mcash.ex-rate           
                 ttMiscellaneousCashReceiptList.rcrd       = ar-mcash.m-no        
                 ttMiscellaneousCashReceiptList.pstd       = string(ar-mcash.posted)
                 ttMiscellaneousCashReceiptList.reckey     = ar-mcash.rec_key .
           IF AVAIL reftable THEN
               ASSIGN
               ttMiscellaneousCashReceiptList.chkno = STRING(reftable.code2,"9999999999") .
  
END. /*IF prmAction = "Select" THEN DO:*/





/*****************************procedure**********************************/
