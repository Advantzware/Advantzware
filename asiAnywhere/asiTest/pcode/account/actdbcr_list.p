
/*------------------------------------------------------------------------
    File        : actdbcr_list.p
    Purpose     : Accounts Receivable
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttActWriteCreditDebitlist NO-UNDO
    FIELD custno       AS CHAR
    FIELD custname     AS CHAR
    FIELD chkno        AS INT         
    FIELD chkdt        AS CHAR     
    FIELD amt          AS DEC 
    FIELD stat         AS CHAR        
    FIELD cur_cod      AS CHAR  
    FIELD ex_rate      AS DECIMAL
    
    FIELD exrate       AS CHAR
    FIELD reckey       AS CHAR

    .

DEFINE DATASET dsActWriteCreditDebitlist FOR ttActWriteCreditDebitlist.
    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcustno       AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmcustname     AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmchkno        AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmchkdt        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmamt          AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmstat         AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmcur_cod      AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmex_rate      AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER prmout          AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHAR       NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsActWriteCreditDebitlist .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


IF prmAction         = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp           = ?  THEN ASSIGN prmComp       = "".
IF prmUser           = ?  THEN ASSIGN prmUser       = "".
IF prmcustno         = ?  THEN ASSIGN prmcustno     = "".
IF prmcustname       = ?  THEN ASSIGN prmcustname   = "".
IF prmchkno          = ?  THEN ASSIGN prmchkno      = 0.
IF prmchkdt          = ?  THEN ASSIGN prmchkdt      = "". 
IF prmamt            = ?  THEN ASSIGN prmamt        = 0. 
IF prmstat           = ?  THEN ASSIGN prmstat       = "".
IF prmcur_cod        = ?  THEN ASSIGN prmcur_cod    = "".
IF prmex_rate        = ?  THEN ASSIGN prmex_rate    = 0.
IF prmout            = ?  THEN ASSIGN prmout        = "".

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



          
DEF VAR ld-not-applied AS DEC NO-UNDO.
DEF BUFFER bf-cashl FOR ar-cashl.
DEF BUFFER bf-cash FOR ar-cash.


FUNCTION display-app-amt RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  ld-not-applied = 0.
  FOR EACH bf-cashl OF ar-cash NO-LOCK:
      ld-not-applied =  ld-not-applied - IF bf-cashl.amt-disc LE 0 THEN (bf-cashl.amt-paid + bf-cashl.amt-disc) ELSE 0.
  END.
  ld-not-applied = ar-cash.check-amt - ld-not-applied.
  return ld-not-applied .


END FUNCTION.



IF prmAction = "Search" THEN DO:
    
     FOR EACH ar-cash WHERE ar-cash.company = cocode 
         AND ar-cash.check-no >= 90000001 
         and ar-cash.check-no <= 99999999
         AND (ar-cash.cust-no BEGINS prmcustno OR prmcustno = "")
         AND ((ar-cash.posted EQ NO  AND prmout = "False") OR    
               (ar-cash.posted EQ YES AND prmout = "True")) NO-LOCK:

        CREATE ttActWriteCreditDebitlist.
           ASSIGN 
                 ttActWriteCreditDebitlist.custno     = ar-cash.cust-no
                 ttActWriteCreditDebitlist.chkno      = ar-cash.check-no
                 ttActWriteCreditDebitlist.chkdt      = string(ar-cash.check-date)
                 ttActWriteCreditDebitlist.amt        = display-app-amt()
                 ttActWriteCreditDebitlist.reckey     = ar-cash.rec_key    .

            
    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "ValidateAdd" THEN DO:

  /* IF prmcustno THEN DO:*/
        FIND FIRST cust WHERE cust.company = cocode AND
                            LOOKUP(cust.active,"A,E") > 0  AND
                            cust.cust-no = prmcustno
                            NO-LOCK NO-ERROR.
        IF NOT AVAIL cust THEN DO:
           cError = "Invalid Customer. Try Help" .
           RETURN.
        END.
        prmcustname = cust.NAME.
  /*   END.*/

 END.

/********************************************************************/

IF prmAction = "Addnew" THEN DO:
     /*FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.*/

   DEF VAR xchk LIKE ar-cash.check-no NO-UNDO.
  DEF VAR xno LIKE ar-cash.c-no NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  for each bf-cash where bf-cash.company eq cocode
                     and memo use-index memo
                      by bf-cash.check-no descending:
       xchk = bf-cash.check-no.
       leave.
  end.
  for each bf-cash by c-no descending:
        xno = bf-cash.c-no. leave.
  end.

  
   xchk = xchk + 1.

   if xchk < 90000001 then xchk = 90000001.
  
   find first bank where bank.company = cocode no-lock no-error.
   FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
   FIND FIRST currency WHERE currency.company = cocode AND
                             currency.c-code = company.curr-code
                             NO-LOCK NO-ERROR.
   CREATE ar-cash .
   assign
        ar-cash.memo       = yes
        ar-cash.c-no       = xno + 1
        ar-cash.check-no   = xchk
        ar-cash.company    = cocode
        ar-cash.check-date = today
        /*ar-cash.bank-code  = bank.bank-code  no-bank for db/cr */
        ar-cash.curr-code[1] = IF AVAIL company THEN company.curr-code ELSE ""
        ar-cash.ex-rate     = IF AVAIL currency THEN currency.ex-rate ELSE 0
        .
   
    ASSIGN prmAction = "View" 
           prmReckey = ar-cash.rec_key.

END.

IF prmAction = "Add" THEN DO:
     FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN
            ar-cash.check-date  = DATE(prmchkdt)
            ar-cash.ex-rate     = prmex_rate 
            ar-cash.cust-no     = prmcustno.
        
        ASSIGN prmAction = "View" .

END.  



/**************Update *************************************************/

IF prmAction = "Update" THEN DO:
       
    /*IF prmcustno THEN DO:*/
        FIND FIRST cust WHERE cust.company = cocode AND
                            LOOKUP(cust.active,"A,E") > 0  AND
                            cust.cust-no = prmcustno
                            NO-LOCK NO-ERROR.
        IF NOT AVAIL cust THEN DO:
           cError = "Invalid Customer. Try Help." .
           RETURN.
        END.
        prmcustname = cust.NAME.
    /* END.*/

END.

IF prmAction = "Update" THEN DO:
     FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN
            ar-cash.check-date  = DATE(prmchkdt)
            ar-cash.ex-rate     = prmex_rate .
        
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


IF prmAction = "HoldRel" THEN DO:

       FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.
    
     IF AVAIL ar-cash THEN DO:
      FIND FIRST reftable WHERE
           reftable.reftable = "ARCASHHOLD" AND
           reftable.rec_key = ar-cash.rec_key
           USE-INDEX rec_key
           EXCLUSIVE-LOCK NO-ERROR.
   
      IF NOT AVAIL reftable THEN DO:
        CREATE reftable.
        ASSIGN
         reftable.reftable = "ARCASHHOLD"
         reftable.rec_key  = ar-cash.rec_key
         reftable.CODE     = "N".
      END.
     
       
      
        reftable.code = IF prmstat EQ "ON HOLD" THEN "H"
                        ELSE "R".

     /* gdm - 07010902 */
     RELEASE reftable.

    END.
        


      ASSIGN
          prmAction = "View" .

END.


/*******************************View************************************/


IF prmAction = "View" THEN DO:
    
     FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey   NO-LOCK NO-ERROR.
     
        CREATE ttActWriteCreditDebitlist.
           ASSIGN 
                 ttActWriteCreditDebitlist.custno      = ar-cash.cust-no
                 ttActWriteCreditDebitlist.chkno       = ar-cash.check-no
                 ttActWriteCreditDebitlist.chkdt       = string(ar-cash.check-date)
                 ttActWriteCreditDebitlist.amt         = display-app-amt()
                /* ttActWriteCreditDebitlist.stat        = ar-cash.stat */
                 ttActWriteCreditDebitlist.cur_cod     = ar-cash.curr-code[1] 
                 ttActWriteCreditDebitlist.ex_rate     = ar-cash.ex-rate
                 ttActWriteCreditDebitlist.reckey      = ar-cash.rec_key  .

           FIND FIRST cust WHERE cust.company = cocode AND
               LOOKUP(cust.active,"A,E") > 0  AND
               cust.cust-no = ar-cash.cust-no
                            NO-LOCK NO-ERROR.
        IF AVAIL cust THEN
            ttActWriteCreditDebitlist.custname = cust.NAME.



           IF AVAIL ar-cash THEN DO:
               FIND FIRST reftable WHERE
                   reftable.reftable = "ARCASHHOLD" AND
                   reftable.rec_key = ar-cash.rec_key
                   USE-INDEX rec_key
                   EXCLUSIVE-LOCK NO-ERROR.

               IF NOT AVAIL reftable THEN DO:
                   CREATE reftable.
                   ASSIGN
                       reftable.reftable = "ARCASHHOLD"
                       reftable.rec_key  = ar-cash.rec_key
                       reftable.CODE     = "N".
               END. 
              ttActWriteCreditDebitlist.stat = IF reftable.code EQ "H" THEN "ON HOLD"
                       ELSE "RELEASED". 
               RELEASE reftable.
           END.

           
        

            
END. /*IF prmAction = "Select" THEN DO:*/





/*****************************procedure**********************************/

PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
    DEF INPUT PARAM ip-display AS LOG NO-UNDO.
   
    IF AVAIL ar-cash THEN DO:
      FIND FIRST reftable WHERE
           reftable.reftable = "ARCASHHOLD" AND
           reftable.rec_key = ar-cash.rec_key
           USE-INDEX rec_key
           EXCLUSIVE-LOCK NO-ERROR.
   
      IF NOT AVAIL reftable THEN DO:
        CREATE reftable.
        ASSIGN
         reftable.reftable = "ARCASHHOLD"
         reftable.rec_key  = ar-cash.rec_key
         reftable.CODE     = "N".
      END.
   
      IF ip-display THEN
        prmstat = IF reftable.CODE EQ "H" THEN "ON HOLD"
                                 ELSE "RELEASED".
      ELSE
        reftable.code = IF prmstat EQ "ON HOLD" THEN "H"
                        ELSE "R".

     /* gdm - 07010902 */
     RELEASE reftable.

    END.
  
END PROCEDURE.
