
/*------------------------------------------------------------------------
    File        : bank_list.p
    Purpose     : Customer

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttbanklistbank NO-UNDO
    FIELD   bank-code       AS CHAR
    FIELD   bank-name       AS CHAR
    FIELD   addr1           AS CHAR 
    FIELD   addr2           AS CHAR
    FIELD   city            AS CHAR 
    FIELD   state           AS CHAR
    FIELD   zip             AS CHAR
    FIELD   phone           AS CHAR 
    FIELD   contact         AS CHAR 
    FIELD   bk-act          AS CHAR 
    FIELD   actnum          AS CHAR 
    FIELD   last-chk        AS CHAR 
    FIELD   bal             AS CHAR 
    FIELD   o-chk           AS CHAR 
    FIELD   dep-tr          AS CHAR 
    FIELD   serv            AS CHAR 
    FIELD   curr-code       AS CHAR 
    FIELD   accdscr         AS CHAR
    FIELD   bankno          AS CHAR
    FIELD   reckey          AS CHAR   .
                     
  

DEFINE DATASET dsbanklistbank FOR ttbanklistbank.
    
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcomp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBankcode  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBankname  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAddr1      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAddr2      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCity       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmState      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmZip        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPhone      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmContact    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBkact     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmActnum     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmLastchk   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBal        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOchk      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDeptr     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmServ       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCurrcode  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBankno     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey     AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsbanklistbank.
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.

     FOR EACH ttbanklistbank:
        DELETE ttbanklistbank .
    END.

     IF prmBankcode   = ?  THEN ASSIGN prmBankcode     = "".
     IF prmBankname   = ?  THEN ASSIGN prmBankname     = "".
     IF prmAddr1      = ?  THEN ASSIGN prmAddr1        = "".
     IF prmAddr2      = ?  THEN ASSIGN prmAddr2        = "".
     IF prmCity       = ?  THEN ASSIGN prmCity         = "".
     IF prmState      = ?  THEN ASSIGN prmState        = "".
     IF prmZip        = ?  THEN ASSIGN prmZip          = "".
     IF prmPhone      = ?  THEN ASSIGN prmPhone        = "".
     IF prmContact    = ?  THEN ASSIGN prmContact      = "".
     IF prmBkact      = ?  THEN ASSIGN prmBkact        = "".
     IF prmActnum     = ?  THEN ASSIGN prmActnum       = "".
     IF prmLastchk    = ?  THEN ASSIGN prmLastchk      = "0".
     IF prmBal        = ?  THEN ASSIGN prmBal          = "0".
     IF prmOchk       = ?  THEN ASSIGN prmOchk         = "0".
     IF prmDeptr      = ?  THEN ASSIGN prmDeptr        = "0".
     IF prmServ       = ?  THEN ASSIGN prmServ         = "0".
     IF prmCurrcode   = ?  THEN ASSIGN prmCurrcode     = "".
     IF prmBankno     = ?  THEN ASSIGN prmBankno       = "".
     IF prmReckey     = ?  THEN ASSIGN prmReckey       = "".
     IF prmComp       = ?  THEN ASSIGN prmComp         = "".
     

     
{sys/inc/var.i NEW SHARED}

    DEF BUFFER b-bank FOR bank.

 DEF VAR lv-msg AS CHAR NO-UNDO.
 DEF VAR lv-types AS CHAR INIT "ACELRT" NO-UNDO.
 DEF VAR lv-type-dscr AS CHAR INIT
     "Asset,Capital,Expense,Liability,Revenue,Total" NO-UNDO.


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
     cocode = prmComp .
       


MESSAGE "test1111111 " prmAction cocode prmUser.
     IF prmAction = "Search" THEN DO:
         MESSAGE "test 3333333 " .
          FOR EACH bank WHERE bank.company = cocode 
             /* AND (bank.bank-code = prmBankcode OR prmBankcode = "")
              AND (bank.phone = prmPhone OR prmPhone = "")
              AND (bank.actnum = prmActnum OR prmActnum = "")*/ NO-LOCK:
 MESSAGE "test2222" .
             CREATE ttbanklistbank.

             ASSIGN 
                 ttbanklistbank.bank-code   = bank.bank-code 
                 ttbanklistbank.bank-name   = bank.bank-name 
                 ttbanklistbank.addr1       = bank.addr[1]
                 ttbanklistbank.addr2       = bank.addr[2]
                 ttbanklistbank.city        = bank.city
                 ttbanklistbank.state       = bank.state
                 ttbanklistbank.zip         = bank.zip
                 ttbanklistbank.phone       = bank.phone
                 ttbanklistbank.contact     = bank.contact
                 ttbanklistbank.bk-act      = bank.bk-act
                 ttbanklistbank.actnum      = bank.actnum
                 ttbanklistbank.last-chk    = string(bank.last-chk)
                 ttbanklistbank.bal         = string(bank.bal)
                 ttbanklistbank.o-chk       = string(bank.o-chk)
                 ttbanklistbank.dep-tr      = string(bank.dep-tr)
                 ttbanklistbank.serv        = string(bank.serv)
                 ttbanklistbank.curr-code   = bank.curr-code[1]
                 ttbanklistbank.reckey      = bank.rec_key
                /* ttbanklistbank.accdscr     = company.NAME */ .
                 
                FIND account WHERE account.company EQ bank.company
                    AND account.actnum EQ  bank.actnum
                    AND account.type    EQ "A"
                NO-LOCK NO-ERROR.

                IF AVAIL account THEN ttbanklistbank.accdscr = account.dscr.
                

      END. /*FOR EACH bank  */
 END. /*IF prmAction = "Search" THEN DO:*/



 IF prmAction = "AddNewRec" THEN DO:

     FIND FIRST bank WHERE bank.company = cocode AND
                         bank.bank-code = prmBankcode NO-LOCK NO-ERROR.
     IF AVAIL bank  THEN DO:
         cError = "Bank Code Already exite whith this company " .
         RETURN.
     END.
    
  FIND FIRST account
                    WHERE account.company EQ cocode
                      AND account.actnum  EQ prmActnum
                      AND account.type    EQ "A" NO-LOCK NO-ERROR.

  IF NOT AVAIL account THEN do:
      cError = "Invalid " +
               TRIM(ENTRY(INDEX(lv-types,"A"),lv-type-dscr)) +
               " account#, try help" .
      RETURN.
  END.

   FIND FIRST b-bank
                WHERE b-bank.company EQ cocode
                  AND b-bank.actnum  EQ prmActnum NO-LOCK NO-ERROR.
      IF AVAIL b-bank THEN do:
          cError = "Another Bank already uses this Account#, please try again".
          RETURN.
      END.
END.  /* end of validate add*/

 IF prmAction = "Add" THEN DO:
     MESSAGE "testadd  " .
     CREATE bank .
     ASSIGN
         bank.bank-code   =       prmBankcode 
         bank.company     =       cocode
         bank.bank-name   =       prmBankname 
         bank.addr[1]     =       prmAddr1    
         bank.addr[2]     =       prmAddr2    
         bank.city        =       prmCity     
         bank.state       =       prmState    
         bank.zip         =       prmZip      
         bank.phone       =       prmPhone    
         bank.contact     =       prmContact  
         bank.bk-act      =       prmBkact    
         bank.actnum      =       prmActnum   
         bank.last-chk    =       int(prmLastchk)
         bank.bal         =       dec(prmBal)
         bank.o-chk       =       dec(prmOchk)
         bank.dep-tr      =       dec(prmDeptr)
         bank.serv        =       dec(prmServ)
         bank.curr-code[1]   =       prmCurrcode    .


    ASSIGN
        prmAction = "View"
        prmReckey = bank.rec_key .


 END.  /* add new rec*/

 

 

 IF prmAction = "UpdateRec"  THEN DO:
     MESSAGE "test1  " .
     FIND FIRST bank WHERE bank.company = cocode
         AND bank.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
MESSAGE "test2  " .
     ASSIGN
         bank.bank-code      =       prmBankcode 
         bank.bank-name      =       prmBankname 
         bank.addr[1]        =       prmAddr1    
         bank.addr[2]        =       prmAddr2    
         bank.city           =       prmCity     
         bank.state          =       prmState    
         bank.zip            =       prmZip      
         bank.phone          =       prmPhone    
         bank.contact        =       prmContact  
         bank.bk-act         =       prmBkact    
         bank.actnum         =       prmActnum   
         bank.last-chk       =       int(prmLastchk) 
         bank.bal            =       dec(prmBal)
         bank.o-chk          =       dec(prmOchk)
         bank.dep-tr         =       dec(prmDeptr)
         bank.serv           =       dec(prmServ)
         bank.curr-code[1]   =       prmCurrcode  
         
        .
   
   ASSIGN
       prmAction = "View".
       
 END. /* end of update */


 IF prmAction = "DeleteRec" THEN DO:

   
   FIND FIRST bank WHERE bank.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL bank THEN
         DELETE bank .

     FIND LAST bank WHERE bank.company = cocode NO-LOCK NO-ERROR.
     IF AVAIL bank  THEN
         ASSIGN 
         prmAction = "View"
         prmReckey =  bank.rec_key .

 END.



IF prmAction = "View" THEN DO:
   FIND FIRST bank WHERE bank.company = cocode AND 
                        bank.rec_key = prmReckey NO-LOCK NO-ERROR.
             CREATE ttbanklistbank.
             IF AVAIL bank THEN 
                 ASSIGN 
                 ttbanklistbank.bank-code   = bank.bank-code 
                 ttbanklistbank.bank-name   = bank.bank-name 
                 ttbanklistbank.addr1       = bank.addr[1]
                 ttbanklistbank.addr2       = bank.addr[2]
                 ttbanklistbank.city        = bank.city
                 ttbanklistbank.state       = bank.state
                 ttbanklistbank.zip         = bank.zip
                 ttbanklistbank.phone       = bank.phone
                 ttbanklistbank.contact     = bank.contact
                 ttbanklistbank.bk-act      = bank.bk-act
                 ttbanklistbank.actnum      = bank.actnum
                 ttbanklistbank.last-chk    = string(bank.last-chk)
                 ttbanklistbank.bal         = string(bank.bal)
                 ttbanklistbank.o-chk       = string(bank.o-chk)
                 ttbanklistbank.dep-tr      = string(bank.dep-tr)
                 ttbanklistbank.serv        = string(bank.serv)
                 ttbanklistbank.curr-code   = bank.curr-code[1]
                 ttbanklistbank.reckey      = bank.rec_key
                /* ttbanklistbank.accdscr     = company.NAME */ .
                 
                FIND account WHERE account.company EQ bank.company
                    AND account.actnum EQ  bank.actnum
                    AND account.type    EQ "A"
                NO-LOCK NO-ERROR.

                IF AVAIL account THEN ttbanklistbank.accdscr = account.dscr.
            
 END. /*IF prmAction = "View" THEN DO:*/




