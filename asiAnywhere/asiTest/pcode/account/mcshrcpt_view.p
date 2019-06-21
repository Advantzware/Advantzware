
/*------------------------------------------------------------------------
    File        : mcshrcpt_view.p
    Purpose     : Accounts Receivable

     Main File   : 
    Syntax      :

    Description : Return a Dataset of all Invpice

    Author(s)   : 
    Created     :  
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttMiscellaneousCashReceiptView NO-UNDO
    FIELD chkno        AS CHAR
    FIELD chkamt       AS DEC
    FIELD actno        AS CHAR
    FIELD actdscr      AS CHAR
    
    
    FIELD ext          AS CHAR
    FIELD reckey       AS CHAR

  
    .

DEFINE DATASET dsMiscellaneousCashReceiptView FOR ttMiscellaneousCashReceiptView.
    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcust         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmchkno        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmchkamt       AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmactno        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmactdscr      AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmOut          AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHARACTER  NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMiscellaneousCashReceiptView .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     FOR EACH ttMiscellaneousCashReceiptView:
        DELETE ttMiscellaneousCashReceiptView .
    END.

IF prmAction     = ?  THEN ASSIGN prmAction      = "Select".
IF prmComp       = ?  THEN ASSIGN prmComp        = "".
IF prmUser       = ?  THEN ASSIGN prmUser        = "".
IF prmchkno      = ?  THEN ASSIGN prmchkno       = "".
IF prmchkamt     = ?  THEN ASSIGN prmchkamt      = 0.
IF prmactno      = ?  THEN ASSIGN prmactno       = "".
IF prmactdscr    = ?  THEN ASSIGN prmactdscr     = "".
IF prmOut        = ?  THEN ASSIGN prmOut         = "".
IF prmReckey     = ?  THEN ASSIGN prmReckey      = "".



DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.


{sys/inc/VAR.i "new shared"}  


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
             
DEF BUFFER bf-mcash FOR ar-mcash.
DEF BUFFER bf-reftable FOR reftable.

DEF TEMP-TABLE ar-mcashl NO-UNDO LIKE ar-mcash
    FIELD acct-dscr LIKE account.dscr.

DEF VAR ll-new-record AS LOG NO-UNDO.


DEF VAR v-check-no AS CHAR NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF BUFFER b-reftable FOR reftable.

  EMPTY TEMP-TABLE ar-mcashl.

FIND FIRST ar-mcash WHERE ar-mcash.company = cocode NO-LOCK NO-ERROR.  

  

IF prmAction = "SelectGrid" THEN DO:
    
    FIND FIRST ar-mcash NO-LOCK
      WHERE ar-mcash.company      EQ cocode
        AND ar-mcash.rec_key      EQ prmOut  NO-ERROR.
    
    FIND FIRST reftable WHERE
          reftable.reftable = "AR-MCASH" AND
          reftable.company  = ar-mcash.company AND
          reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9") AND
          reftable.code     = ar-mcash.rec_key NO-LOCK NO-ERROR.
          
    IF AVAIL reftable THEN DO:
        v-check-no = reftable.code2.
    END.

    FOR EACH bf-mcash NO-LOCK
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
          NO-LOCK:

          CREATE ar-mcashl.
          BUFFER-COPY bf-mcash TO ar-mcashl.
          
      END.

    
    FOR EACH ar-mcashl NO-LOCK:

    
             CREATE ttMiscellaneousCashReceiptView.
             ASSIGN 
                 ttMiscellaneousCashReceiptView.chkamt        = ar-mcashl.check-amt 
                 ttMiscellaneousCashReceiptView.actno         = ar-mcashl.actnum
                 ttMiscellaneousCashReceiptView.reckey        = ar-mcashl.rec_key .


             FIND FIRST account NO-LOCK
              WHERE account.company EQ cocode
              AND account.actnum  EQ ar-mcashl.actnum
              NO-ERROR.

          IF AVAIL account THEN DO:
              ASSIGN ttMiscellaneousCashReceiptView.actdscr = account.dscr .
          END.
             
            
      END. /*FOR EACH ar-mcashl  */
      
END. /*IF prmAction = "Select" THEN DO:*/


/********************************Add **********************************/

IF prmAction = "ValidateAdd" THEN DO:
    

      FIND FIRST account WHERE account.company = cocode AND
                                account.TYPE <> "T" AND
                                account.actnum = prmactno
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          cError = "Invalid GL Account Number.".
          RETURN.
       END.

END.  /* end of validate add*/

/***********************check add user******************************************/


IF prmAction = "AddNew" THEN DO:

  DEF VAR li-next-mno AS INT NO-UNDO.
  FIND FIRST ar-mcash WHERE ar-mcash.rec_key = prmOut NO-LOCK NO-ERROR.
  
  FIND LAST bf-mcash USE-INDEX m-no NO-LOCK NO-ERROR.
  li-next-mno = (IF AVAIL bf-mcash THEN bf-mcash.m-no ELSE 0) + 1.

  CREATE ar-mcashl .
  
  BUFFER-COPY ar-mcash EXCEPT rec_key actnum check-amt TO ar-mcashl
  ASSIGN
   ar-mcashl.m-no = li-next-mno
   ar-mcashl.rec_key = STRING(TODAY,"99999999") + STRING(NEXT-VALUE(rec_key_seq,asi),"99999999").

  CREATE rec_key.
  ASSIGN
     rec_key.rec_key    = ar-mcashl.rec_key
     rec_key.table_name = "ar-mcash".
  RELEASE rec_key.

  CREATE bf-mcash.
  BUFFER-COPY ar-mcashl TO bf-mcash.



  ASSIGN
      prmReckey = ar-mcashl.rec_key 
      prmAction = "View"  .

  

END.

IF prmAction = "Add" THEN DO:
    

    FIND FIRST ar-mcash NO-LOCK
      WHERE ar-mcash.company      EQ cocode
        AND ar-mcash.rec_key      EQ prmOut  NO-ERROR.
    
    FIND FIRST reftable WHERE
          reftable.reftable = "AR-MCASH" AND
          reftable.company  = ar-mcash.company AND
          reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9") AND
          reftable.code     = ar-mcash.rec_key NO-LOCK NO-ERROR.
          
    IF AVAIL reftable THEN DO:
        v-check-no = reftable.code2.
    END.
   
   FIND FIRST bf-mcash WHERE bf-mcash.company = cocode
       AND bf-mcash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR. 
     ASSIGN
        bf-mcash.check-amt    =  prmchkamt    
        bf-mcash.actnum       =  prmactno     
          .          
     FIND FIRST bf-reftable WHERE
       bf-reftable.reftable = "AR-MCASH" AND
       bf-reftable.company  = bf-mcash.company and
       bf-reftable.loc      = STRING(bf-mcash.m-no,">>>>>>9") AND
       bf-reftable.code     = bf-mcash.rec_key
       EXCLUSIVE-LOCK NO-ERROR.

        IF NOT AVAIL bf-reftable THEN DO:
            CREATE bf-reftable.
            ASSIGN
                bf-reftable.reftable = "AR-MCASH"
                bf-reftable.company  = bf-mcash.company
                bf-reftable.loc      = STRING(bf-mcash.m-no,">>>>>>9")
                bf-reftable.code     = bf-mcash.rec_key 
                bf-reftable.code2    = v-check-no   .
        END.
        IF AVAIL bf-reftable  THEN
            bf-reftable.code2    = v-check-no   .

     ASSIGN
    /*  prmReckey = ar-mcashl.rec_key */
      prmAction = "View" .
                               
                               

END. 


/**************Update *************************************************/

IF prmAction = "ValidateUpdate" THEN DO:
      
        FIND FIRST account WHERE account.company = cocode AND
                                account.TYPE <> "T" AND
                                account.actnum = prmactno
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          cError = "Invalid GL Account Number.".
          RETURN.
       END.
       

END.


IF prmAction = "Update" THEN DO:
    
    FIND FIRST ar-mcash WHERE  ar-mcash.company = cocode
           AND ar-mcash.rec_key = prmOut NO-LOCK NO-ERROR.

     FIND FIRST reftable WHERE
          reftable.reftable = "AR-MCASH" AND
          reftable.company  = ar-mcash.company AND
          reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9") AND
          reftable.code     = ar-mcash.rec_key NO-LOCK NO-ERROR.
          
    IF AVAIL reftable THEN DO:
        v-check-no = reftable.code2.
    END.

    FIND FIRST bf-mcash WHERE  bf-mcash.company = cocode
           AND bf-mcash.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

     
     ASSIGN
         bf-mcash.check-amt    =  prmchkamt    
        bf-mcash.actnum       =  prmactno     
        . 

     FIND FIRST bf-reftable WHERE
       bf-reftable.reftable = "AR-MCASH" AND
       bf-reftable.company  = bf-mcash.company and
       bf-reftable.loc      = STRING(bf-mcash.m-no,">>>>>>9") AND
       bf-reftable.code     = bf-mcash.rec_key
       NO-ERROR.

        IF NOT AVAIL bf-reftable THEN DO:
            CREATE bf-reftable.
            ASSIGN
                bf-reftable.reftable = "AR-MCASH"
                bf-reftable.company  = bf-mcash.company
                bf-reftable.loc      = STRING(bf-mcash.m-no,">>>>>>9")
                bf-reftable.code     = bf-mcash.rec_key
                /*bf-reftable.code2    = v-check-no */ .
        END.

         ASSIGN
             prmAction = "View".
END.  

/*********************************delete ******************************/
/*
IF prmAction = "ValidateDelete"  THEN DO:
     FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
     FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    IF ar-cash.posted THEN do:
     cError =  "This Cash Receipt has been posted. No deletion allowed!"  .
     RETURN .
    END.
    

END.*/


IF prmAction = "DataDelete"  THEN DO:
    FIND FIRST ar-mcash WHERE ar-mcash.rec_key = prmOut NO-LOCK NO-ERROR.
    
     FIND FIRST bf-mcash WHERE  bf-mcash.company = cocode AND bf-mcash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL bf-mcash THEN
        DELETE bf-mcash .

FOR EACH bf-mcash NO-LOCK
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
          b-reftable.code     = bf-mcash.rec_key /*AND
          b-reftable.code2    = v-check-no*/
          NO-LOCK:
          ASSIGN
            prmReckey = bf-mcash.rec_key
            prmAction = "View" .
          
      END.
    
       

END.  

IF prmAction = "AddDelete"  THEN DO:

     FIND FIRST bf-mcash WHERE  bf-mcash.company = cocode AND bf-mcash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL bf-mcash THEN
        DELETE bf-mcash .
END.  

/*******************************View************************************/


IF prmAction = "View" THEN DO:
  

    FIND FIRST ar-mcash WHERE  ar-mcash.company = cocode
           AND ar-mcash.rec_key = prmReckey NO-LOCK NO-ERROR.
         
    CREATE ttMiscellaneousCashReceiptView.
             ASSIGN
                 ttMiscellaneousCashReceiptView.chkamt        = ar-mcash.check-amt 
                 ttMiscellaneousCashReceiptView.actno         = ar-mcash.actnum
                 ttMiscellaneousCashReceiptView.reckey        = ar-mcash.rec_key .


             FIND FIRST account NO-LOCK
                 WHERE account.company EQ cocode
                 AND account.actnum  EQ ar-mcash.actnum
                 NO-ERROR.
             IF AVAIL account THEN DO:
                 ASSIGN ttMiscellaneousCashReceiptView.actdscr = account.dscr .
             END.
         
      
END. /*IF prmAction = "Select" THEN DO:*/


/*****************calc-amt *******************************************/

