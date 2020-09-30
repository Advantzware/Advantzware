
/*------------------------------------------------------------------------
    File        : VendorProcs.p
    Purpose     : vend tables process 

    Syntax      :

    Description : Procedures and functions for processing of vend and related tables

    Author(s)   : Sewa Singh
    Created     : Tue Sep 22 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */   


/*Settings Variables*/


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

 



/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE pRecalculateVendorAccountBalance:
    /*------------------------------------------------------------------------------
     Purpose:  
     Notes:
     Syntax:
         RUN pRecalculateVendorAccountBalance( iprwRowid, ipdtAsOfDate, iplAccountBalance,
            iplAverageDays,iplHighBalance,
            OUTPUT oplError, OUTPUT opcMessage).
            
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOfDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER iplAccountBalance AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplAverageDays AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplHighBalance AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dtCheckDate AS DATE      NO-UNDO.
    DEFINE VARIABLE dAmount     AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE cRefnum    AS CHARACTER NO-UNDO.

    DEFINE BUFFER xap-ledger FOR ap-ledger.
    DEFINE BUFFER b-vend     FOR vend.
    
        
    ASSIGN
        dAmount = 0
        . 
    FIND FIRST vend NO-LOCK
          WHERE ROWID(vend) EQ iprwRowid NO-ERROR .
    IF avail vend THEN   
    FOR EACH ap-inv NO-LOCK
        WHERE ap-inv.company   EQ vend.company
        AND ap-inv.vend-no   EQ vend.vend-no
        AND ap-inv.posted    EQ YES
        AND (ap-inv.inv-date LE ipdtAsOfDate )
        USE-INDEX ap-inv ,
    
        FIRST ap-ledger NO-LOCK 
        WHERE ap-ledger.company  EQ vend.company
        AND ap-ledger.vend-no  EQ ap-inv.vend-no
        AND ap-ledger.ref-date EQ ap-inv.inv-date
        AND ap-ledger.refnum   EQ ("INV# " + ap-inv.inv-no)
        AND (ap-ledger.tr-date LE ipdtAsOfDate )
        USE-INDEX ap-ledger
    
        BREAK BY ap-inv.vend-no
        BY (ap-ledger.tr-date)
        BY ap-inv.inv-no:
          
          
        FOR EACH ap-payl NO-LOCK
            WHERE ap-payl.inv-no   EQ ap-inv.inv-no
            AND ap-payl.vend-no  EQ ap-inv.vend-no
            AND ap-payl.posted   EQ YES 
            AND ap-payl.due-date EQ ap-inv.due-date
            USE-INDEX inv-no:

            FIND FIRST ap-pay NO-LOCK
                WHERE ap-pay.company EQ vend.company
                AND ap-pay.c-no EQ ap-payl.c-no
                USE-INDEX c-no NO-ERROR.

            IF AVAILABLE ap-pay THEN
            DO:       
                dtCheckDate = ap-pay.check-date.
                /*check for voided check transaction date*/
                IF ap-payl.amt-paid LT 0  AND
                    ap-payl.memo                EQ NO AND
                    ap-inv.net + ap-inv.freight GT 0 THEN
                DO:
                    cRefnum = "VOIDED CHECK"
                        + STRING(ap-pay.check-no, "zzzzzzz9").

                    FIND FIRST xap-ledger NO-LOCK WHERE
                        xap-ledger.company EQ vend.company AND
                        xap-ledger.vend-no EQ ap-pay.vend-no AND
                        xap-ledger.refnum  EQ cRefnum
                        NO-ERROR.

                    IF AVAILABLE xap-ledger THEN
                    DO:
                        dtCheckDate = xap-ledger.tr-date.
                        RELEASE xap-ledger.
                    END.
                END.

                IF dtCheckDate LE ipdtAsOfDate THEN 
                DO:
                    IF ap-payl.amt-paid NE 0 THEN dAmount = dAmount - ap-payl.amt-paid.
                    IF ap-payl.amt-disc NE 0 THEN 
                    DO:
                        IF NOT ap-payl.memo THEN dAmount = dAmount - ap-payl.amt-disc.
                        IF ap-payl.memo THEN dAmount = dAmount + ap-payl.amt-disc.
                    END.
                END.
            END.

            RELEASE ap-pay.
    
        END. /* for each ap-payl */
  
        dAmount = dAmount + ap-inv.net + ap-inv.freight .
        FIND FIRST b-vend EXCLUSIVE WHERE ROWID(b-vend) EQ ROWID(vend)
                NO-ERROR NO-WAIT.
        IF iplAccountBalance THEN
            b-vend.acc-bal =  dAmount .  
                   
        IF iplHighBalance AND dAmount GE vend.hibal THEN
            ASSIGN
                b-vend.hibal      = dAmount
                b-vend.hibal-date = ap-inv.inv-date.
                  
                
        IF iplAverageDays THEN 
        DO:
            IF ap-inv.due      LE 0 AND
                ap-inv.pay-date NE ? AND
                ap-inv.inv-date NE ? THEN
                       
                ASSIGN
                    b-vend.avg-pay = ((vend.avg-pay * vend.num-inv) +
                                 (ap-inv.pay-date - ap-inv.inv-date)) /
                                 (vend.num-inv + 1) .                     
        END.  /*iplAverageDays */    
                 
    END.    /* for each ap-inv */      
   
END PROCEDURE.



/* ************************  Function Implementations ***************** */ 
