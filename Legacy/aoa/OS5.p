/* OS5.p */

/* ***************************  Instructions ************************** *
   1. create temp-table in definitions section
   2. place business logic to populate temp-table in pPostBOLCreateInvoice
   3. return this program via email as an attachment
 * ******************************************************************** */

/* ***************************  Definitions  ************************** */

/* Post BOL Create Invoice.rpa */
DEFINE TEMP-TABLE ttPostBOLCreateInvoice NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD bolDate AS DATE      LABEL "Date" FORMAT "99/99/9999"
    FIELD bolNo   AS INTEGER   LABEL "BOL.#" FORMAT ">>>>>>>>9"
    FIELD carrier AS CHARACTER LABEL "Carrier" FORMAT "x(5)"
    FIELD trailer AS CHARACTER LABEL "Trailer" FORMAT "x(20)" 
    FIELD freight AS DECIMAL   LABEL "Freight" FORMAT ">>,>>9.99" 
    FIELD cwt     AS DECIMAL   LABEL "Rate" FORMAT ">>9.99"        
    FIELD totWgt  AS DECIMAL   LABEL "Tot WT" FORMAT ">>9.99"   
    FIELD custNo  AS CHARACTER LABEL "Cust#" FORMAT "x(5)" 
    FIELD shipID  AS CHARACTER LABEL "Ship#" FORMAT "x(8)"  
    FIELD deleted AS LOGICAL   LABEL "Deleted"                                                            
    FIELD iNo     AS CHARACTER LABEL "Item#" FORMAT "x(1)"                                                     
    FIELD iName   AS CHARACTER LABEL "Item Name" FORMAT "x(1)"                                   
    FIELD poNo    AS CHARACTER LABEL "P.O. #" FORMAT "x(1)"                                                          
    FIELD ordNo   AS INTEGER   LABEL "Ord#" FORMAT ">>>>>>"                                                     
    FIELD relNo   AS INTEGER   LABEL "Rel.#" FORMAT ">>>>>9"
    FIELD bOrdNo  AS INTEGER   LABEL "B-Ord" FORMAT ">>>>>>>9"                  
    FIELD loc     AS CHARACTER LABEL "Whse" FORMAT "x(1)"                                                            
    FIELD locBin  AS CHARACTER LABEL "Bin Loc" FORMAT "x(8)"                                              
    FIELD tag     AS CHARACTER LABEL "Tag" FORMAT "x(1)"                                                                                                                                                                                                                                     
    FIELD cases   AS INTEGER   LABEL "Cases" FORMAT "->>>,>>9"                                                   
    FIELD qtyCase AS INTEGER   LABEL "Qty/Case" FORMAT "->>>,>>9"                                                   
    FIELD partial AS DECIMAL   LABEL "Partial" FORMAT ">>,>>9"                                                      
    FIELD weight  AS INTEGER   LABEL "Weight" FORMAT ">>>>9" 
    .  
    
/* Post BOL Create Invoice.rpa */

{sys/ref/CustList.i NEW}

/* ***************************  Main Block  *************************** */

RUN pPostBOLCreateInvoice ("001",0,"asi").
RUN aoa/aoaExcel.p (TEMP-TABLE ttPostBOLCreateInvoice:HANDLE).

/* **********************  Internal Procedures  *********************** */

PROCEDURE pPostBOLCreateInvoice:
/*------------------------------------------------------------------------------
  Purpose:     Post BOL Create Invoice.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pPostBOLCreateInvoice.i}

    /* local variables */
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    /* subject business logic */
    RUN oe/r-bolpstAOA.p (INPUT ipcCompany, INPUT TABLE ttPostBOLCreateInvoice).

END PROCEDURE.

PROCEDURE pBuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Template.rpa
  Parameters:  Company, Use List?, Start Cust, End Cust, ID
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplList      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndCust   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcID        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bCust FOR cust.
    
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttCustList.

    IF iplList THEN
    RUN sys/ref/CustList.p (ipcCompany, ipcID, YES, OUTPUT lActive).
    ELSE DO:
        FOR EACH bCust NO-LOCK
            WHERE bCust.company EQ ipcCompany
              AND bCust.cust-no GE ipcStartCust
              AND bCust.cust-no LE ipcEndCust
            :
            CREATE ttCustList.
            ASSIGN 
                ttCustList.cust-no = bCust.cust-no
                ttCustList.log-fld = YES
                .
        END. /* each bcust */
    END. /* else */

END PROCEDURE.
