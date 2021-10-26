
/*------------------------------------------------------------------------
    File        : InvoiceFreightAssessment.p
    Purpose     : Ticket 103686

    Syntax      :

    Description : Review posted invoices with billable freight and compare with BOL freight information		

    Author(s)   : BV
    Created     : Mon Sep 27 10:03:10 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInvoice
    FIELD invoiceID           AS INTEGER 
    FIELD invoiceDate         AS DATE 
    FIELD customerID          AS CHARACTER 
    FIELD BOL                 AS INTEGER
    FIELD bolDate             AS DATE 
    FIELD freightBilled       AS DECIMAL
    FIELD freightBOLHeader    AS DECIMAL
    FIELD freightBOLLines     AS DECIMAL
    FIELD hasMultipleBOLs     AS LOGICAL 
    FIELD BOLList             AS CHARACTER 
    FIELD invoiceEqualBOL     AS LOGICAL
    FIELD BOLheaderEqualLines AS LOGICAL
    FIELD allEqual            AS LOGICAL. 

DEFINE TEMP-TABLE ttBOL
    FIELD BOLID               AS INTEGER 
    FIELD BOLDate             AS DATE 
    FIELD customerID          AS CHARACTER 
    FIELD invoiceID           AS INTEGER
    FIELD invoiceDate         AS DATE  
    FIELD freightBilled       AS DECIMAL
    FIELD freightBOLHeader    AS DECIMAL
    FIELD freightBOLLines     AS DECIMAL 
    FIELD invoiceEqualBOL     AS LOGICAL
    FIELD BOLheaderEqualLines AS LOGICAL
    FIELD allEqual            AS LOGICAL. 


DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO INITIAL '001'.
DEFINE VARIABLE dtAsOf   AS DATE      NO-UNDO INITIAL 12/31/2020.
DEFINE VARIABLE cFile1   AS CHARACTER NO-UNDO INITIAL "C:\tmp\InvoiceFreightCompare.csv".
DEFINE VARIABLE cFile2   AS CHARACTER NO-UNDO INITIAL "C:\tmp\BOLFreightCompare.csv".


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

//RUN pBuildInvoices(cCompany, dtAsOf).
//RUN pExportTable(cFile1, TEMP-TABLE ttInvoice:HANDLE).

RUN pBuildBOLs(cCompany,dtAsOf).
RUN pExportTable(cFile2, TEMP-TABLE ttBOL:HANDLE).


/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildInvoices PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Builds the invoices with billable freight since as of date.  
        Adds freight calculations from bill of lading for comparison purposes
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.



    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company EQ ipcCompany
        AND ar-inv.inv-date GT ipdtAsOf
        AND ar-inv.f-bill EQ TRUE:
    
        CREATE ttInvoice.
        ASSIGN 
            ttInvoice.invoiceID     = ar-inv.inv-no
            ttInvoice.invoiceDate   = ar-inv.inv-date
            ttInvoice.customerID    = ar-inv.cust-no
            ttInvoice.freightBilled = ar-inv.freight
            .
    
        FOR EACH ar-invl NO-LOCK
            WHERE ar-invl.x-no EQ ar-inv.x-no,
            FIRST oe-bolh NO-LOCK
            WHERE oe-bolh.company EQ ar-invl.company
            AND oe-bolh.b-no EQ ar-invl.b-no
            BREAK BY ar-invl.b-no:
        
            IF FIRST-OF(ar-invl.b-no) THEN 
            DO:
                ASSIGN 
                    ttInvoice.bolDate          = oe-bolh.bol-date
                    ttInvoice.BOL              = oe-bolh.bol-no
                    ttInvoice.freightBOLHeader = oe-bolh.freight
                    ttInvoice.BOLList          = ttInvoice.BOLList + STRING(oe-bolh.bol-no) + ","
                    .            
                FOR EACH oe-boll OF oe-bolh NO-LOCK:
                    ttInvoice.freightBOLLines = ttInvoice.freightBOLLines + oe-boll.freight. 
                END.                
            END. //First b-no
            IF LAST-OF(ar-invl.b-no) AND oe-bolh.bol-no NE ttInvoice.BOL THEN 
                ttInvoice.hasMultipleBOLs = YES. 
        
        END.  //Each ar-invl
        ASSIGN 
            ttInvoice.BOLList             = TRIM(ttInvoice.BOLList,",")
            ttInvoice.invoiceEqualBOL     = ttInvoice.freightBilled EQ ttInvoice.freightBOLHeader
            ttInvoice.BOLHeaderEqualLines = ttInvoice.freightBOLHeader EQ ttInvoice.freightBOLLines.
        ttInvoice.allEqual = (ttInvoice.invoiceEqualBOl EQ ttInvoice.BOLHeaderEqualLInes AND ttInvoice.invoiceEqualBOL EQ TRUE)
            .
    END.  // EACH ar-inv
    

END PROCEDURE.

PROCEDURE pBuildBOLS PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Builds the invoices with billable freight since as of date.  
        Adds freight calculations from bill of lading for comparison purposes
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.



    FOR EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company EQ ipcCompany
        AND oe-bolh.bol-date GE ipdtAsOf
        AND oe-bolh.posted EQ TRUE
        AND oe-bolh.frt-pay EQ 'B':
    
        CREATE ttBOL.
        ASSIGN 
            ttBOL.BOLID            = oe-bolh.bol-no
            ttBOL.BOLDate          = oe-bolh.bol-date
            ttBOL.customerID       = oe-bolh.cust-no
            ttBOL.freightBOLHeader = oe-bolh.freight
            .
        FOR EACH oe-boll OF oe-bolh NO-LOCK:
            ttBOL.freightBOLLines = ttBOL.freightBOLLines + oe-boll.freight. 
        END.
        FOR FIRST ar-invl NO-LOCK
            WHERE ar-invl.company EQ oe-bolh.company 
            AND ar-invl.bol-no EQ oe-bolh.bol-no,
            FIRST ar-inv NO-LOCK 
            WHERE ar-inv.x-no EQ ar-invl.x-no
            AND ar-inv.f-bill EQ TRUE
            :
            ASSIGN 
                ttBOL.freightBilled = ar-inv.freight
                ttBOL.invoiceDate = ar-inv.inv-date
                ttBOl.invoiceID = ar-inv.inv-no
                .               
        END.  //First ar-invl
        ASSIGN             
            ttBOL.invoiceEqualBOL     = ttBOL.freightBilled EQ ttBOL.freightBOLHeader
            ttBOL.BOLHeaderEqualLines = ttBOL.freightBOLHeader EQ ttBOL.freightBOLLines.
        ttBOL.allEqual = (ttBOL.invoiceEqualBOl EQ ttBOL.BOLHeaderEqualLInes AND ttBOL.invoiceEqualBOL EQ TRUE)
            .
    END.  // EACH oe-bolh
    

END PROCEDURE.

PROCEDURE pExportTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Exports the contents of the temp-table to a CSV file
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphdTempTable AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hdOutput AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    
    RUN system\OutputProcs.p PERSISTENT SET hdOutput.
    
    RUN Output_TempTableToCSV IN hdOutput (iphdTempTable, ipcFile, YES, YES, OUTPUT lError, OUTPUT cMessage).
    
    DELETE OBJECT hdOutput.
    
END PROCEDURE.

