
/*------------------------------------------------------------------------
    File        : VertexProcsTester.p
    Purpose     : 

    Syntax      :

    Description : Tester program for VertexProcs.p	

    Author(s)   : Mithun Porandla
    Created     : Thu Jun 25 13:16:23 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{system/TaxProcs.i}

DEFINE VARIABLE hdVertexProcs    AS HANDLE    NO-UNDO.

DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTotalTax        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lSuccess         AS LOG       NO-UNDO.
DEFINE VARIABLE cmessage         AS CHARACTER NO-UNDO.

RUN system/VertexProcs.p PERSISTENT SET hdVertexProcs.

FIND FIRST inv-head NO-LOCK 
    WHERE inv-head.company = "001" AND inv-head.inv-no = 1000199 NO-ERROR.

RUN Vertex_CalculateTaxForInvHead IN hdVertexProcs (
    INPUT  ROWID(inv-head),
    INPUT  "MAIN",
    INPUT  "QUOTATION",    /* Message Type "INVOICE" or "QUOTATION" */
    INPUT  FALSE,          /* Post To journal */
    INPUT  "GetTaxAmount", /* GetTaxAmount or GetTaxAmountFinal */ 
    OUTPUT dInvoiceTotal,
    OUTPUT dinvoiceSubTotal,
    OUTPUT dTotalTax,
    OUTPUT TABLE ttTaxDetail,
    OUTPUT lSuccess,
    OUTPUT cMessage    
    ).
    
    
MESSAGE "Invoice Total" dInvoiceTotal SKIP 
    "Invoice Sub Total" dInvoiceSubTotal SKIP 
    "Total Tax" dTotalTax SKIP
    lSuccess SKIP cMessage VIEW-AS ALERT-BOX.
         
FIND FIRST ar-inv NO-LOCK 
    WHERE ar-inv.company = "001" AND ar-inv.inv-no = 1000036 NO-ERROR.

RUN Vertex_CalculateTaxForArInv IN hdVertexProcs (
    INPUT  ROWID(ar-inv),
    INPUT  "MAIN",
    INPUT  "QUOTATION",    /*  Message Type "INVOICE" or "QUOTATION" */    
    INPUT  FALSE,          /* Post To journal */
    INPUT  "GetTaxAmount", /* GetTaxAmount or GetTaxAmountFinal */
    OUTPUT dInvoiceTotal,
    OUTPUT dinvoiceSubTotal,
    OUTPUT dTotalTax,
    OUTPUT TABLE ttTaxDetail,
    OUTPUT lSuccess,
    OUTPUT cMessage   
    ).
    
    
MESSAGE "Invoice Total" dInvoiceTotal SKIP 
    "Invoice Sub Total " dInvoiceSubTotal SKIP 
    "Total Tax" dTotalTax SKIP
    lSuccess SKIP cMessage VIEW-AS ALERT-BOX.         