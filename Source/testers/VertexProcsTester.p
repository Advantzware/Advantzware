
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
DEFINE VARIABLE hdVertexProcs    AS HANDLE    NO-UNDO.

DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTotalTax        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lSuccess         AS LOG       NO-UNDO.
DEFINE VARIABLE cmessage         AS CHARACTER NO-UNDO.

RUN system/VertexProcs.p PERSISTENT SET hdVertexProcs.

FIND LAST inv-head NO-LOCK 
    WHERE inv-head.company = "001" AND inv-head.inv-no = 1000113 NO-ERROR.

RUN Vertex_CalculateTaxForInvHead IN hdVertexProcs (
    INPUT ROWID(inv-head),
    INPUT "MAIN",
    OUTPUT dInvoiceTotal,
    OUTPUT dinvoiceSubTotal,
    OUTPUT dTotalTax,
    OUTPUT lSuccess,
    OUTPUT cMessage
    
    ).
    
    
MESSAGE "Invoice Total" dInvoiceTotal SKIP 
    "Invoice Sub Total" dInvoiceSubTotal SKIP 
    "Total Tax" dTotalTax SKIP
    lSuccess SKIP cMessage VIEW-AS ALERT-BOX.
         
FIND LAST ar-inv NO-LOCK 
    WHERE ar-inv.company = "001" AND ar-inv.inv-no = 1000036 NO-ERROR.

RUN Vertex_CalculateTaxForArInv IN hdVertexProcs (
    INPUT ROWID(ar-inv),
    INPUT "MAIN",
    OUTPUT dInvoiceTotal,
    OUTPUT dinvoiceSubTotal,
    OUTPUT dTotalTax,
    OUTPUT lSuccess,
    OUTPUT cMessage   
    ).
    
    
MESSAGE "Invoice Total" dInvoiceTotal SKIP 
    "Invoice Sub Total " dInvoiceSubTotal SKIP 
    "Total Tax" dTotalTax SKIP
    lSuccess SKIP cMessage VIEW-AS ALERT-BOX.         