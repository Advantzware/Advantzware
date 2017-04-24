/*------------------------------------------------------------------------
    File        : ttPostBolCreateInvoice.i
    Purpose     : 

    Syntax      : {aoa/tempTable/ttPostBolCreateInvoice.i}

    Description : TEMP-TABLE definition

    Author(s)   : Wade
    Created     : Thu Aug 25 17:20:17 EDT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Post BOL Create Invoice.rpa */
DEFINE TEMP-TABLE ttPostBOLCreateInvoice NO-UNDO
{aoa/tempTable/ttFields.i}
    FIELD bolDate   AS DATE      LABEL "Date"      FORMAT "99/99/9999"
    FIELD bolNo     AS INTEGER   LABEL "BOL"       FORMAT ">>>>>>>>"
    FIELD carrier   AS CHARACTER LABEL "Carrier"   FORMAT "x(5)"
    FIELD trailer   AS CHARACTER LABEL "Trailer"   FORMAT "x(20)" 
    FIELD freight   AS DECIMAL   LABEL "Freight"   FORMAT "->>>,>>9.99" 
    FIELD cwt       AS DECIMAL   LABEL "Rate"      FORMAT ">>9.99"        
    FIELD totWgt    AS DECIMAL   LABEL "Tot WT"    FORMAT "->>>,>>9"   
    FIELD custNo    AS CHARACTER LABEL "Customer"  FORMAT "x(8)" 
    FIELD shipID    AS CHARACTER LABEL "Ship ID"   FORMAT "x(8)"  
    FIELD deleted   AS LOGICAL   LABEL "Deleted"                                                            
    FIELD iNo       AS CHARACTER LABEL "Item No"   FORMAT "x(15)"                                                     
    FIELD iName     AS CHARACTER LABEL "Item Name" FORMAT "x(20)"                                   
    FIELD poNo      AS CHARACTER LABEL "PO No"     FORMAT "x(15)"                                                          
    FIELD ordNo     AS INTEGER   LABEL "Order"     FORMAT ">>>>>>"                                                     
    FIELD relNo     AS INTEGER   LABEL "Release"   FORMAT ">>9"
    FIELD bOrdNo    AS INTEGER   LABEL "B-Ord"     FORMAT "99"                  
    FIELD loc       AS CHARACTER LABEL "Whse"      FORMAT "x(5)"                                                            
    FIELD locBin    AS CHARACTER LABEL "Bin Loc"   FORMAT "x(8)"                                              
    FIELD tag       AS CHARACTER LABEL "Tag"       FORMAT "x(20)"                                                                                                                                                                                                                                     
    FIELD cases     AS INTEGER   LABEL "Cases"     FORMAT "->>>,>>9"                                                   
    FIELD qtyCase   AS INTEGER   LABEL "Qty/Case"  FORMAT "->>>,>>9"                                                   
    FIELD partial   AS DECIMAL   LABEL "Partial"   FORMAT "->>>,>>9"                                                      
    FIELD weight    AS INTEGER   LABEL "Weight"    FORMAT "->>>,>>9"
    FIELD bolStatus AS CHARACTER LABEL "Status"    FORMAT "x(10)"
    FIELD reason    AS CHARACTER LABEL "Reason"    FORMAT "x(60)"
    .
/* Post BOL Create Invoice.rpa */
