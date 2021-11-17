
/*------------------------------------------------------------------------
    File        : vendorTagParse.p
    Purpose     : 

    Syntax      :

    Description : Parse vendor tag

    Author(s)   : 
    Created     : Wed Nov 17 12:34:03 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ip-tagno      AS CHARACTER NO-UNDO.

DEFINE OUTPUT PARAMETER op-iPOnumber AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-iPOline   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-iQuantity AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-iUOM      AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE a AS CHARACTER NO-UNDO INIT "PPPPPPPPLLLQQQQQ". /*will be retrieved from sys-ctrl later*/

ASSIGN
    op-iPOnumber = SUBSTRING(ip-tagno,INDEX(a,"P"),R-INDEX(a,"P"))
    op-iPOline   = SUBSTRING(ip-tagno,INDEX(a,"L"),(R-INDEX(a,"L") - LENGTH(op-iPOnumber)))
    op-iQuantity = SUBSTRING(ip-tagno,INDEX(a,"Q"),(R-INDEX(a,"Q") - (LENGTH(op-iPOnumber) + LENGTH(op-iPOline))))
    op-iUOM      = SUBSTRING(ip-tagno,LENGTH(a),LENGTH(ip-tagno)).
