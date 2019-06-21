
/*------------------------------------------------------------------------
    File        : invhighrep.p
    Purpose     :  Invoice Highlights

    Syntax      :

    Description : Return a Dataset of Request Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttInvoiceReport NO-UNDO
          FIELD vInvoiceFile AS CHAR              
    .

    DEFINE DATASET dsInvoiceReport FOR ttInvoiceReport .
    DEFINE VAR cError  AS CHAR NO-UNDO.

DEFINE VARIABLE arglist AS CHARACTER.
DEFINE VARIABLE numarg  AS INTEGER.
DEFINE VARIABLE inputFile AS CHARACTER.
DEFINE TEMP-TABLE tt_info
    FIELD company AS CHARACTER
    FIELD asofdate AS DATE
    FIELD vname AS CHAR
    .

CONNECT -db asi  -H asisbs -N TCP -S 3800 -db nosweat  -H asisbs -N TCP -S 3802 NO-ERROR.
PROPATH = ENTRY(1,PROPATH) + ",D:\webapps\asinet\pcode,D:\asi_gui10\pco1010,D:\asi10test\rco1010".

                           
arglist = session:parameter.
numarg = num-entries(arglist).
IF numarg = 1  THEN /* data file name passed */
DO:
   inputFile = ENTRY(1,arglist).
END.

INPUT FROM VALUE(inputFile).
REPEAT:
    CREATE tt_info.
    IMPORT DELIMITER "," tt_info.
END.
INPUT CLOSE.

FOR EACH tt_info WHERE tt_info.company = "":
    DELETE tt_info.
END.

OUTPUT CLOSE.
FIND FIRST tt_info NO-LOCK NO-ERROR.

IF tt_info.vname = "invoice" THEN do:

RUN REPORTS\InvoiceHighReport2.p(INPUT "Admin", INPUT "RunReport",INPUT tt_info.asofdate, INPUT tt_info.company,INPUT "Yes",
                       INPUT-OUTPUT DATASET dsInvoiceReport, OUTPUT cError).
END.

ELSE IF tt_info.vname = "salesrep" THEN do:

    RUN REPORTS\SalesRepHighReport2.p(INPUT "Admin", INPUT "RunReport",INPUT tt_info.asofdate, INPUT tt_info.company,INPUT "Yes",
                       INPUT-OUTPUT DATASET dsInvoiceReport, OUTPUT cError).

END.

ELSE DO:

     RUN REPORTS\BookingsHighReport2.p(INPUT "Admin", INPUT "RunReport",INPUT tt_info.asofdate, INPUT tt_info.company,INPUT "Yes",
                       INPUT-OUTPUT DATASET dsInvoiceReport, OUTPUT cError).

END.
