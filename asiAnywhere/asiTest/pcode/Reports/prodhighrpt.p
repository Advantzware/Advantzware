
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
          FIELD  vMachine AS CHAR
          FIELD  vMachDesc    AS CHAR  .
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
    FIELD vmach1 AS CHAR
    FIELD vmach2 AS CHAR
    FIELD vmach3 AS CHAR
    FIELD vmach4 AS CHAR
    FIELD vmach5 AS CHAR
    FIELD vmach6 AS CHAR
    FIELD vmach7 AS CHAR
    FIELD vmach8 AS CHAR
    FIELD vmach9 AS CHAR
    FIELD vmach10 AS CHAR
    FIELD vmach11 AS CHAR
    FIELD vmach12 AS CHAR
    FIELD vmach13 AS CHAR
    FIELD vmach14 AS CHAR
    FIELD vmach15 AS CHAR

    .

CONNECT -db asi  -H asisbs -N TCP -S 3803 -db nosweat  -H asisbs -N TCP -S 3812 NO-ERROR.
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

RUN REPORTS\ProdHighReport2.p(INPUT "Admin", INPUT "RunReport",INPUT tt_info.asofdate, INPUT tt_info.company,INPUT tt_info.vmach1,INPUT tt_info.vmach2,INPUT tt_info.vmach3,INPUT tt_info.vmach4,INPUT tt_info.vmach5,INPUT tt_info.vmach6,INPUT tt_info.vmach7,INPUT tt_info.vmach8,INPUT tt_info.vmach9,INPUT tt_info.vmach10,INPUT tt_info.vmach11,INPUT tt_info.vmach12,INPUT tt_info.vmach13,INPUT tt_info.vmach14,INPUT tt_info.vmach15,
                     INPUT "Yes", INPUT-OUTPUT DATASET dsInvoiceReport, OUTPUT cError).


