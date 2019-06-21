
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

DEFINE TEMP-TABLE ttCorrEstimateList NO-UNDO
          FIELD vInvoiceFile AS CHAR              
    .

    DEFINE DATASET dsInvoiceReport FOR ttCorrEstimateList .
    DEFINE VAR cError  AS CHAR NO-UNDO.

DEFINE VARIABLE arglist AS CHARACTER.
DEFINE VARIABLE numarg  AS INTEGER.
DEFINE VARIABLE inputFile AS CHARACTER.
DEFINE TEMP-TABLE tt_viewcalc
    FIELD company AS CHAR
    FIELD out AS CHAR
    FIELD estno AS CHAR
    FIELD formno AS CHAR
    FIELD blankno AS CHAR
    FIELD lineno AS CHAR
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
    CREATE tt_viewcalc.
    IMPORT DELIMITER "," tt_viewcalc.
END.
INPUT CLOSE.

/*FOR EACH tt_viewcalc WHERE tt_viewcalc.company = "":
    DELETE tt_viewcalc.
END.*/

OUTPUT CLOSE.
FIND FIRST tt_viewcalc NO-LOCK NO-ERROR.



RUN CorrugatedBox\CorrEstimatePrint2.p(INPUT "Admin", INPUT "RunReport",INPUT tt_viewcalc.out,INPUT "", INPUT "", INPUT "", INPUT "", INPUT tt_viewcalc.estno , INPUT tt_viewcalc.formno,INPUT tt_viewcalc.blankno ,INPUT tt_viewcalc.lineno,
                       INPUT-OUTPUT DATASET dsInvoiceReport, OUTPUT cError).


