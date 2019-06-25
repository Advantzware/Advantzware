/*------------------------------------------------------------------------
    File        : dirwatcher_print_bol.p
    Purpose     : BOL Print

    Syntax      :

    Description : Return a Dataset of all Order Inquiry invoice BOL

    Author(s)   : 
    Created     : feb 5 2010
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */


DEFINE VARIABLE arglist AS CHARACTER.
DEFINE VARIABLE numarg  AS INTEGER.
DEFINE VARIABLE inputFile AS CHARACTER.

DEFINE TEMP-TABLE tt_info
    FIELD inputfile AS CHARACTER
    FIELD vUSER AS CHAR
    FIELD company AS CHAR
    FIELD OUTPUTfile AS CHAR
    .


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


 OS-COPY VALUE(tt_info.inputfile) VALUE (tt_info.OUTPUTfile) .

 OS-DELETE VALUE("d:\webapps\signbol.csv") .
 
