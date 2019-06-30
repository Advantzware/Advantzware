
/*------------------------------------------------------------------------
    File        MachHrInt.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Material Information Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE tt_mch_Hr NO-UNDO
 FIELD job-no AS CHAR
  FIELD job-no2 AS INT
  FIELD machine-code AS CHAR
  FIELD sheet-no AS INT
  FIELD blank-no AS INT
 .

DEFINE DATASET dsMachHrHdrInt FOR tt_mch_Hr .

DEFINE QUERY q-MachHrHdrIntQuery FOR tt_mch_Hr.

DEFINE DATA-SOURCE src-MachHrHdrInt  FOR QUERY q-MachHrHdrIntQuery.

BUFFER tt_mch_Hr :ATTACH-DATA-SOURCE(DATA-SOURCE src-MachHrHdrInt  :HANDLE).
