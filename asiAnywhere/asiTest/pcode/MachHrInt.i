
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


DEFINE TEMP-TABLE tt_mch_tran NO-UNDO
 FIELD tran-type AS CHAR
  FIELD tran-date AS DATE
  FIELD job-no AS CHAR
  FIELD job-no2 AS INT
  FIELD machine-code AS CHAR
  FIELD sheet-no AS INT
  FIELD blank-no AS INT
  FIELD i-no AS CHAR
  FIELD charge-code AS CHAR
  FIELD start-time AS CHAR
  FIELD start-am AS CHAR
  FIELD end-time AS CHAR
  FIELD end-am AS CHAR
  FIELD total-hours AS DEC
  FIELD qty-posted AS DEC
  FIELD qty-waste AS DEC
  FIELD complete AS LOG FORMAT "Y/N"
  FIELD ROWID AS ROWID
  INDEX tt-mch-tran-idx IS PRIMARY tran-date ASC.

DEFINE DATASET dsMachHrInt FOR tt_mch_tran.

DEFINE QUERY q-MachHrIntQuery FOR tt_mch_tran.

DEFINE DATA-SOURCE src-MachHrInt  FOR QUERY q-MachHrIntQuery.

BUFFER tt_mch_tran :ATTACH-DATA-SOURCE(DATA-SOURCE src-MachHrInt  :HANDLE).
