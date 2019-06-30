
/*------------------------------------------------------------------------
    File        Material.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Material Information Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tt_mat_tran NO-UNDO
  FIELD tran-date AS DATE
  FIELD job-no AS CHAR
  FIELD job-no2 AS INT
  FIELD sheet-no AS INT
  FIELD blank-no AS INT
  FIELD i-no AS CHAR
  FIELD qty-posted AS DEC
  FIELD qty-waste AS DEC
  FIELD tag AS CHAR
  FIELD ext-cost AS DEC
  FIELD qty-uom AS CHAR
  FIELD ROWID AS ROWID
  INDEX tt-mat-tran-idx IS PRIMARY tran-date ASC job-no ASC job-no2 ASC
        sheet-no ASC blank-no ASC i-no ASC.





DEFINE DATASET dsMaterialInt FOR tt_mat_tran .

DEFINE QUERY q-MaterialIntQuery FOR tt_mat_tran.

DEFINE DATA-SOURCE src-MaterialInt  FOR QUERY q-MaterialIntQuery.

BUFFER tt_mat_tran :ATTACH-DATA-SOURCE(DATA-SOURCE src-MaterialInt  :HANDLE).
