
/*------------------------------------------------------------------------
    File        : ttTransBins.i
    Purpose     : 

    Syntax      :

    Description : temp-table definition for ttFGBin

    Author(s)   : Sewa.singh
    Created     : Wed 7 july 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} {2} TEMP-TABLE ttTransBin NO-UNDO LIKE fg-bin
    FIELD row-id   AS ROWID
    FIELD has-rec  AS LOG INIT NO
    FIELD invoiced AS LOG INIT NO
    FIELD old-tag AS CHARACTER
    FIELD ret-loc AS CHARACTER
    FIELD ret-loc-bin AS CHARACTER 
    FIELD blank-cust AS CHARACTER
    FIELD IS-SELECTED as logical COLUMN-LABEL "" VIEW-AS TOGGLE-BOX  .
            

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
