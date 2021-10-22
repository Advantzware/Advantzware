
/*------------------------------------------------------------------------
    File        : ttEstProcInk.i
    Purpose     : It will be used for Ink count calculation in Estimate Procs

    Syntax      :

    Description : TEMP-TABLE definition for ttInk for Estimate Procs

    Author(s)   : Sakshi Singh
    Created     : Wed Oct 21 06:36:15 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUniqueInk NO-UNDO 
    FIELD Company          AS CHARACTER 
    FIELD EstNo            AS CHARACTER
    FIELD FormNo           AS INTEGER
    FIELD BlankNo          AS INTEGER
    FIELD Pass             AS INTEGER
    FIELD ItemCode         AS CHARACTER 
    FIELD UnitNo           AS INTEGER
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
