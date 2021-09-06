
/*------------------------------------------------------------------------
    File        : ttMachineRoutings.i
    Purpose     : Used in OPerations Procs routine and associated Program 

    Syntax      :

    Description : Temp-table definition for Machine Routing Table

    Author(s)   : Sakshi Singh
    Created     : Mon Sep 06 07:43:06 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttRouting NO-UNDO
    FIELD Company      AS CHARACTER
    FIELD EstimateNo   AS CHARACTER
    FIELD FormId       AS INTEGER
    FIELD BlankId      AS INTEGER
    FIELD departmentID AS CHARACTER
    FIELD DeptSeq      AS INTEGER
    FIELD OperationId  AS CHARACTER.
