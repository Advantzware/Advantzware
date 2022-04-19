
/*------------------------------------------------------------------------
    File        : OperationProcsTT.i
    Purpose     : Used in OPerations Porcs routine and Tester Program

    Syntax      :

    Description : Temp-table definitions for temp-tables used in Operations Procs program

    Author(s)   : Sakshi Singh
    Created     : Tue Jul 06 02:05:29 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttOperation NO-UNDO {&REFONLY}
    LIKE estCostOperation
    FIELD linealFeetPerFeed AS DECIMAL
    FIELD estType           AS CHARACTER
    FIELD quantityMaster    AS DECIMAL
    FIELD FGCumulativeNumOut AS INTEGER 
    .
DEFINE TEMP-TABLE ttEstBlank NO-UNDO {&REFONLY}
    FIELD BlankID             AS INTEGER
    FIELD FormID              AS INTEGER
    FIELD iOut                AS INTEGER
    FIELD dQtyInOut           AS DECIMAL
    FIELD dQtyInOutRunWaste   AS DECIMAL
    FIELD dQtyInOutSetupWaste AS DECIMAL
    FIELD lOutputInitialized  AS LOGICAL /*Truly a temp-table field and not a db field*/
    .
DEFINE TEMP-TABLE ttAxis NO-UNDO {&REFONLY}
    FIELD axisType       AS CHARACTER 
    FIELD axisCoordinate AS INTEGER 
    FIELD axisValue      AS DECIMAL
    FIELD axisPage       AS INTEGER
    .

DEFINE TEMP-TABLE ttJobMch NO-UNDO {&REFONLY}
    LIKE job-mch 
    FIELD riJobMch    AS ROWID
    FIELD isExtraCopy AS LOGICAL
    .
    
DEFINE TEMP-TABLE ttEstOp NO-UNDO {&REFONLY}
    LIKE est-op
    FIELD machSeq AS INTEGER
    .    
    
DEFINE TEMP-TABLE ttInk NO-UNDO {&REFONLY}
    FIELD FormId       AS INTEGER
    FIELD BlankId      AS INTEGER
    FIELD Pass         AS INTEGER
    FIELD InkCode      AS CHARACTER
    FIELD InkCount     AS INTEGER
    FIELD VarnishCount AS INTEGER
    FIELD IsCoating    AS LOGICAL
    FIELD PressType    AS CHARACTER 
    .

DEFINE TEMP-TABLE ttMachineList NO-UNDO {&REFONLY}
    FIELD Company       AS CHARACTER
    FIELD OperationId   AS CHARACTER
    .
        
DEFINE TEMP-TABLE ttDeptLimit NO-UNDO {&REFONLY}
    FIELD DeptCode       AS CHARACTER
    FIELD ShtLength      AS DECIMAL 
    FIELD ShtWidth       AS DECIMAL 
    FIELD Caliper        AS DECIMAL
    FIELD NumColors      AS INTEGER
    FIELD PressType      AS CHARACTER
    .  
    
    