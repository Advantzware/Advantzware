/*------------------------------------------------------------------------
    File        : ttRunStatements.i
    Purpose     : Temp-Table definition for ProparseHelper:GetInternalRunStatements

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 15 11:01:21 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {1} TEMP-TABLE ttRunStatements NO-UNDO 
    
    FIELD ProcedureName       AS CHARACTER 
    FIELD RunFileName         AS CHARACTER 
    FIELD RunLineNumber       AS INTEGER  
    FIELD Statement           AS CHARACTER /* The full run statement */
    FIELD RunNode             AS Progress.Lang.Object XML-NODE-TYPE "HIDDEN"
    FIELD ParentNode          AS Progress.Lang.Object XML-NODE-TYPE "HIDDEN"
    FIELD ParentType          AS CHARACTER 
    FIELD ParentFileName      AS CHARACTER 
    FIELD ParentLineNumber    AS INTEGER  
    FIELD ParentProcedureName AS CHARACTER
    FIELD TriggerWidget       AS CHARACTER 
    FIELD TriggerEvent        AS CHARACTER  

    INDEX ProcedureName ProcedureName . 
     
    
     