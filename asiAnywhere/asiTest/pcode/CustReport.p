
   /*------------------------------------------------------------------------
    File        : CustReport.p
    Purpose     : Cust Report

    Syntax      :

    Description : Return a Dataset of Fgrep Reports

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */ 
    DEFINE TEMP-TABLE ttCustReport NO-UNDO 
        FIELD Cust      AS CHARACTER 
        .

    DEFINE DATASET dsCustReport FOR ttCustReport .

    DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.  
   
   DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustReport.


       
    FIND FIRST usercust WHERE usercust.user_id = prmUser AND usercust.company = prmComp  NO-LOCK NO-ERROR.
    IF AVAIL usercust THEN DO:
        CREATE ttCustReport.
        assign
            ttCustReport.Cust        = usercust.cust-no.
    END.  
