 
DEFINE TEMP-TABLE ttvalboard NO-UNDO  
 FIELD vCaliper AS DECIMAL.
 
 
DEFINE DATASET dsvalboard FOR ttvalboard.
DEFINE INPUT PARAMETER RfqBoard   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmComp   AS CHAR  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsvalboard.
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 
     
 
 
 
 FIND FIRST item WHERE item.company = prmComp and item.i-no = RfqBoard  AND
        (item.mat-type = "B" or item.mat-type = "P" or item.mat-type = "1" 
         OR item.mat-type = "2" or item.mat-type = "3" or item.mat-type = "4") NO-LOCK NO-ERROR.
    if not avail item then do:
        ASSIGN 
            cError = "Invalid Board".
            RETURN. 
    end. /*if not avail item then do:*/ 
    ELSE DO:
        CREATE ttvalboard.
        ASSIGN ttvalboard.vCaliper = item.cal.
        
    END.
     
