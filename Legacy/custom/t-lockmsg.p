/*
Goal How to programmatically determine what user has a record share-locked
Fact Progress 9.x
Fact OpenEdge 10.x
Fix The following sample code (although limited) will give you the basics on how to programmatically determine what user has a record share-locked:
*/

FUNCTION RecordShareLockedBy RETURNS CHARACTER (INPUT rRecID AS RECID):   
     /* Only Works When One Database is Connected, Modify to Support Multiple */
     /* Databases and Where the RECID Exists in More Than One Table           */   
   DEFINE VARIABLE iLoop  AS INTEGER NO-UNDO.  
   DEFINE VARIABLE iRecID AS INTEGER NO-UNDO.   

   ASSIGN iRecID = INTEGER(rRecID).                 

   FOR EACH _UserLock NO-LOCK WHERE _UserLock-Name <> ?:     
      DO iLoop = 1 TO 512:        
         IF _UserLock-Recid[iLoop] = iRecID AND _UserLock-Flags[iLoop] = 'S' THEN DO:  
            FIND FIRST _Connect WHERE _Connect-Usr = _UserLock-Usr NO-LOCK. 
            RETURN _Connect-Name. 
         END.  
      END.
   END.  
   RETURN ''.  
END FUNCTION.

/*
/*Note The following is a variation of the above code
     which simply returns yes/no as to whether the record is share locked:*/

FUNCTION RecordShareLocked RETURNS LOGICAL (INPUT rRecID AS RECID):  
      /* Only Works When One Database is Connected, Modify to Support Multiple */
      /* Databases and Where the RECID Exists in More Than One Table           */                                            DEFINE VARIABLE iLoop  AS INTEGER NO-UNDO.
DEFINE VARIABLE iRecID AS INTEGER NO-UNDO. 
ASSIGN iRecID = INTEGER(rRecID). 
FOR EACH _UserLock NO-LOCK WHERE _UserLock-Name <> ?:  
   DO iLoop = 1 TO 512:     
      IF _UserLock-Recid[iLoop] = iRecID AND _UserLock-Flags[iLoop] = 'S' THEN 
          RETURN TRUE.    
    END. 
END.
RETURN FALSE. 
END FUNCTION.

*/

