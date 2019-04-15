
/*------------------------------------------------------------------------
    File        : fgRecsByUser.p
    Purpose     : 

    Syntax      :

    Description : Return w-fg-rctd for fg-rctd records per user

    Author(s)   : WFK
    Created     : Thu Mar 14 16:12:51 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
/* For fgpostBatch.p */
{fg/invrecpt.i NEW}
{fg/fgPostBatch.i}

DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcRitaCode AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcUserCode AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR w-fg-rctd.
/* ***************************  Definitions  ************************** */


/* ***************************  Main Block  *************************** */
FOR EACH fg-rctd NO-LOCK 
     WHERE fg-rctd.company EQ ipcCompany
       AND fg-rctd.rita-code EQ ipcRitaCode
       AND (
             fg-rctd.created-by EQ ipcUserCode
             OR fg-rctd.updated-by EQ ipcUserCode
           )
     :
     
    CREATE w-fg-rctd.
    BUFFER-COPY fg-rctd TO w-fg-rctd
        ASSIGN 
        w-fg-rctd.row-id  = ROWID(fg-rctd)
        w-fg-rctd.has-rec = YES.
END.