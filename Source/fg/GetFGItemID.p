/*------------------------------------------------------------------------
    File        : GetFGItemID.p
    Purpose     : Accept key arguments and derives information from eb rowid, with optional overrides.
                  Wrapper for persistent procedure (fg/FGItemIDProcs.p)
    Syntax      :

    Description : Returns the Next Automated FG Item ID (i-no) based on the setting for FGITEM# NK1
                  Replaces:
                      fg/autofg.p
                      fg/hughesfg.p
                      fg/fibre-fg.p                      
                        

    Author(s)   : BV
    Created     : Tue Jul 17 20:57:42 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
                                        
DEFINE INPUT PARAMETER ipriEb AS   ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipcFGItemIDSetHeader AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcFGItemID  AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdFGItemIDProcs AS HANDLE.

/* ***************************  Main Block  *************************** */
RUN fg/FGItemIDProcs.p PERSISTENT SET hdFGItemIDProcs.

RUN pGetFGItemID IN hdFGItemIDProcs (ipriEb, ipcFGItemIDSetHeader, OUTPUT opcFGItemID).


/* **********************  Internal Procedures  *********************** */

