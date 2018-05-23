
/*------------------------------------------------------------------------
    File        : moduleActive.p
    Purpose     : Returns true if a given module is active.

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue May 22 20:03:13 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcModule AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplActive AS LOGICAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
oplActive = FALSE. 
FIND FIRST module NO-LOCK 
       WHERE  module.module EQ ipcModule
        NO-ERROR.
IF AVAILABLE module AND module.is-used THEN 
  oplActive = TRUE. 
    