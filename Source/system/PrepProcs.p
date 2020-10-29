/*------------------------------------------------------------------------
    File        : PrepProcs.p
    Purpose     : 

    Syntax      :

    Description : Various procedures for output of data

    Author(s)   : Sewa Singh
    Created     : Wed Sep 23 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */



/* ***************************  Main Block  *************************** */

    
/* **********************  Internal Procedures  *********************** */

PROCEDURE pDisplayPrepDisposedMessage :
/*------------------------------------------------------------------------------
 Purpose: display message of disposed prep item 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID. 
          .
    FIND FIRST Prep NO-LOCK 
         WHERE rowid(prep) eq iprwRowid NO-ERROR.
    IF AVAIL prep and prep.disposal-date gt 01/02/1900 and prep.disposal-date LE today THEN
     RUN displayMessage ( INPUT "51").

 END PROCEDURE.



/* ************************  Function Implementations ***************** */


