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


PROCEDURE Prep_ValidateAndDeletePlatePrep:
    /*------------------------------------------------------------------------------
     Purpose: Validate Plate Peps and delete if No Ink colors available
     Notes: Moved this logic from write.trg/eb.p
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipRowIdEB AS ROWID NO-UNDO. 
    
    DEFINE BUFFER bf-eb       FOR eb.
    DEFINE BUFFER bf-est-prep FOR est-prep.
    DEFINE BUFFER bf-prep     FOR prep.
    
    FIND FIRST bf-eb NO-LOCK
        WHERE ROWID(bf-eb) = ipRowIdEB NO-ERROR.
         
    IF AVAILABLE bf-eb 
    AND (bf-eb.est-type NE 4 AND bf-eb.est-type NE 8) AND (bf-eb.i-col + bf-eb.i-coat) EQ 0 THEN 
    DO:
        FOR EACH bf-est-prep
            WHERE bf-est-prep.company  EQ bf-eb.company
            AND bf-est-prep.est-no   EQ bf-eb.est-no
            AND bf-est-prep.s-num    EQ bf-eb.form-no
            AND bf-est-prep.mat-type EQ "P",
            FIRST bf-prep NO-LOCK
            WHERE bf-prep.company EQ bf-est-prep.company
            AND bf-prep.code    EQ bf-est-prep.code
            AND bf-prep.dfault  EQ YES:
    
            DELETE bf-est-prep.  
        END.
    END.

END PROCEDURE.


/* ************************  Function Implementations ***************** */


