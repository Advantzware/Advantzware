
/*------------------------------------------------------------------------
    File        : oe/w-oecred.p
    Purpose     : 

    Syntax      :

    Description : Caller Program for OC Screen

    Author(s)   : Rahul Rawat
    Created     : Tue Feb 09 05:00:06 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

    {methods/prgsecur.i}
    
    IF access-close THEN 
        RETURN.   
        
    RUN oe/wOrderEntryMaster.w(
        INPUT "OC"
        ).
    IF VALID-HANDLE(THIS-PROCEDURE) THEN     
        DELETE PROCEDURE THIS-PROCEDURE.        