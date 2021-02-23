
/*------------------------------------------------------------------------
    File        : oe/w-oeweb.p
    Purpose     : 

    Syntax      :

    Description : Caller Program for OW Screen

    Author(s)   : Rahul Rawat
    Created     : Tue Feb 09 03:27:01 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

    {methods/prgsecur.i}
    
    IF access-close THEN 
        RETURN.   
           
    RUN oe/wOrderEntryMaster.w(
        INPUT "OW"
        ).
    IF VALID-HANDLE(THIS-PROCEDURE) THEN     
        DELETE PROCEDURE THIS-PROCEDURE.        