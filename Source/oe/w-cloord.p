
/*------------------------------------------------------------------------
    File        : oe/w-cloord.p
    Purpose     : 

    Syntax      :

    Description : Caller Program for OU6 screen

    Author(s)   : Rahul Rawat
    Created     : Tue Feb 09 05:30:35 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    {methods/prgsecur.i}
    
     IF access-close THEN 
        RETURN.  
    
    RUN oe/wOrderEntryMaster.w(
        INPUT "OU6"
        ).
    IF VALID-HANDLE(THIS-PROCEDURE) THEN     
        DELETE PROCEDURE THIS-PROCEDURE.        