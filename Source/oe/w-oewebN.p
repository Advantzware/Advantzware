
/*------------------------------------------------------------------------
    File        : oe/w-oewebN.p
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
           
    RUN oe/w-order.w(
        INPUT "OW"
        ).