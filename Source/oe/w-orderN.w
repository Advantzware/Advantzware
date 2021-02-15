
/*------------------------------------------------------------------------
    File        : oe/w-orderN.p
    Purpose     : 

    Syntax      :

    Description : Caller Program for OU1 Screen

    Author(s)   : Rahul Rawat
    Created     : Thu Jan 21 09:26:19 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

    {methods/prgsecur.i}
    
    IF access-close THEN 
        RETURN.
    
    RUN oe/w-order.w(
        INPUT "OU1"
        ).