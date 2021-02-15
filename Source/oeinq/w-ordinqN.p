
/*------------------------------------------------------------------------
    File        : oeinq/w-ordinqN.p
    Purpose     : 

    Syntax      :

    Description : Caller Program for OQ1 screen

    Author(s)   : Rahul Rawat
    Created     : Tue Feb 09 01:20:52 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    {methods/prgsecur.i}
    
    IF access-close THEN 
        RETURN.  
        
    RUN oe/w-order.w(
        INPUT "OQ1"
        ).
