
/*------------------------------------------------------------------------
    File        : oe/w-orderN.p
    Purpose     : 

    Syntax      :

    Description : Caller Program for w-order.w

    Author(s)   : Rahul Rawat
    Created     : Thu Jan 21 09:26:19 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

    {methods/prgsecur.i}
    
    RUN oe/w-order.w(
        INPUT "OU1"
        ).