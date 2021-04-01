
/*------------------------------------------------------------------------
    File        : methods/windows/initial/oe-ordl.i
    Purpose     : 

    Syntax      :

    Description : Custom local initialize include for po-ordl


    Author(s)   : Sewa Singh
    Created     : Mon March 22 01:20:35 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

IF "{&proc-init}" NE "" THEN RUN "{&proc-init}".
