
/*------------------------------------------------------------------------
    File        : methods/viewers/delete/dept.i
    Purpose     : 

    Syntax      :

    Description : Custom delete include for dept table


    Author(s)   : Rahul Rawat
    Created     : Mon Dec 21 00:57:31 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

IF dept.isRequired THEN DO:
    MESSAGE "Cannot delete required department."
        VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR":U.    
END.    
