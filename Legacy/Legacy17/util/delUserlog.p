
/*------------------------------------------------------------------------
    File        : custom.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Apr 10 12:25:43 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FOR EACH userLog EXCLUSIVE-LOCK:
    DELETE userLog.
END.