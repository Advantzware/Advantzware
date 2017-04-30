
/*------------------------------------------------------------------------
    File        : clearUserLog.p
    Purpose     : 

    Syntax      :

    Description : Clear out user log records

    Author(s)   : Wade Kaldawi
    Created     : Thu Apr 27 14:29:33 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FOR EACH userLog EXCLUSIVE-LOCK:
    DELETE userLog.
 END.