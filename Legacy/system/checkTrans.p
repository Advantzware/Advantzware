
/*------------------------------------------------------------------------
    File        : checkTrans.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jul 18 13:29:06 EDT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE OUTPUT PARAMETER oplIsTransaction AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIsTrans AS LOGICAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
lIsTrans = FALSE.
FIND FIRST asi._myconnection.

FOR EACH asi._trans WHERE asi._trans._Trans-Usrnum = _MyConn-UserId .
  lIsTrans = TRUE.
END.

FIND FIRST ASI._myconnection.

FOR EACH ASI._trans WHERE ASI._trans._Trans-Usrnum = ASI._myconnection._MyConn-UserId .
  lIsTrans = TRUE.
END.

oplIsTransaction = lIsTrans.