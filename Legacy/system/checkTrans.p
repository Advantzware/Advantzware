
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

FIND FIRST nosweat._myconnection.

FOR EACH nosweat._trans WHERE nosweat._trans._Trans-Usrnum = nosweat._myconnection._MyConn-UserId .
  lIsTrans = TRUE.
END.

oplIsTransaction = lIsTrans.