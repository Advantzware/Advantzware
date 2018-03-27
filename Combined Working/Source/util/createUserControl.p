
/*------------------------------------------------------------------------
    File        : createUserControl.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Apr 20 18:01:00 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST userControl NO-LOCK NO-ERROR. 
IF NOT AVAILABLE userControl THEN DO:
    CREATE userControl.
    ASSIGN 
      maxAllowedUsers    = 1
      numUsersOverLimit  = 0
      numLicensedUsers   = 1
      maxSessionsPerUser = 2.
END.
