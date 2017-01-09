/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : ab_connect.p
    Purpose     : AppServer Connect Procedure

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 26 23:55:01 CET 2012
    Notes       : Uses the "AppServer Info" Custom LogEntryType
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.Framework.*        FROM PROPATH .
USING Consultingwerk.OERA.*             FROM PROPATH .
USING Consultingwerk.Util.*             FROM PROPATH .
USING Consultingwerk.Framework.Server.* FROM PROPATH .
USING Consultingwerk.Framework.Session.* FROM PROPATH.

{Consultingwerk/products.i}

DEFINE VARIABLE lBound AS LOGICAL NO-UNDO INIT FALSE .

/* ***************************  Main Block  *************************** */

LogManager:WriteMessage ("[AppServer Info] SESSION:SERVER-OPERATING-MODE: ":U + 
                         SESSION:SERVER-OPERATING-MODE,
                         "AppServer Info":U) .

&IF DEFINED (PacificAppServer) NE 0 &THEN
IF SESSION:SERVER-CONNECTION-BOUND-REQUEST THEN DO:  
    ASSIGN lBound = TRUE . 
      
    LogManager:WriteMessage ("[AppServer Info] Ending bound session.":U, "AppServer Info":U) .

    SESSION:SERVER-CONNECTION-BOUND-REQUEST = FALSE .
END.                                   
&ENDIF
                 
FINALLY:
    SessionManager:Reset() .	
    
    IF lBound = TRUE THEN 
        QUIT .
         	
END FINALLY.                
                  
