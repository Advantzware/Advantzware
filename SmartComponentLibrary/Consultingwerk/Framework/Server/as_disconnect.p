/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : as_connect.p
    Purpose     : AppServer Disconnect Procedure

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 26 23:55:01 CET 2012
    Notes       : Uses the "AppServer Info" Custom LogEntryType
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*         FROM PROPATH .
USING Consultingwerk.Framework.Server.*  FROM PROPATH .
USING Consultingwerk.Framework.Session.* FROM PROPATH .
USING Consultingwerk.OERA.*              FROM PROPATH .
USING Consultingwerk.Util.*              FROM PROPATH .

DEFINE VARIABLE lBound AS LOGICAL NO-UNDO INITIAL FALSE .

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

    IF lBound = TRUE THEN DO:
        ServiceManager:StopAllBusinessServices() .

        /* Mike Fechner, Consultingwerk Ltd. 19.02.2016
           Don't issue QUIT. here. QUIT on a bound PASOE session
           does not perform as expected (similar behaviour as a
           state-reset classic AppServer agent). QUIT removes all
           object instances (services, business entities) from
           memory but does not reset static properties. This may
           bring the session and framework components into an
           unstable state. This misbehaviour is confirmed by
           Progress Software and it is expected to be fixed in
           a future OpenEdge release. For now just shutting
           down all Business Entities and Business Tasks brings
           us very close to the demanded behaviour of resetting
           the business logic components of the application without
           negative side effects. */

/*        QUIT .*/
    END.

END FINALLY.
