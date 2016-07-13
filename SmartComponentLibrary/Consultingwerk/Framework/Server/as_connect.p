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

{Consultingwerk/products.i}

DEFINE INPUT  PARAMETER pcUserName AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pcPassword AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pcOptions  AS CHARACTER NO-UNDO.

/* ***************************  Main Block  *************************** */

LogManager:WriteMessage ("[AppServer Info] SESSION:SERVER-OPERATING-MODE: ":U + 
                         SESSION:SERVER-OPERATING-MODE,
                         "AppServer Info":U) .

LogManager:WriteMessage ("[AppServer Info] ServerOperatingModeEnum:IsSessionManaged(): ":U + 
                         STRING (ServerOperatingModeEnum:IsSessionManaged()),
                         "AppServer Info":U) .

&IF DEFINED (PacificAppServer) NE 0 &THEN
IF ServerOperatingModeEnum:IsSessionManaged() THEN DO:                                  
    LogManager:WriteMessage ("[AppServer Info] Starting bound session.":U, "AppServer Info":U) .
                                  
    SESSION:SERVER-CONNECTION-BOUND-REQUEST = TRUE  .                                  
END.                                   
&ENDIF
                                  