/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : Environment
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 09 23:02:13 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Environment:

    DEFINE STATIC VARIABLE iPid AS INTEGER NO-UNDO INITIAL ? .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves the process identifier of the calling process.
        Notes:   https://msdn.microsoft.com/de-de/library/windows/desktop/ms683180(v=vs.85).aspx
        @return The return value is the process identifier of the calling process.
    ------------------------------------------------------------------------------*/
    PROCEDURE GetCurrentProcessId EXTERNAL "kernel32.dll":U:
        DEFINE RETURN PARAMETER ppid AS LONG NO-UNDO.
    END PROCEDURE.

    /*------------------------------------------------------------------------------
        Purpose: Returns the NewLine character on the current platform
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY NewLine AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Process Id of the current session
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY Pid AS INTEGER NO-UNDO
    GET:
        DEFINE VARIABLE cPid AS CHARACTER  NO-UNDO.

        IF iPid <> ? THEN
            RETURN iPid .

        CASE OPSYS:
            WHEN "WIN32":U THEN DO:
                RUN GetCurrentProcessId (OUTPUT iPid) .
            END.
            WHEN "UNIX":U THEN DO:
                INPUT THROUGH "echo $PPID":U .
                IMPORT cPid.
                INPUT CLOSE.

                ASSIGN iPid = DataTypeHelper:ToInteger(TRIM (cPid)) .
            END.
        END CASE .

        RETURN iPid .

    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Returns the time when the session was started
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TimeStarted AS DATETIME-TZ NO-UNDO
    GET.
    PRIVATE SET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Environment class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR STATIC Environment ():

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        /* Mike Fechner, Consultingwerk Ltd. 21.05.2014
           10.2B Windows compiled code running on the AppServer will raise this
           error here when accessing System.Environmet:NewLine
           Error attempting to push run time parameters onto the stack. (984) */
        IF PROVERSION = "10.2B":U AND SESSION:REMOTE OR
           PROVERSION BEGINS "11.5":U AND SESSION:CLIENT-TYPE = ClientType:MULTI-SESSION-AGENT THEN
            ASSIGN Consultingwerk.Environment:NewLine = "~n":U .
        ELSE
            ASSIGN Consultingwerk.Environment:NewLine = System.Environment:NewLine .
&ELSE
        ASSIGN Consultingwerk.Environment:NewLine = "~n":U .
&ENDIF

        Consultingwerk.Environment:Initialize () .

    END CONSTRUCTOR.


    /*------------------------------------------------------------------------------
        Purpose: Initializes the class
        Notes:   Called form the static constructor
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID Initialize ():

        Consultingwerk.Environment:TimeStarted = NOW .

    END METHOD.

END CLASS.
