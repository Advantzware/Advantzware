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
    File        : OpenEdgeArchitectHelper
    Purpose     : Provides support for working with the OpenEdgeArchitect /
                  Progress Developer Studio for OpenEdge
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 03 18:15:50 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Util.OpenEdgeArchitectHelper:

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED OpenEdgeArchitectHelper ():
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Searches the Registry for the location of an PDSOE Project
        Notes:   Based on the registry values under HKEY_CURRENT_USER\Software\Consultingwerk Ltd.\IDE Support
                 that are created by the _idestartup.
        @param pcPathName The path to lookup
        @return The name of the project or ? when no project is found
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER FindProjectNameForPath (pcPathName AS CHARACTER):

        DEFINE VARIABLE oRegistry AS Microsoft.Win32.RegistryKey NO-UNDO .
        DEFINE VARIABLE cName     AS CHARACTER                   NO-UNDO .

        oRegistry = Consultingwerk.Framework.Registry:FindRegistryKey ("USER":U,
                                                                       "Software~\Consultingwerk Ltd.~\IDE Support":U) .

        {Consultingwerk/foreach.i System.Object oName in oRegistry:GetValueNames()}

            ASSIGN cName = UNBOX (oName) .

            IF pcPathName BEGINS cName THEN
                RETURN ENTRY (1, UNBOX (oRegistry:GetValue (cName)), " ":U) .
        END.

        RETURN ?.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Searches the Registry for the location of an PDSOE Project
        Notes:   Based on the registry values under HKEY_CURRENT_USER\Software\Consultingwerk Ltd.\IDE Support
                 that are created by the _idestartup.
        @param pcPathName The path to lookup
        @return The path of the project or ? when no project is found
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER FindProjectPath (pcPathName AS CHARACTER):

        DEFINE VARIABLE oRegistry AS Microsoft.Win32.RegistryKey NO-UNDO .
        DEFINE VARIABLE cName     AS CHARACTER                   NO-UNDO .

        oRegistry = Consultingwerk.Framework.Registry:FindRegistryKey ("USER":U,
                                                                       "Software~\Consultingwerk Ltd.~\IDE Support":U) .

        {Consultingwerk/foreach.i System.Object oName in oRegistry:GetValueNames()}

            ASSIGN cName = UNBOX (oName) .

            IF pcPathName BEGINS cName THEN
                RETURN cName .
        END.

        RETURN ?.

    END METHOD .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns a logical value indicating if the current session was started
                 from the IDE
        Notes:
        @return Logical value indicating if the current session was started from the IDE
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL LaunchedFromIde ():

        DEFINE VARIABLE cStartupProcedure AS CHARACTER NO-UNDO.

        ASSIGN cStartupProcedure = StartupParameterHelper:GetStartupParameterValue ("-p":U) .

        ASSIGN cStartupProcedure = REPLACE (cStartupProcedure, "~\":U, "/":U) .

        RETURN ENTRY (NUM-ENTRIES (cStartupProcedure, "/":U), cStartupProcedure, "/":U) = "_debuglauncher.p":U .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sends a request/command to an OpenEdge Architect instance
        Notes:   See adecomm/oeideservice.p for details
        @param pcCommand The request to send to OpenEdge Architect
        @param piOEAPort The port number of the OpenEdge Architect instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SendRequest (pcCommand AS CHARACTER,
                                           piOEAPort AS INTEGER):

        DEFINE VARIABLE hSocket AS HANDLE      NO-UNDO.
        DEFINE VARIABLE mBuffer AS MEMPTR      NO-UNDO.
        DEFINE VARIABLE lStatus AS LOGICAL     NO-UNDO.

        IF pcCommand = ? THEN
            RETURN.

        CREATE SOCKET hSocket.

        lStatus = hSocket:CONNECT(SUBSTITUTE ("-H localhost -S &1":U, piOEAPort)) NO-ERROR.
        IF NOT lStatus THEN
            UNDO, THROW NEW AppError ("Unable to connect to OpenEdge Architect."{&TRAN}, 0) .

        pcCommand = pcCommand + "~n":U .
        SET-SIZE(mBuffer)      = 0.
        SET-SIZE(mBuffer)      = LENGTH(pcCommand) + 1.
        PUT-STRING(mBuffer, 1) = pcCommand.

        lStatus = hSocket:WRITE (mBuffer, 1, LENGTH(pcCommand)).
        SET-SIZE (mBuffer) = 0.
        IF NOT lStatus THEN
            RETURN.

        hSocket:DISCONNECT() NO-ERROR.

        FINALLY:
            IF VALID-HANDLE (hSocket) THEN
                DELETE OBJECT hSocket.
        END FINALLY.

    END METHOD.

END CLASS.
