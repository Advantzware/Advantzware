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
    File        : PostMessage.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Mar 30 21:28:45 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT  PARAMETER piHwnd        AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER piUmsg        AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER piwParam      AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER pilParam      AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER piReturnValue AS INTEGER NO-UNDO.

/* ***************************  Main Block  *************************** */

PROCEDURE PostMessageA EXTERNAL "user32":
    DEFINE INPUT PARAMETER hwnd         AS LONG.
    DEFINE INPUT PARAMETER umsg         AS LONG.
    DEFINE INPUT PARAMETER wparam       AS LONG.
    DEFINE INPUT PARAMETER lparam       AS LONG.
    DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE .


IF piHwnd > 0 THEN
    RUN PostMessageA (piHwnd, piUmsg, piwParam, pilParam, OUTPUT piReturnValue) .

