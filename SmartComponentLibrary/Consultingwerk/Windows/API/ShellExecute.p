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
    File        : ShellExecute.p
    Purpose     :

    Syntax      :

    Description : See http://www.oehive.org/node/521

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 06 00:02:21 CET 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT  PARAMETER piHwnd AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER pcOperation AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pcFile AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pcParameter AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pcDirectory AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER nShowCmd AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER piInstance AS INTEGER NO-UNDO.

/* ***************************  Main Block  *************************** */

PROCEDURE ShellExecuteA EXTERNAL "shell32" :
    DEFINE INPUT PARAMETER HWND AS LONG.
    DEFINE INPUT PARAMETER lpOperation AS CHARACTER.
    DEFINE INPUT PARAMETER lpFile AS CHARACTER.
    DEFINE INPUT PARAMETER lpParameters AS CHARACTER.
    DEFINE INPUT PARAMETER lpDirectory AS CHARACTER.
    DEFINE INPUT PARAMETER nShowCmd AS LONG.
    DEFINE RETURN PARAMETER hInstance AS LONG.
END PROCEDURE .

RUN ShellExecuteA (piHwnd, pcOperation, pcFile, pcParameter, pcDirectory, nShowCmd, OUTPUT piInstance) .
