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
    File        : parse_winuser_h.p
    Purpose     : Parser to winuser.h which is part of the Windows SDK

    Syntax      :

    Description : Generates %TEMP%\winuser_consts.i

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 29 21:33:08 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cConst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHex AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNum AS INT64 NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE STREAM out .

/* ***************************  Main Block  *************************** */

INPUT FROM "C:~\Program Files (x86)~\Microsoft SDKs~\Windows~\v7.0A~\Include~\WinUser.h":U .
OUTPUT STREAM out TO VALUE (SESSION:TEMP-DIRECTORY + "winuser_consts.i":U) .

DEFAULT-WINDOW:WIDTH = 210  .

REPEAT:
    IMPORT UNFORMATTED cLine .

    IF cLine BEGINS "#define":U AND NOT cLine BEGINS "#define _":U THEN DO:

        DO i = 1 TO LENGTH (cLine):
            ASSIGN cLine = REPLACE (cLine, "  ":U, " ":U) .
        END.

        IF NOT NUM-ENTRIES (cLine, " ":U) > 2 THEN
            NEXT .

        ASSIGN cConst = ENTRY (2, cLine, " ":U)
               cHex   = ENTRY (3, cLine, " ":U) .

        IF NOT cConst BEGINS "WM_":U OR cConst BEGINS "BM_":U THEN
            NEXT .

        IF cHex BEGINS "0x":U THEN DO ON ERROR UNDO, THROW:
            iNum = System.Int64:Parse(SUBSTRING (cHex, 3), System.Globalization.NumberStyles:AllowHexSpecifier) .

            @SuppressUnusedWarnings.
            CATCH err AS Progress.Lang.Error:
                NEXT .
            END CATCH.

        END.
        ELSE DO:
            ASSIGN iNum = INTEGER (cHex) NO-ERROR .

            IF ERROR-STATUS:ERROR OR ERROR-STATUS:NUM-MESSAGES > 0 THEN
                NEXT .
        END.

/*        DISPLAY cLine FORMAT "x(70)":U cConst cHex FORMAT "x(20)" iNum WITH WIDTH 200 .*/

        PUT STREAM out UNFORMATTED
            SUBSTITUTE ("DEFINE PUBLIC STATIC PROPERTY &1 AS INTEGER NO-UNDO INIT &2 /* &3 */":U,
                        cConst, iNum, cHex) SKIP
                       "GET.":U SKIP (1) .

    END.

END.


    MESSAGE "done":U
        VIEW-AS ALERT-BOX.