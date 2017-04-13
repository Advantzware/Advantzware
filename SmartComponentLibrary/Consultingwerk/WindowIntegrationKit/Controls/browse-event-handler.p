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
    File        : browse-event-handler.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 04 12:45:30 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .

DEFINE INPUT PARAMETER poGrid   AS RenderedBrowseControl NO-UNDO .
DEFINE INPUT PARAMETER phBrowse AS HANDLE                NO-UNDO .

DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
DEFINE VARIABLE i       AS INTEGER NO-UNDO.

/* ***************************  Main Block  *************************** */

DO i = 1 TO phBrowse:NUM-COLUMNS:
    ASSIGN hColumn = phBrowse:GET-BROWSE-COLUMN (i) .

    ON "entry":U OF hColumn
        PERSISTENT RUN HandleBrowseCellEnter IN THIS-PROCEDURE (hColumn) .
END.

PROCEDURE HandleBrowseCellEnter:
    DEFINE INPUT PARAMETER phColumn AS HANDLE NO-UNDO.

    IF poGrid:ApplyingEntry THEN
        RETURN .

    IF VALID-OBJECT (poGrid) AND VALID-HANDLE (phColumn) THEN
        poGrid:EnterBrowseCell (phColumn) .

    IF poGrid:ApplyingEntry = FALSE THEN
        RETURN NO-APPLY .

END PROCEDURE .
