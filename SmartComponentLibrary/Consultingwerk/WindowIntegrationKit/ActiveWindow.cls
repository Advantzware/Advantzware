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
    File        : ActiveWindow
    Purpose     : Helper method to return the ACTIVE-WINDOW for embedded
                  and non embedded ABL Windows
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 30 23:27:56 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.*       FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .
USING System.Windows.Forms.*                      FROM ASSEMBLY .

CLASS Consultingwerk.WindowIntegrationKit.ActiveWindow:

    /*------------------------------------------------------------------------------
        Purpose: Returns the ACTIVE-WINDOW handle
        Notes:   Works for embedded (WinKit) and non embedded ABL Windows
        @return The HANLDE of the ACTIVE-WINDOW
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC HANDLE GetHandle ():

        DEFINE VARIABLE oForm               AS Form                NO-UNDO .
        DEFINE VARIABLE oEmbeddedWindowForm AS IEmbeddedWindowForm NO-UNDO .

        IF VALID-HANDLE (ACTIVE-WINDOW) THEN
            RETURN ACTIVE-WINDOW .

        /* Embedded Form */
        IF VALID-OBJECT (ACTIVE-FORM) AND TYPE-OF (ACTIVE-FORM, IEmbeddedWindowForm) THEN DO:

            oEmbeddedWindowForm = CAST (CAST (ACTIVE-FORM, Progress.Lang.Object), IEmbeddedWindowForm) .

            IF VALID-HANDLE (oEmbeddedWindowForm:EmbeddedWindow) THEN
                RETURN oEmbeddedWindowForm:EmbeddedWindow .
        END.

        /* MDI Child is Embedded Form */
        IF VALID-OBJECT (ACTIVE-FORM) AND TYPE-OF (ACTIVE-FORM, Form) THEN DO:

            oForm = CAST (CAST (ACTIVE-FORM, Progress.Lang.Object), Form) .

            IF VALID-OBJECT (oForm:ActiveMdiChild) AND TYPE-OF (oForm:ActiveMdiChild, IEmbeddedWindowForm) THEN DO:

                oEmbeddedWindowForm = CAST (CAST (oForm:ActiveMdiChild, Progress.Lang.Object), IEmbeddedWindowForm) .

                IF VALID-HANDLE (oEmbeddedWindowForm:EmbeddedWindow) THEN
                    RETURN oEmbeddedWindowForm:EmbeddedWindow .
            END.
        END.

        RETURN ? .

    END METHOD.

END CLASS.
