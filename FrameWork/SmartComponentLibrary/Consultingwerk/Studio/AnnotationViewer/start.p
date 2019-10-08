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
    File        : start.p
    Purpose     : Annotation Viewer Form startup procedure

    Syntax      :

    Description :

    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Wed Nov 30 13:01:59 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*    FROM PROPATH .
USING Consultingwerk.OERA.Context.* FROM PROPATH .

FUNCTION GetAnnotationFileName RETURNS CHARACTER () FORWARD.

{Consultingwerk/products.i}

DEFINE VARIABLE oForm           AS Consultingwerk.Studio.AnnotationViewer.AnnotationViewerForm NO-UNDO .
DEFINE VARIABLE cFileName       AS CHARACTER                                                   NO-UNDO .
DEFINE VARIABLE oTaskbarManager AS Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager         NO-UNDO .

/* Mike Fechner, Consultingwerk Ltd. 01.05.2013
   Best default for GUI apps */
SESSION:APPL-ALERT-BOXES = TRUE .
SESSION:DEBUG-ALERT = TRUE .
SESSION:ERROR-STACK-TRACE = TRUE  .
SESSION:SYSTEM-ALERT-BOXES = TRUE .

IF Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:IsPlatformSupported THEN DO:
    oTaskbarManager = Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:Instance .
    oTaskbarManager:ApplicationId = "de.consultingwerk.annotationviewer":U .
END.

ASSIGN cFileName = GetAnnotationFileName ().

IF cFileName NE ? AND cFileName NE "":U THEN
    oForm = NEW Consultingwerk.Studio.AnnotationViewer.AnnotationViewerForm (cFileName) .
ELSE
    oForm = NEW Consultingwerk.Studio.AnnotationViewer.AnnotationViewerForm () .

WAIT-FOR System.Windows.Forms.Application:RUN (oForm) .

CATCH e AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .
END CATCH.

FINALLY:
    DELETE OBJECT Consultingwerk.Framework.FrameworkSettings:WaitStateManager .

    QUIT .
END FINALLY.

/**
 * Purpose: Extracts the Annotations FileName from the session:parameter list
 * Notes:
 * @return Returns the FileName
 */
FUNCTION GetAnnotationFileName RETURNS CHARACTER ():

    DEFINE VARIABLE cParam  AS CHARACTER NO-UNDO INITIAL ?.
    DEFINE VARIABLE cRetVal AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i       AS INTEGER   NO-UNDO.

    parameterLoop:
    DO i = 1 TO NUM-ENTRIES (SESSION:PARAMETER):

        cParam = ENTRY (i, SESSION:PARAMETER).

        IF cParam BEGINS "open=":U THEN DO:

            ASSIGN cRetVal = ENTRY (2, cParam, "=":U).

            LEAVE parameterLoop .
        END.
    END.

    RETURN cRetVal.

END FUNCTION.
