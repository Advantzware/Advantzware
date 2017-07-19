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
    File        : embedwindow.i
    Purpose     :

    Syntax      :

    Description : Used as a Method Library, prepares the embedding of the
                  ABL Window

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 03 16:54:43 CET 2008
    Notes       : The preprocessor varible WinKitIgnoreCustomizations should
                  be used to avoid any impact of customer customizations to
                  the execution of the WinKit utilities.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{src/winkit/winkitactive.i}

DEFINE VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.
DEFINE VARIABLE hUpdateButton  AS HANDLE NO-UNDO.
{methods/defines/lValidateError.i}

&IF DEFINED (winkitactive) NE 0 &THEN


/* Mike Fechner, Consultingwerk Ltd. 06.02.2016
   Advantzware default is to use tab folders */
&IF "{&WinKitFormType}" = "" &THEN
&GLOBAL-DEFINE WinKitFormType Advantzware.WinKit.Forms.EmbeddedWindowForm
&ENDIF


DEFINE VARIABLE oForm AS Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm NO-UNDO .

&IF "{&WinKitFormType}":U <> "":U &THEN
DEFINE VARIABLE oFormControl AS {&WinKitFormType} NO-UNDO .
&ELSE
DEFINE VARIABLE oFormControl AS Consultingwerk.WindowIntegrationKit.Forms.EmbeddedWindowForm  NO-UNDO .
&ENDIF
&ENDIF


/* ************************  Function Prototypes ********************** */

&IF DEFINED (winkitactive) NE 0 &THEN
FUNCTION getEmbeddedWindowForm RETURNS Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm
    (  ) FORWARD.
&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED (winkitactive) NE 0 &THEN

FUNCTION getEmbeddedWindowForm RETURNS Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm
        (  ):
/*------------------------------------------------------------------------------
    Purpose:
    Notes:
------------------------------------------------------------------------------*/

    RETURN oForm .

END FUNCTION.
&ENDIF

/* ***************************  Main Block  *************************** */

&IF DEFINED (winkitactive) NE 0 &THEN

/* Mike Fechner, Consultingwerk Ltd. 23.08.2012
   An CHARCATER to hold a list of all frames in case  */
DEFINE VARIABLE cWinKitListOfFrameHandles AS CHARACTER NO-UNDO.
IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive AND Consultingwerk.Util.ProcedureHelper:HasEntry (THIS-PROCEDURE,
                                                 "WinKitAssignFrameHandles":U,
                                                 "PROCEDURE":U) THEN
    RUN WinKitAssignFrameHandles .

/* Mike Fechner, Consultingwerk Ltd. 08.03.2012 - WinKit
   Allow to not embed specific windows even when the session is configured to */
&IF DEFINED (WinKitDontEmbed) EQ 0 &THEN

&IF DEFINED (WINDOW-NAME) NE 0 AND "{&PROCEDURE-TYPE}":U NE "DIALOG-BOX":U &THEN

/* Mike Fechner, Consultingwerk Ltd. 08.12.2008
   Only create embedded window when WinKit has been activated */
IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive THEN
DO ON ERROR UNDO, THROW:

    DEFINE VARIABLE hWinKitTmpMenuBar AS HANDLE NO-UNDO .

    ASSIGN hWinKitTmpMenuBar = {&WINDOW-NAME}:MENU-BAR .

    /* Mike Fechner, Consultingwerk Ltd. 09.07.2012
       Possible Openedge 11.x issue: When starting the first EmbeddedWindowForm,
       the BaseForm constructor raises an SysError with no message... In this
       case retrying the Embedding does work. */
    DO ON ERROR UNDO, THROW:
        RUN Advantzware/WinKit/embedwindow.p ({&WINDOW-NAME}:HANDLE, "{&WinKitFormType}":U, OUTPUT oForm) .

        CATCH tmperr AS Progress.Lang.SysError :
            IF tmperr:NumMessages = 0 THEN
                RUN winkit/embedwindow.p ({&WINDOW-NAME}:HANDLE, "{&WinKitFormType}":U, OUTPUT oForm) .
            ELSE
                UNDO, THROW tmperr .
        END CATCH.
    END.

&IF "{&WinKitFormType}":U <> "":U &THEN
    ASSIGN oFormControl = CAST(CAST(oForm, Progress.Lang.Object), {&WinKitFormType})
&ELSE
    ASSIGN oFormControl = CAST(CAST(oForm, Progress.Lang.Object), Consultingwerk.WindowIntegrationKit.Forms.EmbeddedWindowForm)
&ENDIF
           oForm:WindowPositionRegistryKey = THIS-PROCEDURE:FILE-NAME
           oForm:ProcedureHandle           = THIS-PROCEDURE
           oForm:MENU-BAR                  = hWinKitTmpMenuBar
           oForm:OriginalWindowTitle       = {&window-name}:TITLE .

    /* Mike Fechner, Consultingwerk Ltd. 28.03.2011
       WinKit Windows will get deleted by closewindow.i, the
       new functionality in BaseForm:OnFormClosed / DeleteOnFormClosed
       is not required here */
    IF TYPE-OF (oFormControl, Consultingwerk.Forms.BaseForm) THEN
        CAST (oFormControl, Consultingwerk.Forms.BaseForm):DeleteOnFormClosed = FALSE .

    CATCH ple AS Progress.Lang.Error:

        Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple, "Error during embedding of ABL Window") .

    END CATCH.
END.
&ENDIF
&ENDIF
&ENDIF
