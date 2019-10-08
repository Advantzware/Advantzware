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
    File        : DetailViewGenerator
    Purpose     : Generates an html page containing a Kendo UI grid
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 04 11:05:32 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                                      FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.* FROM PROPATH .
USING Consultingwerk.Studio.ProcedureEditor.*                         FROM PROPATH .
USING Consultingwerk.Util.*                                           FROM PROPATH .
USING Progress.Lang.*                                                 FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.DetailViewGenerator:

    DEFINE VARIABLE lcSource  AS LONGCHAR NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Generates the html page based on the IKendoUiGridBuilder instance
        Notes:
        @param poBuilder The reference to the KendoUiGridBuilder instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Generate (poBuilder AS IDetailViewBuilder):

        DEFINE VARIABLE lcColumns AS LONGCHAR NO-UNDO .

        ObjectAssert:IsValid (poBuilder) .

        lcColumns = poBuilder:GetViewColumns() .

        COPY-LOB FROM FILE poBuilder:TemplateFileName TO lcSource .

        ASSIGN lcSource = REPLACE (lcSource, "@VIEW_ID@":U,      poBuilder:ViewID)
               lcSource = REPLACE (lcSource, "@VIEW_MODEL@":U,   poBuilder:ViewModel)
               lcSource = REPLACE (lcSource, "@VIEW_TITLE@":U,   poBuilder:ViewTitle)
               lcSource = REPLACE (lcSource, "@LIST_VIEW_ID@":U, poBuilder:ListViewID)
               lcSource = REPLACE (lcSource, "@LIST_ITEMS@":U,   lcColumns)
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows the preview
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ShowPreview ():

        DEFINE VARIABLE oEditorForm AS ProcedureEditorForm NO-UNDO .
        DEFINE VARIABLE hWindow     AS HANDLE              NO-UNDO .
        DEFINE VARIABLE hEditor     AS HANDLE              NO-UNDO .

        Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive = TRUE .

        RUN adecommx/_pwmain.p ("":U,
                                "Consultingwerk/BusinessEntityDesigner/Generator/TelerikMobile/blank.html":U,
                                "":U,
                                OUTPUT oEditorForm) .

        oEditorForm:MakeMdiChild = FALSE .


        ASSIGN hWindow = oEditorForm:EmbeddedWindow .

        RUN adecomm/_pwgeteh.p (hWindow , OUTPUT hEditor).

        oEditorForm:Text = "HTML Preview":U .

        hEditor:SCREEN-VALUE = "":U.

        hEditor:INSERT-STRING (STRING (lcSource)) .
        hEditor:MODIFIED = FALSE .

        oEditorForm:Show () .

    END METHOD .

END CLASS.
