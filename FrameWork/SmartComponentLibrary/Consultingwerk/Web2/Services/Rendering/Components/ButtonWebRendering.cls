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
    File        : ButtonWebRendering
    Purpose     : Rendering of html buttons for Angular JS and Angular 2
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 16 23:35:49 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                    FROM PROPATH .
USING Consultingwerk.Assertion.*                          FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Web2.Services.Rendering.*            FROM PROPATH .
USING Consultingwerk.Web2.Services.Rendering.Components.* FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.*           FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.Web2.Services.Rendering.Components.ButtonWebRendering
    IMPLEMENTS IWebRenderingService:

    /**
     * Purpose: Renders a repository object as html
     * Notes:
     * @param pcObjectName The name of the repository field
     * @param pcScope The scope to bind the field to
     * @param pcFieldModel The model to bind the field to
     * @param pcFieldId  The Id of the field (JQuery)
     * @param plMandatory Logical value indicating if the field is mandatory
     * @param plReadOnly Logical value indicating if the field is mandatory
     * @param phAttributes The record buffer with the object attributes
     * @param phDataset The handle of the Business Entity Dataset
     * @param pcTables The list of tables (entity table + entity view)
     * @param poBindingStyle The style of data bindings (AngularJS or Angular2)
     * @return The longchar representation (hmtl code) of the field
     */
    METHOD PUBLIC LONGCHAR RenderHtml (pcObjectName AS CHARACTER,
                                       pcScope AS CHARACTER,
                                       pcFieldModel AS CHARACTER,
                                       pcFieldId AS CHARACTER,
                                       plMandatory AS LOGICAL,
                                       plReadOnly AS LOGICAL,
                                       phAttributes AS HANDLE,
                                       phDataset AS HANDLE,
                                       pcTables AS CHARACTER,
                                       poBindingStyle AS ViewerBindingStyleEnum):

        DEFINE VARIABLE cStyle AS CHARACTER NO-UNDO.

        HandleAssert:WidgetType (phAttributes, WidgetTypeEnum:Buffer) .
        BufferAssert:IsAvailable (phAttributes) .

        IF BufferHelper:HasField (phAttributes, "WebStyle":U) AND phAttributes::WebStyle > "":U THEN
            cStyle = phAttributes::WebStyle .

        IF BufferHelper:HasField (phAttributes, "HEIGHT-PIXELS":U) AND phAttributes::HEIGHT-PIXELS > 0 THEN
            cStyle = cStyle + SUBSTITUTE (" height: &1px;":U, phAttributes::HEIGHT-PIXELS) .

        IF BufferHelper:HasField (phAttributes, "WIDTH-PIXELS":U) AND phAttributes::WIDTH-PIXELS > 0 THEN
            cStyle = cStyle + SUBSTITUTE (" width: &1px;":U, phAttributes::WIDTH-PIXELS) .

        cStyle = TRIM (cStyle) .

        IF poBindingStyle = ViewerBindingStyleEnum:AngularJS THEN
            RETURN SUBSTITUTE ('<kendo-button &2 style="&3">&1</kendo-button>':U,
                               phAttributes:BUFFER-FIELD ("LABEL":U):BUFFER-VALUE,
                               (IF BufferHelper:HasField (phAttributes, "Click":U) AND phAttributes::Click > "":U
                                    THEN SUBSTITUTE ("ng-click=&1()":U, QUOTER (phAttributes::Click)) ELSE "":U),
                               cStyle) .
        ELSE
            RETURN SUBSTITUTE ('<button kendoButton &2 style="&3">&1</button>':U,
                               phAttributes:BUFFER-FIELD ("LABEL":U):BUFFER-VALUE,
                               (IF BufferHelper:HasField (phAttributes, "Click":U) AND phAttributes::Click > "":U
                                    THEN SUBSTITUTE ("(click)=&1()":U, QUOTER (phAttributes::Click)) ELSE "":U),
                               cStyle) .

    END METHOD .

END CLASS.
