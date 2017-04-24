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
    File        : EditorWebRendering
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 16 23:35:49 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                    FROM PROPATH .
USING Consultingwerk.Assertion.*                          FROM PROPATH .
USING Consultingwerk.Web2.Services.Rendering.*            FROM PROPATH .
USING Consultingwerk.Web2.Services.Rendering.Components.* FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.*           FROM PROPATH.
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.Web2.Services.Rendering.Components.EditorWebRendering
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

        DEFINE VARIABLE cRequired       AS CHARACTER NO-UNDO .

        HandleAssert:WidgetType (phAttributes, WidgetTypeEnum:Buffer) .
        BufferAssert:IsAvailable (phAttributes) .

        IF plMandatory THEN
            ASSIGN cRequired = "ng-required":U .

        IF poBindingStyle = ViewerBindingStyleEnum:AngularJS THEN
            RETURN SUBSTITUTE ('<textarea kendo-editor k-ng-model="&1" &2 style="width: &3px; height: &4px;"></textarea>':U,
                               SUBSTITUTE ("&1&3&2":U, pcScope, pcFieldModel, (IF pcScope > "":U THEN ".":U ELSE "":U)),
                               cRequired,
                               phAttributes::WIDTH-PIXELS,
                               phAttributes::HEIGHT-PIXELS) .
        ELSE
            RETURN SUBSTITUTE ('<textarea kendo-editor [(ngModel)]="&1" [ngModelOptions]="~{standalone: true~}" &2 style="width: &3px; height: &4px;"></textarea>':U,
                               SUBSTITUTE ("&1&3&2":U, pcScope, pcFieldModel, (IF pcScope > "":U THEN ".":U ELSE "":U)),
                               cRequired,
                               phAttributes::WIDTH-PIXELS,
                               phAttributes::HEIGHT-PIXELS) .


    END METHOD .

END CLASS.
