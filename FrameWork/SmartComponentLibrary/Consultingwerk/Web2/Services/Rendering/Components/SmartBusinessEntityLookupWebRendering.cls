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
    File        : SmartBusinessEntityLookupWebRendering
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 16 19:47:17 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                    FROM PROPATH .
USING Consultingwerk.Assertion.*                          FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH.
USING Consultingwerk.Web2.Services.Rendering.*            FROM PROPATH .
USING Consultingwerk.Web2.Services.Rendering.Components.* FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.*           FROM PROPATH.
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.Web2.Services.Rendering.Components.SmartBusinessEntityLookupWebRendering
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
        DEFINE VARIABLE cGridLayout     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cLookupControls AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hBuffer         AS HANDLE    NO-UNDO .
        DEFINE VARIABLE i               AS INTEGER   NO-UNDO .

        HandleAssert:WidgetType (phAttributes, WidgetTypeEnum:Buffer) .
        BufferAssert:IsAvailable (phAttributes) .

        DO i = 1 TO NUM-ENTRIES (phAttributes::LookupControls):

            IF BufferHelper:HasField (phDataset:GET-BUFFER-HANDLE (ENTRY (1, pcTables)),
                                      ENTRY (i, phAttributes::LookupControls)) THEN

                ASSIGN cLookupControls = cLookupControls + ",":U + SUBSTITUTE ("&1.&2":U,
                                                                               pcScope,
                                                                               ENTRY (i, phAttributes::LookupControls)) .
            ELSE
                ASSIGN hBuffer = DatasetHelper:FindBufferWithField (phDataset,
                                                                    pcTables,
                                                                    ENTRY (i, phAttributes::LookupControls))
                       cLookupControls = cLookupControls + ",":U + SUBSTITUTE ("&1.&2[0].&3":U,
                                                                               pcScope,
                                                                               hBuffer:NAME,
                                                                               ENTRY (i, phAttributes::LookupControls)) .
        END.

        IF plMandatory THEN
            ASSIGN cRequired = "  ng-required ~n":U .

        IF INDEX (phAttributes::LookupGridLayout, "/":U) = 0 THEN
            ASSIGN cGridLayout = SUBSTITUTE ("&1/&2":U,
                                             phAttributes::EntityName,
                                             phAttributes::LookupGridLayout) .
        ELSE
            ASSIGN cGridLayout = phAttributes::LookupGridLayout .

        RETURN SUBSTITUTE (
               SUBSTITUTE ('<smart-lookup ~n':U +
                           '  smart-lookup-header="&1" ~n':U +
                           '  smart-lookup-key-field="&2" ~n':U +
                           '  smart-lookup-key-value-field="&3" ~n':U +
                           '  smart-lookup-fields="&4" ~n':U +
                           '  smart-lookup-fields-binding="&5" ~n':U +
                           '  smart-business-entity-name="&6" ~n':U +
                           '  smart-entity-table="&7" ~n':U +
                           '  smart-grid-layout="&8" ~n':U +
                           '  id="&9" ~n':U +

                           (IF poBindingStyle = ViewerBindingStyleEnum:AngularJS THEN
                           '  ng-model="&&1" ~n':U
                           ELSE
                           '  smartModel="&&1" ~n':U
                           ) +
                           '&&2':U +
                           '  style="width: &&3px;" ~n':U +
                           '  smart-lookup-dialog-options="width=700px; height=500px;"></smart-lookup> ~n':U,
                           phAttributes::LookupDialogTitle,
                           CharacterType:EmptyStringForUnknown(phAttributes::LookupKeyField),
                           CharacterType:EmptyStringForUnknown(phAttributes::LookupKeyValueColumn),
                           CharacterType:EmptyStringForUnknown(phAttributes::LookupFields),
                           TRIM (cLookupControls, ",":U),
                           phAttributes::EntityName,
                           phAttributes::EntityTable,
                           cGridLayout,
                           pcFieldId),
                           SUBSTITUTE ("&1&3&2":U, pcScope, pcFieldModel, (IF pcScope > "":U THEN ".":U ELSE "":U)),
                           cRequired,
                           phAttributes::WIDTH-PIXELS) .

    END METHOD .

END CLASS.
