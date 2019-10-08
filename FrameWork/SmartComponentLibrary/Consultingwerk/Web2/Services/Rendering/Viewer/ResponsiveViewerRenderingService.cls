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
    File        : ResponsiveViewerRenderingService
    Purpose     : Rendering service for responsive viewer designs
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 02 15:00:57 CET 2017
    Notes       : Supports custom log entry type "FieldSecurity"
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH.
USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.Assertion.*                      FROM PROPATH .
USING Consultingwerk.Exceptions.*                     FROM PROPATH .
USING Consultingwerk.Framework.Authorization.*        FROM PROPATH .
USING Consultingwerk.OERA.*                           FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.*      FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Web2.Services.Exceptions.*       FROM PROPATH .
USING Consultingwerk.Web2.Services.Rendering.*        FROM PROPATH .
USING Consultingwerk.Web2.Services.Rendering.Viewer.* FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.*       FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Web2.Services.Rendering.Viewer.ResponsiveViewerRenderingService
    IMPLEMENTS IWebViewerRenderingService:

    /**
     * Purpose: Renders the viewer template
     * Notes:
     * @param pcObjectName The repository object name of render
     * @param pcScope The binding scope
     * @param poBindings The ViewerBindingStyleEnum describing the style of the html template
     * @return The Angular viewer template
     */
    METHOD PUBLIC LONGCHAR RenderDynamicViewer (pcObjectName AS CHARACTER,
                                                pcScope AS CHARACTER,
                                                poBindings AS ViewerBindingStyleEnum):

        DEFINE VARIABLE oRepository     AS ISmartRepositoryService        NO-UNDO .
        DEFINE VARIABLE oParameter      AS FetchRepositoryObjectParameter NO-UNDO .
        DEFINE VARIABLE hRepositoryData AS HANDLE                         NO-UNDO .

        oRepository = {Consultingwerk/get-service.i ISmartRepositoryService "NEW SmartRepositoryService ()"} .

        oParameter = NEW FetchRepositoryObjectParameter (pcObjectName, FALSE) .

        hRepositoryData = oRepository:FetchRepositoryObject (oParameter) .

        RETURN THIS-OBJECT:RenderDynamicViewer (hRepositoryData, pcScope, poBindings) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hRepositoryData).
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Renders the viewer template
     * Notes:
     * @param phRepositoryData The repository data to render (ProDataset)
     * @param pcScope The binding scope
     * @param poBindingStyle The ViewerBindingStyleEnum describing the style of the html template
     * @return The Angular viewer template
     */
    METHOD PUBLIC LONGCHAR RenderDynamicViewer (phRepositoryData AS HANDLE,
                                                pcScope AS CHARACTER,
                                                poBindingStyle AS ViewerBindingStyleEnum):

        DEFINE VARIABLE hInstanceQuery              AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE lcHtml                      AS LONGCHAR                             NO-UNDO .
        DEFINE VARIABLE hAttributes                 AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE hDataSource                 AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE hDataSourceAttributes       AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE oRepository                 AS ISmartRepositoryService              NO-UNDO .
        DEFINE VARIABLE cEntityName                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cEntityTable                AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cEntityView                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cColumnName                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE oBufferFieldName            AS BufferFieldName                      NO-UNDO .
        DEFINE VARIABLE cTables                     AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE hDataset                    AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE hBuffer                     AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE hBufferField                AS HANDLE                               NO-UNDO .

        DEFINE VARIABLE oFieldAuthorizationProvider AS IFieldAuthorizationProvider          NO-UNDO .
        DEFINE VARIABLE oRestricted                 AS FieldRestrictionEnum                 NO-UNDO .
        DEFINE VARIABLE cSecurityField              AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE lFieldSecurityLogging       AS LOGICAL                              NO-UNDO .
        DEFINE VARIABLE oStopWatch                  AS Stopwatch                            NO-UNDO .
        DEFINE VARIABLE cFieldModel                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cFieldId                    AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE lReadOnly                   AS LOGICAL                              NO-UNDO .
        DEFINE VARIABLE lMandatory                  AS LOGICAL                              NO-UNDO .

        DEFINE VARIABLE oParameter                  AS GetBusinessEntityDescriptorParameter NO-UNDO .
        DEFINE VARIABLE oBusinessEntityDescriptor   AS IBusinessEntityDescriptor            NO-UNDO .
        DEFINE VARIABLE oTableDescriptor            AS IBusinessEntityTableDescriptor       NO-UNDO .
        DEFINE VARIABLE cObjectName                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE iWidth                      AS INTEGER                              NO-UNDO .
        DEFINE VARIABLE cClass                      AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cStyle                      AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cLabelClass                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cLabelStyle                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cLabel                      AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE lSuppressLabel              AS LOGICAL                              NO-UNDO .
        DEFINE VARIABLE cEventBinding               AS CHARACTER                            NO-UNDO .

        phRepositoryData::eSmartObjectMaster:FIND-FIRST () .

        ASSIGN
            oFieldAuthorizationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.Authorization.IFieldAuthorizationProvider}
            lFieldSecurityLogging       = LogManager:IsCustomLogEntryTypeActive("FieldSecurity":U) .

            hInstanceQuery = QueryHelper:CreatePreparedQuery (phRepositoryData::eSmartObjectInstance,
                                                              SUBSTITUTE ("for each eSmartObjectInstance where ContainerObjectMasterGuid = &1 ~
                                                                                                           and PageGuid = '' ~
                                                                                                           by ObjectSequence":U,
                                                                            QUOTER (phRepositoryData::eSmartObjectMaster::ObjectMasterGuid))) .

        oRepository = {Consultingwerk/get-service.i ISmartRepositoryService "NEW SmartRepositoryService ()"} .

        hAttributes = oRepository:GetAttributeBuffer (?, phRepositoryData) .

        DO ON ERROR UNDO, THROW:
            hDataSource = oRepository:FetchRepositoryObject (NEW FetchRepositoryObjectParameter (phRepositoryData::eSmartObjectMaster::DesignTimeDataSourceGuid,
                                                                                                 FALSE)) .

            hDataSourceAttributes = oRepository:GetAttributeBuffer(?, hDataSource) .

            ASSIGN cEntityName  = hDataSourceAttributes::EntityName
                   cEntityTable = hDataSourceAttributes::EntityTable
                   cEntityView  = hDataSourceAttributes::EntityView
                   cTables      = TRIM (SUBSTITUTE ("&1,&2":U, cEntityTable, cEntityView), ",":U).

            oParameter = NEW GetBusinessEntityDescriptorParameter () .
            oParameter:ReturnDataset = TRUE .

            ServiceInterface:InvokeMethod (cEntityName,
                                           "GetBusinessEntityDescriptor":U,
                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                           oParameter) .

            oBusinessEntityDescriptor = oParameter:Descriptor .

            CATCH err AS Progress.Lang.Error:
                UNDO, THROW NEW Exception (err, "Error fetching design time data-source from the repository."{&TRAN}, 0) .
            END CATCH.
        END.

        ASSIGN lcHtml = "<form":U .

        IF BufferHelper:HasField(hAttributes, "WebClass":U) AND hAttributes::WebClass > "":U THEN
            ASSIGN lcHtml = lcHtml + SUBSTITUTE (" class=&1":U, QUOTER (hAttributes::WebClass)) .

        IF BufferHelper:HasField(hAttributes, "WebStyle":U) AND hAttributes::WebStyle > "":U THEN
            ASSIGN lcHtml = lcHtml + SUBSTITUTE (" style=&1":U, QUOTER (hAttributes::WebStyle)) .

        IF BufferHelper:HasField(hAttributes, "WebController":U) AND hAttributes::WebController > "":U THEN
            ASSIGN lcHtml = lcHtml + SUBSTITUTE (" ng-controller=&1":U, QUOTER (hAttributes::WebController)) .

        GarbageCollectorHelper:DeleteObject (hAttributes) .

        ASSIGN lcHtml = lcHtml + ">~n    <fieldset>~n":U .

        columnLoop:
        DO WHILE NOT hInstanceQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

            ASSIGN hAttributes = oRepository:GetAttributeBuffer (phRepositoryData::eSmartObjectInstance::InstanceName,
                                                                 phRepositoryData) .

            IF NUM-ENTRIES (phRepositoryData::eSmartObjectInstance::ObjectMasterName, ".":U) > 1 THEN
                ASSIGN cColumnName = phRepositoryData::eSmartObjectInstance::ObjectMasterName .
            ELSE
                ASSIGN cColumnName = phRepositoryData::eSmartObjectInstance::InstanceName .

            ASSIGN oBufferFieldName = BufferHelper:ParseFieldName (cColumnName) .

            IF oBufferFieldName:TableName > "":U THEN
                ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (oBufferFieldName:TableName) .
            ELSE
                ASSIGN hBuffer = DatasetHelper:FindBufferWithField (hDataset,
                                                                    cTables,
                                                                    oBufferFieldName:FieldName)  .

            IF VALID-HANDLE (hBuffer) THEN DO:

                BufferAssert:HasField (hBuffer, oBufferFieldName:FieldName) .

                IF VALID-OBJECT (oFieldAuthorizationProvider) THEN DO:
                    IF lFieldSecurityLogging THEN
                        oStopWatch = NEW Stopwatch().

                    ASSIGN cSecurityField = SUBSTITUTE ("&1.&2":U, hBuffer:NAME, oBufferFieldName:FieldName)
                           oRestricted = oFieldAuthorizationProvider:IsFieldRestricted (? /* Requestor is meaningless here */,
                                                                                        cSecurityField,
                                                                                        phRepositoryData::eSmartObjectMaster::ObjectName) .

                    IF lFieldSecurityLogging THEN DO:
                        LogManager:WriteFormattedMessage ("[FieldSecurity] Security Settings for field &1 : &2 (&3 msec)":U,
                                                          cSecurityField,
                                                          oRestricted:ToString(),
                                                          STRING (oStopWatch:Elapsed),
                                                          NEW MessageSubSystem ("FieldSecurity":U)) .
                    END.

                    IF oRestricted = FieldRestrictionEnum:Hidden THEN
                        NEXT columnLoop .
                END.
                ELSE
                    ASSIGN oRestricted = ? .

                ASSIGN hBufferField = hBuffer:BUFFER-FIELD (oBufferFieldName:FieldName) .

                IF hBuffer:NAME = cEntityTable THEN
                    ASSIGN cFieldModel = oBufferFieldName:FieldName
                           cFieldId    = oBufferFieldName:FieldName .
                ELSE
                    ASSIGN cFieldModel = SUBSTITUTE ("&1[0].&2":U, hBuffer:NAME, oBufferFieldName:FieldName)
                           cFieldId    = SUBSTITUTE ("&1&2":U, hBuffer:NAME, oBufferFieldName:FieldName) .

                IF oBusinessEntityDescriptor:Tables:ContainsName (hBuffer:NAME) THEN DO:
                    oTableDescriptor = oBusinessEntityDescriptor:Tables:GetItem (hBuffer:NAME) .

                    ASSIGN lMandatory = ListHelper:EntryIsInList (hBufferField:NAME, oTableDescriptor:MandatoryColumns)
                           lReadOnly  = ListHelper:EntryIsInList (hBufferField:NAME, oTableDescriptor:ReadOnlyColumns) .
                END .
                ELSE
                    ASSIGN lMandatory = FALSE
                           lReadOnly  = FALSE .
            END.

            IF BufferHelper:HasField(hAttributes, "ENABLED":U) THEN
                ASSIGN lReadOnly = NOT hAttributes::ENABLED .

            IF VALID-OBJECT (oRestricted) AND oRestricted = FieldRestrictionEnum:Disabled THEN
                ASSIGN lReadOnly = TRUE .

            IF BufferHelper:HasField (hAttributes, "WebRenderingService":U) AND
                hAttributes::WebRenderingService > "":U THEN

                ASSIGN cObjectName = phRepositoryData::eSmartObjectInstance::ObjectMasterName .
            ELSE
                ASSIGN cObjectName = "":U .

            IF BufferHelper:HasField (hAttributes, "WebClass":U) AND hAttributes::WebClass > "":U THEN
                ASSIGN cClass = hAttributes::WebClass .
            ELSE
                ASSIGN cClass = "col-md-14":U .

            IF BufferHelper:HasField (hAttributes, "WebStyle":U) AND hAttributes::WebStyle > "":U THEN
                ASSIGN cStyle = hAttributes::WebStyle .
            ELSE
                ASSIGN cStyle = "":U .

            IF BufferHelper:HasField (hAttributes, "WebLabelClass":U) AND hAttributes::WebLabelClass > "":U THEN
                ASSIGN cLabelClass = hAttributes::WebLabelClass .
            ELSE
                ASSIGN cLabelClass = "col-md-14":U .

            IF BufferHelper:HasField (hAttributes, "WebLabelStyle":U) AND hAttributes::WebLabelStyle > "":U THEN
                ASSIGN cLabelStyle = hAttributes::WebLabelStyle .
            ELSE
                ASSIGN cLabelStyle = "":U .

            IF BufferHelper:HasField (hAttributes, "LABEL":U) AND hAttributes:BUFFER-FIELD ("LABEL":U):BUFFER-VALUE > "":U THEN
                ASSIGN cLabel = hAttributes:BUFFER-FIELD ("LABEL":U):BUFFER-VALUE .
            ELSE
                ASSIGN cLabel = hBufferField:LABEL.

            IF BufferHelper:HasField (hAttributes, "SuppressLabel":U) AND hAttributes::SuppressLabel <> ? THEN
                ASSIGN lSuppressLabel = hAttributes::SuppressLabel .
            ELSE
                ASSIGN lSuppressLabel = FALSE .

            IF cObjectName > "":U THEN
                ASSIGN lcHtml = lcHtml + SUBSTITUTE ('        <div class="form-group">~n':U +
                                                     (IF lSuppressLabel THEN "":U ELSE
                                                     '            <label for="input&1" class="&4" style="&5">&3:</label>~n':U) +
                                                     '            <div class="&6" style="&7">~n':U +
                                                     '            &2~n':U +
                                                     '            </div>~n':U +
                                                     '        </div>~n':U ,
                                                     cFieldId,
                                                     oRepository:GetHtmlField (cObjectName,
                                                                               pcScope,
                                                                               cFieldModel,
                                                                               cFieldId,
                                                                               lMandatory,
                                                                               lReadOnly,
                                                                               hDataset,
                                                                               cTables,
                                                                               poBindingStyle,
                                                                               hAttributes),
                                                     cLabel,
                                                     cLabelClass,
                                                     cLabelStyle,
                                                     cClass,
                                                     cStyle) .
            ELSE DO:
                IF BufferHelper:HasField(hAttributes, "WIDTH-PIXELS":U) AND hAttributes::WIDTH-PIXELS > 0 THEN
                    iWidth = hAttributes::WIDTH-PIXELS .
                ELSE
                    ASSIGN iWidth = FormatHelper:GetFormatWidthPixels (hBufferField:FORMAT) * 1.1 + 24 .

                ASSIGN cEventBinding = "":U .

                IF BufferHelper:HasField(hAttributes, "LEAVE":U) AND hAttributes:BUFFER-FIELD ("LEAVE":U):BUFFER-VALUE > "":U THEN DO:
                    IF poBindingStyle = ViewerBindingStyleEnum:AngularJS THEN
                        cEventBinding = cEventBinding + SUBSTITUTE (' ng-blur="&1()"':U, hAttributes:BUFFER-FIELD ("LEAVE":U):BUFFER-VALUE) .
                    ELSE
                        cEventBinding = cEventBinding + SUBSTITUTE (' (blur)="&1()"':U, hAttributes:BUFFER-FIELD ("LEAVE":U):BUFFER-VALUE) .
                END.

                IF BufferHelper:HasField(hAttributes, "ValueChanged":U) AND hAttributes::ValueChanged > "":U THEN DO:
                    IF poBindingStyle = ViewerBindingStyleEnum:AngularJS THEN
                        cEventBinding = cEventBinding + SUBSTITUTE (' ng-change="&1()"':U, hAttributes::ValueChanged) .
                    ELSE
                        cEventBinding = cEventBinding + SUBSTITUTE (' (ngModelChange)="&1()"':U, hAttributes::ValueChanged) .
                END.

                ASSIGN cEventBinding = TRIM (cEventBinding) .

                ASSIGN cStyle = TRIM (cStyle + SUBSTITUTE (' width: &1px;':U, iWidth))
                       lcHtml = lcHtml + SUBSTITUTE ('        <div class="form-group">~n':U +
                                                     (IF lSuppressLabel THEN "":U ELSE
                                                     '            <label for="input&1" class="':U + cLabelClass + '" style="':U + cLabelStyle + '">&3:</label>~n':U) +
                                                     '            <div class="col-md-14">~n':U +

                                                     (IF poBindingStyle = ViewerBindingStyleEnum:AngularJS THEN
                                                     '                <input type="text" class="':U + cClass + '" style="':U + cStyle + '" &4 &5 id="input&1" ng-model="&6&8&2" &9 />~n':U
                                                     ELSE
                                                     '                <input type="text" class="':U + cClass + '" style="':U + cStyle + '" &4 &5 id="input&1" [(ngModel)]="&6&8&2" [ngModelOptions]="~{standalone: true~}" &9 >~n':U) +
                                                     '            </div>~n':U +
                                                     '        </div>~n':U ,
                                                     cFieldId,
                                                     cFieldModel,
                                                     cLabel,
                                                     IF lMandatory THEN (IF poBindingStyle = ViewerBindingStyleEnum:AngularJS THEN 'ng-required':U ELSE 'required':U) ELSE '':U,
                                                     IF lReadOnly THEN (IF poBindingStyle = ViewerBindingStyleEnum:AngularJS THEN 'ng-disabled="true"':U ELSE 'disabled':U) ELSE '':U,
                                                     pcScope,
                                                     iWidth,
                                                     IF pcScope > "":U THEN ".":U ELSE "":U,
                                                     cEventBinding) .
            END.

            hInstanceQuery:GET-NEXT () .

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hAttributes) .
            END FINALLY.
        END.

        RETURN lcHtml + "    </fieldset>~n</form>":U .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hInstanceQuery) .
            GarbageCollectorHelper:DeleteObject (hAttributes) .
            GarbageCollectorHelper:DeleteObject (hDataSourceAttributes) .
            GarbageCollectorHelper:DeleteObject (hDataSource) .
            GarbageCollectorHelper:DeleteObject (hDataset) .
        END FINALLY.

    END METHOD .

END CLASS.
