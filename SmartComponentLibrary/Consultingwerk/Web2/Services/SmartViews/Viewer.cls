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
    File        : Viewer
    Purpose     : Returns the html fragment for a html viewer of
                  SmartComponents.Web2
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 18 11:04:20 CEST 2015
    Notes       : Supports Business Entity Descriptor defined views:
                  "Consultingwerk.SmartComponentsDemo.OERA.Sports2000.CustomerBusinessEntity#customer"
                  Supports custom log entry type "FieldSecurity"
  ----------------------------------------------------------------------*/

@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.Assertion.*                     FROM PROPATH .
USING Consultingwerk.Framework.*                     FROM PROPATH .
USING Consultingwerk.Framework.Authorization.*       FROM PROPATH .
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.*     FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Consultingwerk.Web2.Services.Exceptions.*      FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.*      FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.Services.SmartViews.Viewer:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Viewer class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Viewer ():

        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Generates the Viewer hmtl code
     * Notes:
     * @param poBusinessEntityDescriptor The reference to the Business Entity Descriptor of the Business Entity
     * @param pcEntityTable The name of the Entity Table (first table)
     * @param pcScope The scope to bind the UI controls to (including the .selected portion if required)
     * @param phDataset The handle of the Business Entity Dataset
     * @param pcTables The list of tables (entity table + entity view)
     * @param pcViewerColumns The list of columns to generate in the viewer
     * @param poBindingStyle The style of data bindings (AngularJS or Angular2)
     * @param plPerformSecurityCheck Logical value indicating if the method should perform field level security checks (hidden, read-only)
     * @param pcSecurityKey The security key
     * @return The html code
     */
    METHOD PUBLIC LONGCHAR GenerateViewer (poBusinessEntityDescriptor AS IBusinessEntityDescriptor,
                                           pcEntityTable AS CHARACTER,
                                           pcScope AS CHARACTER,
                                           phDataset AS HANDLE,
                                           pcTables AS CHARACTER,
                                           pcViewerColumns AS CHARACTER,
                                           poBindingStyle AS ViewerBindingStyleEnum,
                                           plPerformSecurityCheck AS LOGICAL,
                                           pcSecurityKey AS CHARACTER):

        DEFINE VARIABLE oTableDescriptor            AS IBusinessEntityTableDescriptor       NO-UNDO .
        DEFINE VARIABLE oRepository                 AS ISmartRepositoryService              NO-UNDO .
        DEFINE VARIABLE i                           AS INTEGER                              NO-UNDO .
        DEFINE VARIABLE cColumnName                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE oBufferFieldName            AS BufferFieldName                      NO-UNDO .
        DEFINE VARIABLE hBuffer                     AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE hBufferField                AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE cFieldId                    AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cFieldModel                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE lMandatory                  AS LOGICAL                              NO-UNDO .
        DEFINE VARIABLE lReadOnly                   AS LOGICAL                              NO-UNDO .
        DEFINE VARIABLE iWidth                      AS INTEGER                              NO-UNDO .
        DEFINE VARIABLE cCode                       AS LONGCHAR                             NO-UNDO .
        DEFINE VARIABLE cObjectName                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE oFieldAuthorizationProvider AS IFieldAuthorizationProvider          NO-UNDO .
        DEFINE VARIABLE oRestricted                 AS FieldRestrictionEnum                 NO-UNDO .
        DEFINE VARIABLE cSecurityField              AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE lFieldSecurityLogging       AS LOGICAL                              NO-UNDO .
        DEFINE VARIABLE oStopWatch                  AS Stopwatch                            NO-UNDO .

        oRepository = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Repository.ISmartRepositoryService
                                                    "NEW SmartRepositoryService ()"} .

        IF plPerformSecurityCheck THEN
            ASSIGN
                oFieldAuthorizationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.Authorization.IFieldAuthorizationProvider}
                lFieldSecurityLogging       = LogManager:IsCustomLogEntryTypeActive("FieldSecurity":U) .

        ASSIGN cCode = '<form class="form-horizontal" style="margin-top: 0.5em; background-color: white;">~n':U +
                       '    <fieldset>~n':U .

        IF pcScope = "":U AND poBindingStyle = ViewerBindingStyleEnum:Angular2 THEN
            pcScope = "selected":U .

        columnLoop:
        DO i = 1 TO NUM-ENTRIES (pcViewerColumns):

            ASSIGN cColumnName      = ENTRY (i, pcViewerColumns)
                   oBufferFieldName = BufferHelper:ParseFieldName (cColumnName) .

            IF oBufferFieldName:TableName > "":U THEN
                ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (oBufferFieldName:TableName) .
            ELSE
                ASSIGN hBuffer = DatasetHelper:FindBufferWithField (phDataset,
                                                                    pcTables,
                                                                    oBufferFieldName:FieldName)  .

            IF NOT VALID-HANDLE (hBuffer) THEN
                UNDO, THROW NEW Web2ServiceInterfaceException (SUBSTITUTE ("Invalid buffer for field name &1."{&TRAN}, cColumnName), 0) .

            BufferAssert:HasField (hBuffer, oBufferFieldName:FieldName) .

            IF plPerformSecurityCheck AND VALID-OBJECT (oFieldAuthorizationProvider) THEN DO:
                IF lFieldSecurityLogging THEN
                    oStopWatch = NEW Stopwatch().

                ASSIGN cSecurityField = SUBSTITUTE ("&1.&2":U, hBuffer:NAME, oBufferFieldName:FieldName)
                       oRestricted = oFieldAuthorizationProvider:IsFieldRestricted (? /* Requestor is meaningless here */,
                                                                                    cSecurityField,
                                                                                    pcSecurityKey) .

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

            IF hBuffer:NAME = pcEntityTable THEN
                ASSIGN cFieldModel = hBufferField:NAME
                       cFieldId    = hBufferField:NAME .
            ELSE
                ASSIGN cFieldModel = SUBSTITUTE ("&1[0].&2":U, hBuffer:NAME, hBufferField:NAME)
                       cFieldId     = SUBSTITUTE ("&1&2":U, hBuffer:NAME, hBufferField:NAME) .

            IF poBusinessEntityDescriptor:Tables:ContainsName (hBuffer:NAME) THEN DO:
                oTableDescriptor = poBusinessEntityDescriptor:Tables:GetItem (hBuffer:NAME) .

                ASSIGN lMandatory = ListHelper:EntryIsInList (hBufferField:NAME, oTableDescriptor:MandatoryColumns)
                       lReadOnly  = ListHelper:EntryIsInList (hBufferField:NAME, oTableDescriptor:ReadOnlyColumns) .
            END .
            ELSE
                ASSIGN lMandatory = FALSE
                       lReadOnly  = FALSE .

            IF VALID-OBJECT (oRestricted) AND oRestricted = FieldRestrictionEnum:Disabled THEN
                ASSIGN lReadOnly = TRUE .

            ASSIGN cObjectName = oRepository:GetEntityFieldMapping (poBusinessEntityDescriptor:BusinessEntityName,
                                                                    hBuffer:NAME,
                                                                    oBufferFieldName:FieldName,
                                                                    "WEB":U) .

            IF cObjectName > "":U THEN
                ASSIGN cCode = cCode + SUBSTITUTE ('        <div class="form-group">~n':U +
                                                   '            <label for="input&1" class="col-md-10 control-label">&3:</label>~n':U +
                                                   '            <div class="col-md-14">~n':U +
                                                   '&2~n':U +
                                                   '            </div>~n':U +
                                                   '        </div>~n':U ,
                                                   cFieldId,
                                                   oRepository:GetHtmlField (cObjectName,
                                                                             pcScope,
                                                                             cFieldModel,
                                                                             cFieldId,
                                                                             lMandatory,
                                                                             lReadOnly,
                                                                             phDataset,
                                                                             pcTables,
                                                                             poBindingStyle),
                                                   hBufferField:LABEL) .
            ELSE
                ASSIGN iWidth = FormatHelper:GetFormatWidthPixels (hBufferField:FORMAT) * 1.1 + 24
                       cCode = cCode + SUBSTITUTE ('        <div class="form-group">~n':U +
                                                   '            <label for="input&1" class="col-md-10 control-label">&3:</label>~n':U +
                                                   '            <div class="col-md-14">~n':U +

                                                   (IF poBindingStyle = ViewerBindingStyleEnum:AngularJS THEN
                                                   '                <input type="text" class="form-control" &4 &5 id="input&1" ng-model="&6&8&2" style="width: &7px;" />~n':U
                                                   ELSE
                                                   '                <input type="text" class="form-control" &4 &5 id="input&1" [(ngModel)]="&6&8&2" style="width: &7px;" [ngModelOptions]="~{standalone: true~}">~n':U) +
                                                   '            </div>~n':U +
                                                   '        </div>~n':U ,
                                                   cFieldId,
                                                   cFieldModel,
                                                   hBufferField:LABEL,
                                                   IF lMandatory THEN (IF poBindingStyle = ViewerBindingStyleEnum:AngularJS THEN 'ng-required':U ELSE 'required':U) ELSE '':U,
                                                   IF lReadOnly THEN (IF poBindingStyle = ViewerBindingStyleEnum:AngularJS THEN 'ng-disabled="true"':U ELSE 'disabled':U) ELSE '':U,
                                                   pcScope,
                                                   iWidth,
                                                   IF pcScope > "":U THEN ".":U ELSE "":U) .
        END.

        ASSIGN cCode = cCode + '    </fieldset>~n':U +
                               '</form>~n':U .

        RETURN cCode.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the HTML Fragment for the SmartViewer
        Notes:
        @param pcEntityName The name of the Business Entity to return a view spec form
        @param pcViewName The name of the view
        @param pcScope The Angular.JS scope property to bind to - selected will be used when not set
        @param poBindingStyle The style of data bindings (AngularJS or Angular2)
        @param pcCode OUTPUT The html fragment
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    METHOD PUBLIC VOID GetFragment (pcEntityName AS CHARACTER,
                                    pcViewName AS CHARACTER,
                                    pcScope AS CHARACTER,
                                    poBindingStyle AS ViewerBindingStyleEnum,
                                    OUTPUT pcCode AS LONGCHAR):

        DEFINE VARIABLE hDataset         AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE oParameter       AS GetBusinessEntityDescriptorParameter NO-UNDO .
        DEFINE VARIABLE oViewDescriptor  AS IBusinessEntityViewDescriptor        NO-UNDO .
        DEFINE VARIABLE cScope           AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cTables          AS CHARACTER                            NO-UNDO .

        Assert:NotNullOrEmpty (pcEntityName, "pcEntityName":U) .
        Assert:NotNullOrEmpty (pcViewName, "pcViewName":U) .

        IF pcScope > "":U THEN
            cScope = pcScope .
        ELSE
            cScope = "selected":U .

        oParameter = NEW GetBusinessEntityDescriptorParameter () .
        oParameter:ReturnDataset = TRUE .

        ServiceInterface:InvokeMethod (pcEntityName,
                                       "GetBusinessEntityDescriptor":U,
                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                       oParameter) .

        IF NOT oParameter:Descriptor:Views:ContainsName (pcViewName) THEN
            UNDO, THROW NEW Web2ServiceInterfaceException ("Invalid view name."{&TRAN}, 0) .

        ASSIGN oViewDescriptor = oParameter:Descriptor:Views:GetItem (pcViewName)
               cTables         = TRIM (SUBSTITUTE ("&1,&2":U, oViewDescriptor:EntityTable, oViewDescriptor:EntityView), ",":U).

        ASSIGN pcCode = THIS-OBJECT:GenerateViewer (oParameter:Descriptor,
                                                    oViewDescriptor:EntityTable,
                                                    pcScope,
                                                    hDataset,
                                                    cTables,
                                                    oViewDescriptor:ViewerColumns,
                                                    poBindingStyle,
                                                    TRUE,
                                                    SUBSTITUTE ("&1/&2":U, pcEntityName, pcViewName)) .

        CATCH err AS Progress.Lang.Error:
            LogManager:WriteError (err) .

            UNDO, THROW err .
        END CATCH.

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD .

END CLASS.
