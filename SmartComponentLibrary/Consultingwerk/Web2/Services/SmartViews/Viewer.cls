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
  ----------------------------------------------------------------------*/

@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.Assertion.*                     FROM PROPATH .
USING Consultingwerk.Framework.*                     FROM PROPATH .
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Consultingwerk.Web2.Services.Exceptions.*      FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.*      FROM PROPATH .  
USING Progress.Lang.*                                FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.Services.SmartViews.Viewer: 

    DEFINE VARIABLE oConfig AS IConfigurationProvider NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Viewer class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Viewer ():
        
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

        SUPER ().

        FileAssert:Exists (".restapplicationsettings":U) .
        
        ASSIGN cFileName = FileHelper:FindFile (".restapplicationsettings":U) .
        
        ASSIGN oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider
                                                       "NEW ConfigurationProvider (cFileName)"} .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the HTML Fragment for the SmartViewer 
        Notes:   
        @param pcEntityName The name of the Business Entity to return a view spec form 
        @param pcViewName The name of the view 
        @param pcScope The Angular.JS scope property to bind to - selected will be used when not set
        @param pcCode OUTPUT The html fragment
    ------------------------------------------------------------------------------*/
	@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
	METHOD PUBLIC VOID GetFragment (pcEntityName AS CHARACTER,
	                                pcViewName AS CHARACTER,
	                                pcScope AS CHARACTER,
	                                OUTPUT pcCode AS LONGCHAR):
		
        DEFINE VARIABLE hDataset         AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE oParameter       AS GetBusinessEntityDescriptorParameter NO-UNDO .
        DEFINE VARIABLE oViewDescriptor  AS IBusinessEntityViewDescriptor        NO-UNDO . 
        DEFINE VARIABLE oTableDescriptor AS IBusinessEntityTableDescriptor       NO-UNDO . 
        DEFINE VARIABLE i                AS INTEGER                              NO-UNDO .
        DEFINE VARIABLE cColumnName      AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE oBufferFieldName AS BufferFieldName                      NO-UNDO . 
        DEFINE VARIABLE hBuffer          AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE hBufferField     AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE cTables          AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cFieldId         AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cFieldModel      AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE lMandatory       AS LOGICAL                              NO-UNDO .
        DEFINE VARIABLE lReadOnly        AS LOGICAL                              NO-UNDO .
        DEFINE VARIABLE cScope           AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE iWidth           AS INTEGER                              NO-UNDO .

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
        
        ASSIGN pcCode = '<form class="form-horizontal" style="margin-top: 0.5em; background-color: white;">~n':U + 
                        '    <fieldset>~n':U . 

        ASSIGN oViewDescriptor = oParameter:Descriptor:Views:GetItem (pcViewName) 
               cTables         = TRIM (SUBSTITUTE ("&1,&2":U, oViewDescriptor:EntityTable, oViewDescriptor:EntityView), ",":U).
        
        DO i = 1 TO NUM-ENTRIES (oViewDescriptor:ViewerColumns):
            
            ASSIGN cColumnName      = ENTRY (i, oViewDescriptor:ViewerColumns)
                   oBufferFieldName = BufferHelper:ParseFieldName (cColumnName) .
            
            IF oBufferFieldName:TableName > "":U THEN 
                ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (oBufferFieldName:TableName) .            
            ELSE 
                ASSIGN hBuffer = DatasetHelper:FindBufferWithField (hDataset, 
                                                                    cTables, 
                                                                    oBufferFieldName:FieldName)  .     
            
            IF NOT VALID-HANDLE (hBuffer) THEN 
                UNDO, THROW NEW Web2ServiceInterfaceException (SUBSTITUTE ("Invalid buffer for field name &1."{&TRAN}, cColumnName), 0) .
                
            BufferAssert:HasField (hBuffer, oBufferFieldName:FieldName) .    
                
            ASSIGN hBufferField = hBuffer:BUFFER-FIELD (oBufferFieldName:FieldName) . 
                       
            IF hBuffer:NAME = oViewDescriptor:EntityTable THEN 
                ASSIGN cFieldModel = hBufferField:NAME 
                       cFieldId    = hBufferField:NAME .
            ELSE 
                ASSIGN cFieldModel = SUBSTITUTE ("&1[0].&2":U, hBuffer:NAME, hBufferField:NAME) 
                       cFieldId     = SUBSTITUTE ("&1&2":U, hBuffer:NAME, hBufferField:NAME) .

            IF oParameter:Descriptor:Tables:ContainsName (hBuffer:NAME) THEN DO:
                oTableDescriptor = oParameter:Descriptor:Tables:GetItem (hBuffer:NAME) .
                
                ASSIGN lMandatory = ListHelper:EntryIsInList (hBufferField:NAME, oTableDescriptor:MandatoryColumns) 
                       lReadOnly  = ListHelper:EntryIsInList (hBufferField:NAME, oTableDescriptor:ReadOnlyColumns) .
            END .
            ELSE 
                ASSIGN lMandatory = FALSE
                       lReadOnly  = FALSE . 

            ASSIGN iWidth = FormatHelper:GetFormatWidthPixels (hBufferField:FORMAT) + 24. 
                   pcCode = pcCode + SUBSTITUTE ('        <div class="form-group">~n':U +
                                                 '            <label for="input&1" class="col-md-10 control-label">&3:</label>~n':U +
                                                 '            <div class="col-md-14">~n':U +
                                                 '                <input type="text" class="form-control" &4 &5 id="input&1" ng-model="&6.&2" style="width: &7px;" />~n':U +
                                                 '            </div>~n':U +
                                                 '        </div>~n':U , 
                                                 cFieldId, 
                                                 cFieldModel, 
                                                 hBufferField:LABEL,
                                                 IF lMandatory THEN 'ng-required':U ELSE '':U,
                                                 IF lReadOnly THEN 'ng-disabled="true"':U ELSE '':U,
                                                 cScope,
                                                 iWidth) .
        END.
            
        ASSIGN pcCode = pcCode + '    </fieldset>~n':U +
                                 '</form>~n':U .        
        
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
