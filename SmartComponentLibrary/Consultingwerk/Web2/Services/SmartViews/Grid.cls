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
    File        : Grid
    Purpose     : Returns the json fragment for a Kendo UI grid of 
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
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Consultingwerk.Web.Kendo.Grid.*                FROM PROPATH .
USING Consultingwerk.Web2.Services.Exceptions.*      FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.*      FROM PROPATH .  
USING Progress.Lang.*                                FROM PROPATH .
USING Consultingwerk.Framework.*                     FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.Services.SmartViews.Grid: 

    DEFINE VARIABLE oConfig AS IConfigurationProvider NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Grid class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Grid ():
        
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

        SUPER ().

        FileAssert:Exists (".restapplicationsettings":U) .
        
        ASSIGN cFileName = FileHelper:FindFile (".restapplicationsettings":U) .
        
        ASSIGN oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider
                                                       "NEW ConfigurationProvider (cFileName)"} .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the HTML Fragment for the SmartGrid 
        Notes:   
        @param pcEntityName The name of the Business Entity to return a view spec form 
        @param pcViewName The name of the view 
        @param pcCode OUTPUT The html fragment
    ------------------------------------------------------------------------------*/
	@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
	METHOD PUBLIC VOID GetFragment (pcEntityName AS CHARACTER,
	                                pcViewName AS CHARACTER,
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
        DEFINE VARIABLE oGrid            AS KendoGrid                            NO-UNDO .
        DEFINE VARIABLE oColumn          AS Column                               NO-UNDO . 

        Assert:NotNullOrEmpty (pcEntityName, "pcEntityName":U) .
        Assert:NotNullOrEmpty (pcViewName, "pcViewName":U) .

        oParameter = NEW GetBusinessEntityDescriptorParameter () .
        oParameter:ReturnDataset = TRUE .  
         
        ServiceInterface:InvokeMethod (pcEntityName,
                                       "GetBusinessEntityDescriptor":U,
                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                       oParameter) .

        IF NOT oParameter:Descriptor:Views:ContainsName (pcViewName) THEN 
            UNDO, THROW NEW Web2ServiceInterfaceException ("Invalid view name."{&TRAN}, 0) .
        
/*
{
                columns: [
                    { field: 'CustNum', title: 'Cust Num', width: 100 },
                    { field: 'Name' },
                    { field: 'State' },
                    { field: 'Country' },
                    { field: 'SalesRep' },
                    { field: 'RepName', title: 'Salesrep Name', sortable: false, filterable: false,
                        template: function (dataItem) 
                                  {
                                    if ((dataItem.eSalesrep) && dataItem.eSalesrep.length > 0 )
                                    {
                                        return dataItem.eSalesrep[0].RepName;
                                    }
                                    else 
                                        return "---";
                                  }
                    },
                    
                    { command: ['edit', 'destroy'], title: '&nbsp;', width: '250px' }
                ]                      
            }

*/                

        ASSIGN oViewDescriptor  = oParameter:Descriptor:Views:GetItem (pcViewName) 
               cTables          = TRIM (SUBSTITUTE ("&1,&2":U, oViewDescriptor:EntityTable, oViewDescriptor:EntityView), ",":U)
               
               oGrid            = NEW KendoGrid ()
               oGrid:dataSource = ?  .
        
        /* Default grid properties */
        ASSIGN oGrid:height =  400 
               oGrid:width = 600 
               oGrid:groupable = FALSE
               oGrid:reorderable = TRUE 
               oGrid:resizable = TRUE     
               oGrid:sortable = TRUE 
               oGrid:selectable = "row":U 
               oGrid:filterable = NEW Filterable ("menu, row":U) 
               oGrid:editable = NEW Editable ("inline":U)  

               oGrid:pageable = NEW Consultingwerk.Web.Kendo.Grid.Pageable () 
               oGrid:pageable:refresh = TRUE 
               oGrid:pageable:pageSizes = ArrayHelper:Array (10, 25, 100) 
               oGrid:pageable:pageSize = 10 .                           
               oGrid:pageable:buttonCount = 5 .
        
        DO i = 1 TO NUM-ENTRIES (oViewDescriptor:ListColumns):
            
            ASSIGN cColumnName      = ENTRY (i, oViewDescriptor:ListColumns)
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

            oColumn = oGrid:Columns:Add (hBufferField:NAME, hBufferField:LABEL) .

            IF hBuffer:NAME <> oViewDescriptor:EntityTable THEN 
            
                ASSIGN oColumn:Template = SUBSTITUTE ("function (dataItem) 
                                                      ~{
                                                          if ((dataItem.&1) &&&& dataItem.&1.length > 0 )
                                                          ~{
                                                              return dataItem.&1[0].&2;
                                                          }
                                                          else 
                                                              return ~"~";
                                                      }":U, 
                                                      hBuffer:NAME, 
                                                      hBufferField:NAME) .
        END.

        oColumn = oGrid:Columns:Add (NEW Column()) .
        oColumn:Command = ArrayHelper:Array ("destroy":U) .
        oColumn:title = "&nbsp;":U .
        oColumn:Width = "100px":U .   

        pcCode = oGrid:Serialize () . 

/*        pcCode = REPLACE (pcCode, "~"template~": ~"", "~"template~": ") .*/
/*        pcCode = REPLACE (pcCode, "}~"", "}") .                          */
/*        pcCode = REPLACE (pcCode, "\~"", "~"") .                         */

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
