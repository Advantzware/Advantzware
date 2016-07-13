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
    File        : SmartFrameworkSupportPlugin
    Purpose     : Business Entity Designer Plugin that allows creation 
                  of SmartFramework Support fields on the Business Entity 
                  Table
    Syntax      : 
    Description : Provides functionality to create the SmartFramework 
                  default fields to business entity temp-tables. Imports
                  the field definition from the XML file
                  Consultingwerk/BusinessEntityDesigner/Plugins/SmartFrameworkDefaultFields.template
                  - SmartAttachments (LOGICAL)
                  - SmartComments (LOGICAL)
                  - SmartRecordKey (CHARACTER)
                  - SmartCopiedFrom (CHARACTER)
                  
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 21 20:38:49 CEST 2012
    Notes       : Can be used as a template to add other standard fields
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                  FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.*           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*   FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Services.*  FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.UI.*        FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.UI.Shapes.* FROM PROPATH . 
USING Consultingwerk.Framework.Collections.*            FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH . 
USING Infragistics.Win.UltraWinToolbars.*               FROM ASSEMBLY  . 
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.SmartFrameworkSupportPlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

    DEFINE VARIABLE hTemplateDataset AS HANDLE NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the BusinessEntityDatasetController 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Controller AS BusinessEntityDatasetController NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Parent Form 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Form AS System.Windows.Forms.Form NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDataObjectImporterPlugin class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartFrameworkSupportPlugin ():
        SUPER ().
        
    END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
		Purpose:  Creates the SmartFramework default fields to the currently selected
		          temp-table 																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID CreateDefaultFields ():
		
        DEFINE VARIABLE oTables AS Crainiate.ERM4.Elements NO-UNDO . 
        DEFINE VARIABLE oModel  AS Crainiate.ERM4.Model    NO-UNDO.
        DEFINE VARIABLE oTable  AS TempTableShape          NO-UNDO .  
        DEFINE VARIABLE i       AS INTEGER                 NO-UNDO . 
        DEFINE VARIABLE hQuery  AS HANDLE                  NO-UNDO.
        DEFINE VARIABLE cField  AS CHARACTER NO-UNDO.
        
        DEFINE BUFFER b_eField FOR eField .
        
        ASSIGN hQuery = QueryHelper:CreatePreparedQuery (hTemplateDataset::eField) .
        
        oModel = THIS-OBJECT:Controller:Model . 
        
        ASSIGN oTables    = oModel:SelectedElements(Progress.Util.TypeHelper:GetType ("Crainiate.ERM4.Table":U)) .

        {Consultingwerk/foreach.i System.Collections.DictionaryEntry oEntry in oTables}

            oTable = CAST (oEntry:Value, 
                           TempTableShape) .

            hQuery:QUERY-OPEN () .
            hQuery:GET-FIRST () .
            
            DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

                FIND b_eField WHERE b_eField.TempTableName = oTable:Heading
                                AND b_eField.FieldName     = hTemplateDataset::eField::FieldName 
                    NO-ERROR .                 

                IF NOT AVAILABLE b_eField THEN DO:
    
                    ASSIGN cField = THIS-OBJECT:Controller:AddNewField (oTable) .
    
                    FIND b_eField WHERE b_eField.TempTableName = oTable:Heading
                                    AND b_eField.FieldName     = cField .                 
    
                    BUFFER b_eField:BUFFER-COPY (hTemplateDataset::eField:HANDLE,
                                                 "BusinessEntityName,TempTableName,FieldOrder":U) . 
                    
                END.
                
                FINALLY:
                    hQuery:GET-NEXT () .		
                END FINALLY.
            END.

        END.

        FINALLY:
            THIS-OBJECT:Controller:DrawShapes (oModel) .		
        END FINALLY.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shuts down the plugin                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Shutdown ():

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the plugin                                                                      
        Notes:                      
        @param dsBusinessEntity Business Entity with the Design dataset, called with the BIND keyword
        @param poController The BusinessEntityDatasetController
        @param poForm The main Form of the BusinessEntityDesigner
        @param poToolbar The reference to the UltraToolbarsManager used by the BusinessEntityDesigner                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Startup (DATASET FOR dsBusinessEntity BIND, 
                                poController AS BusinessEntityDatasetController, 
                                poForm AS BusinessEntityDesignerForm, 
                                poToolbar AS UltraToolbarsManager):
        
        DEFINE VARIABLE oGenerarteFields AS ButtonTool  NO-UNDO .
        DEFINE VARIABLE oGroup           AS RibbonGroup NO-UNDO .
        
        /* Initialize Template Dataset */
        CREATE DATASET hTemplateDataset . 
        hTemplateDataset:CREATE-LIKE (DATASET dsBusinessEntity:HANDLE) .

        FILE-INFO:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/Plugins/SmartFrameworkDefaultFields.template":U .         
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            hTemplateDataset:READ-XML ("FILE":U, FILE-INFO:FULL-PATHNAME, "EMPTY":U, ?, ?) .  
        
        THIS-OBJECT:Form = poForm . 
        THIS-OBJECT:Controller = poController . 
        
        oGenerarteFields = NEW ButtonTool ("SmartFrameworkSupport_Plugin_CreateDefaultFields":U) .
        oGenerarteFields:SharedProps:Caption = "Default fields"{&TRAN} .
        oGenerarteFields:SharedProps:ToolTipText = "Add default fields to the selected table"{&TRAN} .
        oGenerarteFields:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oGenerarteFields) .        

        FILE-INFO:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/table_new_16.png":U .        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            ASSIGN oGenerarteFields:SharedProps:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile 
                        (FILE-INFO:FULL-PATHNAME)  .        

        FILE-INFO:FILE-NAME = "Consultingwerk/BusinessEntityDesigner/UI/Images/table_new_32.png":U .        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            ASSIGN oGenerarteFields:SharedProps:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile 
                        (FILE-INFO:FULL-PATHNAME)  .        
        
        oGroup = poToolbar:Ribbon:Tabs [0]:Groups:Add ("SmartFramework":U) .
  
        oGroup:Tools:AddTool ("SmartFrameworkSupport_Plugin_CreateDefaultFields":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
        oGroup:Tools["SmartFrameworkSupport_Plugin_CreateDefaultFields":U]:InstanceProps:IsFirstInGroup = TRUE . 
  
        poToolbar:ToolClick:Subscribe (ToolClickHandler) .
  
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handles the ToolClick event of the Business Entity Designer's 
                 UltraToolbarsManager                                                                         
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToolClickHandler (sender AS System.Object,
                                            e AS ToolClickEventArgs):
        
        CASE e:Tool:Key:
            WHEN "SmartFrameworkSupport_Plugin_CreateDefaultFields":U THEN
                CreateDefaultFields () .
        END.
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END METHOD.

END CLASS.
