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
    File        : BusinessEntityDatasetController
    Purpose     : Dataset Controller for the Business Entity Dataset
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 13 20:03:46 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                                         FROM PROPATH .
USING Consultingwerk.Assertion.*                                               FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.*                                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*                        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.*           FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.Services.*                         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*                               FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.Shapes.*                        FROM PROPATH .
USING Consultingwerk.Exceptions.*                                              FROM PROPATH . 
USING Consultingwerk.Framework.Collections.*                                   FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                                    FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                                    FROM PROPATH . 
USING Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter   FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter FROM PROPATH . 
USING Consultingwerk.SmartComponents.Implementation.*                          FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*                              FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                                          FROM PROPATH .
USING Consultingwerk.Util.*                                                    FROM PROPATH .
USING Consultingwerk.Windows.API.*                                             FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                                      FROM PROPATH.
USING Progress.Lang.*                                                          FROM PROPATH .
USING System.Windows.Forms.*                                                   FROM ASSEMBLY .
 
CLASS Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController IMPLEMENTS IDatasetController: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i }
    { Consultingwerk/BusinessEntityDesigner/Services/eField.i &PREFIX="copy_" &NO-BEFORE}

    { Consultingwerk/BusinessEntityDesigner/Services/ttCustomProperties.i }

    DEFINE VARIABLE oBusinessEntityAdapter AS SmartBusinessEntityAdapter NO-UNDO .
    DEFINE VARIABLE oTableAdapter          AS SmartDatasetChildAdapter   NO-UNDO .
    DEFINE VARIABLE oDataRelationAdapter   AS SmartDatasetChildAdapter   NO-UNDO .
    DEFINE VARIABLE oFieldAdapter          AS SmartDatasetChildAdapter   NO-UNDO .
    DEFINE VARIABLE oIndexAdapter          AS SmartDatasetChildAdapter   NO-UNDO .

    DEFINE VARIABLE ro_Table        AS ROWID NO-UNDO . 
    DEFINE VARIABLE ro_DataRelation AS ROWID NO-UNDO .
    DEFINE VARIABLE ro_Field        AS ROWID NO-UNDO . 
    DEFINE VARIABLE ro_Index        AS ROWID NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Event raised after the source code has been compiled                                                                      
        Notes:          
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterCompileSource
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Event raised after a new file is created                                                                      
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterFileNew 
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS EventArgs).
        
    /*------------------------------------------------------------------------------
        Purpose: Event raised after a file is opened                                                                      
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The FileEventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterFileOpen 
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS FileEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised after a file is opened                                                                      
        Notes:         
        @param sender The reference to the object that raised the event
        @param e The FileEventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterFileSave 
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS FileEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised after the source code is generated                                                                       
        Notes:         
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterGenerateSource
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised after the source code is re-generated                                                                       
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterRegenerateSource
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS EventArgs).
                
    /*------------------------------------------------------------------------------
        Purpose: Raised for every field while adding a new Database Table to the design 
                 canvas 
        Notes:   Cancelable event, cancelling does not add the field to the new table
        @param sender The object that raised the BeforeAddingFieldToNewTable event
        @param e The BeforeAddingFieldToNewTableEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeAddingFieldToNewTable SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                    e AS BeforeAddingFieldToNewTableEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised for every Index while adding a new Database Table to the design 
                 canvas 
        Notes:   Cancelable event, cancelling does not add the Index to the new table
        @param sender The object that raised the BeforeAddingIndexToNewTable event
        @param e The BeforeAddingIndexToNewTableEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeAddingIndexToNewTable SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                    e AS BeforeAddingIndexToNewTableEventArgs).
                
    /*------------------------------------------------------------------------------
        Purpose: Event raised before the source code has been compiled                                                                      
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.CancelableEventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeCompileSource
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS CancelableEventArgs) .
        
    /*------------------------------------------------------------------------------
        Purpose: Event raised before a new file is created                                                                      
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.CancelableEventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeFileNew 
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS CancelableEventArgs).
        
    /*------------------------------------------------------------------------------
        Purpose: Event raised before a file is opened                                                                      
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The CancelableFileEventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeFileOpen 
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS CancelableFileEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised before a file is opened                                                                      
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The CancelableFileEventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeFileSave 
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS CancelableFileEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised before the source code is generated                                                                       
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.CancelableEventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeGenerateSource
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS CancelableEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised before the source code is re-generated                                                                       
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.CancelableEventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeRegenerateSource
        SIGNATURE VOID (sender AS Progress.Lang.Object, e AS CancelableEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised to allow plugins to return names of available custom property   
                 names
        Notes:   
        @param sender The object that raised the GetCustomPropertyNames event
        @param e The GetCustomPropertyNamesEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT GetCustomPropertyNames SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                      e AS GetCustomPropertyNamesEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised to allow plugins to return possible values for custom properties 
        Notes:   
        @param sender The object that raised the GetCustomPropertyValues event
        @param e The GetCustomPropertyValuesEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT GetCustomPropertyValues SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                e AS GetCustomPropertyValuesEventArgs).
    
	/*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeAssignRecord event handler of the DataRelation
                 Temp-Table Adapter 
        Notes:   
        @param sender The object that raised the BeforeAssignRecord event
        @param e The BeforeAssignRecordEventArgs with the data for the event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID BeforeAssignRecordDataRelationHandler (sender AS System.Object, 
                                                               e AS BeforeAssignRecordEventArgs):
		
		DEFINE BUFFER b_eDataRelation FOR eDataRelation. 
		
		IF UNBOX (oDataRelationAdapter:BindingSource:InputValue ["DataRelationNotActive":U]) = FALSE THEN DO:

            IF CAN-FIND (FIRST b_eDataRelation WHERE ROWID (b_eDataRelation)               <> e:BufferHandle:ROWID
                                                 AND b_eDataRelation.ChildTempTableName    = e:BufferHandle::ChildTempTableName 
                                                 AND b_eDataRelation.DataRelationNotActive = FALSE) THEN DO: 
		    
		      
		        MessageFormHelper:ShowMessage ("Child Temp-Table already has an active parent relation. Please make that inactive first!"{&TRAN},
		                                       "Business Entity Designer"{&TRAN},
		                                       MessageFormImages:ImageInfo) .
		        e:Cancel = TRUE . 
		    END.
		END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeAssignRecord event handler of the Table
                 Temp-Table Adapter 
        Notes:   
        @param sender The object that raised the BeforeAssignRecord event
        @param e The BeforeAssignRecordEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BeforeAssignRecordTableHandler (sender AS System.Object, 
                                                        e AS BeforeAssignRecordEventArgs):
        
        DEFINE VARIABLE cNewBufferNames AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cNewBufferName  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cOldBufferName  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i               AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cField          AS CHARACTER NO-UNDO .
        
        DEFINE BUFFER b_eField FOR eField. 
        
        ASSIGN cNewBufferNames = UNBOX (oTableAdapter:BindingSource:InputValue ["SourceBufferNames":U]) .
        
        IF e:BufferHandle::SourceBufferNames > "":U THEN .
        ELSE 
            RETURN . 
        
        IF e:BufferHandle::SourceBufferNames = cNewBufferNames THEN 
            RETURN . 
        
        DO i = 1 TO MIN (NUM-ENTRIES (e:BufferHandle::SourceBufferNames), NUM-ENTRIES (cNewBufferNames)):
            
            ASSIGN cNewBufferName = ENTRY (i, cNewBufferNames)
                   cOldBufferName = ENTRY (i, e:BufferHandle::SourceBufferNames) .
            
            
            
            FOR EACH b_eField WHERE b_eField.BusinessEntityName = e:BufferHandle::BusinessEntityName
                                AND b_eField.TempTableName      = e:BufferHandle::TempTableName
                                AND b_eField.FieldSource        BEGINS cOldBufferName + ".":U:

                ASSIGN cField                   = b_eField.FieldSource 
                       ENTRY (1, cField, ".":U) = cNewBufferName
                       b_eField.FieldSourc      = cField .
            END.
        END.

        oFieldAdapter:BindingSource:Refresh() .        

    END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Builds the name of the before table for the given temp-table 
	    Notes:
	    @param pcTableName Builds the Before Table Name from the Temp Table Name 
	    @return The before table name
	------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER BeforeTableName (pcTableName AS CHARACTER):
		
		FIND FIRST eBusinessEntity. 
		
        RETURN SUBSTITUTE ("&1&2&3":U, 
                           eBusinessEntity.DefaultBeforeTablePrefix,
                           SUBSTRING (pcTableName, LENGTH (eBusinessEntity.DefaultTablePrefix) + 1),
                           eBusinessEntity.DefaultBeforeTableSuffix) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides package information for the Entity Class of a temp-table
        Notes:   Calls into the current Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.IEntityPackageNameProvider
                 service or returns the Business Entity Package
        @param pcTempTableName The name of the new temp-table
        @return The Entity Package name         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetEntityPackageNameForTable (pcTempTableName AS CHARACTER):
		
		DEFINE VARIABLE oService AS IEntityPackageNameProvider NO-UNDO .
		DEFINE VARIABLE cPackage AS CHARACTER                  NO-UNDO .
		
		DEFINE BUFFER b_eBusinessEntity FOR eBusinessEntity .
		
		FIND FIRST b_eBusinessEntity .
		
		oService = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.IEntityPackageNameProvider} .
		
		IF VALID-OBJECT (oService) THEN 
		    ASSIGN cPackage = oService:GetEntityPackageNameForTable (pcTempTableName,
		                                                             DATASET dsBusinessEntity BY-REFERENCE) .
		    
		IF cPackage > "":U THEN 
		    RETURN cPackage .
		ELSE 
		    RETURN b_eBusinessEntity.BusinessEntityPackage . 
		
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the TempTableShape with the given key
        Notes:   
        @param pcKey The key (eTable ROWID) to locate
        @return The TempTableShape with the given key 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TempTableShape GetTableByKey (pcKey AS CHARACTER):
        
        {Consultingwerk/foreach.i System.Collections.DictionaryEntry oEntry in THIS-OBJECT:Model:Shapes}
            IF UNBOX (oEntry:Key) = pcKey THEN 
                RETURN CAST (oEntry:Value, TempTableShape) .
        END .

        RETURN ? . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE if the model already contains a Table with the given key
        Notes:   
        @param pcKey The key (eTable ROWID) to locate
        @return LOGICAL value indicating if the Model already has a table with the given key 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL HasTableWithKey (pcKey AS CHARACTER):
		
        {Consultingwerk/foreach.i System.Collections.DictionaryEntry oEntry in THIS-OBJECT:Model:Shapes}
            IF UNBOX (oEntry:Key) = pcKey THEN 
                RETURN TRUE .
        END .

        RETURN FALSE . 

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Returns TRUE when the field with the given Tag is an Extend field
	    Notes:   If the field cannot be found, no error is trown and FALSE is returned
	    @param pcFieldKey The field's Tag
	    @return Logical value indicating if the field is an array field
	------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL IsExtentField (pcFieldKey AS CHARACTER):
		
		DEFINE BUFFER b_eField FOR eField . 
		
		FIND b_eField WHERE ROWID (b_eField) = TO-ROWID (ENTRY (2, pcFieldKey)) NO-ERROR . 
		
		IF AVAILABLE b_eField THEN 
		    RETURN (b_eField.FieldExtent > 1) .
		    
		RETURN FALSE . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the GetCustomPropertyValues
        Notes:   
        @param e The GetCustomPropertyValuesEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnGetCustomPropertyValues (e AS GetCustomPropertyValuesEventArgs):
    		
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "GetCustomPropertyValues":U) .
     
        THIS-OBJECT:GetCustomPropertyValues:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raised an item in a Table Shape is selected
        Notes:   
        @param sender The object that raised the TableItemSelected event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT TableItemSelected SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                          e AS Consultingwerk.EventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Event raised when the UnsavedChanges property changes                                                                       
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT UnsavedChangesChanged SIGNATURE VOID 
        (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the BusinessEntityViewerControl 
                 instance                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityViewerControl AS SmartViewerControl NO-UNDO 
    GET.
    SET (arg AS SmartViewerControl):
        
        THIS-OBJECT:BusinessEntityViewerControl = arg . 
        
        arg:SmartTableIOStateChanged:Subscribe (SmartTableIOStateChangedHandler) .
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the DataRelationViewerControl 
                 instance                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataRelationViewerControl AS SmartViewerControl NO-UNDO 
    GET.
    SET (arg AS SmartViewerControl):
        
        THIS-OBJECT:DataRelationViewerControl = arg . 
        
        arg:SmartTableIOStateChanged:Subscribe (SmartTableIOStateChangedHandler) .
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the HANDLE of the dataset                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the UltraDockManager instance of the
                 Form                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DockManager AS Infragistics.Win.UltraWinDock.UltraDockManager NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the FieldViewerControl 
                 instance                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldViewerControl AS SmartViewerControl NO-UNDO 
    GET.
    SET (arg AS SmartViewerControl):
        
        THIS-OBJECT:FieldViewerControl = arg . 
        
        arg:SmartTableIOStateChanged:Subscribe (SmartTableIOStateChangedHandler) .
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the IndexViewerControl 
                 instance                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY IndexViewerControl AS SmartViewerControl NO-UNDO 
    GET.
    SET (arg AS SmartViewerControl):
        
        THIS-OBJECT:IndexViewerControl = arg . 
        
        arg:SmartTableIOStateChanged:Subscribe (SmartTableIOStateChangedHandler) .
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the Crainiate.ERM.Model                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Model AS Crainiate.ERM4.Model NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the ServiceContainer used by the 
                 Business Entity Designer                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ServiceContainer AS Consultingwerk.Framework.IServiceContainer NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the question to use the Business Entity Package 
                 should by used for all temp-tables with no path
        Notes:   Set to true in the new Business Entity Wizard                                                                        
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SuppressQuestionAboutDefaultPackage AS LOGICAL NO-UNDO INIT FALSE  
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the TableViewerControl 
                 instance                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableViewerControl AS SmartViewerControl NO-UNDO 
    GET.
    SET (arg AS SmartViewerControl):
        
        THIS-OBJECT:TableViewerControl = arg . 
        
        arg:SmartTableIOStateChanged:Subscribe (SmartTableIOStateChangedHandler) .
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the UltraTree Control used to show 
                 the temp-table hierachie              
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TreeView AS Infragistics.Win.UltraWinTree.UltraTree NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Dataset has unchanged changes                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UnsavedChanges AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET (arg AS LOGICAL):
        THIS-OBJECT:UnsavedChanges = arg .
        
        OnUnsavedChangesChanged (Consultingwerk.EventArgs:Empty) .
    END SET.  

    /*------------------------------------------------------------------------------
        Purpose: Default Constructor of the BusinessEntityDatasetController class                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityDatasetController ():
        SUPER ().

        DEFINE VARIABLE oFieldNameGeneratorService AS IFieldNameGeneratorService NO-UNDO .
        DEFINE VARIABLE oDataRelationFieldsService AS IDataRelationFieldsService NO-UNDO .
        DEFINE VARIABLE oTableNameGeneratorService AS ITableNameGeneratorService NO-UNDO .

        THIS-OBJECT:DatasetHandle = DATASET dsBusinessEntity:HANDLE .         
        
        BusinessEntityDesignerSettings:SettingsChanged:Subscribe (BusinessEntityDesignerSettingsSettingsChangedHandler) . 
        
        THIS-OBJECT:ServiceContainer = NEW Consultingwerk.Framework.ServiceContainer () .
        
        /* Mike Fechner, Consultingwerk Ltd. 17.02.2011
           Load FieldNameServiceGenerator */
        IF BusinessEntityDesignerSettings:FieldNameGeneratorService > "":U THEN 
        DO ON ERROR UNDO, THROW:   
            oFieldNameGeneratorService = DYNAMIC-NEW (BusinessEntityDesignerSettings:FieldNameGeneratorService) () .
    
            THIS-OBJECT:ServiceContainer:AddService (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.IFieldNameGeneratorService":U),
                                                     oFieldNameGeneratorService) .
            
            CATCH err AS Progress.Lang.Error :
                Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err,
                                                                  "Unable to load Field Name Generator"{&TRAN}) .
                                                                  
                THIS-OBJECT:ServiceContainer:AddService (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.IFieldNameGeneratorService":U),
                                                         NEW FieldNameGeneratorService ()) .
            END CATCH.
        END .
        
        /* Mike Fechner, Consultingwerk Ltd. 20.02.2011
           Load DataRelationFieldsService */
        IF BusinessEntityDesignerSettings:DataRelationFieldsService > "":U THEN 
        DO ON ERROR UNDO, THROW:   
            oDataRelationFieldsService = DYNAMIC-NEW (BusinessEntityDesignerSettings:DataRelationFieldsService) () .
    
            THIS-OBJECT:ServiceContainer:AddService (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.IDataRelationFieldsService":U),
                                                     oDataRelationFieldsService) .
            
            CATCH err AS Progress.Lang.Error :
                Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err,
                                                                  "Unable to load Data-Relation Fields Name Generator"{&TRAN}) .
                                                                  
                THIS-OBJECT:ServiceContainer:AddService (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.IDataRelationFieldsService":U),
                                                         NEW DataRelationFieldsService ()) .
            END CATCH.
        END .
    
        /* Mike Fechner, Consultingwerk Ltd. 25.02.2013
           Load the TableNameGeneratorService */
        IF BusinessEntityDesignerSettings:TableNameGeneratorService > "":U THEN 
        DO ON ERROR UNDO, THROW:   
            oTableNameGeneratorService = DYNAMIC-NEW (BusinessEntityDesignerSettings:TableNameGeneratorService) () .
    
            THIS-OBJECT:ServiceContainer:AddService (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.ITableNameGeneratorService":U),
                                                     oTableNameGeneratorService) .
            
            CATCH err AS Progress.Lang.Error :
                Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err,
                                                                  "Unable to load Table-Name Generator Service"{&TRAN}) .
                                                                  
                THIS-OBJECT:ServiceContainer:AddService (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.ITableNameGeneratorService":U),
                                                         NEW TableNameGeneratorService ()) .
            END CATCH.
        END .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Addes the child tables to the current UltraTreeNode                                                                       
        Notes:                                                      
        @param poNode The UltraTreeNode to add the child tables to
        @param pcBusinessEntityName The name of the Business Entity
        @param pcTableName The name of the current temp-table                   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddChildTables (poNode AS Infragistics.Win.UltraWinTree.UltraTreeNode,
                                          pcBusinessEntityName AS CHARACTER, 
                                          pcTableName AS CHARACTER):
        
        DEFINE BUFFER b_eTable        FOR eTable .
        DEFINE BUFFER b_eDataRelation FOR eDataRelation. 
        
        DEFINE VARIABLE oNode AS Infragistics.Win.UltraWinTree.UltraTreeNode .        
        
        FOR EACH b_eDataRelation WHERE b_eDataRelation.BusinessEntityName    = pcBusinessEntityName
                                   AND b_eDataRelation.ParentTempTableName   = pcTableName
                                   AND b_eDataRelation.DataRelationNotActive = FALSE,
            FIRST b_eTable WHERE b_eTable.BusinessEntityName = b_eDataRelation.BusinessEntityName
                             AND b_eTable.TempTableName      = b_eDataRelation.ChildTempTableName ON ERROR UNDO, THROW: 

                oNode = poNode:Nodes:Add (STRING (ROWID (b_eTable)), b_eTable.TempTableName) .
                
                AddChildTables (oNode, pcBusinessEntityName, b_eTable.TempTableName) .
        END.

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Add's am existing database Field to a Temp-Table                                                                        
        Notes:                 
        @param poTable The reference to the TempTableShape
        @param pcFieldName The name of the field 
        @param pcFieldProperties The CHR(1) delimited list of field properties
        @param plKeepSourceInformation Logical field indicating if the source information should be kept
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddExistingField (poTable AS TempTableShape,
                                         pcFieldName AS CHARACTER,
                                         pcFieldProperties AS CHARACTER,
                                         plKeepSourceInformation AS LOGICAL):

        DEFINE VARIABLE oFieldNameGeneratorService AS IFieldNameGeneratorService NO-UNDO .
        DEFINE VARIABLE oNewFieldHandler           AS INewFieldHandler           NO-UNDO . 
        DEFINE VARIABLE oBufferFieldName           AS BufferFieldName            NO-UNDO . 

        DEFINE BUFFER b_eTable FOR eTable .  
        DEFINE BUFFER b_eField FOR eField . 
        
        DEFINE VARIABLE hQuery AS HANDLE  NO-UNDO .
        
        DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO .
        
        /* Mike Fechner, Consultingwerk Ltd. 23.04.2011
           Deselect all other talbes */
        IF VALID-OBJECT (THIS-OBJECT:Model) THEN DO:
            DeselectShapes () .   

            poTable:Selected = TRUE . 
        END .

        oNewFieldHandler = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.INewFieldHandler} . 

        oFieldNameGeneratorService = CAST (THIS-OBJECT:ServiceContainer:GetService(Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.IFieldNameGeneratorService":U)),
                                           IFieldNameGeneratorService) .
        
        FIND b_eTable WHERE ROWID (b_eTable) = TO-ROWID (poTable:Key) .
        
        /* Mike Fechner, Consultingwerk Ltd. 23.02.2015
           SCL-670: Use of FieldNameGenerator Service */
        ASSIGN oBufferFieldName = BufferHelper:ParseFieldName (pcFieldName)
        
               cFieldName       = oFieldNameGeneratorService:GenerateFieldName (oBufferFieldName:TableName,
                                                                                oBufferFieldName:FieldName,
                                                                                b_eTable.TempTableName,
                                                                                TABLE eField BY-REFERENCE) .                
        
        IF CAN-FIND (b_eField WHERE b_eField.BusinessEntityName  = b_eTable.BusinessEntityName   
                                AND b_eField.TempTableName       = b_eTable.TempTableName         
                                AND b_eField.FieldName           = cFieldName) THEN 
            ASSIGN cFieldName = BuildUniqueFieldName (b_eTable.TempTableName, cFieldName).             
         
        CREATE b_eField.
        
        ASSIGN b_eField.BusinessEntityName  = b_eTable.BusinessEntityName   
               b_eField.TempTableName       = b_eTable.TempTableName         
               b_eField.FieldName           = cFieldName             
               b_eField.FieldOrder          = 99999  . /* very high */   

        IF NUM-ENTRIES (pcFieldProperties, CHR (1)) > 0 THEN 
            ASSIGN b_eField.FieldDataType = UPPER (ENTRY (1, pcFieldProperties, CHR (1))) .

        IF NUM-ENTRIES (pcFieldProperties, CHR (1)) > 1 THEN 
            ASSIGN b_eField.FieldInitial = ENTRY (2, pcFieldProperties, CHR (1)) .

        IF NUM-ENTRIES (pcFieldProperties, CHR (1)) > 2 THEN 
            ASSIGN b_eField.FieldLabel = ENTRY (3, pcFieldProperties, CHR (1)) .

        IF NUM-ENTRIES (pcFieldProperties, CHR (1)) > 3 THEN 
            ASSIGN b_eField.FieldFormat = ENTRY (4, pcFieldProperties, CHR (1)) .
            
        IF plKeepSourceInformation THEN 
            ASSIGN b_eField.FieldSource = SUBSTITUTE ("&1.&2":U, 
                                                      ENTRY (2, pcFieldName, ".":U),
                                                      ENTRY (3, pcFieldName, ".":U)) .
            
        IF VALID-OBJECT (oNewFieldHandler) THEN DO:                  
            oNewFieldHandler:HandleNewField (b_eField.TempTableName,
                                             b_eField.FieldName, 
                                             INPUT-OUTPUT DATASET dsBusinessEntity BY-REFERENCE) .  
        END.

        RELEASE b_eField . 

        ReorderFields (b_eTable.TempTableName) .

        IF VALID-OBJECT (THIS-OBJECT:Model) THEN
            poTable:DrawShapeDetails () .

        IF VALID-OBJECT (oFieldAdapter) THEN DO:
            hQuery = oFieldAdapter:QueryHandle .
            hQuery:QUERY-OPEN () .      
            
            IF oFieldAdapter:BindingSource:Count > 0 THEN 
                oFieldAdapter:BindingSource:Position = oFieldAdapter:BindingSource:Count - 1 .  
            
            IF VALID-OBJECT (THIS-OBJECT:FieldViewerControl) THEN DO:
                THIS-OBJECT:DockManager:PaneFromControl (THIS-OBJECT:FieldViewerControl):Activate () . 
            END.
        END.

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Add's am existing database Field to a Temp-Table                                                                        
        Notes:   Variant of this method without reference to the Business Entity Designer
                 UI and Model, usable in wizards
        @param proTableRowid The ROWID of the temp-table to add the field to
        @param pcFieldName The name of the field 
        @param pcFieldProperties The CHR(1) delimited list of field properties
        @param plKeepSourceInformation Logical field indicating if the source information should be kept
        @return The name of the field that was created
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER AddExistingField (proTableRowid AS ROWID,
                                              pcFieldName AS CHARACTER,
                                              pcFieldProperties AS CHARACTER,
                                              plKeepSourceInformation AS LOGICAL):

        DEFINE VARIABLE oFieldNameGeneratorService AS IFieldNameGeneratorService NO-UNDO .
        DEFINE VARIABLE oNewFieldHandler           AS INewFieldHandler           NO-UNDO . 
        DEFINE VARIABLE oBufferFieldName           AS BufferFieldName            NO-UNDO . 
        DEFINE VARIABLE cFormat                    AS CHARACTER                  NO-UNDO .
        DEFINE VARIABLE hBuffer                    AS HANDLE                     NO-UNDO .
        DEFINE VARIABLE hField                     AS HANDLE                     NO-UNDO . 

        DEFINE BUFFER b_eTable FOR eTable .  
        DEFINE BUFFER b_eField FOR eField . 
        
        DEFINE VARIABLE hQuery AS HANDLE  NO-UNDO .
        
        DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO .
        
        oNewFieldHandler = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.INewFieldHandler} . 

        oFieldNameGeneratorService = CAST (THIS-OBJECT:ServiceContainer:GetService(Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.IFieldNameGeneratorService":U)),
                                           IFieldNameGeneratorService) .
        
        FIND b_eTable WHERE ROWID (b_eTable) = proTableRowid .
        
        /* Mike Fechner, Consultingwerk Ltd. 23.02.2015
           SCL-670: Use of FieldNameGenerator Service */
        ASSIGN oBufferFieldName = BufferHelper:ParseFieldName (pcFieldName)
        
               cFieldName       = oFieldNameGeneratorService:GenerateFieldName (oBufferFieldName:TableName,
                                                                                oBufferFieldName:FieldName,
                                                                                eTable.TempTableName,
                                                                                TABLE eField BY-REFERENCE) .                
        
        IF oBufferFieldName:Index > 0 THEN DO ON ERROR UNDO, THROW:
            ASSIGN hField = ?  .
            
            IF oBufferFieldName:DatabaseName > "":U THEN 
                CREATE BUFFER hBuffer FOR TABLE SUBSTITUTE ("&1.&2":U, 
                                                            oBufferFieldName:DatabaseName,
                                                            oBufferFieldName:TableName) NO-ERROR .
            ELSE 
                CREATE BUFFER hBuffer FOR TABLE oBufferFieldName:TableName NO-ERROR .
                
            IF VALID-HANDLE (hBuffer) THEN 
                hField = hBuffer:BUFFER-FIELD (oBufferFieldName:FieldName) . 

            IF VALID-HANDLE (hField) THEN             
                ASSIGN cFormat = FormatHelper:IntegerFormatForMax (hField:EXTENT) .
            ELSE 
                ASSIGN cFormat = FormatHelper:IntegerFormatForMax (oBufferFieldName:Index) .
        
            ASSIGN cFieldName = SUBSTITUTE ("&1_&2":U, cFieldName, STRING (oBufferFieldName:Index, cFormat)) .

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hBuffer) .
                ErrorHelper:ResetErrorStatus() .
            END FINALLY.        
        END.
        
        IF CAN-FIND (b_eField WHERE b_eField.BusinessEntityName  = b_eTable.BusinessEntityName   
                                AND b_eField.TempTableName       = b_eTable.TempTableName         
                                AND b_eField.FieldName           = cFieldName) THEN 
            ASSIGN cFieldName = BuildUniqueFieldName (b_eTable.TempTableName, cFieldName).             
         
        CREATE b_eField.
        
        ASSIGN b_eField.BusinessEntityName  = b_eTable.BusinessEntityName   
               b_eField.TempTableName       = b_eTable.TempTableName         
               b_eField.FieldName           = cFieldName             
               b_eField.FieldOrder          = 99999  . /* very high */   

        IF NUM-ENTRIES (pcFieldProperties, CHR (1)) > 0 THEN 
            ASSIGN b_eField.FieldDataType = UPPER (ENTRY (1, pcFieldProperties, CHR (1))) .

        IF NUM-ENTRIES (pcFieldProperties, CHR (1)) > 1 THEN 
            ASSIGN b_eField.FieldInitial = ENTRY (2, pcFieldProperties, CHR (1)) .

        IF NUM-ENTRIES (pcFieldProperties, CHR (1)) > 2 THEN 
            ASSIGN b_eField.FieldLabel = ENTRY (3, pcFieldProperties, CHR (1)) .

        IF NUM-ENTRIES (pcFieldProperties, CHR (1)) > 3 THEN 
            ASSIGN b_eField.FieldFormat = ENTRY (4, pcFieldProperties, CHR (1)) .
            
        IF plKeepSourceInformation THEN 
            ASSIGN b_eField.FieldSource = SUBSTITUTE ("&1.&2":U, 
                                                      ENTRY (2, pcFieldName, ".":U),
                                                      ENTRY (3, pcFieldName, ".":U)) .
            
        IF VALID-OBJECT (oNewFieldHandler) THEN DO:                  
            oNewFieldHandler:HandleNewField (b_eField.TempTableName,
                                             b_eField.FieldName, 
                                             INPUT-OUTPUT DATASET dsBusinessEntity BY-REFERENCE) .  
        END.

        RELEASE b_eField . 

        ReorderFields (b_eTable.TempTableName) .

        RETURN cFieldName .

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Add's a new Field to a Temp-Table                                                                        
        Notes:  
        @param poTable The TempTableShape that is currently selected
        @return The name of the new field                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER AddNewField (poTable AS TempTableShape):
        
        DEFINE BUFFER b_eTable FOR eTable .  
        DEFINE BUFFER b_eField FOR eField . 
        
        DEFINE VARIABLE hQuery AS HANDLE  NO-UNDO .
        
        DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO INIT "Field":U .
        
        /* Mike Fechner, Consultingwerk Ltd. 23.04.2011
           Deselect all other talbes */
        DeselectShapes () .   

        poTable:Selected = TRUE . 
        
        FIND b_eTable WHERE ROWID (b_eTable) = TO-ROWID (poTable:Key) .
        
        ASSIGN cFieldName = BuildUniqueFieldName (b_eTable.TempTableName, cFieldName).             
         
        CREATE b_eField.
        
        ASSIGN b_eField.BusinessEntityName  = b_eTable.BusinessEntityName   
               b_eField.TempTableName       = b_eTable.TempTableName         
               b_eField.FieldName           = cFieldName             
               b_eField.FieldOrder          = 99999         /* very high */           
               b_eField.FieldDataType       = "CHARACTER":U         
               b_eField.FieldFormat         = "X(8)":U .
        
        RELEASE b_eField . 

        ReorderFields (b_eTable.TempTableName) .

        poTable:DrawShapeDetails () .

        hQuery = oFieldAdapter:QueryHandle .
        hQuery:QUERY-OPEN () .      
        
        IF oFieldAdapter:BindingSource:Count > 0 THEN 
            oFieldAdapter:BindingSource:Position = oFieldAdapter:BindingSource:Count - 1 .  
        
        IF VALID-OBJECT (THIS-OBJECT:FieldViewerControl) THEN DO:
            THIS-OBJECT:DockManager:PaneFromControl (THIS-OBJECT:FieldViewerControl):Activate() . 
        END.

        RETURN cFieldName . 

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Add's a new Index to a Temp-Table                                                                        
        Notes:             
        @param poTable The reference to the TempTableShape to add the Index to                                                           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddNewIndex (poTable AS TempTableShape):
        
        DEFINE BUFFER b_eTable FOR eTable .  
        DEFINE BUFFER b_eIndex FOR eIndex . 
        
        DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
        
        DEFINE VARIABLE cIndexName AS CHARACTER NO-UNDO INIT "Index":U .
        
        /* Mike Fechner, Consultingwerk Ltd. 23.04.2011
           Deselect all other talbes */
        DeselectShapes () .
        
        poTable:Selected = TRUE . 
        
        FIND b_eTable WHERE ROWID (b_eTable) = TO-ROWID (poTable:Key) .
        
        ASSIGN cIndexName = BuildUniqueIndexName (b_eTable.TempTableName, cIndexName).             
         
        CREATE b_eIndex.
        
        ASSIGN b_eIndex.BusinessEntityName  = b_eTable.BusinessEntityName   
               b_eIndex.TempTableName       = b_eTable.TempTableName         
               b_eIndex.IndexName           = cIndexName             
               b_eIndex.IndexOrder          = 99999         /* very high */  .
        
        RELEASE b_eIndex . 

        ReorderIndexes (b_eTable.TempTableName) .

        poTable:DrawShapeDetails () .

        hQuery = oIndexAdapter:QueryHandle .
        hQuery:QUERY-OPEN () .      
        
        IF oIndexAdapter:BindingSource:Count > 0 THEN 
            oIndexAdapter:BindingSource:Position = oIndexAdapter:BindingSource:Count - 1 .  
        
        IF VALID-OBJECT (THIS-OBJECT:IndexViewerControl) THEN DO:
            THIS-OBJECT:DockManager:PaneFromControl (THIS-OBJECT:IndexViewerControl):Activate() . 
        END.

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds another source table to an existing temp-table                                                                      
        Notes:   
        @param phDataset The HANDLE of the Dataset
        @param pcTempTableKey The key of the temp-table to add the source table to
        @param poModel The reference to the Crainiate Model component                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddSourceTable (phDataset AS HANDLE,
                                       pcTempTableKey AS CHARACTER,
                                       poModel AS Crainiate.ERM4.Model):
        
        DEFINE VARIABLE oFieldNameGeneratorService AS IFieldNameGeneratorService NO-UNDO .
        DEFINE VARIABLE hQuery                     AS HANDLE                     NO-UNDO .
        DEFINE VARIABLE iPos                       AS INTEGER                    NO-UNDO INIT 1.
        DEFINE VARIABLE oShape                     AS Crainiate.ERM4.Shape       NO-UNDO . 
        DEFINE VARIABLE cBufferName                AS CHARACTER                  NO-UNDO .
        DEFINE VARIABLE cFieldName                 AS CHARACTER                  NO-UNDO .
        DEFINE VARIABLE oDialogResultEnum          AS DialogResultEnum           NO-UNDO .
        DEFINE VARIABLE oNewFieldHandler           AS INewFieldHandler           NO-UNDO . 

        DEFINE BUFFER eTable   FOR eTable.
        DEFINE BUFFER eField   FOR eField. 
        DEFINE BUFFER b_eField FOR eField .

        oFieldNameGeneratorService = CAST (THIS-OBJECT:ServiceContainer:GetService(Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.IFieldNameGeneratorService":U)),
                                           IFieldNameGeneratorService) .
        
        phDataset::eTable:FIND-FIRST () .

        FIND eTable WHERE ROWID (eTable) = TO-ROWID (pcTempTableKey) . 

        FOR EACH eField OF eTable:
            ASSIGN iPos = MAX (iPos, eField.FieldOrder) . 
        END. 

        ASSIGN hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (phDataset::eField) 
        
              cBufferName = THIS-OBJECT:BuildUniqueBufferName (THIS-OBJECT:ShortTableName (phDataset::eTable::TempTableName)) .
        
        oNewFieldHandler = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.INewFieldHandler} . 
        
        DO WHILE NOT hQuery:QUERY-OFF-END:
            /* Mike Fechner, Consultingwerk Ltd. 17.02.2011
               Use of FieldNameGenerator Service */
            ASSIGN cFieldName = oFieldNameGeneratorService:GenerateFieldName (phDataset::eTable::TempTableName,
                                                                              phDataset::eField::FieldName,
                                                                              eTable.TempTableName,
                                                                              TABLE eField BY-REFERENCE) .                

            /* Mike Fechner, Consultingwerk Ltd. 22.11.2012
               When the table already contains a field with that name, 
               prompt if we should create a unique name */
            IF CAN-FIND (eField WHERE eField.BusinessEntityName = eTable.BusinessEntityName
                                  AND eField.TempTableName      = eTable.TempTableName
                                  AND eField.FieldName          = cFieldName) THEN DO:
        
                oDialogResultEnum = MessageFormHelper:AskQuestion (SUBSTITUTE ("The Table already contains a field named ~"&1~". Add anyway and ensure a unique field name?"{&TRAN}, cFieldName),
                                                                   "Business Entity Designer"{&TRAN}) .
        
                IF oDialogResultEnum = DialogResultEnum:DialogResultYes THEN DO:
                    
                    ASSIGN cFieldName = THIS-OBJECT:EnsureUniqueFieldName (eTable.BusinessEntityName,
                                                                           eTable.TempTableName,
                                                                           cFieldName) .
                END.
            END.

            /* Mike Fechner, Consultingwerk Ltd. 26.01.2011
               Fixed ability to add a second source table to a temp-table */
            IF NOT CAN-FIND (eField WHERE eField.BusinessEntityName = eTable.BusinessEntityName
                                      AND eField.TempTableName      = eTable.TempTableName
                                      AND eField.FieldName          = cFieldName) THEN DO:
            
                CREATE eField . 
                BUFFER eField:BUFFER-COPY (phDataset::eField:HANDLE) .
                
                ASSIGN iPos                      = iPos + 1
                       eField.BusinessEntityName = eTable.BusinessEntityName
                       eField.TempTableName      = eTable.TempTableName
                       eField.FieldDataType      = UPPER (eField.FieldDataType) 
                       eField.FieldOrder         = iPos  
                       eField.FieldSource        = SUBSTITUTE ("&1.&2":U,
                                                               cBufferName,
                                                               eField.FieldName)
                       eField.FieldName          = cFieldName
                       eField.FieldSerializeName = RemoveInvalidXmlCharacters (cFieldName)
                       .
                       
                IF VALID-OBJECT (oNewFieldHandler) THEN DO:                  
                    oNewFieldHandler:HandleNewField (eField.TempTableName,
                                                     eField.FieldName, 
                                                     INPUT-OUTPUT DATASET dsBusinessEntity BY-REFERENCE) .  
                END.
            END. 
                               
            hQuery:GET-NEXT () .                   
        END.
        
        DELETE OBJECT hQuery .

        ASSIGN eTable.SourceTableNames  = TRIM (eTable.SourceTableNames + 
                                                ",":U + 
                                                THIS-OBJECT:ShortTableName (phDataset::eTable::TempTableName), 
                                                ",":U) 
               eTable.SourceBufferNames = TRIM (eTable.SourceBufferNames + 
                                                ",":U + 
                                                cBufferName, 
                                                ",":U).

        /* Mike Fechner, Consultingwerk Ltd. 24.04.2013
           SCL-54: Remove other tables first */
        poModel:SelectElements (FALSE) .

        /* Mike Fechner, Consultingwerk Ltd. 05.07.2015
           SCL-609 - Ability to spilt array fields by default */
        IF BusinessEntityDesignerSettings:SplitArrayFields THEN DO:
            
            FOR EACH b_eField WHERE b_eField.BusinessEntityName = eTable.BusinessEntityName
                                AND b_eField.TempTableName      = eTable.TempTableName
                                AND b_eField.FieldExtent        > 0 
                    ON ERROR UNDO, THROW:

                THIS-OBJECT:SplitExtentField (b_eField.TempTableName, b_eField.FieldName) .                        
            END.                              

            IF VALID-OBJECT (THIS-OBJECT:Model) THEN 
                THIS-OBJECT:DrawShapes (THIS-OBJECT:Model) .
        END.


        ASSIGN oShape = CAST(poModel:Shapes[pcTempTableKey],
                             Crainiate.ERM4.Shape) .        

        IF TYPE-OF (oShape, TempTableShape) THEN DO: 
            CAST (oShape, TempTableShape):DrawShapeDetails () .

            oShape:Selected = TRUE .             
        END.  

        hQuery = oFieldAdapter:QueryHandle .
        hQuery:QUERY-OPEN () .      
        oTableAdapter:BindingSource:Refresh() . 

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterUpdateRecord event of the eBusinessEntity
                 Data Adapter                                                                       
        Notes:               
        @param sender The reference to the object that raised the event
        @param e The AfterUpdateRecordEventArgs with the data for this event                                                          
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterUpdateBusinessEntityRecordHandler (sender AS Progress.Lang.Object, 
                                                                  e AS AfterUpdateRecordEventArgs):
        
        DEFINE VARIABLE oDialogResult AS DialogResultEnum NO-UNDO . 
        
        DEFINE BUFFER b_eTable FOR eTable . 
        DEFINE BUFFER b_eDataRelation FOR eDataRelation . 
        DEFINE BUFFER b_eField FOR eField . 
        DEFINE BUFFER b_eIndex FOR eIndex . 
               
        FOR EACH b_eTable:
            ASSIGN b_eTable.BusinessEntityName = e:BufferHandle::BusinessEntityName . 
        END.

        FOR EACH b_eDataRelation:
            ASSIGN b_eDataRelation.BusinessEntityName = e:BufferHandle::BusinessEntityName . 
        END.

        FOR EACH b_eField:
            ASSIGN b_eField.BusinessEntityName = e:BufferHandle::BusinessEntityName . 
        END.

        FOR EACH b_eIndex:
            ASSIGN b_eIndex.BusinessEntityName = e:BufferHandle::BusinessEntityName . 
        END.

        /* Mike Fechner, Consultingwerk Ltd. 28.01.2011
           Update temp-table path? */
        IF e:BufferHandle::DefaultTablePath > "":U AND 
           CAN-FIND (FIRST b_eTable WHERE b_eTable.TempTablePath = "":U) THEN DO:
                        
            IF THIS-OBJECT:SuppressQuestionAboutDefaultPackage THEN 
                oDialogResult = DialogResultEnum:DialogResultYes .                   
            ELSE 
                oDialogResult = MessageFormHelper:AskQuestion 
                    ("Use the default temp-table path for all existing temp-tables that have no path?"{&TRAN},
                     "Business Entity Designer"{&TRAN}) .                    
           
            IF oDialogResult:Equals (DialogResultEnum:DialogResultYes) THEN DO:
                
                THIS-OBJECT:AssignTempTablePath (e:BufferHandle::DefaultTablePath) .                
            END.
        END.

        IF VALID-OBJECT (oTableAdapter) THEN DO:
            oTableAdapter:ParentPositionChanged (?, ?) .
            oTableAdapter:BindingSource:Refresh () .
        END.
        
        IF VALID-OBJECT (oDataRelationAdapter) THEN 
            oDataRelationAdapter:ParentPositionChanged (?, ?) .
        
        IF VALID-OBJECT (oFieldAdapter) THEN
            oFieldAdapter:ParentPositionChanged (?, ?) .
        
        IF VALID-OBJECT (oIndexAdapter) THEN 
            oIndexAdapter:ParentPositionChanged (?, ?) .

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterUpdateRecord event of the eField Data Adapter                                                                       
        Notes:               
        @param sender The reference to the object that raised the event
        @param e The AfterUpdateRecordEventArgs with the data for this event                                                          
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterUpdateFieldRecordHandler (sender AS Progress.Lang.Object, 
                                                         e AS AfterUpdateRecordEventArgs):
        
        DEFINE VARIABLE oShape AS Crainiate.ERM4.Shape NO-UNDO . 
        
        DEFIN BUFFER b_eTable FOR eTable . 
        
        FIND b_eTable WHERE b_eTable.BusinessEntityName = e:BufferHandle::BusinessEntityName
                        AND b_eTable.TempTableName      = e:BufferHandle::TempTableName
              NO-LOCK NO-ERROR . 

        ASSIGN oShape = CAST(THIS-OBJECT:Model:Shapes[STRING(ROWID(b_eTable))],
                             Crainiate.ERM4.Shape) .        

        IF TYPE-OF (oShape, TempTableShape) THEN 
            CAST (oShape, TempTableShape):DrawShapeDetails () .    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterUpdateRecord event of the eIndex Data Adapter                                                                       
        Notes:               
        @param sender The reference to the object that raised the event
        @param e The AfterUpdateRecordEventArgs with the data for this event                                                          
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterUpdateIndexRecordHandler (sender AS Progress.Lang.Object, 
                                                         e AS AfterUpdateRecordEventArgs):
        
        DEFINE VARIABLE oShape AS Crainiate.ERM4.Shape NO-UNDO . 
        
        DEFIN BUFFER b_eTable FOR eTable . 
        
        FIND b_eTable WHERE b_eTable.BusinessEntityName = e:BufferHandle::BusinessEntityName
                        AND b_eTable.TempTableName      = e:BufferHandle::TempTableName
              NO-LOCK NO-ERROR . 

        ASSIGN oShape = CAST(THIS-OBJECT:Model:Shapes[STRING(ROWID(b_eTable))],
                             Crainiate.ERM4.Shape) .        

        IF TYPE-OF (oShape, TempTableShape) THEN 
            CAST (oShape, TempTableShape):DrawShapeDetails () .    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterUpdateRecord event of the eRelation
                 Data Adapter                                                                       
        Notes:               
        @param sender The reference to the object that raised the event
        @param e The AfterUpdateRecordEventArgs with the data for this event                                                          
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterUpdateDataRelationRecordHandler (sender AS Progress.Lang.Object, 
                                                                e AS AfterUpdateRecordEventArgs):
        
        DEFINE VARIABLE oConnector AS Crainiate.ERM4.Connector NO-UNDO . 
        
        ASSIGN oConnector = CAST(THIS-OBJECT:Model:Lines[STRING(e:BufferHandle:ROWID)],
                                 Crainiate.ERM4.Connector) .
        
        IF e:BufferHandle::DataRelationNotActive = CAST (oConnector, DataRelationConnector):Active 
            THEN THIS-OBJECT:BuildTreeView () .
        
        IF TYPE-OF (oConnector, DataRelationConnector) THEN 
            CAST (oConnector, DataRelationConnector):DrawShapeDetails () .    

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterUpdateRecord event of the eTable Data Adapter                                                                       
        Notes:               
        @param sender The reference to the object that raised the event
        @param e The AfterUpdateRecordEventArgs with the data for this event                                                          
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterUpdateTableRecordHandler (sender AS Progress.Lang.Object, 
                                                         e AS AfterUpdateRecordEventArgs):
        
        DEFINE VARIABLE oShape AS Crainiate.ERM4.Shape NO-UNDO . 
        
        DEFINE BUFFER b_eField FOR eField .
        DEFINE BUFFER b_eIndex FOR eIndex .
        DEFINE BUFFER b_eDataRelation FOR eDataRelation . 
        
        DEFINE VARIABLE lRelationChanged AS LOGICAL NO-UNDO INIT FALSE .
        
        ASSIGN oShape = CAST(THIS-OBJECT:Model:Shapes[STRING(e:BufferHandle:ROWID)],
                             Crainiate.ERM4.Shape) .
        
        FOR EACH b_eField WHERE b_eField.BusinessEntityName = e:BufferHandle::BusinessEntityName 
                            AND b_eField.TempTableName = CAST(oShape, TempTableShape):Heading:
            ASSIGN b_eField.TempTableName = e:BufferHandle::TempTableName . 
        END.

        FOR EACH b_eIndex WHERE b_eIndex.BusinessEntityName = e:BufferHandle::BusinessEntityName 
                            AND b_eIndex.TempTableName = CAST(oShape, TempTableShape):Heading:
                                
            ASSIGN b_eIndex.TempTableName = e:BufferHandle::TempTableName . 
        END.
        
        FOR EACH b_eDataRelation WHERE b_eDataRelation.BusinessEntityName  = e:BufferHandle::BusinessEntityName 
                                   AND b_eDataRelation.ParentTempTableName = CAST(oShape, TempTableShape):Heading:
  
            ASSIGN b_eDataRelation.ParentTempTableName = e:BufferHandle::TempTableName 
                   lRelationChanged                    = TRUE .
        END . 

        FOR EACH b_eDataRelation WHERE b_eDataRelation.BusinessEntityName  = e:BufferHandle::BusinessEntityName 
                                   AND b_eDataRelation.ChildTempTableName = CAST(oShape, TempTableShape):Heading:
  
            ASSIGN b_eDataRelation.ChildTempTableName = e:BufferHandle::TempTableName 
                   lRelationChanged                   = TRUE .
        END . 
        
        IF TYPE-OF (oShape, TempTableShape) THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 26.06.2011
               Rename of shape */
            CAST (oShape, TempTableShape):Heading = e:BufferHandle::TempTableName .
            CAST (oShape, TempTableShape):DrawShapeDetails () .
        END.    

        THIS-OBJECT:BuildTreeView () .

        IF lRelationChanged THEN 
            THIS-OBJECT:oDataRelationAdapter:BindingSource:Refresh () .

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Assigns a path and package to all temp-tables that do not have a 
                 path or package assigned yet 
        Notes:   Allows customers that implement their own packages to use the same     
                 code that is executed at the "Use the default temp-table path for 
                 all existing temp-tables that have no path?" prompt in the Visual 
                 Designer 
        @param pcTempTablePath The path to assign to all temp-tables that don't have a path
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID AssignTempTablePath (pcTempTablePath AS CHARACTER):
                
        DEFINE BUFFER b_eTable FOR eTable .                 
                
        FOR EACH b_eTable WHERE b_eTable.TempTablePath = "":U:
            ASSIGN b_eTable.TempTablePath = pcTempTablePath .
        END.
        
        FOR EACH b_eTable WHERE b_eTable.EntityPackageName = "":U:
            ASSIGN b_eTable.EntityPackageName = THIS-OBJECT:GetEntityPackageNameForTable (b_eTable.TempTableName) .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeUpdateRecord event of the eBusinessEntity
                 Data Adapter                                                                       
        Notes:               
        @param sender The reference to the object that raised the event
        @param e The BeforeUpdateRecordEventArgs with the data for this event                                                          
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID BeforeUpdateBusinessEntityRecordHandler (sender AS Progress.Lang.Object, 
                                                                   e AS BeforeUpdateRecordEventArgs):
		
		oBusinessEntityAdapter:QueryHandle:GET-FIRST () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Checks if a Buffer is already used as a SourceBufferName                                                                       
        Notes:   Used by BuildUniqueBufferName  
        @param pcBufferName The buffer name to test
        @return Logical value indicating if the buffer name does already exist                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL BufferNameExists (pcBufferName AS CHARACTER):
        
        DEFINE BUFFER b_eTable FOR eTable .
        
        /* Is default buffer o.k. ? */
        FOR EACH b_eTable:
            IF LOOKUP (pcBufferName, b_eTable.SourceBufferNames) > 0 THEN 
                RETURN TRUE .
        END.
        
        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates the Tree View of the DATA-RELATION's                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BuildTreeView ():
        
        DEFINE VARIABLE oNode AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO . 

        DEFINE BUFFER b_eTable        FOR eTable .
        DEFINE BUFFER b_eDataRelation FOR eDataRelation. 
        
        THIS-OBJECT:TreeView:Nodes:Clear () .
         
        FOR EACH b_eTable:
            IF NOT CAN-FIND (b_eDataRelation WHERE b_eDataRelation.BusinessEntityName    = b_eTable.BusinessEntityName
                                               AND b_eDataRelation.ChildTempTableName    = b_eTable.TempTableName
                                               AND b_eDataRelation.DataRelationNotActive = FALSE) THEN DO ON ERROR UNDO, THROW: 

                oNode = THIS-OBJECT:TreeView:Nodes:Add (STRING (ROWID (b_eTable)), b_eTable.TempTableName) .
                
                AddChildTables (oNode, b_eTable.BusinessEntityName, b_eTable.TempTableName) .
            END.          
        END.
        
        THIS-OBJECT:TreeView:ExpandAll() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a unique buffer name for a table                                                                        
        Notes:   Returns either the default buffer name (EQ table name)
                 of a buffer name in the form of bn_tablename where n
                 starts at 1 
        @param pcSourceTableName The name of the source table
        @return The unique buffer name                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER BuildUniqueBufferName (pcSourceTableName AS CHARACTER):
        
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO INIT 1.
        DEFINE VARIABLE cBufferName AS CHARACTER NO-UNDO.
        
        /* Mike Fechner, Consultingwerk Ltd. 11.02.2011
           Can we use the default buffer name (EQ table name)? */
        IF NOT THIS-OBJECT:BufferNameExists (pcSourceTableName) THEN 
            RETURN pcSourceTableName .
            
        REPEAT:  
            
            ASSIGN cBufferName = SUBSTITUTE ("b&1_&2":U, 
                                             i, 
                                             pcSourceTableName) .
                                             
            IF NOT THIS-OBJECT:BufferNameExists (cBufferName) THEN 
                RETURN cBufferName . 
            
            ASSIGN i = i + 1 .            
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a unique name for a data-relation
        Notes:   
        @param pcDataRelationName The build name of the data relation
        @return The unique data relation name                                   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER BuildUniqueDataRelationName (pcDataRelationName AS CHARACTER):
        
        DEFINE VARIABLE cTest AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER   NO-UNDO INIT 1.
        
        REPEAT:
            ASSIGN cTest = pcDataRelationName + STRING (i) .
            
            IF NOT CAN-FIND (eDataRelation WHERE eDataRelation.DataRelationName = cTest) THEN 
                RETURN cTest .             
            
            ASSIGN i = i + 1 .            
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a unique name for a temp-table field                                                                       
        Notes:                                               
        @param pcTableName The name of the temp-table
        @param pcFieldName The name of the field
        @return The unique name for the temp-table field                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER BuildUniqueFieldName (pcTableName AS CHARACTER, 
                                                  pcFieldName AS CHARACTER):
        
        DEFINE BUFFER b_eField FOR eField . 
        
        DEFINE VARIABLE cTest AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER   NO-UNDO INIT 1.
        
        REPEAT:
            ASSIGN cTest = pcFieldName + STRING (i) .
            
            IF NOT CAN-FIND (b_eField WHERE b_eField.TempTableName = pcTableName 
                                        AND b_eField.FieldName = cTest) THEN 
                RETURN cTest .             
            
            ASSIGN i = i + 1 .            
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an unique name for an index
        Notes:                                                                        
        @param pcTableName The name of the temp-table
        @param pcIndexName The proposed name of the index
        @return The unique index name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER BuildUniqueIndexName (pcTableName AS CHARACTER, 
                                                  pcIndexName AS CHARACTER):
        
        DEFINE BUFFER b_eIndex FOR eIndex . 
        
        DEFINE VARIABLE cTest AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER   NO-UNDO INIT 1.
        
        REPEAT:
            ASSIGN cTest = pcIndexName + STRING (i) .
            
            IF NOT CAN-FIND (b_eIndex WHERE b_eIndex.TempTableName = pcTableName 
                                        AND b_eIndex.IndexName = cTest) THEN 
                RETURN cTest .             
            
            ASSIGN i = i + 1 .            
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an unique name for a temp-table                                                                       
        Notes:                                          
        @param pcTableName The proposed name of the temp-table 
        @return The unique name of the temp-table                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER BuildUniqueTableName (pcTableName AS CHARACTER):
        
        DEFINE BUFFER b_eTable FOR eTable . 
        
        DEFINE VARIABLE cTest AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER   NO-UNDO INIT 1.
        
        REPEAT:
            ASSIGN cTest = pcTableName + STRING (i) .
            
            IF NOT CAN-FIND (b_eTable WHERE b_eTable.TempTableName = cTest) THEN 
                RETURN cTest .             
            
            ASSIGN i = i + 1 .            
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SettingsChanged event of the BusinessEntityDesignerSettings
                 object                                                                        
        Notes:         
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event                                                               
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BusinessEntityDesignerSettingsSettingsChangedHandler 
        (sender AS Progress.Lang.Object, e AS Consultingwerk.EventArgs):
        
        DEFINE VARIABLE oDialogResultEnum AS DialogResultEnum NO-UNDO .
        
        oDialogResultEnum = MessageFormHelper:AskQuestion 
                              ("The temp-table settings have been changed."{&TRAN} + 
                               System.Environment:NewLine + 
                               "Update the current Business Entity design with the new defaults?"{&TRAN},
                               "Business Entity Designer"{&TRAN}) .
                               
        IF oDialogResultEnum:Equals (DialogResultEnum:DialogResultYes) THEN DO:
            FIND FIRST eBusinessEntity. 

            ASSIGN eBusinessEntity.DefaultTablePrefix            = BusinessEntityDesignerSettings:DefaultTablePrefix 
                   eBusinessEntity.DefaultTableSuffix            = BusinessEntityDesignerSettings:DefaultTableSuffix
                   eBusinessEntity.DefaultBeforeTablePrefix      = BusinessEntityDesignerSettings:DefaultBeforeTablePrefix
                   eBusinessEntity.DefaultBeforeTableSuffix      = BusinessEntityDesignerSettings:DefaultBeforeTableSuffix
                   eBusinessEntity.DistinctTempTableIncludeFiles = BusinessEntityDesignerSettings:DistinctTempTableIncludeFiles
                .
                
            oBusinessEntityAdapter:QueryHandle:QUERY-OPEN () .
            oBusinessEntityAdapter:BindingSource:RefreshAll () .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Compiles the source of the generated business entity                                                                        
        Notes:                                                                        
        @return Logical value indicating if the compilation was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL CompileSource ():
        
        DEFINE VARIABLE oGenerator AS IBusinessEntityGenerator NO-UNDO .  
        DEFINE VARIABLE e          AS CancelableEventArgs      NO-UNDO .
        
        StoreRecordPosition() .

        e = NEW CancelableEventArgs () .
        
        OnBeforeCompileSource (e) . 
        
        IF VALID-OBJECT (e) AND e:Cancel = TRUE THEN 
            RETURN FALSE . 

        oGenerator = DYNAMIC-NEW (BusinessEntityDesignerSettings:BusinessEntityGenerator) () .

        oGenerator:Compile (DATASET dsBusinessEntity) .

        OnAfterCompileSource (Consultingwerk.EventArgs:Empty) . 
        
        RETURN TRUE . 

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oGenerator)  .
        
            RestoreRecordPosition() .            
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates a Data-Relation on the model                                                                      
        Notes:                                       
        @param poModel The reference to the Crainiate ERM4 Model
        @param pcParentTable The name of the parent temp-table
        @param pcChildTable The name of the child temp-table                                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateDataRelation (poModel AS Crainiate.ERM4.Model, 
                                           pcParentTable AS CHARACTER,
                                           pcChildTable AS CHARACTER):

        THIS-OBJECT:CreateDataRelation (poModel, 
                                        pcParentTable,
                                        pcChildTable,
                                        TRUE) . 

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Creates a Data-Relation on the model                                                                      
        Notes:                                       
        @param poModel The reference to the Crainiate ERM4 Model
        @param pcParentTable The name of the parent temp-table
        @param pcChildTable The name of the child temp-table
        @param plActive ACTIVE Flag of the new data-relation                                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateDataRelation (poModel AS Crainiate.ERM4.Model, 
                                           pcParentTable AS CHARACTER,
                                           pcChildTable AS CHARACTER,
                                           plActive AS LOGICAL):
            
        DEFINE VARIABLE oDataRelationFieldsService AS IDataRelationFieldsService NO-UNDO . 
        DEFINE VARIABLE oConnector                 AS DataRelationConnector      NO-UNDO .     
        DEFINE VARIABLE cDataRelationName          AS CHARACTER                  NO-UNDO INIT "DataRelation":U.
        DEFINE VARIABLE i                          AS INTEGER                    NO-UNDO .
        DEFINE VARIABLE oDataRelationCustomizer    AS IDataRelationCustomizer    NO-UNDO .
        
        DEFINE BUFFER b_ParentTable FOR eTable .
        DEFINE BUFFER b_ChildTable FOR eTable . 

        oDataRelationFieldsService = CAST (THIS-OBJECT:ServiceContainer:GetService(Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.IDataRelationFieldsService":U)),
                                           IDataRelationFieldsService) .

        FIND FIRST eBusinessEntity. 

        FIND b_ParentTable WHERE ROWID (b_ParentTable) = TO-ROWID (pcParentTable)  . 
        FIND b_ChildTable WHERE ROWID (b_ChildTable) = TO-ROWID (pcChildTable) . 
                                        
        IF BusinessEntityDesignerSettings:RelationNameSubstitute > "":U THEN 
            ASSIGN cDataRelationName = SUBSTITUTE (BusinessEntityDesignerSettings:RelationNameSubstitute, 
                                                   b_ParentTable.TempTableName,
                                                   b_ChildTable.TempTableName) . 

        IF CAN-FIND (eDataRelation WHERE eDataRelation.DataRelationName = cDataRelationName) THEN 
            ASSIGN cDataRelationName = BuildUniqueDataRelationName (cDataRelationName) .
            
        RELEASE eDataRelation .            
            
        CREATE eDataRelation . 
        ASSIGN eDataRelation.BusinessEntityName    = eBusinessEntity.BusinessEntityName
               eDataRelation.ParentTempTableName   = b_ParentTable.TempTableName 
               eDataRelation.ChildTempTableName    = b_ChildTable.TempTableName
               eDataRelation.DataRelationName      = cDataRelationName
               eDataRelation.ChildPropertyName     = b_ChildTable.TempTableName + "s":U 
               eDataRelation.DataRelationNotActive = NOT plActive .

        IF VALID-OBJECT (oDataRelationFieldsService) THEN 
           eDataRelation.RelationFields      = oDataRelationFieldsService:GenerateDataRelationFields (eDataRelation.ParentTempTableName,
                                                                                                      eDataRelation.ChildTempTableName,
                                                                                                      DATASET dsBusinessEntity BY-REFERENCE) .          
                                                                                                                                     
        oDataRelationCustomizer = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.IDataRelationCustomizer}  .

        IF VALID-OBJECT (oDataRelationCustomizer) THEN 
            oDataRelationCustomizer:CustomizeDataRelation (ROWID (eDataRelation), 
                                                           DATASET dsBusinessEntity BY-REFERENCE) . 

        IF eBusinessEntity.DefaultTablePrefix > "":U AND 
           eDataRelation.ChildPropertyName BEGINS eBusinessEntity.DefaultTablePrefix THEN 
            eDataRelation.ChildPropertyName = SUBSTRING (eDataRelation.ChildPropertyName, 1 + LENGTH (eBusinessEntity.DefaultTablePrefix)) NO-ERROR . 
                       
        FIND CURRENT eDataRelation NO-ERROR .             
                       
        oConnector = NEW Consultingwerk.BusinessEntityDesigner.UI.Shapes.DataRelationConnector
                (DATASET dsBusinessEntity BIND, ROWID (eDataRelation), poModel) . 

        poModel:Lines:Add (STRING (ROWID (eDataRelation)), oConnector) .
               
        oConnector:SelectedChanged:Subscribe (DataRelationConnectorSelectedChangedHandler) .               
               
        oDataRelationAdapter:QueryHandle:QUERY-OPEN () .
        oDataRelationAdapter:BindingSource:RefreshAll() .               
               
        THIS-OBJECT:Model:SelectElements (FALSE) .
        oConnector:Selected = TRUE .               
               
        BuildTreeView() .               
               
        CATCH err AS Progress.Lang.Error :
            IF AVAILABLE eDataRelation THEN 
                DELETE eDataRelation . 
                
            UNDO, THROW err .     
        END CATCH.               
             
        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.
                                        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Creates a new unmapped temp-table                                                                      
        Notes:   Custom -> TempTable from the Designer Toolbox      
        @param poModel The reference to the Crainiate ERM4 Model control
        @param piX The X location of the new table on the design canvas
        @param piY The Y location of the new table on the design canvas 
        @return The generated temp-table name  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER CreateTempTable (poModel AS Crainiate.ERM4.Model, 
                                             piX AS INTEGER, 
                                             piY AS INTEGER):
                                        
        DEFINE VARIABLE hDataset   AS HANDLE    NO-UNDO .
        
        CREATE DATASET hDataset . 
        hDataset:CREATE-LIKE (DATASET dsBusinessEntity:HANDLE) .

        hDataset::eTable:BUFFER-CREATE () .
        hDataset::eTable::TempTableName = "TempTable":U .        

        RETURN THIS-OBJECT:CreateTempTable (hDataset,
                                            poModel, 
                                            piX, 
                                            piY,
                                            TRUE,
                                            TRUE) .

        FINALLY:  
            GarbageCollectorHelper:DeleteObject (hDataset) .                                         

            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.
                                    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates a new unmapped temp-table                                                                      
        Notes:   Used for custom wizards
        @param pcTableName The name of the temp-table to create
        @return The generated temp-table name  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER CreateTempTable (pcTableName AS CHARACTER):
                                        
        DEFINE VARIABLE hDataset   AS HANDLE    NO-UNDO .
        
        CREATE DATASET hDataset . 
        hDataset:CREATE-LIKE (DATASET dsBusinessEntity:HANDLE) .

        hDataset::eTable:BUFFER-CREATE () .
        hDataset::eTable::TempTableName = pcTableName .        

        RETURN THIS-OBJECT:CreateTempTable (hDataset,
                                            ?,
                                            ?, 
                                            ?,
                                            FALSE,
                                            TRUE) .

        FINALLY:  
            GarbageCollectorHelper:DeleteObject (hDataset) .                                         

            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.
                                    
    END METHOD .                                                
    /*------------------------------------------------------------------------------
        Purpose: Creates a new temp-table in the design                                                                        
        Notes:   phDataset contains a Dataset containing the source table schema 
        @param phDataset An instance of the dsBusinessEntity Dataset that contains the new table to be created
        @param poModel The reference to the Crainiate ERM4 Model control
        @param piX The X location of the new table on the design canvas
        @param piY The Y location of the new table on the design canvas 
        @return The generated temp-table name                                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER CreateTempTable (phDataset AS HANDLE,
                                             poModel AS Crainiate.ERM4.Model, 
                                             piX AS INTEGER, 
                                             piY AS INTEGER):

        RETURN THIS-OBJECT:CreateTempTable (phDataset,
                                            poModel, 
                                            piX, 
                                            piY,
                                            TRUE,
                                            TRUE) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Creates a new temp-table in the design                                                                        
        Notes:   phDataset contains a Dataset containing the source table schema 
        @param phDataset An instance of the dsBusinessEntity Dataset that contains the new table to be created
        @param poModel The reference to the Crainiate ERM4 Model control
        @param piX The X location of the new table on the design canvas
        @param piY The Y location of the new table on the design canvas                                                                               
        @param plUseFieldNameService Logical value indicating if the IFieldNameGeneratorService should be used for naming fields
        @param plBuildSourceFieldName Logical value indicating if the source field name should be built/assigned
        @return The generated temp-table name                                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER CreateTempTable (phDataset AS HANDLE,
                                             poModel AS Crainiate.ERM4.Model, 
                                             piX AS INTEGER, 
                                             piY AS INTEGER,
                                             plUseFieldNameService AS LOGICAL,
                                             plBuildSourceFieldName AS LOGICAL):
        
        DEFINE VARIABLE oFieldNameGeneratorService AS IFieldNameGeneratorService           NO-UNDO .
        DEFINE VARIABLE oTableNameGeneratorService AS ITableNameGeneratorService           NO-UNDO .
        DEFINE VARIABLE oShape                     AS TempTableShape                       NO-UNDO .
        DEFINE VARIABLE hQuery                     AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE cTempTableBaseName         AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cTableName                 AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE cIndexFieldNames           AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE i                          AS INTEGER                              NO-UNDO .
        DEFINE VARIABLE oFieldEventArgs            AS BeforeAddingFieldToNewTableEventArgs NO-UNDO . 
        DEFINE VARIABLE oIndexEventArgs            AS BeforeAddingIndexToNewTableEventArgs NO-UNDO . 
        DEFINE VARIABLE iOffset                    AS INTEGER                              NO-UNDO INIT 0 .        
        DEFINE VARIABLE oNewTempTableHandler       AS INewTempTableHandler                 NO-UNDO . 
        DEFINE VARIABLE oNewFieldHandler           AS INewFieldHandler                     NO-UNDO . 
        DEFINE VARIABLE roTableRowid               AS ROWID                                NO-UNDO .

        DEFINE BUFFER b_eField FOR eField .

        IF plUseFieldNameService THEN 
            oFieldNameGeneratorService = CAST (THIS-OBJECT:ServiceContainer:GetService(Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.IFieldNameGeneratorService":U)),
                                               IFieldNameGeneratorService) .

        FIND FIRST eBusinessEntity. 

        phDataset::eTable:FIND-FIRST () .
    
        /* Mike Fechner, Consultingwerk Ltd. 25.02.2013
           Use the TableNameGeneratorService */
        IF phDataset::eTable::BusinessEntityName > "":U THEN DO:
            oTableNameGeneratorService = CAST (THIS-OBJECT:ServiceContainer:GetService(Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.ITableNameGeneratorService":U)),
                                               ITableNameGeneratorService) .
            
            IF VALID-OBJECT (oTableNameGeneratorService) THEN  
                ASSIGN cTempTableBaseName = oTableNameGeneratorService:GenerateTempTableName (phDataset::eTable::BusinessEntityName,
                                                                                              THIS-OBJECT:ShortTableName (phDataset::eTable::TempTableName),
                                                                                              DATASET dsBusinessEntity BY-REFERENCE) .
        
            IF cTempTableBaseName > "":U THEN .
            ELSE 
                ASSIGN cTempTableBaseName = THIS-OBJECT:ShortTableName (phDataset::eTable::TempTableName) .
                
            ASSIGN cTableName = SUBSTITUTE ("&1&2&3":U,
                                            eBusinessEntity.DefaultTablePrefix,
                                            cTempTableBaseName,
                                            eBusinessEntity.DefaultTableSuffix) .
        END.
        ELSE         
            ASSIGN cTableName = SUBSTITUTE ("&1&2&3":U,
                                            eBusinessEntity.DefaultTablePrefix,
                                            THIS-OBJECT:ShortTableName (phDataset::eTable::TempTableName),
                                            eBusinessEntity.DefaultTableSuffix) .

        IF CAN-FIND (eTable WHERE eTable.TempTableName = cTableName) THEN 
            ASSIGN cTableName = BuildUniqueTableName (cTableName) .
   
        CREATE eTable . 
        BUFFER eTable:BUFFER-COPY (phDataset::eTable:HANDLE, "TempTableName":U) .
        
        ASSIGN eTable.BusinessEntityName  = eBusinessEntity.BusinessEntityName
               eTable.LayoutColumn        = piX 
               eTable.LayoutRow           = piY 
               eTable.TempTableName       = cTableName 
               eTable.TempTableBeforeName = THIS-OBJECT:BeforeTableName (cTableName)
               eTable.TempTablePath       = eBusinessEntity.DefaultTablePath  
               eTable.EntityPackageName   = THIS-OBJECT:GetEntityPackageNameForTable (eTable.TempTableName) 
               .

        /* Mike Fechner, Consultingwerk Ltd. 17.02.2011
           Do not map new temp-tables to db table */
        IF phDataset::eTable::TempTableName <> "TempTable":U THEN DO:
            IF plBuildSourceFieldName THEN 
                ASSIGN 
                   eTable.SourceTableNames    = THIS-OBJECT:ShortTableName (phDataset::eTable::TempTableName)
                   eTable.SourceBufferNames   = THIS-OBJECT:BuildUniqueBufferName (eTable.SourceTableNames) . 
             
            ASSIGN        
               eTable.EntityClassName     = (IF NOT eTable.TempTableName BEGINS BusinessEntityDesignerSettings:DefaultTablePrefix
                                             THEN eTable.TempTableName
                                             ELSE SUBSTRING (eTable.TempTableName, LENGTH (BusinessEntityDesignerSettings:DefaultTablePrefix) + 1))
               .
        END.
        ELSE
            ASSIGN 
               eTable.EntityClassName     = "TempTable":U . 

        hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (phDataset::eField) .
    
        oNewFieldHandler = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.INewFieldHandler} . 
        
        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
            ASSIGN oFieldEventArgs = NEW BeforeAddingFieldToNewTableEventArgs (phDataset::eField::BusinessEntityName,
                                                                               phDataset::eField::TempTableName,
                                                                               phDataset::eField::FieldName) .
            OnBeforeAddingFieldToNewTable (oFieldEventArgs) .

            IF VALID-OBJECT (oFieldEventArgs) AND oFieldEventArgs:Cancel THEN DO:
                eTable.DeletedFields = ListHelper:InsertEntry (eTable.DeletedFields,
                                                               1, 
                                                               SUBSTITUTE ("&1.&2":U,
                                                                           
                                                                           ENTRY (NUM-ENTRIES (phDataset::eField::TempTableName, ".":U),
                                                                                  phDataset::eField::TempTableName, ".":U),
                                                                           phDataset::eField::FieldName, ".":U)) . 
                NEXT . 
            END.
            
            CREATE eField . 
            BUFFER eField:BUFFER-COPY (phDataset::eField:HANDLE, "TempTableName":U) .
            
            ASSIGN eField.BusinessEntityName = eTable.BusinessEntityName
                   eField.TempTableName      = eTable.TempTableName
                   eField.FieldDataType      = UPPER (eField.FieldDataType) .
                   
            IF plBuildSourceFieldName THEN 
                 ASSIGN eField.FieldSource   = SUBSTITUTE ("&1.&2":U,
                                                           eTable.SourceBufferNames,
                                                           eField.FieldName) .
                   
            IF VALID-OBJECT (oFieldNameGeneratorService) THEN 
                eField.FieldName = oFieldNameGeneratorService:GenerateFieldName (phDataset::eTable::TempTableName,
                                                                                 phDataset::eField::FieldName,
                                                                                 eTable.TempTableName,
                                                                                 TABLE eField BY-REFERENCE) .                   
            ASSIGN eField.FieldSerializeName = RemoveInvalidXmlCharacters (eField.FieldName) .
                  
            IF VALID-OBJECT (oNewFieldHandler) THEN DO:                  
                oNewFieldHandler:HandleNewField (eField.TempTableName,
                                                 eField.FieldName, 
                                                 INPUT-OUTPUT DATASET dsBusinessEntity BY-REFERENCE) .  
            END.
                  
            FINALLY:
                hQuery:GET-NEXT () .    		
            END FINALLY.                  
        END.
        
        DELETE OBJECT hQuery .
        
        hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (phDataset::eIndex) .
        
        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:
            ASSIGN oIndexEventArgs = NEW BeforeAddingIndexToNewTableEventArgs (phDataset::eIndex::BusinessEntityName,
                                                                               phDataset::eIndex::TempTableName,
                                                                               phDataset::eIndex::IndexName,
                                                                               phDataset::eIndex::FieldNames,
                                                                               phDataset::eIndex::FieldSort,
                                                                               phDataset::eIndex::IndexUnique,
                                                                               phDataset::eIndex::IndexPrimary,
                                                                               phDataset::eIndex::IndexWordIndex) .
            
            OnBeforeAddingIndexToNewTable (oIndexEventArgs) .

            IF VALID-OBJECT (oIndexEventArgs) AND oIndexEventArgs:Cancel THEN 
                NEXT .
            
            CREATE eIndex . 
            BUFFER eIndex:BUFFER-COPY (phDataset::eIndex:HANDLE, "TempTableName":U) .
            
            ASSIGN eIndex.BusinessEntityName          = eTable.BusinessEntityName
                   eIndex.TempTableName               = eTable.TempTableName
                   eIndex.DefaultSearchCodeGeneration = eIndex.IndexPrimary
                   cIndexFieldNames                   = "":U.
                   
            /* Mike Fechner, Consultingwerk Ltd. 20.02.2011
               Map index field names */  
            IF eTable.SourceBufferNames > "":U THEN    DO:              
                DO i = 1 TO NUM-ENTRIES (eIndex.FieldNames):
                    FIND eField WHERE eField.TempTableName = eTable.TempTableName
                                  AND eField.FieldSource   = SUBSTITUTE ("&1.&2":U,
                                                                         eTable.SourceBufferNames,
                                                                         ENTRY (i, eIndex.FieldNames)) NO-ERROR .
    
                    /* Mike Fechner, Consultingwerk Ltd. 26.09.2013
                       eField record may be filtered out by BeforeAddingFieldToNewTableEventArgs
                       event handler */
                    IF AVAILABLE eField THEN 
                        ASSIGN cIndexFieldNames = cIndexFieldNames + eField.FieldName + ",":U .      
                    ELSE DO:
                        ASSIGN eIndex.FieldSort = ListHelper:RemoveEntry (eIndex.FieldSort, i - iOffset) 
                               iOffset = iOffset + 1.
                    END.                                                                               
                END.                   
            
                ASSIGN eIndex.FieldNames = TRIM (cIndexFieldNames, ",":U) .
            END.
            
            FINALLY:
                hQuery:GET-NEXT () .    		
            END FINALLY.
        END.
        
        DELETE OBJECT hQuery .

        ASSIGN cTableName   = eTable.TempTableName 
               roTableRowid = ROWID (eTable).

        oNewTempTableHandler = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.INewTempTableHandler} . 
        
        IF VALID-OBJECT (oNewTempTableHandler) THEN DO: 
            oNewTempTableHandler:HandleNewTempTable (cTableName, 
                                                     INPUT-OUTPUT DATASET dsBusinessEntity BY-REFERENCE) .
                                                     
            FIND FIRST eBusinessEntity . 
            
            THIS-OBJECT:AfterUpdateBusinessEntityRecordHandler (THIS-OBJECT, 
                                                                NEW AfterUpdateRecordEventArgs ("Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity":U, 
                                                                                                "eBusinessEntity":U, 
                                                                                                BUFFER eBusinessEntity:HANDLE, 
                                                                                                oBusinessEntityAdapter:QueryHandle)) .  
                   
            /* Mike Fechner, Consultingwerk Ltd. 31.03.2014
               SCL-221: Need to refetch eTable record as Table may have been renamed by 
               INewTempTableHandler service, need to re-link fields and indizes */
            FIND eTable WHERE ROWID(eTable) = roTableRowid.
            IF cTableName <> eTable.TempTableName
            THEN DO:
                FOR EACH eField WHERE eField.TempTableName = cTableName:
                    ASSIGN eField.TempTableName = eTable.TempTableName.
                END.
                FOR EACH eIndex WHERE eIndex.TempTableName = cTableName:
                    ASSIGN eIndex.TempTableName = eTable.TempTableName.
                END.
                
                ASSIGN cTableName = eTable.TempTableName .
            END.
        END.

        IF VALID-OBJECT (poModel) THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 04.06.2014
               SCL-304: An INewTempTableHandler might have already called the Refresh() 
                        method which might already have drawn the tabel*/
            IF NOT THIS-OBJECT:HasTableWithKey (STRING (roTableRowid)) THEN DO:
                oShape = NEW Consultingwerk.BusinessEntityDesigner.UI.Shapes.TempTableShape
                        (DATASET dsBusinessEntity BIND, ROWID (eTable)) . 
                                      
                oShape:Width = 200 .
                poModel:Shapes:Add (STRING (roTableRowid), oShape) .
                
                oShape:SelectedChanged:Subscribe (TableShapeSelectedChangedHandler) .
                oShape:SelectedItemChanged:Subscribe (TableSelectedItemChangedHandler) .
            END.
            ELSE 
                oShape = THIS-OBJECT:GetTableByKey (STRING (roTableRowid)) .
        END.
        
        IF VALID-OBJECT (oTableAdapter) THEN DO:
            oTableAdapter:QueryHandle:QUERY-OPEN () .
            oTableAdapter:BindingSource:RefreshAll() .
        END.

        IF VALID-OBJECT (poModel) THEN DO:
            THIS-OBJECT:Model:SelectElements (FALSE) .
            oShape:Selected = TRUE . 
            
            BuildTreeView() .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 05.07.2015
           SCL-609 - Ability to spilt array fields by default */
        IF BusinessEntityDesignerSettings:SplitArrayFields THEN DO:
            
            FOR EACH b_eField WHERE b_eField.BusinessEntityName = eTable.BusinessEntityName
                                AND b_eField.TempTableName      = eTable.TempTableName
                                AND b_eField.FieldExtent        > 0 
                    ON ERROR UNDO, THROW:

                THIS-OBJECT:SplitExtentField (b_eField.TempTableName, b_eField.FieldName) .                        
            END.                              

            IF VALID-OBJECT (THIS-OBJECT:Model) THEN DO: 
                THIS-OBJECT:StoreTablePosition() .
                THIS-OBJECT:DrawShapes (THIS-OBJECT:Model) .
            END.
        END.
                
        RETURN cTableName . 
        
        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SelectedChanged event of the data relation 
                 connector instances
        Notes:                      
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                                                          
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DataRelationConnectorSelectedChangedHandler (sender AS System.Object,
                                                                       e AS System.EventArgs):
        
        DEFINE BUFFER b_eDataRelation FOR eDataRelation . 
        
        DEFINE VARIABLE iPosition  AS INTEGER                  NO-UNDO.
        DEFINE VARIABLE oConnector AS Crainiate.ERM4.Connector NO-UNDO .
        DEFINE VARIABLE hQuery     AS HANDLE                   NO-UNDO.
        
        oConnector = CAST (sender, Crainiate.ERM4.Connector) . 
        
        IF oConnector:Selected = FALSE THEN 
          RETURN . 
        
        FIND b_eDataRelation WHERE ROWID(b_eDataRelation) = TO-ROWID (oConnector:Key) . 
        
        hQuery = oDataRelationAdapter:QueryHandle .
        hQuery:REPOSITION-TO-ROWID (ROWID (b_eDataRelation)) .
        
        IF hQuery:CURRENT-RESULT-ROW > 0 THEN 
            oDataRelationAdapter:BindingSource:Position = hQuery:CURRENT-RESULT-ROW - 1 .  
        
        IF VALID-OBJECT (THIS-OBJECT:DataRelationViewerControl) THEN DO:
            THIS-OBJECT:DockManager:PaneFromControl (THIS-OBJECT:DataRelationViewerControl):Activate() . 
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when detaching from the Dataset of the DatasetController.
                 Allows event unsubscription etc.                                                                      
        Notes:   Interface IDatasetController member        
        @param poConsumer The reference of the Object detaching from the Dataset Controller                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterConsumer (INPUT poConsumer AS Progress.Lang.Object):
        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deselectes all Shapes                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DeselectShapes ():
        
        {Consultingwerk/foreach.i System.Collections.DictionaryEntry oEntry in THIS-OBJECT:Model:Shapes}
            CAST (oEntry:Value, 
                  Crainiate.ERM4.Shape):Selected = FALSE .
        END .           

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: (Re)draws the Model                                                                     
        Notes:       
        @param poModel The reference to the Model instance                                                                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DrawShapes (poModel AS Crainiate.ERM4.Model):
        
        DEFINE VARIABLE oShape AS Consultingwerk.BusinessEntityDesigner.UI.Shapes.TempTableShape NO-UNDO .
        DEFINE VARIABLE oConnector AS Consultingwerk.BusinessEntityDesigner.UI.Shapes.DataRelationConnector NO-UNDO .
        
        Win32:LockWindowUpdate (poModel) .
        
        poModel:Shapes:Clear() .
        poModel:Lines:Clear() .
        
        FOR EACH eTable:

            oShape = NEW Consultingwerk.BusinessEntityDesigner.UI.Shapes.TempTableShape
                    (DATASET dsBusinessEntity BIND, ROWID (eTable)) . 

            poModel:Shapes:Add (STRING (ROWID(eTable)), oShape) .
            
            oShape:SelectedChanged:Subscribe (TableShapeSelectedChangedHandler) .
            oShape:SelectedItemChanged:Subscribe (TableSelectedItemChangedHandler) .
            
        END.
        
        FOR EACH eDataRelation:
            
            oConnector = NEW Consultingwerk.BusinessEntityDesigner.UI.Shapes.DataRelationConnector 
                         (DATASET dsBusinessEntity BIND, ROWID (eDataRelation), poModel) .

            poModel:Lines:Add (STRING (ROWID (eDataRelation)), oConnector) .

            oConnector:SelectedChanged:Subscribe (DataRelationConnectorSelectedChangedHandler) .
        END.
        
        FINALLY:
            Win32:LockWindowUpdate (0) .		
        END FINALLY.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Duplicates the given table
        Notes:   
        @param poTable as TempTableShape
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID DuplicateTable (poTable AS TempTableShape):
		
		DEFINE BUFFER b_eTable FOR eTable . 
		DEFINE BUFFER b_eField FOR eField .
		DEFINE BUFFER b_eIndex FOR eIndex . 

        DEFINE BUFFER b2_eTable FOR eTable . 
        DEFINE BUFFER b2_eField FOR eField .
        DEFINE BUFFER b2_eIndex FOR eIndex . 

        DEFINE VARIABLE oShape       AS TempTableShape  NO-UNDO .
        DEFINE VARIABLE cTableName   AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER         NO-UNDO .

        ObjectAssert:IsValid (poTable, "Table to Duplicate":U) .
		
		/* Find the source table record */
        FIND b_eTable WHERE ROWID (b_eTable) = TO-ROWID (poTable:Key) .		
		
		CREATE b2_eTable.
		BUFFER-COPY b_eTable TO b2_eTable 
		    ASSIGN b2_eTable.TempTableName = BuildUniqueTableName (b_eTable.TempTableName) .  
		
		ASSIGN b2_eTable.LayoutColumn = b2_eTable.LayoutColumn + 50 
		       b2_eTable.LayoutRow    = b2_eTable.LayoutRow + 50 . 

        DO i = 1 TO NUM-ENTRIES (b2_eTable.SourceBufferNames):
            ASSIGN ENTRY (i, b2_eTable.SourceBufferNames) = THIS-OBJECT:BuildUniqueBufferName (ENTRY (i, b2_eTable.SourceTableNames)) . 
        END.
		
		FOR EACH b_eField OF b_eTable:
		    
		    CREATE b2_eField .
		    BUFFER-COPY b_eField TO b2_eField
		        ASSIGN b2_eField.TempTableName = b2_eTable.TempTableName .
		       
		    IF NUM-ENTRIES (b2_eField.FieldSource, ".":U) = 2 THEN DO:
                /* Lookup source buffer */
                i = LOOKUP (ENTRY (1, b2_eField.FieldSource, ".":U), b_eTable.SourceBufferNames) .
		        
		        /* replace with n-th source-buffer from new table */
		        IF i > 0 THEN 
		            ENTRY (1, b2_eField.FieldSource, ".":U) = ENTRY (i, b2_eTable.SourceBufferNames) .
		    END.
		END.
		
        FOR EACH b_eIndex OF b_eTable:
            
            CREATE b2_eIndex .
            BUFFER-COPY b_eIndex TO b2_eIndex
                ASSIGN b2_eIndex.TempTableName = b2_eTable.TempTableName .  
        END.

        oShape = NEW Consultingwerk.BusinessEntityDesigner.UI.Shapes.TempTableShape
                (DATASET dsBusinessEntity BIND, ROWID (b2_eTable)) . 

        THIS-OBJECT:Model:Shapes:Add (STRING (ROWID (b2_eTable)), oShape) .
        
        oShape:SelectedChanged:Subscribe (TableShapeSelectedChangedHandler) .
        oShape:SelectedItemChanged:Subscribe (TableSelectedItemChangedHandler) .

        oTableAdapter:QueryHandle:QUERY-OPEN () .
        oTableAdapter:BindingSource:RefreshAll() .

        THIS-OBJECT:Model:SelectElements (FALSE) .
        oShape:Selected = TRUE . 
        
        BuildTreeView() .
        
        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Field Editor Form
        Notes:   
        @param pcTableName The name of the selected table 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID EditFields (pcTableName AS CHARACTER):
	
	    DEFINE VARIABLE oForm         AS FieldEditorForm                   NO-UNDO . 
        DEFINE VARIABLE lOk           AS LOGICAL                           NO-UNDO .
	    
	    DEFINE BUFFER b_eField FOR eField .
	
	    EMPTY TEMP-TABLE copy_eField .
	
	    FOR EACH b_eField WHERE b_eField.TempTableName = pcTableName ON ERROR UNDO, THROW:
	
	        CREATE copy_eField.
	        BUFFER-COPY b_eField TO copy_eField . 
	
	    END.
		
        oForm = NEW FieldEditorForm () . 
        
        lOk = oForm:ShowEditor (pcTableName,
                                INPUT-OUTPUT TABLE copy_eField BIND,
                                CAST (FieldViewerControl, FieldViewerControl):GetDataTypeValueList ()) . 
		
        IF lOk = TRUE THEN . 
        ELSE RETURN . 

        THIS-OBJECT:UnsavedChanges = TRUE . 

        FOR EACH b_eField WHERE b_eField.TempTableName = pcTableName ON ERROR UNDO, THROW:
            DELETE b_eField . 
        END.
        
        FOR EACH copy_eField ON ERROR UNDO, THROW:
            CREATE b_eField. 
            BUFFER-COPY copy_eField TO b_eField 
                ASSIGN b_eField.TempTableName = pcTableName . 
        END.

        THIS-OBJECT:oFieldAdapter:RetrieveData() .
        
        IF VALID-OBJECT (THIS-OBJECT:Model) THEN 
            THIS-OBJECT:DrawShapes (THIS-OBJECT:Model) .
        
        FINALLY:
            EMPTY TEMP-TABLE copy_eField .
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates a unqiue field name in a temp-table
        Notes:   
        @param pcBusinessEntityName The name of the Business Entity
        @param pcTempTableName The name of the Temp Table
        @param pcFieldName The base name of the Field
        @return The Unique field name
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER EnsureUniqueFieldName (pcBusinessEntityName AS CHARACTER,
                                                      pcTempTableName AS CHARACTER, 
                                                      pcFieldName AS CHARACTER):
		
		DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO .
		DEFINE VARIABLE i          AS INTEGER   NO-UNDO INIT 2.

        DEFINE BUFFER eField FOR eField . 

        IF NOT CAN-FIND (eField WHERE eField.BusinessEntityName = pcBusinessEntityName
                                  AND eField.TempTableName      = pcTempTableName
                                  AND eField.FieldName          = pcFieldName) THEN 
            RETURN pcFieldName . 
            
        REPEAT:
            ASSIGN cFieldName = SUBSTITUTE ("&1&2":U,
                                            pcFieldName,
                                            i) .

            IF NOT CAN-FIND (eField WHERE eField.BusinessEntityName = pcBusinessEntityName
                                      AND eField.TempTableName      = pcTempTableName
                                      AND eField.FieldName          = cFieldName) THEN 
                RETURN cFieldName . 
            
            i = i + 1 . 
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows a dialog that allows the user to reorder the fields of the 
                 temp-table                      
        Notes:        
        @param poTable The TempTableShape representing the TempTable                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FieldsReorder (poTable AS TempTableShape):

        DEFINE BUFFER b_eTable FOR eTable .
        DEFINE BUFFER b_eField FOR eField . 
        
        DEFINE VARIABLE oForm         AS Consultingwerk.BusinessEntityDesigner.UI.ReorderFieldsForm NO-UNDO .
        DEFINE VARIABLE cCurrentOrder AS CHARACTER                                                  NO-UNDO.
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult                          NO-UNDO .

        FIND b_eTable WHERE b_eTable.TempTableName = poTable:Heading . 

        FOR EACH eField WHERE eField.BusinessEntityName = b_eTable.BusinessEntityName
                          AND eField.TempTableName      = b_eTable.TempTableName
                           BY eField.FieldOrder:

            ASSIGN cCurrentOrder = cCurrentOrder + eField.FieldName + ",":U . 

        END.

        ASSIGN cCurrentOrder = TRIM (cCurrentOrder, ",":U)  . 

        oForm = NEW Consultingwerk.BusinessEntityDesigner.UI.ReorderFieldsForm () .
        oForm:DeleteOnFormClosed = FALSE .
        
        oForm:Fields = cCurrentOrder .  

        WAIT-FOR oForm:ShowDialog () SET oDialogResult .

        IF Progress.Util.EnumHelper:AreEqual (oDialogResult, System.Windows.Forms.DialogResult:Ok) THEN DO:
            
            REPEAT PRESELECT EACH b_eField WHERE b_eField.TempTableName = b_eTable.TempTableName BY b_eField.FieldOrder
                ON ERROR UNDO, THROW:
                FIND NEXT b_eField . 
                
                ASSIGN b_eField.FieldOrder = 10000 + LOOKUP (b_eField.FieldName, oForm:Fields) .
            END. 

            ReorderFields (b_eTable.TempTableName) .
            
            poTable:DrawShapeDetails () .
            
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END . 

        DELETE OBJECT oForm . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the ttCustomProperties records for the given scope 
        Notes:   
        @param poCustomPropertiesScope The property scope to edit the custom properties for
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID FillCustomProperties (poCustomPropertiesScope AS CustomPropertiesScopeEnum):
		
        DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO.
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        EMPTY TEMP-TABLE ttCustomProperties .

        THIS-OBJECT:PrepareCustomPropertiesScope (poCustomPropertiesScope,
                                                  OUTPUT hBuffer, 
                                                  OUTPUT hQuery) .
        
        DO WHILE NOT hQuery:QUERY-OFF-END:
            CREATE ttCustomProperties . 
            
            BUFFER ttCustomProperties:BUFFER-COPY (hBuffer) .
            
            hQuery:GET-NEXT . 
        END. 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of the custom property names for the given scope 
        Notes:   
        @param poCustomPropertiesScope The property scope to edit the custom properties for
        @return The CharacterList with the custom property names 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC Consultingwerk.Framework.Collections.CharacterList GetCustomPropertyNames (poCustomPropertiesScope AS CustomPropertiesScopeEnum):
	
		DEFINE VARIABLE oList AS Consultingwerk.Framework.Collections.CharacterList NO-UNDO.

        oList = NEW Consultingwerk.Framework.Collections.CharacterList ("":U, CHR(1)) . 

        THIS-OBJECT:OnGetCustomPropertyNames (NEW GetCustomPropertyNamesEventArgs (poCustomPropertiesScope,
                                                                                   oList)) .

		RETURN oList .

	END METHOD .
	
    /*------------------------------------------------------------------------------
        Purpose: Returns the list of possible values for the given property 
        Notes:   
        @param poCustomPropertiesScope The property scope to edit the custom properties for
        @param pcPropertyName The name of the property to return possible values for 
        @return The CharacterList with the custom property names 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Consultingwerk.Framework.Collections.CharacterList GetCustomPropertyValues (poCustomPropertiesScope AS CustomPropertiesScopeEnum,
                                                                                              pcPropertyName AS CHARACTER):
    
        DEFINE VARIABLE oList AS Consultingwerk.Framework.Collections.CharacterList NO-UNDO.

        oList = NEW Consultingwerk.Framework.Collections.CharacterList ("":U, CHR(1)) . 

        THIS-OBJECT:OnGetCustomPropertyValues (NEW GetCustomPropertyValuesEventArgs (poCustomPropertiesScope,
                                                                                     pcPropertyName,
                                                                                     oList)) .

        RETURN oList .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds the default file name to save the .bedgm file to                                                                     
        Notes:                                                          
        @return The default file name for the .bedgm file              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetDefaultFileName ():

        DEFINE BUFFER b_eBusinessEntity FOR eBusinessEntity . 
        DEFINE VARIABLE cSourceFolder   AS CHARACTER NO-UNDO.         

        IF BusinessEntityDesignerSettings:SourceCodeBaseFolder > "":U THEN DO:
            FILE-INFO:FILE-NAME = BusinessEntityDesignerSettings:SourceCodeBaseFolder .
            
            IF FILE-INFO:FULL-PATHNAME > "":U AND FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN 
                ASSIGN cSourceFolder = FILE-INFO:FULL-PATHNAME + "~\":U. 
            ELSE 
                UNDO, THROW NEW AppError (SUBSTITUTE ("Source folder '&1' is not available."{&TRAN}, 
                                                      BusinessEntityDesignerSettings:SourceCodeBaseFolder),
                                          0) . 
        END.
        ELSE 
            ASSIGN cSourceFolder = "":U .             
            
        FIND FIRST b_eBusinessEntity .    
            
        FILE-INFO:FILE-NAME = cSourceFolder + REPLACE (b_eBusinessEntity.BusinessEntityPackage, ".":U, "\":U) .

        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            RETURN SUBSTITUTE ("&1\&2.bedgm":U,
                               FILE-INFO:FULL-PATHNAME, 
                               b_eBusinessEntity.BusinessEntityName) . 
        ELSE 
            RETURN SUBSTITUTE ("&1.bedgm":U,
                               b_eBusinessEntity.BusinessEntityName) . 
        
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Generates the actual source code.                                                                        
        Notes:                                                                        
        @return Logical value indicating if the code generation was successful
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL GenerateSource ():
        
        DEFINE VARIABLE oGenerator AS IBusinessEntityGenerator NO-UNDO .  
        
        DEFINE VARIABLE e          AS CancelableEventArgs      NO-UNDO .
        
        StoreRecordPosition() .

        e = NEW CancelableEventArgs () .
        
        OnBeforeGenerateSource (e) . 
        
        IF VALID-OBJECT (e) AND e:Cancel = TRUE THEN 
            RETURN FALSE . 

        oGenerator = DYNAMIC-NEW (BusinessEntityDesignerSettings:BusinessEntityGenerator) () .

        oGenerator:OpenFilesInIDE = BusinessEntityDesignerSettings:OpenFilesInIDE . 
        
        IF oGenerator:GenerateCode (DATASET dsBusinessEntity) = TRUE THEN 
            OnAfterGenerateSource (Consultingwerk.EventArgs:Empty) . 

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oGenerator) .
    
            RestoreRecordPosition() .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE if the given temp-table has an active parent data relation                                                                          
        Notes:   Used for validation when adding a new parent relation to a temp-table
                 (timer1_Tick in BusinessEntityDesignerForm)                      
        @param pcTempTable The Name of the Temp-Table to check for parent relations
        @return Logical value indicating if the temp-table has an active parent relation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HasActiveParentRelation (pcTempTable AS CHARACTER):
        
        DEFINE BUFFER b_eDataRelation FOR eDataRelation. 
        DEFINE BUFFER b_eTable        FOR eTable .
        
        FIND b_eTable WHERE b_eTable.TempTableName = pcTempTable .        

        IF CAN-FIND (FIRST b_eDataRelation WHERE b_eDataRelation.BusinessEntityName    = b_eTable.BusinessEntityName
                                             AND b_eDataRelation.ChildTempTableName    = b_eTable.TempTableName
                                             AND b_eDataRelation.DataRelationNotActive = FALSE) THEN
            RETURN TRUE . 
            
        RETURN FALSE .                                                     

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE if the given temp-table has a parent data relation                                                                          
        Notes:   Used for validation when adding a new parent relation to a temp-table
                 (timer1_Tick in BusinessEntityDesignerForm)                      
        @param pcTempTable The Name of the Temp-Table to check for parent relations
        @return Logical value indicating if the temp-table has a parent relation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HasParentRelation (pcTempTable AS CHARACTER):
        
        DEFINE BUFFER b_eDataRelation FOR eDataRelation. 
        DEFINE BUFFER b_eTable        FOR eTable .
        
        FIND b_eTable WHERE b_eTable.TempTableName = pcTempTable .        

        IF CAN-FIND (FIRST b_eDataRelation WHERE b_eDataRelation.BusinessEntityName = b_eTable.BusinessEntityName
                                             AND b_eDataRelation.ChildTempTableName = b_eTable.TempTableName) THEN
            RETURN TRUE . 
            
        RETURN FALSE .                                                     

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Imports Table(s) from the given source code file
        Notes:   
        @param pcFileName The file name to import
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ImportTableFromFile (pcFileName AS CHARACTER):
        
        DEFINE VARIABLE oImporter     AS TempTableImporter NO-UNDO . 
        DEFINE VARIABLE oTables       AS CharacterList     NO-UNDO .
        DEFINE VARIABLE cShortName    AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE oDialogResult AS DialogResultEnum  NO-UNDO . 
        
        DEFINE BUFFER eTable FOR eTable .

        oImporter = NEW TempTableImporter () .
        
        oTables = oImporter:ImportTablesFromFile (pcFileName,
                                                  THIS-OBJECT) .

        DELETE OBJECT oImporter . 

        ASSIGN cShortName = FileHelper:ShortFileNameWithoutExtension (pcFileName) .

        IF oTables:ContainsValue (cShortName) THEN DO:

            oDialogResult = MessageFormHelper:AskQuestion (SUBSTITUTE ("Keep file name of imported temp-table &1?"{&TRAN}, cShortName),
                                                           "Business Entity Designer"{&TRAN}) .

            IF oDialogResult = DialogResultEnum:DialogResultYes THEN DO:

                FIND eTable WHERE eTable.TempTableName = cShortName .

                ASSIGN eTable.TempTablePath = FileHelper:DirectoryName (FileHelper:RelativeFileName (pcFileName)) .
        
                RELEASE eTable. 
                
                oTableAdapter:BindingSource:Refresh() . 
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Imports a code block (clip-board) as a temp-table into the model
        Notes:   
        @param plcCodeBlock The Code Block to import
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ImportTableFromLongChar (plcCodeBlock AS LONGCHAR):
        
        DEFINE VARIABLE oImporter AS TempTableImporter NO-UNDO . 

        oImporter = NEW TempTableImporter () .
        
        oImporter:ImportTablesFromCodeBlock (plcCodeBlock,
                                             THIS-OBJECT) .

        DELETE OBJECT oImporter . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Business Entity Design                                                                        
        Notes:   Variant for use in wizards outside of the Business Entity Designer      
        @return Logical value indicating if a new design was created
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL New ():

        DEFINE VARIABLE oBusinessEntityAdapter AS SmartBusinessEntityAdapter NO-UNDO . 
        DEFINE VARIABLE e                      AS CancelableEventArgs        NO-UNDO . 
        
        e = NEW CancelableEventArgs () .
        
        OnBeforeFileNew (e) .
        
        IF VALID-OBJECT (e) AND e:Cancel = TRUE THEN 
            RETURN FALSE . 
    
        ASSIGN oBusinessEntityAdapter               = SmartBusinessEntityAdapterSettings:FromDatasetController (THIS-OBJECT) 
               oBusinessEntityAdapter:ContextString = "<NEW>":U . 

        oBusinessEntityAdapter:RetrieveData () .

        THIS-OBJECT:UnsavedChanges = FALSE . 
    
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Business Entity Design                                                                        
        Notes:     
        @param poModel The reference to the Crainiate ERM4 Model                                                                   
        @return Logical value indicating if a new design was created
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL New (poModel AS Crainiate.ERM4.Model):
        
        DEFINE VARIABLE oBusinessEntityAdapter AS SmartBusinessEntityAdapter NO-UNDO . 
        DEFINE VARIABLE e                      AS CancelableEventArgs        NO-UNDO . 
        
        e = NEW CancelableEventArgs () .
        
        OnBeforeFileNew (e) .
        
        IF VALID-OBJECT (e) AND e:Cancel = TRUE THEN 
            RETURN FALSE . 
        
        ASSIGN oBusinessEntityAdapter               = SmartBusinessEntityAdapterSettings:FromDatasetController (THIS-OBJECT) 
               oBusinessEntityAdapter:ContextString = "<NEW>":U . 
               
        oBusinessEntityAdapter:RetrieveData () .

        poModel:Shapes:Clear () .
        poModel:Lines:Clear () .
        
        IF VALID-OBJECT (poModel) THEN 
            THIS-OBJECT:DrawShapes (poModel) .
        
        THIS-OBJECT:UnsavedChanges = FALSE . 
        
        IF VALID-OBJECT (THIS-OBJECT:BusinessEntityViewerControl) THEN 
            THIS-OBJECT:DockManager:PaneFromControl (THIS-OBJECT:BusinessEntityViewerControl):Activate() . 
        
        THIS-OBJECT:TreeView:Nodes:Clear () .
        
        OnAfterFileNew (Consultingwerk.EventArgs:Empty) .
        
        RETURN TRUE . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterCompileSource event                                                                        
        Notes:  
        @param e The Consultingwerk.EventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterCompileSource (e AS EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 
 
        THIS-OBJECT:AfterCompileSource:Publish (THIS-OBJECT, e) .
 
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterFileNew event                                                                        
        Notes:            
        @param e The Consultingwerk.EventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterFileNew (e AS EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty .

        THIS-OBJECT:AfterFileNew:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterFileOpen event                                                                        
        Notes:                                                                        
        @param e The FileEventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterFileOpen (e AS FileEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("The Event AfterFileOpen requires a valid FileEventArgs object."{&TRAN}, 0) .

        THIS-OBJECT:AfterFileOpen:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterFileSave event                                                                        
        Notes:                                                                        
        @param e The FileEventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterFileSave (e AS FileEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("The Event AfterFileSave requires a valid FileEventArgs object."{&TRAN}, 0) .

        THIS-OBJECT:AfterFileSave:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterGenerateSource event                                                                      
        Notes:                                                                        
        @param e The Consultingwerk.EventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterGenerateSource (e AS EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 
 
        THIS-OBJECT:AfterGenerateSource:Publish (THIS-OBJECT, e) .
 
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterRegenerateSource event                                                                      
        Notes:                                                                        
        @param e The Consultingwerk.EventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterRegenerateSource (e AS EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 
 
        THIS-OBJECT:AfterRegenerateSource:Publish (THIS-OBJECT, e) .
 
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeAddingFieldToNewTable
        Notes:   
        @param e The BeforeAddingFieldToNewTableEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeAddingFieldToNewTable (e AS BeforeAddingFieldToNewTableEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BeforeAddingFieldToNewTable":U) .
     
        THIS-OBJECT:BeforeAddingFieldToNewTable:Publish (THIS-OBJECT, e) .
    
    END METHOD .                

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeAddingIndexToNewTable
        Notes:   
        @param e The BeforeAddingIndexToNewTableEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeAddingIndexToNewTable (e AS BeforeAddingIndexToNewTableEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BeforeAddingIndexToNewTable":U) .
     
        THIS-OBJECT:BeforeAddingIndexToNewTable:Publish (THIS-OBJECT, e) .
    
    END METHOD .                

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeCompileSource event                                                                      
        Notes:                                                                        
        @param e The Consultingwerk.CancelableEventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeCompileSource (e AS CancelableEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("The Event BeforeCompileSource requires a valid CancelableEventArgs object."{&TRAN}, 0) .
 
        THIS-OBJECT:BeforeCompileSource:Publish (THIS-OBJECT, e) .
 
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeFileNew event                                                                        
        Notes:                                                                        
        @param e The Consultingwerk.CancelableEventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeFileNew (e AS CancelableEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("The Event BeforeFileNew requires a valid CancelableEventArgs object."{&TRAN}, 0) .

        THIS-OBJECT:BeforeFileNew:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeFileOpen event                                                                        
        Notes:                                                                        
        @param e The Consultingwerk.CancelableEventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeFileOpen (e AS CancelableFileEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("The Event BeforeFileOpen requires a valid CancelableFileEventArgs object."{&TRAN}, 0) .

        THIS-OBJECT:BeforeFileOpen:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeFileSave event                                                                        
        Notes:                                                                        
        @param e The Consultingwerk.CancelableEventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeFileSave (e AS CancelableFileEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("The Event BeforeFileSave requires a valid CancelableFileEventArgs object."{&TRAN}, 0) .

        THIS-OBJECT:BeforeFileSave:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeGenerateSource event                                                                      
        Notes:                                                                        
        @param e The Consultingwerk.CancelableEventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeGenerateSource (e AS CancelableEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("The Event BeforeGenerateSource requires a valid CancelableEventArgs object."{&TRAN}, 0) .
 
        THIS-OBJECT:BeforeGenerateSource:Publish (THIS-OBJECT, e) .
 
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeRegenerateSource event                                                                      
        Notes:                                                                        
        @param e The Consultingwerk.CancelableEventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeRegenerateSource (e AS CancelableEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("The Event BeforeRegenerateSource requires a valid CancelableEventArgs object."{&TRAN}, 0) .
 
        THIS-OBJECT:BeforeRegenerateSource:Publish (THIS-OBJECT, e) .
 
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the GetCustomPropertyNames
        Notes:   
        @param e The GetCustomPropertyNamesEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnGetCustomPropertyNames (e AS GetCustomPropertyNamesEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "GetCustomPropertyNames":U) .
     
        THIS-OBJECT:GetCustomPropertyNames:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the TableItemSelected
        Notes:   
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnTableItemSelected (e AS Consultingwerk.EventArgs):
            
    /*    Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "TableItemSelected":U) . */
     
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 
    
        THIS-OBJECT:TableItemSelected:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the UnsavedChangesChanged event                                                                        
        Notes:                                                                        
        @param e The Consultingwerk.EventArgs with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnUnsavedChangesChanged (e AS Consultingwerk.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty .
        
        THIS-OBJECT:UnsavedChangesChanged:Publish (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Opens an existing Business Entity Design                                                                        
        Notes:  
        @param pcFileName The file name to open
        @param poModel The reference to the Crainiate ERM4 Model
        @return Logical value indicating if a file has been opened                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Open (pcFileName AS CHARACTER, 
                                poModel AS Crainiate.ERM4.Model):
        
        DEFINE VARIABLE oBusinessEntityAdapter AS SmartBusinessEntityAdapter NO-UNDO . 
        DEFINE VARIABLE e                      AS CancelableFileEventArgs    NO-UNDO . 
    
        IF NOT FileHelper:Exists (pcFileName) THEN 
            UNDO, THROW NEW ExceptionWithTitle (SUBSTITUTE ("The file &1 was not found."{&TRAN}, pcFileName), 
                                                0,
                                                "Business Entity Designer"{&TRAN}) .
        
        e = NEW CancelableFileEventArgs (pcFileName) .
        
        OnBeforeFileOpen (e) .
        
        IF VALID-OBJECT (e) AND e:Cancel = TRUE THEN 
            RETURN FALSE .         
        
        ASSIGN oBusinessEntityAdapter               = SmartBusinessEntityAdapterSettings:FromDatasetController (THIS-OBJECT) 
               oBusinessEntityAdapter:ContextString = pcFileName . 
               
        oBusinessEntityAdapter:RetrieveData () .

        poModel:Shapes:Clear () .
        poModel:Lines:Clear () .
        
        IF VALID-OBJECT (poModel) THEN 
            THIS-OBJECT:DrawShapes (poModel) .

        BuildTreeView () .

        THIS-OBJECT:UnsavedChanges = FALSE . 

        OnAfterFileOpen (NEW FileEventArgs (pcFileName)) .

        RETURN TRUE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Prepares the query for accessing the ttCustomProperties records 
                 for the given scope 
        Notes:   
        @param poCustomPropertiesScope The property scope to edit the custom properties for
        @param phBuffer OUTPUT The buffer handle of the custom properties source buffer
        @param phQuery OUTPUT The query handle to access the custom properties records
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PrepareCustomPropertiesScope (poCustomPropertiesScope AS CustomPropertiesScopeEnum,
	                                                    OUTPUT phBuffer AS HANDLE,
	                                                    OUTPUT phQuery AS HANDLE):
		
		DEFINE VARIABLE cKey1 AS CHARACTER NO-UNDO.
		DEFINE VARIABLE cKey2 AS CHARACTER NO-UNDO.
		
        CASE poCustomPropertiesScope:
            WHEN CustomPropertiesScopeEnum:BusinessEntity THEN DO:
                ASSIGN phBuffer = BUFFER eBusinessEntityProperties:HANDLE 
                       phQuery  = QueryHelper:CreatePreparedQuery (phBuffer)  .
            END.
            
            WHEN CustomPropertiesScopeEnum:TempTable THEN DO:
                ASSIGN cKey1    = oTableAdapter:GetFieldValues ("TempTableName":U)
                       phBuffer = BUFFER eTableProperties:HANDLE 
                       phQuery  = QueryHelper:CreatePreparedQuery (phBuffer, 
                                                                   SUBSTITUTE ("for each eTableProperties where eTableProperties.TempTableName = &1":U,
                                                                               QUOTER (cKey1))) .
            END .
            
            WHEN CustomPropertiesScopeEnum:DataRelation THEN DO:
                ASSIGN cKey1    = oDataRelationAdapter:GetFieldValues ("DataRelationName":U)
                       phBuffer = BUFFER eDataRelationProperties:HANDLE 
                       phQuery  = QueryHelper:CreatePreparedQuery (phBuffer, 
                                                                   SUBSTITUTE ("for each eDataRelationProperties where eDataRelationProperties.DataRelationName = &1":U,
                                                                               QUOTER (cKey1))) .
            END .

            WHEN CustomPropertiesScopeEnum:Field THEN DO:
                ASSIGN cKey1    = oFieldAdapter:GetFieldValues ("TempTableName":U)
                       cKey2    = oFieldAdapter:GetFieldValues ("FieldName":U)
                       phBuffer = BUFFER eFieldProperties:HANDLE 
                       phQuery  = QueryHelper:CreatePreparedQuery (phBuffer, 
                                                                   SUBSTITUTE ("for each eFieldProperties where eFieldProperties.TempTableName = &1 and eFieldProperties.FieldName = &2":U,
                                                                               QUOTER (cKey1),
                                                                               QUOTER (cKey2))) .
            END .

            WHEN CustomPropertiesScopeEnum:Index THEN DO:
                ASSIGN cKey1    = oIndexAdapter:GetFieldValues ("TempTableName":U)
                       cKey2    = oIndexAdapter:GetFieldValues ("IndexName":U)
                       phBuffer = BUFFER eIndexProperties:HANDLE 
                       phQuery  = QueryHelper:CreatePreparedQuery (phBuffer, 
                                                                   SUBSTITUTE ("for each eIndexProperties where eIndexProperties.TempTableName = &1 and eIndexProperties.IndexName = &2":U,
                                                                               QUOTER (cKey1),
                                                                               QUOTER (cKey2))) .
            END .
            
        END CASE . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the Model on the Screen 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Refresh ():
		
        THIS-OBJECT:Model:Shapes:Clear () .
        THIS-OBJECT:Model:Lines:Clear () .
        
        THIS-OBJECT:DrawShapes (THIS-OBJECT:Model) .

        BuildTreeView () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates a temp-table from the database schema                                                                       
        Notes:                                                
        @param poTable The reference to the TempTableShape to update
        @param poDatabaseSchema The reference to the DatabaseSchemaDatasetController to update from                        
        @return The message to be shown to the user
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RefreshTable (poTable          AS TempTableShape,
                                          poDatabaseSchema AS DatabaseSchemaDatasetController):
        
        DEFINE BUFFER b_eTable FOR eTable .
        DEFINE BUFFER b_eField FOR eField .
        DEFINE BUFFER eField   FOR eField . 
        
        DEFINE VARIABLE hDataset                   AS HANDLE                            NO-UNDO .
        DEFINE VARIABLE hQuery                     AS HANDLE                            NO-UNDO .
        DEFINE VARIABLE iOrder                     AS INTEGER                           NO-UNDO .
        DEFINE VARIABLE iTable                     AS INTEGER                           NO-UNDO .
        DEFINE VARIABLE lFound                     AS LOGICAL                           NO-UNDO INIT FALSE .
        DEFINE VARIABLE lFieldExists               AS LOGICAL                           NO-UNDO .
        DEFINE VARIABLE cFieldName                 AS CHARACTER                         NO-UNDO .

        DEFINE VARIABLE oBufferFieldName           AS BufferFieldName                   NO-UNDO .
        DEFINE VARIABLE oFieldNameGeneratorService AS IFieldNameGeneratorService        NO-UNDO .
        DEFINE VARIABLE oFieldRemovedPromptForm    AS FieldRemovedPromptForm            NO-UNDO .        
        DEFINE VARIABLE oDialogResult              AS System.Windows.Forms.DialogResult NO-UNDO . 
        
        oFieldNameGeneratorService = CAST (THIS-OBJECT:ServiceContainer:GetService (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.IFieldNameGeneratorService":U)),
                                           IFieldNameGeneratorService) .
        
        FIND b_eTable WHERE b_eTable.TempTableName = poTable:Heading . 
    
        FIND LAST b_eField WHERE b_eField.TempTableName = b_eTable.TempTableName
            USE-INDEX Order . 
        
        IF AVAILABLE b_eField THEN 
            ASSIGN iOrder = b_eField.FieldOrder . 
        
        DO iTable = 1 TO NUM-ENTRIES (b_eTable.SourceTableNames) ON ERROR UNDO, THROW:

            DO ON ERROR UNDO, THROW:        
                ASSIGN hDataset = poDatabaseSchema:GetTableSchema (ENTRY (iTable, b_eTable.SourceTableNames)) .

                CATCH err AS Progress.Lang.Error :
                    UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid source table: &1"{&TRAN}, 
                                                          ENTRY (iTable, b_eTable.SourceTableNames)), 0) .
                END CATCH.
            END.

            IF NOT VALID-HANDLE (hDataset) THEN 
                UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid source table: &1"{&TRAN}, 
                                                      ENTRY (iTable, b_eTable.SourceTableNames)), 0) .
         
            bufferFieldLoop:
            FOR EACH b_eField WHERE b_eField.BusinessEntityName = b_eTable.BusinessEntityName
                                AND b_eField.TempTableName      = b_eTable.TempTableName
                                AND b_eField.FieldSource        > "":U:
                
                ASSIGN oBufferFieldName = BufferHelper:ParseFieldName (b_eField.FieldSource) .

                IF oBufferFieldName:TableName <> ENTRY (iTable, b_eTable.SourceBufferNames) THEN  
                    NEXT bufferFieldLoop .

                lFieldExists = hDataset::eField:FIND-FIRST (SUBSTITUTE ("WHERE FieldName = ~"&1~"":U,
                                                                        oBufferFieldName:FieldName)) NO-ERROR .

                IF NOT lFieldExists THEN DO:
                    oFieldRemovedPromptForm = NEW FieldRemovedPromptForm ()  .               
                    oFieldRemovedPromptForm:Owner = THIS-OBJECT:FieldViewerControl:FindForm() .
                    oFieldRemovedPromptForm:FieldName = SUBSTITUTE ("&1.&2":U,
                                                                    ENTRY (iTable, b_eTable.SourceTableNames),
                                                                    oBufferFieldName:FieldName) .
                    
                    Consultingwerk.Util.FormHelper:ShowDialog (oFieldRemovedPromptForm,
                                                               OUTPUT oDialogResult) .
                    
                    {Consultingwerk/check-dialogresult-ok.i oDialogResult ?} 
                    
                    CASE oFieldRemovedPromptForm:FieldRemovedHandling:
                        WHEN FieldRemovedHandlingEnum:RemoveSourceColumn THEN DO:
                            ASSIGN b_eField.FieldSource = "":U . 
                            RELEASE b_eField .
                        END.
                        WHEN FieldRemovedHandlingEnum:RemoveField THEN DO:
                            DELETE b_eField .
                        END.
                    END.
                    
                END.
            END.
         
            hDataset::eField:BUFFER-RELEASE () .
         
            hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (hDataset::eField) .

            databasefieldsloop: 
            DO WHILE NOT hQuery:QUERY-OFF-END:
                
                /* Mike Fechner, Consultingwerk Ltd. 23.07.2013
                   SCL-84 - Don't import deleted fields */
                IF b_eTable.DeletedFields > "":U AND LOOKUP (SUBSTITUTE ("&1.&2":U,
                                                                         ENTRY (iTable, b_eTable.SourceBufferNames),
                                                                         hDataset::eField::FieldName),
                                                             b_eTable.DeletedFields) > 0 THEN DO:
                     hQuery:GET-NEXT () .
                     NEXT databasefieldsloop . 
                END.
            
                IF NOT CAN-FIND (b_eField WHERE b_eField.BusinessEntityName = b_eTable.BusinessEntityName
                                            AND b_eField.TempTableName      = b_eTable.TempTableName
                                            AND b_eField.FieldSource        = SUBSTITUTE ("&1.&2":U,
                                                                                          ENTRY (iTable, b_eTable.SourceBufferNames),
                                                                                          hDataset::eField::FieldName)) THEN DO:
                    CREATE eField .
                    BUFFER eField:BUFFER-COPY (hDataset::eField:HANDLE, "FieldOrder,FieldName":U) .

                    ASSIGN iOrder                    = iOrder + 1
                           eField.BusinessEntityName = b_eTable.BusinessEntityName
                           eField.TempTableName      = b_eTable.TempTableName
                           eField.FieldOrder         = iOrder .
                           eField.FieldSource        = SUBSTITUTE ("&1.&2":U,
                                                                   ENTRY (iTable, b_eTable.SourceBufferNames),
                                                                   hDataset::eField::FieldName) .

                    IF VALID-OBJECT (oFieldNameGeneratorService) THEN
                        cFieldName = oFieldNameGeneratorService:GenerateFieldName (hDataset::eTable::TempTableName,
                                                                                   hDataset::eField::FieldName,
                                                                                   b_eTable.TempTableName,
                                                                                   TABLE eField BY-REFERENCE) .
                    ELSE 
                        ASSIGN cFieldName = hDataset::eField::FieldName .

                    IF CAN-FIND (b_eField WHERE b_eField.BusinessEntityName = b_eTable.BusinessEntityName
                                            AND b_eField.TempTableName      = b_eTable.TempTableName
                                            AND b_eField.FieldName          = cFieldName) THEN 
                        eField.FieldName = BuildUniqueFieldName (b_eTable.TempTableName, cFieldName) .
                    ELSE
                        eField.FieldName = cFieldName .                           

                    ASSIGN eField.FieldSerializeName = RemoveInvalidXmlCharacters (eField.FieldName) 
                           eField.FieldDataType      = UPPER (eField.FieldDataType).
                           
                    RELEASE eField . 
                END.

                hQuery:GET-NEXT () .
             END.

            ASSIGN lFound = TRUE . 
         
            FINALLY:
                GarbageCollectorHelper:DeleteObject (hQuery) . 
                GarbageCollectorHelper:DeleteObject (hDataset) .         
            END FINALLY.
        END.

        THIS-OBJECT:ReorderFields (b_eTable.TempTableName) .
        
        poTable:DrawShapeDetails() .

        oFieldAdapter:QueryHandle:QUERY-OPEN () .            
           
        IF lFound THEN 
            RETURN "Temp-Table has been updated."{&TRAN} .
        ELSE 
            RETURN "The Temp-Table has no source tables."{&TRAN} .

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .  
            
            GarbageCollectorHelper:DeleteObject (oBufferFieldName) .       
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refreshes a Temp-Table definition from the include file
        Notes:   
        @param poTable The TempTableShape to update from the include file
        @return The message from this function
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER RefreshTableFromInclude (poTable AS TempTableShape):
		
        DEFINE VARIABLE oImporter AS TempTableImporter NO-UNDO . 

        oImporter = NEW TempTableImporter () .
        
        oImporter:RefreshTableFromFile (poTable:Heading,
                                        DATASET dsBusinessEntity BY-REFERENCE) .

        DELETE OBJECT oImporter . 

        RETURN "Temp-Table has been updated.":U . 

        CATCH err AS Progress.Lang.Error:
            RETURN "Error refreshing Temp-Table:":U + Consultingwerk.Environment:NewLine + 
                   err:GetMessage (1) .	
        END CATCH.

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .  

            poTable:DrawShapeDetails () .		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: ReGenerates source code                                                                        
        Notes:                                                                        
        @return Logical value indicating if the source was regenerated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RegenerateSource ():
        
        DEFINE VARIABLE oGenerator AS IBusinessEntityGenerator NO-UNDO .  
        DEFINE VARIABLE e          AS CancelableEventArgs      NO-UNDO .
        
        StoreRecordPosition() .

        e = NEW CancelableEventArgs () .
        
        OnBeforeCompileSource (e) . 
        
        IF VALID-OBJECT (e) AND e:Cancel = TRUE THEN 
            RETURN FALSE . 

        oGenerator = DYNAMIC-NEW (BusinessEntityDesignerSettings:BusinessEntityGenerator) () .

        oGenerator:OpenFilesInIDE = BusinessEntityDesignerSettings:OpenFilesInIDE . 

        IF oGenerator:RegenerateCode (DATASET dsBusinessEntity) = TRUE THEN 
            OnAfterRegenerateSource (Consultingwerk.EventArgs:Empty) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oGenerator) .
    
            RestoreRecordPosition() .
        END FINALLY.
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when attaching to the Dataset of the DatasetController.
                 Allows event subscription etc.                                                                     
        Notes:   Interface IDatasetController member                                                                        
        @param poConsumer The reference of the Object attaching to the Dataset Controller                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterConsumer (poConsumer AS Progress.Lang.Object):

        IF TYPE-OF (poConsumer, SmartBusinessEntityAdapter) THEN DO: 
           
           ASSIGN oBusinessEntityAdapter = CAST (poConsumer, SmartBusinessEntityAdapter) .

           oBusinessEntityAdapter:AfterUpdateRecord:Subscribe (AfterUpdateBusinessEntityRecordHandler) .
           oBusinessEntityAdapter:BeforeUpdateRecord:Subscribe (BeforeUpdateBusinessEntityRecordHandler) . 
        END.

        IF TYPE-OF (poConsumer, SmartDatasetChildAdapter) AND 
           CAST (poConsumer, SmartDatasetChildAdapter):EntityTable = "eTable":U THEN DO: 
           
           ASSIGN oTableAdapter = CAST (poConsumer, SmartDatasetChildAdapter) .

           oTableAdapter:AfterUpdateRecord:Subscribe (AfterUpdateTableRecordHandler) .
           oTableAdapter:BeforeAssignRecord:Subscribe (BeforeAssignRecordTableHandler) . 
        END.

        IF TYPE-OF (poConsumer, SmartDatasetChildAdapter) AND 
           CAST (poConsumer, SmartDatasetChildAdapter):EntityTable = "eDataRelation":U THEN DO: 
           
           ASSIGN oDataRelationAdapter = CAST (poConsumer, SmartDatasetChildAdapter) .

           oDataRelationAdapter:BeforeAssignRecord:Subscribe (BeforeAssignRecordDataRelationHandler) .
           oDataRelationAdapter:AfterUpdateRecord:Subscribe (AfterUpdateDataRelationRecordHandler) .
        END.        
       
        IF TYPE-OF (poConsumer, SmartDatasetChildAdapter) AND 
           CAST (poConsumer, SmartDatasetChildAdapter):EntityTable = "eField":U THEN DO: 
           
           ASSIGN oFieldAdapter = CAST (poConsumer, SmartDatasetChildAdapter) .

           oFieldAdapter:AfterUpdateRecord:Subscribe (AfterUpdateFieldRecordHandler) .
        END.

        IF TYPE-OF (poConsumer, SmartDatasetChildAdapter) AND 
           CAST (poConsumer, SmartDatasetChildAdapter):EntityTable = "eIndex":U THEN DO: 
           
           ASSIGN oIndexAdapter = CAST (poConsumer, SmartDatasetChildAdapter) .

           oIndexAdapter:AfterUpdateRecord:Subscribe (AfterUpdateIndexRecordHandler) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Converts a temp-table field name to an XML Field name
        Notes:   Replaces # and % with _
        @param pcFieldName The temp-table field name to cleanup
        @return The XML field name
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER RemoveInvalidXmlCharacters (pcFieldName AS CHARACTER):
		
		DEFINE VARIABLE cNonXmlCharacters AS CHARACTER NO-UNDO EXTENT 2 
		    INIT ["#":U, "%":U].
        DEFINE VARIABLE i                 AS INTEGER   NO-UNDO .

        DO i = 1 TO EXTENT (cNonXmlCharacters):
            
            ASSIGN pcFieldName = REPLACE (pcFieldName, cNonXmlCharacters[i], "_":U) .
        
        END.

        RETURN pcFieldName . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes a Data-Relation from the Design                                                                        
        Notes:                                          
        @param poModel The Crainitate ERM4 Model to delete from
        @param poRelation The DataRelationConnector to be deleted from the Model and the Business Entity Schema                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveRelation (poModel AS Crainiate.ERM4.Model,
                                       poRelation AS Consultingwerk.BusinessEntityDesigner.UI.Shapes.DataRelationConnector):
        
        DEFINE BUFFER b_eDataRelation FOR eDataRelation . 
    
        FIND b_eDataRelation WHERE ROWID (b_eDataRelation) = TO-ROWID (poRelation:Key) 
            NO-ERROR .

        /* Mike Fechner, Consultingwerk Ltd. 14.11.2013
           When the relation is not available it has already been deleted. This method
           is executed twice. The first time actually removes the record followed by 
           the connector on the model. The second time if triggered by deleting the 
           connector */
        IF NOT AVAILABLE b_eDataRelation
            THEN RETURN .     

        DELETE b_eDataRelation. 
        
        poModel:Lines:Remove(poRelation:Key) .
        
        DELETE OBJECT poRelation .

        oDataRelationAdapter:QueryHandle:QUERY-OPEN () .
        oDataRelationAdapter:BindingSource:RefreshAll() .               

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         

            THIS-OBJECT:BuildTreeView () .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes the currently selected field of the selected table                                                                        
        Notes: 
        @param poTable The reference to the TempTableShape to remove a field from                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RemoveSelectedField (poTable AS TempTableShape):
        
        DEFINE BUFFER b_eField FOR eField . 
        DEFINE BUFFER b_eTable FOR eTable . 
        
        DEFINE VARIABLE oTable        AS TempTableShape   NO-UNDO . 
        DEFINE VARIABLE oDialogResult AS DialogResultEnum NO-UNDO .
        DEFINE VARIABLE hQuery        AS HANDLE           NO-UNDO .

        DEFINE VARIABLE iWidth        AS INTEGER NO-UNDO .
        DEFINE VARIABLE iHeight       AS INTEGER NO-UNDO .

        /* poTable may already be removed from the Model. So
           get the current shape with the same key for the 
           DrawShapeDetails method. */
        oTable = CAST (THIS-OBJECT:Model:Shapes [poTable:Key],
                       TempTableShape) .

        ASSIGN iWidth  = poTable:Width
               iHeight = poTable:Height .
        
        FIND b_eField WHERE ROWID(b_eField) = TO-ROWID (ENTRY (2, poTable:SelectedItem:Tag:ToString())) . 

        oDialogResult = MessageFormHelper:AskQuestion (SUBSTITUTE ("Delete field &1?"{&TRAN}, b_eField.FieldName),
                                                       "Business Entity Designer"{&TRAN}) .

        IF NOT oDialogResult:Equals(DialogResultEnum:DialogResultYes) THEN RETURN . 

        /* Mike Fechner, Consultingwerk Ltd. 23.07.2013
           SCL-84 Remember deleted field sources */
        IF b_eField.FieldSource > "":U THEN DO:
            FIND b_eTable OF b_eField NO-ERROR .
            
            IF AVAILABLE b_eTable THEN DO:
                b_eTable.DeletedFields = ListHelper:InsertEntry (b_eTable.DeletedFields, 
                                                                 1, 
                                                                 b_eField.FieldSource) .

                FIND CURRENT b_eTable NO-LOCK .
                
                FIND eTable WHERE ROWID (eTable) = ROWID (b_eTable) .
                FIND CURRENT eTable NO-LOCK .
                
                oTableAdapter:BindingSource:Refresh () .                                                                 
            END .             
        END.

        DELETE b_eField .
        
        oTable:DrawShapeDetails () .

        hQuery = oFieldAdapter:QueryHandle .
        hQuery:QUERY-OPEN () .      
        
        THIS-OBJECT:UnsavedChanges = TRUE .         

        FINALLY:
            IF iWidth > 0 AND iHeight > 0 THEN 
                ASSIGN oTable:Width  = iWidth
                       oTable:Height = iHeight .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes the currently selected index of the selected table                                                                       
        Notes:                                                                        
        @param poTable The reference to the TempTableShape to remove a field from                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RemoveSelectedIndex (poTable AS TempTableShape):
        
        DEFINE BUFFER b_eIndex FOR eIndex . 
        
        DEFINE VARIABLE oTable        AS TempTableShape   NO-UNDO . 
        DEFINE VARIABLE oDialogResult AS DialogResultEnum NO-UNDO .
        DEFINE VARIABLE hQuery        AS HANDLE           NO-UNDO.

        DEFINE VARIABLE iWidth        AS INTEGER NO-UNDO .
        DEFINE VARIABLE iHeight       AS INTEGER NO-UNDO .

        /* poTable may already be removed from the Model. So
           get the current shape with the same key for the 
           DrawShapeDetails method. */
        oTable = CAST (THIS-OBJECT:Model:Shapes [poTable:Key],
                       TempTableShape) .
        
        ASSIGN iWidth  = poTable:Width
               iHeight = poTable:Height .
        
        FIND b_eIndex WHERE ROWID(b_eIndex) = TO-ROWID (ENTRY (2, poTable:SelectedItem:Tag:ToString())) . 

        oDialogResult = MessageFormHelper:AskQuestion (SUBSTITUTE ("Delete index &1?"{&TRAN}, b_eIndex.IndexName),
                                                       "Business Entity Designer"{&TRAN}) .

        IF NOT oDialogResult:Equals (DialogResultEnum:DialogResultYes) THEN RETURN . 

        DELETE b_eIndex .
        
        oTable:DrawShapeDetails () .

        hQuery = oFieldAdapter:QueryHandle .
        hQuery:QUERY-OPEN () .      
        
        THIS-OBJECT:UnsavedChanges = TRUE .         

        FINALLY:
            IF iWidth > 0 AND iHeight > 0 THEN 
                ASSIGN oTable:Width  = iWidth
                       oTable:Height = iHeight .
        END FINALLY.
        
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Removes a Table from the Model                                                                        
        Notes:   Removes all connectors to/from the Table as well 
        @param poModel The Model to delete the Table from
        @param poTable The TempTableShape to delete from the Model                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveTable (poModel AS Crainiate.ERM4.Model,
                                    poTable AS TempTableShape):
        
        DEFINE BUFFER b_eDataRelation FOR eDataRelation . 
        DEFINE BUFFER b_eTable FOR eTable .
        DEFINE BUFFER b_eField FOR eField .
        DEFINE BUFFER b_eIndex FOR eIndex .
    
        DEFINE VARIABLE oDialogResult AS DialogResultEnum NO-UNDO . 
           
        IF VALID-OBJECT (poTable:SelectedItem) AND VALID-OBJECT (poTable:SelectedItem:Tag) THEN DO:
            CASE ENTRY (1, poTable:SelectedItem:Tag:ToString()):
                WHEN "eField":U THEN DO:
                    /* Cancel/undo deletion of Table Shape */
                    THIS-OBJECT:StoreTablePosition() .
                    
                    IF VALID-OBJECT (THIS-OBJECT:Model) THEN 
                        THIS-OBJECT:DrawShapes (THIS-OBJECT:Model) .
                    
                    RemoveSelectedField (poTable) .
                END.
                WHEN "eIndex":U THEN DO:
                    /* Cancel/undo deletion of Table Shape */ 
                    THIS-OBJECT:StoreTablePosition() .
                    
                    IF VALID-OBJECT (THIS-OBJECT:Model) THEN 
                        THIS-OBJECT:DrawShapes (THIS-OBJECT:Model) .

                    RemoveSelectedIndex (poTable) .
                END.
            END CASE. 
            
            RETURN . 
        END.

        FIND b_eTable WHERE ROWID (b_eTable) = TO-ROWID (poTable:Key) . 

        /* Mike Fechner, Consultingwerk Ltd. 16.02.2011
           Store position of table to be deleted */
        ASSIGN b_eTable.LayoutRow    = poTable:Y
               b_eTable.LayoutColumn = poTable:X
               b_eTable.LayoutHeight = poTable:Height
               b_eTable.LayoutWidth  = poTable:Width
               .

        oDialogResult = MessageFormHelper:AskQuestion (SUBSTITUTE ("Delete table &1?"{&TRAN}, b_eTable.TempTableName),
                                                       "Business Entity Designer"{&TRAN}) .
                                                       
        IF NOT oDialogResult:Equals (DialogResultEnum:DialogResultYes) THEN DO:  
                                                       
            THIS-OBJECT:StoreTablePosition() .
            IF VALID-OBJECT (THIS-OBJECT:Model) THEN 
                THIS-OBJECT:DrawShapes (THIS-OBJECT:Model) .
                
            RETURN . 
        END.             

        FOR EACH b_eDataRelation WHERE (b_eDataRelation.BusinessEntityName  = b_eTable.BusinessEntityName
                                   AND  b_eDataRelation.ParentTempTableName = b_eTable.TempTableName)
                                    OR (b_eDataRelation.BusinessEntityName  = b_eTable.BusinessEntityName
                                   AND  b_eDataRelation.ChildTempTableName = b_eTable.TempTableName):

            IF poModel:Lines:Contains (STRING (ROWID (b_eDataRelation))) THEN  
                THIS-OBJECT:RemoveRelation (poModel, CAST(poModel:Lines:Item[STRING(ROWID(b_eDataRelation))],
                                                          DataRelationConnector)) .
        END.

        FOR EACH b_eField WHERE b_eField.BusinessEntityName = b_eTable.BusinessEntityName
                            AND b_eField.TempTableName      = b_eTable.TempTableName:
            
            DELETE b_eField .                     
        END.                                

        FOR EACH b_eIndex WHERE b_eIndex.BusinessEntityName = b_eTable.BusinessEntityName
                            AND b_eIndex.TempTableName      = b_eTable.TempTableName:
            
            DELETE b_eIndex .                     
        END.                                

        DELETE b_eTable .
        
        poModel:Shapes:Remove(poTable:Key) .
        
        DELETE OBJECT poTable .

        oTableAdapter:QueryHandle:QUERY-OPEN () .
        oTableAdapter:BindingSource:RefreshAll() .               

        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Reorders the fields in the named temp-table                                                                        
        Notes:   Ensures the FieldOrder starts with 1 and provides no gaps                                             
        @param pcTable The name of the temp-table to reorder 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ReorderFields (pcTable AS CHARACTER):
        
        DEFINE BUFFER b_eField FOR eField .
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        REPEAT PRESELECT EACH b_eField WHERE b_eField.TempTableName = pcTable BY b_eField.FieldOrder:
            FIND NEXT b_eField . 
            
            ASSIGN i                   = i + 1 
                   b_eField.FieldOrder = i .
        END. 
        
        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Reorders the indexes in the named temp-table                                                                        
        Notes:   Ensures the IndexOrder starts with 1 and provides no gaps                                             
        @param pcTable The name of the temp-table to reorder 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ReorderIndexes (pcTable AS CHARACTER):
        
        DEFINE BUFFER b_eIndex FOR eIndex .
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        REPEAT PRESELECT EACH b_eIndex WHERE b_eIndex.TempTableName = pcTable BY b_eIndex.IndexOrder:
            FIND NEXT b_eIndex . 
            
            ASSIGN i                   = i + 1 
                   b_eIndex.IndexOrder = i . 
        END. 
        
        FINALLY:
            THIS-OBJECT:UnsavedChanges = TRUE .         
        END FINALLY.
        
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Restored a previously saved record position                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RestoreRecordPosition ():
        
        FIND FIRST eBusinessEntity NO-ERROR . 
        
        IF ro_Table <> ? THEN 
            FIND eTable WHERE ROWID (eTable) = ro_Table NO-ERROR .         

        IF ro_DataRelation <> ? THEN 
            FIND eDataRelation WHERE ROWID (eDataRelation) = ro_DataRelation NO-ERROR .         

        IF ro_Index <> ? THEN 
            FIND eIndex WHERE ROWID (eIndex) = ro_Index NO-ERROR .         

        IF ro_Field <> ? THEN 
            FIND eField WHERE ROWID (eField) = ro_Field NO-ERROR .         

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the current Business Entity Design                                                                        
        Notes:       
        @param pcFileName The name of the .bedgm file to save to
        @return Logical value indicating if the design has been saved                                                                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Save (pcFileName AS CHARACTER):
        
        DEFINE VARIABLE oBusinessEntityAdapter AS SmartBusinessEntityAdapter NO-UNDO . 
        DEFINE VARIABLE oOpenSaveParameter     AS OpenSaveParameter          NO-UNDO .
        DEFINE VARIABLE e                      AS CancelableFileEventArgs    NO-UNDO . 
        
        e = NEW CancelableFileEventArgs (pcFileName) .
        
        OnBeforeFileSave (e) .
        
        IF VALID-OBJECT (e) AND e:Cancel = TRUE THEN 
            RETURN FALSE .           
        
        StoreRecordPosition () .
        
        IF VALID-OBJECT (THIS-OBJECT:Model) THEN 
            StoreTablePosition () .

        ASSIGN oBusinessEntityAdapter      = SmartBusinessEntityAdapterSettings:FromDatasetController (THIS-OBJECT) 
               oOpenSaveParameter          = NEW OpenSaveParameter ()
               oOpenSaveParameter:FileName = pcFileName.
        
        oBusinessEntityAdapter:InvokeMethod ("Save":U, 
                                             INPUT-OUTPUT DATASET dsBusinessEntity,
                                             oOpenSaveParameter) .
    
        IF VALID-OBJECT (THIS-OBJECT:Model) THEN 
            THIS-OBJECT:DrawShapes (THIS-OBJECT:Model) .

        RestoreRecordPosition () .
                
        THIS-OBJECT:UnsavedChanges = FALSE . 
        
        OnAfterFileSave (NEW FileEventArgs (pcFileName)) .
        
        RETURN TRUE . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a shortened table name, without the DB part, when present                                                                       
        Notes:      
        @param pcTempTableName The input table name
        @return The shortened temp-table name
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER ShortTableName (pcTempTableName AS CHARACTER):

        RETURN ENTRY (NUM-ENTRIES (pcTempTableName, ".":U), pcTempTableName, ".":U) .        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SmartTableIOStateChanged event of the five 
                 property viewers in the Business Entity Designer
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The SmartTableIOStateChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SmartTableIOStateChangedHandler (sender AS Progress.Lang.Object,
	                                                       e AS SmartTableIOStateChangedEventArgs):
	
	    IF NOT VALID-OBJECT (THIS-OBJECT:Model) THEN 
	        RETURN . 
		
		IF (VALID-OBJECT (THIS-OBJECT:BusinessEntityViewerControl) AND THIS-OBJECT:BusinessEntityViewerControl:SmartTableIOState = TableIOStateEnum:ModifyingData) OR  
           (VALID-OBJECT (THIS-OBJECT:TableViewerControl)          AND THIS-OBJECT:TableViewerControl:SmartTableIOState          = TableIOStateEnum:ModifyingData) OR
           (VALID-OBJECT (THIS-OBJECT:DataRelationViewerControl)   AND THIS-OBJECT:DataRelationViewerControl:SmartTableIOState   = TableIOStateEnum:ModifyingData) OR
           (VALID-OBJECT (THIS-OBJECT:FieldViewerControl)          AND THIS-OBJECT:FieldViewerControl:SmartTableIOState          = TableIOStateEnum:ModifyingData) OR
           (VALID-OBJECT (THIS-OBJECT:IndexViewerControl)          AND THIS-OBJECT:IndexViewerControl:SmartTableIOState          = TableIOStateEnum:ModifyingData) THEN 
            
            THIS-OBJECT:Model:Enabled = FALSE . 
        ELSE 
            THIS-OBJECT:Model:Enabled = TRUE . 

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Splits an Extent field into multiple individual fields
	    Notes:   Raises an error, when the field cannot be found
        @param pcTempTableName The name of the Temp-Table
        @param pcFieldName The name of the Temp-Table field
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SplitExtentField (pcTempTableName AS CHARACTER, 
	                                     pcFieldName AS CHARACTER):
	
        DEFINE VARIABLE i       AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cFormat AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cTable  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cField  AS CHARACTER NO-UNDO .

        DEFINE BUFFER b_eField FOR eField . 
        DEFINE BUFFER b2_eField FOR eField . 
    
        THIS-OBJECT:StoreTablePosition () . 
        
        FIND b_eField WHERE b_eField.TempTableName = pcTempTableName
                        AND b_eField.FieldName     = pcFieldName . 
        
        /* Move following fields out of the way */
        REPEAT PRESELECT EACH b2_eField WHERE b2_eField.TempTableName = b_eField.TempTableName 
                                          AND b2_eField.FieldOrder    > b_eField.FieldOrder 
                                          BY b_eField.FieldOrder:
            FIND NEXT b2_eField . 
            
            ASSIGN b2_eField.FieldOrder = b2_eField.FieldOrder + 10000 .
        END. 

        ASSIGN cFormat = FormatHelper:IntegerFormatForMax (b_eField.FieldExtent) .

        DO i = 1 TO b_eField.FieldExtent:
            
            CREATE b2_eField.
            BUFFER-COPY b_eField TO b2_eField
                ASSIGN b2_eField.FieldExtent = 0 
                       b2_eField.FieldName   = SUBSTITUTE ("&1_&2":U,
                                                           b_eField.FieldName,
                                                           STRING (i, cFormat)).
    
            IF b2_eField.FieldXmlNodeName > "":U THEN 
                ASSIGN b2_eField.FieldXmlNodeName = SUBSTITUTE ("&1_&2":U,
                                                                b_eField.FieldXmlNodeName,
                                                                STRING (i, cFormat)).
            
            
            IF b2_eField.FieldSerializeName > "":U THEN 
                ASSIGN b2_eField.FieldSerializeName = SUBSTITUTE ("&1_&2":U,
                                                                  b_eField.FieldSerializeName,
                                                                  STRING (i, cFormat)).
                
            IF b_eField.FieldSource > "":U THEN 
                b2_eField.FieldSource = SUBSTITUTE ("&1[&2]":U,
                                                    b_eField.FieldSource, i) .
        END.

        ASSIGN cTable = b_eField.TempTableName 
               cField = b_eField.FieldName . 

        DELETE b_eField . 

        THIS-OBJECT:ReorderFields (cTable) .

        THIS-OBJECT:Refresh () .

        oFieldAdapter:RetrieveData () . 

        DO ON ERROR UNDO, THROW:
        
            oFieldAdapter:FindRowWhere (SUBSTITUTE ("WHERE eField.TempTableName = &1 AND eField.FieldName = ~"&2_&3~"":U,
                                                    QUOTER (cTable),
                                                    cField,
                                                    STRING (1, cFormat))) . 

            CATCH err AS Progress.Lang.SysError:
            	IF err:GetMessageNum (1) <> 7331 THEN 
                    UNDO, THROW err .             		
            END CATCH.
    
        END .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the ttCustomProperties records for the given scope 
        Notes:   
        @param poCustomPropertiesScope The property scope to edit the custom properties for
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreCustomProperties (poCustomPropertiesScope AS CustomPropertiesScopeEnum):
		
        DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO.
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
        
        DEFINE VARIABLE cKey1 AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cKey2 AS CHARACTER NO-UNDO.

        THIS-OBJECT:PrepareCustomPropertiesScope (poCustomPropertiesScope,
                                                  OUTPUT hBuffer, 
                                                  OUTPUT hQuery) .

        DO WHILE NOT hQuery:QUERY-OFF-END:
            hBuffer:BUFFER-DELETE () .

            hQuery:GET-NEXT . 
        END. 
        
        FOR EACH ttCustomProperties ON ERROR UNDO, THROW:
            hBuffer:BUFFER-CREATE () . 
            
            hBuffer:BUFFER-COPY (BUFFER ttCustomProperties:HANDLE) . 
            
            CASE poCustomPropertiesScope:
                WHEN CustomPropertiesScopeEnum:TempTable THEN DO:
                    ASSIGN cKey1    = oTableAdapter:GetFieldValues ("TempTableName":U) .
                
                    hBuffer::TempTableName = cKey1 . 
                END.  

                WHEN CustomPropertiesScopeEnum:DataRelation THEN DO:
                    ASSIGN cKey1    = oDataRelationAdapter:GetFieldValues ("DataRelationName":U) .
                
                    hBuffer::DataRelationName = cKey1 . 
                END.  

                WHEN CustomPropertiesScopeEnum:Field THEN DO:
                    ASSIGN cKey1    = oFieldAdapter:GetFieldValues ("TempTableName":U)
                           cKey2    = oFieldAdapter:GetFieldValues ("FieldName":U) .
                
                    hBuffer::TempTableName = cKey1 .
                    hBuffer::FieldName = cKey2 . 
                END.  

                WHEN CustomPropertiesScopeEnum:Index THEN DO:
                    ASSIGN cKey1    = oIndexAdapter:GetFieldValues ("TempTableName":U)
                           cKey2    = oIndexAdapter:GetFieldValues ("IndexName":U) .
                
                    hBuffer::TempTableName = cKey1 . 
                    hBuffer::IndexName = cKey2 . 
                END.  

            END CASE . 

            hBuffer:BUFFER-RELEASE () . 
        END.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stores the current record position                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreRecordPosition ():
        
        IF AVAILABLE eTable THEN 
            ASSIGN ro_Table = ROWID (eTable) . 
        ELSE ASSIGN ro_Table = ? . 
        
        IF AVAILABLE eDataRelation THEN 
            ASSIGN ro_DataRelation = ROWID (eDataRelation) . 
        ELSE ASSIGN ro_DataRelation = ? . 

        IF AVAILABLE eIndex THEN 
            ASSIGN ro_Index = ROWID (eIndex) . 
        ELSE ASSIGN ro_Index = ? . 

        IF AVAILABLE eField THEN 
            ASSIGN ro_Field = ROWID (eField) . 
        ELSE ASSIGN ro_Field = ? . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the position on the design canvas for each table.                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreTablePosition ():

        DEFINE VARIABLE oShape AS Crainiate.ERM4.Shape NO-UNDO . 
        
        DEFINE BUFFER b_eTable FOR eTable . 
        
        FOR EACH b_eTable:

            IF NOT THIS-OBJECT:Model:Shapes:Contains (STRING (ROWID (b_eTable))) THEN 
                LEAVE .               
              
            oShape = CAST (THIS-OBJECT:Model:Shapes:Item [STRING (ROWID (b_eTable))],
                           Crainiate.ERM4.Shape) .
            
            ASSIGN b_eTable.LayoutRow    = oShape:Y
                   b_eTable.LayoutColumn = oShape:X
                   b_eTable.LayoutHeight = oShape:Height
                   b_eTable.LayoutWidth  = oShape:Width
                   .
        END. 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SelectedItemChanged event of the TempTableShapes                                                                       
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The reference to the System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID TableSelectedItemChangedHandler (sender AS System.Object,
                                                           e AS System.EventArgs):
        
        DEFINE VARIABLE oTable AS TempTableShape NO-UNDO .
        DEFINE VARIABLE hQuery AS HANDLE         NO-UNDO .
        DEFINE VARIABLE cTag   AS CHARACTER      NO-UNDO .
        
        DEFINE BUFFER b_eField FOR eField . 
        DEFINE BUFFER b_eIndex FOR eIndex . 
        DEFINE BUFFER b_eTable FOR eTable . 
        
        IF NOT TYPE-OF (sender, TempTableShape) THEN 
            RETURN . 
            
        oTable = CAST (sender, TempTableShape) .
        
        IF oTable:Selected = FALSE THEN 
          RETURN . 
        
        FIND b_eTable WHERE ROWID(b_eTable) = TO-ROWID (oTable:Key) . 
               
        hQuery = oTableAdapter:QueryHandle .        

        hQuery:QUERY-OPEN () .
        hQuery:REPOSITION-TO-ROWID (ROWID (b_eTable)) .
        
        /* Mike Fechner, Consultingwerk Ltd. 09.05.2011
           Ensure that fields and indexes can be selected */
        oFieldAdapter:RetrieveData() .
        oIndexAdapter:RetrieveData() .
        
        IF NOT VALID-OBJECT (oTable:SelectedItem) THEN    
          RETURN .  
        
        ASSIGN cTag = UNBOX (oTable:SelectedItem:Tag) . 
        
        IF ENTRY (1, cTag) = "eField":U THEN DO:
            FIND b_eField WHERE ROWID(b_eField) = TO-ROWID (ENTRY (2, cTag)) . 
            
            hQuery = oFieldAdapter:QueryHandle .  
            hQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH eField WHERE eField.TempTableName = &1 BY eField.FieldOrder":U,
                                              QUOTER (b_eTable.TempTableName))) .
            hQuery:QUERY-OPEN () .                                                
            
            hQuery:REPOSITION-TO-ROWID (ROWID (b_eField)) .
            
            IF hQuery:CURRENT-RESULT-ROW > 0 THEN 
                oFieldAdapter:BindingSource:Position = hQuery:CURRENT-RESULT-ROW - 1 .  
            
            IF VALID-OBJECT (THIS-OBJECT:FieldViewerControl) THEN DO:
                THIS-OBJECT:DockManager:PaneFromControl (THIS-OBJECT:FieldViewerControl):Show () . 
            END.
        END.
        ELSE IF ENTRY (1, cTag) = "eIndex":U THEN DO:
            FIND b_eIndex WHERE ROWID(b_eIndex) = TO-ROWID (ENTRY (2, cTag)) . 
                    
            hQuery = oIndexAdapter:QueryHandle .      
            hQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH eIndex WHERE eIndex.TempTableName = &1 BY eIndex.IndexOrder":U,
                                              QUOTER (b_eTable.TempTableName))) .
            hQuery:QUERY-OPEN () .                                                
              
            hQuery:REPOSITION-TO-ROWID (ROWID (b_eIndex)) .
            
            IF hQuery:CURRENT-RESULT-ROW > 0 THEN 
                oIndexAdapter:BindingSource:Position = hQuery:CURRENT-RESULT-ROW - 1 .  
            
            IF VALID-OBJECT (THIS-OBJECT:IndexViewerControl) THEN DO:
                THIS-OBJECT:DockManager:PaneFromControl (THIS-OBJECT:IndexViewerControl):Show () . 
            END.
        END .  
        
        OnTableItemSelected (Consultingwerk.EventArgs:Empty) .
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SelectedChanged event of the TempTableShapes                                                                       
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The reference to the System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID TableShapeSelectedChangedHandler (sender AS System.Object,
                                                            e AS System.EventArgs):
        
        DEFINE BUFFER b_eTable FOR eTable . 
        
        DEFINE VARIABLE iPosition AS INTEGER              NO-UNDO.
        DEFINE VARIABLE oTable    AS Crainiate.ERM4.Table NO-UNDO .
        DEFINE VARIABLE hQuery    AS HANDLE               NO-UNDO.
        
        oTable = CAST (sender, Crainiate.ERM4.Table) . 
        
        IF oTable:Selected = FALSE THEN 
          RETURN . 

        FIND b_eTable WHERE ROWID(b_eTable) = TO-ROWID (oTable:Key) . 
                
        hQuery = oTableAdapter:QueryHandle .        
        hQuery:QUERY-OPEN () .
        hQuery:REPOSITION-TO-ROWID (ROWID (b_eTable)) .
        
        /* Mike Fechner, Consultingwerk Ltd. 09.05.2011
           Ensure that fields and indexes can be selected */
        oFieldAdapter:RetrieveData() .
        oIndexAdapter:RetrieveData() .
        
        IF hQuery:CURRENT-RESULT-ROW > 0 THEN 
            oTableAdapter:BindingSource:Position = hQuery:CURRENT-RESULT-ROW - 1 .  
        
        IF VALID-OBJECT (THIS-OBJECT:TableViewerControl) THEN DO:
            THIS-OBJECT:DockManager:PaneFromControl (THIS-OBJECT:TableViewerControl):Show () . 
        END.
        
        IF VALID-OBJECT (oTable:SelectedItem) THEN
            THIS-OBJECT:TableSelectedItemChangedHandler (oTable, System.EventArgs:Empty) .
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Updates the custom properties for the currently selected object of 
                 the given scope 
        Notes:   
        @param poCustomPropertiesScope The property scope to edit the custom properties for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateCustomProperties (poCustomPropertiesScope AS CustomPropertiesScopeEnum):
        
        DEFINE VARIABLE oForm AS CustomPropertiesEditorForm NO-UNDO . 
        DEFINE VARIABLE lOk   AS LOGICAL                    NO-UNDO .
        
        EMPTY TEMP-TABLE ttCustomProperties .
        		
        THIS-OBJECT:FillCustomProperties (poCustomPropertiesScope) .        		
        		
        oForm = NEW CustomPropertiesEditorForm () . 
        oForm:UpdateCustomProperties (poCustomPropertiesScope, 
                                      INPUT-OUTPUT TABLE ttCustomProperties BY-REFERENCE, 
                                      OUTPUT lOk) .         		
        		
        IF NOT lOk THEN RETURN .         		
        		
        THIS-OBJECT:StoreCustomProperties (poCustomPropertiesScope) .		
		THIS-OBJECT:UnsavedChanges = TRUE . 
		
		FINALLY:
            EMPTY TEMP-TABLE ttCustomProperties . 		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Uses DataRelationFieldPickerForm to update the fields of a DATA-RELATION                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateDataRelationFields ():
        
        DEFINE VARIABLE oForm         AS Consultingwerk.BusinessEntityDesigner.UI.DataRelationFieldPickerForm NO-UNDO .
        
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult                                    NO-UNDO . 
        DEFINE VARIABLE oParent       AS System.Windows.Forms.Form                                            NO-UNDO .

        DEFINE BUFFER eDataRelation FOR eDataRelation. 

        DEFINE BUFFER b_ParentTable FOR eTable . 
        DEFINE BUFFER b_ChildTable FOR eTable . 
        
        oForm = NEW Consultingwerk.BusinessEntityDesigner.UI.DataRelationFieldPickerForm () . 

        FIND eDataRelation WHERE eDataRelation.DataRelationName = oDataRelationAdapter:GetFieldValues ("DataRelationName":U) .
        
        FIND FIRST b_ParentTable WHERE b_ParentTable.BusinessEntityName = eDataRelation.BusinessEntityName 
                                   AND b_ParentTable.TempTableName      = eDataRelation.ParentTempTableName .
                                   
        FIND FIRST b_ChildTable  WHERE b_ChildTable.BusinessEntityName  = eDataRelation.BusinessEntityName 
                                   AND b_ChildTable.TempTableName       = eDataRelation.ChildTempTableName .
        
        oForm:Dataset = THIS-OBJECT:DatasetHandle .
        oForm:DeleteOnFormClosed = FALSE .  
        oForm:ParentTableBuffer = BUFFER b_ParentTable:HANDLE . 
        oForm:ChildTableBuffer = BUFFER b_ChildTable:HANDLE . 
        oForm:RelationFields = eDataRelation.RelationFields .
            
        oParent = THIS-OBJECT:model:FindForm() . 
        
        WAIT-FOR oForm:ShowDialog (oParent) SET oDialogResult.

        IF Progress.Util.EnumHelper:AreEqual (oDialogResult, System.Windows.Forms.DialogResult:Ok) THEN DO: 
            ASSIGN eDataRelation.RelationFields = oForm:RelationFields . 
                   
            oDataRelationAdapter:BindingSource:Refresh () .
        END.
        
        DELETE OBJECT oForm . 
        
    END METHOD.

END CLASS.
