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
    File        : SmartComboEditor
    Purpose     : Extension to the Infragistics UltraComboEditor
    Syntax      : 
    Description : Combo-Box that can populate itself with a ValueList based
                  on data retrieved from a Business Entity Table
                  As the Infragistics UltraComboEditor does not provide 
                  editor-support this class uses the UltraComboEditorWithDesigner
                  as a base class
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 01 13:49:20 CET 2013
    Notes       : Leverages the ValueListHelper class to build a value list
                  Accesses the backend directly through the ServiceAdapter
                  as no binding source and no navigation facilities are 
                  required (both would be features that would justify using 
                  a SmartBusinessEntityAdapter like the Lookup does.
                  The FlagValue and FlagText are only supported, when the 
                  ImageField (BLOB) is not set
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                      FROM PROPATH . 
USING Consultingwerk.SmartComponents.*                FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH . 
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH . 
USING Consultingwerk.OERA.*                           FROM PROPATH . 
USING Consultingwerk.Util.*                           FROM PROPATH . 
USING Infragistics.Win.*                              FROM ASSEMBLY . 
USING Infragistics.Win.UltraWinEditors.*              FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .
USING System.ComponentModel.*                         FROM ASSEMBLY .
USING System.ComponentModel.Design.*                  FROM ASSEMBLY .
USING System.Windows.Forms.*                          FROM ASSEMBLY . 

CLASS Consultingwerk.SmartComponents.Implementation.SmartComboEditor 
    INHERITS UltraComboEditor
    IMPLEMENTS ICustomTypeDescriptor, IHasDesignerVerbs,  ISupportInitialize: 

    /* Mike Fechner, Consultingwerk Ltd. 19.05.2010
       Core implementation of ICustomTypeDescriptor */
    {Consultingwerk/SmartComponents/Base/ICustomTypeDescriptor.i}
    {Consultingwerk/SmartComponents/Base/CreateVerbs.i}

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the name of the AppServer Partition to be used
                 by this SmartComboEditor instance
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AppServerPartition AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to generate Designer Verbs (actions for the 
                 Visual Designer)                                                                 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PROTECTED PROPERTY DesignerVerbs AS CHARACTER NO-UNDO 
    GET. 
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Propery flags if the current instance of the SmartBindingSource
                 is running at design time or runtime.                                                                        
        Notes:   Set during constructor using a check at the 
                 System.ComponentModel.LicenseManager                                                                     
    ------------------------------------------------------------------------------*/        
    DEFINE PROTECTED PROPERTY DesignTime AS LOGICAL NO-UNDO INIT FALSE
    GET.
    PRIVATE SET.          

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the comma-delimited list of field named that should be 
                 used for the display text of the value list
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DisplayFields AS CHARACTER NO-UNDO INIT "":U 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the substitute expression (e.g. &1/&2) to be used for 
                 the display text of the value list
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DisplaySubstitute AS CHARACTER NO-UNDO INIT "&1":U
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the BusinessEntity that is the data source 
                 for this Control 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO INIT "":U
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Temp-Table that is the data source 
                 for this Control 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Display Text for the flag value
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FlagText AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the CHARACTER value used for a Flag value (e.g. all 
                 or none) 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY FlagValue AS CHARACTER NO-UNDO INIT "":U 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the field (BLOB) that contains the Image
                 data for the ValueList
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ImageField AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the field that contains the image key to be used
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ImageKeyField AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the SUBSTITUTE pattern to be used to build the actual image key to be used
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ImageKeyFieldSubstitute AS CHARACTER INITIAL "&1":U NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the field from the temp-table that should 
                 be used as the Value
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY KeyField AS CHARACTER NO-UNDO INIT "":U 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 should not show up in the Visual Designers Property Grid                                                                 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PROTECTED PROPERTY NonBrowsableProperties AS CHARACTER NO-UNDO 
    GET. 
    SET.    

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the QueryString that is used when fetching data from 
                 the Business Entity
        Notes:   Leave empty to return all rows from the backend   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY QueryString AS CHARACTER NO-UNDO INIT ""
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the SmartComboEditor should retrieve data during 
                 the EndInit method 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY RetrieveDataOnInit AS LOGICAL INITIAL TRUE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the ServiceAdapter that is used
                 by this Control
        Notes:   Defaults to Consultingwerk.Framework.FrameworkSettings:ServiceAdapter
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ServiceAdapter AS IServiceAdapter NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartComboEditor class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SmartComboEditor ():
		SUPER ().
	
        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .
                                                                   
        IF THIS-OBJECT:DesignTime THEN  
            THIS-OBJECT:SetDesignerProperties () .
        		
        THIS-OBJECT:ServiceAdapter = FrameworkSettings:ServiceAdapter . 
        		
        THIS-OBJECT:DropDownStyle = DropDownStyle:DropDownList.
        THIS-OBJECT:LimitToList = TRUE.
        		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invoked at the beginning of a Containers InitializeComponents 
                 method                                                       
        Notes:   Enforced by System.ComponentModel.ISupportInitialize           
                 http://msdn.microsoft.com/de-de/library/system.componentmodel.isupportinitialize.begininit(VS.80).aspx                                                   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID BeginInit ():
		
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invoked at the end of a Containers InitializeComponents method                                                       
        Notes:   Enforced by System.ComponentModel.ISupportInitialize           
                 http://msdn.microsoft.com/de-de/library/system.componentmodel.isupportinitialize.endinit(VS.80).aspx                                                 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID EndInit ():
		
		IF NOT THIS-OBJECT:DesignTime AND THIS-OBJECT:RetrieveDataOnInit THEN 
		    THIS-OBJECT:RetrieveData () . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs                                                                       
        Notes:   This method is intended to be overridden    
        @param pcVerbText The name (Label) of the Designer Verb that was invoked                                                                   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OnVerbClicked (pcVerbText AS CHARACTER):
        
        Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE .
        
        CASE pcVerbText:
            WHEN "Select Table":U THEN
                ShowTablePickerDialog () .
            WHEN "Select BusinessEntity":U THEN 
                ShowBusinessEntityPickerDialog () .      
        END CASE . 

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

        FINALLY:
            Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE .
        END FINALLY.
    END METHOD . 
            
    /*------------------------------------------------------------------------------
        Purpose: Retrieves data from the Backend and populates the ValueList 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RetrieveData ():
		
        DEFINE VARIABLE hDataset          AS HANDLE           NO-UNDO .
        DEFINE VARIABLE hTable            AS HANDLE           NO-UNDO .
        DEFINE VARIABLE oFetchDataRequest AS FetchDataRequest NO-UNDO . 

        IF Consultingwerk.CharacterType:IsNullOrEmpty (THIS-OBJECT:EntityTable) OR 
           Consultingwerk.CharacterType:IsNullOrEmpty (THIS-OBJECT:EntityName) THEN 
           
           RETURN . 

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i THIS-OBJECT:ServiceAdapter
                                                         """Service Adapter"":U"} .
                           
        oFetchDataRequest = NEW FetchDataRequest (THIS-OBJECT:EntityTable,
                                                  THIS-OBJECT:QueryString) .                           

        THIS-OBJECT:ServiceAdapter:RetrieveData (THIS-OBJECT:AppServerPartition,
                                                 THIS-OBJECT:EntityName,
                                                 oFetchDataRequest,
                                                 OUTPUT DATASET-HANDLE hDataset) . 

        ASSIGN hTable = hDataset:GET-BUFFER-HANDLE (THIS-OBJECT:EntityTable):TABLE-HANDLE .

        IF THIS-OBJECT:ImageField > "":U THEN 
            THIS-OBJECT:ValueList = ValueListHelper:FromTempTable (hTable,
                                                                   THIS-OBJECT:KeyField,
                                                                   THIS-OBJECT:DisplayFields,
                                                                   THIS-OBJECT:DisplaySubstitute,
                                                                   THIS-OBJECT:ImageField) .
        ELSE 
            IF THIS-OBJECT:ImageKeyField > "":U AND 
               THIS-OBJECT:ImageKeyFieldSubstitute > "":U THEN 
                
            THIS-OBJECT:ValueList = ValueListHelper:FromTempTableAndImageProvider (hTable,
                                                                                   THIS-OBJECT:KeyField,
                                                                                   THIS-OBJECT:DisplayFields,
                                                                                   THIS-OBJECT:DisplaySubstitute,
                                                                                   THIS-OBJECT:ImageKeyField,
                                                                                   THIS-OBJECT:ImageKeyFieldSubstitute,
                                                                                   THIS-OBJECT:FlagValue,
                                                                                   THIS-OBJECT:FlagText) .
        ELSE 
            THIS-OBJECT:ValueList = ValueListHelper:FromTempTable (hTable,
                                                                   THIS-OBJECT:KeyField,
                                                                   THIS-OBJECT:DisplayFields,
                                                                   THIS-OBJECT:DisplaySubstitute,
                                                                   THIS-OBJECT:FlagValue,
                                                                   THIS-OBJECT:FlagText) .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN 
                DELETE OBJECT hDataset . 		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes design time properties 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID SetDesignerProperties ():
		
		THIS-OBJECT:DesignerVerbs = "Select BusinessEntity,Select Table":U .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows a dialog to select a Business Entity                                                                     
        Notes:   Invoked by the "Select BusinessEntity" Designer Verb, only intended 
                 for DesignTime                                                                           
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowBusinessEntityPickerDialog ():
        
        DEFINE VARIABLE cEntityName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOldValue   AS CHARACTER NO-UNDO.

        IF NOT THIS-OBJECT:DesignTime THEN 
            RETURN .        

        BusinessEntityDesignerSupport:SelectBusinessEntityName (OUTPUT cEntityName). 

        IF cEntityName > "":U THEN DO:
            ASSIGN cOldValue              = THIS-OBJECT:EntityName  
                   THIS-OBJECT:EntityName = cEntityName .

            Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT, 
                                                                      "EntityName":U,
                                                                      BOX (cOldValue), 
                                                                      BOX (cEntityName)) .
        END.
        
        Consultingwerk.Util.DesignerHelper:RefreshPropertyGrid (THIS-OBJECT) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Table Picker Dialog and sets properties of the 
                 SmartBusinessEntityLookup during Design Time                                                                        
        Notes:   Invoked by the "Select Tables" Designer Verb. Only intended for 
                 DesignTime                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowTablePickerDialog ():

        IF NOT THIS-OBJECT:DesignTime AND NOT FrameworkSettings:AllowDesignFeatures THEN 
            RETURN . 

        SmartComboEditorSupport:ShowTablePickerDialog (THIS-OBJECT) .

        Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT, 
                                                                  "LookupEntityTable":U,
                                                                  ?, 
                                                                  ?) .   
                                                                             
        Consultingwerk.Util.DesignerHelper:RefreshPropertyGrid (THIS-OBJECT) .

    END METHOD.

END CLASS.
