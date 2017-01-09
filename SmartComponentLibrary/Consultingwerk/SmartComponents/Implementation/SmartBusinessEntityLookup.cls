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
    File        : SmartBusinessEntityLookup
    Purpose     : Lookup implementation with Support for OERA Business Entities
    Syntax      : 
    Description : Allows record selection using lookup field search and 
                  a lookup dialog
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Apr 07 08:29:30 CEST 2009
    Notes       : The SmartBusinessEntityLookup can be used as an embedded
                  Editor for an UltraGrid Control's column. When doing so, 
                  the OwningGrid property of the SmartLookup property 
                  needs to by set to the reference of the UltraGrid.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.*            FROM PROPATH . 
USING Consultingwerk.SmartComponents.Base.*             FROM PROPATH . 
USING Consultingwerk.SmartComponents.Enum.*             FROM PROPATH . 
USING Consultingwerk.SmartComponents.Support.*          FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*   FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*       FROM PROPATH .
USING Consultingwerk.Util.*                             FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                FROM ASSEMBLY .
USING Progress.Lang.*                                   FROM ASSEMBLY .
USING Progress.Util.*                                   FROM ASSEMBLY .
USING System.Windows.Forms.*                            FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup 
    INHERITS SmartBusinessEntityLookupDesign: 

    DEFINE VARIABLE oLookupDialogValueDictionary AS "System.Collections.Generic.Dictionary<System.String,System.Object>" NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the lookups SmartBusinessEntityAdapter is initialized
        Notes:      
        @param sender The reference to the object that raised the event
        @param e The AfterInitializeBusinessEntityAdapterEventArgs with the data for this event            
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterInitializeBusinessEntityAdapter SIGNATURE VOID (sender AS System.Object, 
                                                                             e AS AfterInitializeBusinessEntityAdapterEventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Raised before the Lookup Dialog is shown
        Notes:   
        @param sender The object that raised the BeforeLookupDialogShown event
        @param e The BeforeLookupDialogShownEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeLookupDialogShown SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                e AS BeforeLookupDialogShownEventArgs).
        
    /*------------------------------------------------------------------------------
        Purpose: Name of the Type of SmartBusinessEntityAdapter used to lookup 
                 values from the backend.
        Notes:   Modified from a child class only   
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY AdapterType AS CHARACTER NO-UNDO
        INIT "Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter":U 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the dictionary that contains the values 
                 for the LookupAdditionalFields  
        Notes:   This dictionary is populated so that is can be accessed in event handlers 
                 for the AfterPerformLookup and LookupComplete events 
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY AdditionalLookupFieldValues AS CharacterDictionary NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the name of the AppServer Partition to be used
                 by this SmartBusinessEntityLookup instance
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AppServerPartition AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the reference to the SmartBusinessEntityAdapter instance
                 used by this SmartBusinessEntityLookup
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY SmartBusinessEntityAdapter AS SmartBusinessEntityAdapter NO-UNDO 
    GET.
    SET. 
            
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartBusinessEntityLookup control                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityLookup ():
        SUPER ().
    
        IF THIS-OBJECT:DesignTime THEN     
/*            THIS-OBJECT:SetDesignerProperties ()*/
            .
        ELSE 
            THIS-OBJECT:AdditionalLookupFieldValues = NEW CharacterDictionary () .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Handles the AfterInitializeBusinessEntityAdapter event of the 
                 Lookup Dialog                                                                       
        Notes:   Forwards the event 
        @param sender The reference to the object that raised the event
        @param e The AfterInitializeBusinessEntityAdapterEventArgs with the data for this event                                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterInitializeBusinessEntityAdapterHandler (sender AS System.Object,
                                                                       e AS AfterInitializeBusinessEntityAdapterEventArgs):
        
        OnAfterInitializeBusinessEntityAdapter (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Launches the SmartBusinessEntityAdapter instance                                                                        
        Notes:
        @return The reference to the launched SmartBusinessEntityAdapter instance                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SmartBusinessEntityAdapter LaunchBusinessEntityAdapter ():

        DEFINE VARIABLE oAdapter   AS SmartBusinessEntityAdapter            NO-UNDO . 
        
        IF VALID-OBJECT (THIS-OBJECT:SmartBusinessEntityAdapter) THEN 
            ASSIGN oAdapter = THIS-OBJECT:SmartBusinessEntityAdapter . 
        ELSE DO:
            IF THIS-OBJECT:AdapterType > "":U THEN 
                oAdapter = DYNAMIC-NEW (THIS-OBJECT:AdapterType) () .
            ELSE 
                oAdapter = NEW SmartBusinessEntityAdapter () .
        END.
                
        ASSIGN 
            oAdapter:BatchSize          = 1 
            oAdapter:AppServerPartition = THIS-OBJECT:AppServerPartition
            oAdapter:EntityName         = THIS-OBJECT:LookupEntityName 
            oAdapter:EntityTable        = THIS-OBJECT:LookupEntityTable
            oAdapter:EntityView         = THIS-OBJECT:LookupEntityView 
            oAdapter:EntityJoin         = TRIM(FILL ("YES,":U, NUM-ENTRIES(THIS-OBJECT:LookupEntityView)), ",":U) 
            .

        OnAfterInitializeBusinessEntityAdapter (NEW AfterInitializeBusinessEntityAdapterEventArgs (oAdapter, 
                                                                                                   AdapterOriginEnum:LookupControl)) .         

        RETURN oAdapter .
        
        FINALLY:
            ASSIGN THIS-OBJECT:SmartBusinessEntityAdapter = oAdapter .                    
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the FormClosed event of the Lookup Dialog
        Notes:   Stores value from the lookup in the oLookupDialogValueDictionary 
                 dictionary so that the values are available even after the Lookup 
                 Dialog was disposed (after closing)
        @param sender The reference to the object that raised the event
        @param e The FormClosedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID LookupDialogFormClosedHandler (sender AS System.Object,
	                                                     e AS FormClosedEventArgs):
	
		DEFINE VARIABLE oLookupDialog AS ISmartBusinessEntityLookupDialog NO-UNDO . 
        DEFINE VARIABLE i             AS INTEGER                          NO-UNDO .

        IF EnumHelper:AreEqual (CAST (sender, Form):DialogResult, DialogResult:Ok) THEN DO:
      	    
      	    oLookupDialog = CAST (sender, ISmartBusinessEntityLookupDialog) . 
      	    
      	    oLookupDialogValueDictionary = NEW "System.Collections.Generic.Dictionary<System.String,System.Object>" () . 
            
            oLookupDialogValueDictionary:Add (THIS-OBJECT:LookupKeyField,
                                              oLookupDialog:GetLookupFieldValue (THIS-OBJECT:LookupKeyField)) . 
            
            IF THIS-OBJECT:LookupKeyValueColumn > "":U AND NOT oLookupDialogValueDictionary:ContainsKey (THIS-OBJECT:LookupKeyValueColumn) THEN 
                oLookupDialogValueDictionary:Add (THIS-OBJECT:LookupKeyValueColumn,
                                                  oLookupDialog:GetLookupFieldValue (THIS-OBJECT:LookupKeyValueColumn)) .

            DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupFields):
                IF NOT oLookupDialogValueDictionary:ContainsKey (ENTRY(i, THIS-OBJECT:LookupFields)) THEN 
                    oLookupDialogValueDictionary:Add (ENTRY(i, THIS-OBJECT:LookupFields),
                                                      oLookupDialog:GetLookupFieldValue (ENTRY(i, THIS-OBJECT:LookupFields))) .
                
            END.
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterInitializeBusinessEntityAdapter event                                                                        
        Notes:
        @param e The AfterInitializeBusinessEntityAdapterEventArgs with the data for this event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterInitializeBusinessEntityAdapter (e AS AfterInitializeBusinessEntityAdapterEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("Unable to raise the AfterInitializeBusinessEntityAdapter event without AfterInitializeBusinessEntityAdapterEventArgs.", 0) . 

        THIS-OBJECT:AfterInitializeBusinessEntityAdapter:Publish (THIS-OBJECT, e) . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeLookupDialogShown
        Notes:   
        @param e The BeforeLookupDialogShownEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeLookupDialogShown (e AS BeforeLookupDialogShownEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BeforeLookupDialogShown":U) .
            
        THIS-OBJECT:BeforeLookupDialogShown:Publish (THIS-OBJECT, e) .
    
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs lookup when user presses a key in the lookup field                                                                       
        Notes:       
        @param plLostFocus Logical value indicating if the Lookup is performed due to losing the focus
    ------------------------------------------------------------------------------*/    
    METHOD OVERRIDE PROTECTED VOID PerformLookup (plLostFocus AS LOGICAL):        

        DEFINE VARIABLE oAdapter    AS SmartBusinessEntityAdapter            NO-UNDO . 
        DEFINE VARIABLE hBuffer     AS HANDLE                                NO-UNDO .
        DEFINE VARIABLE lAvailable  AS LOGICAL                               NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER                               NO-UNDO .
        DEFINE VARIABLE oControl    AS System.Windows.Forms.Control          NO-UNDO .
        DEFINE VARIABLE oParent     AS System.Windows.Forms.Control          NO-UNDO .
        DEFINE VARIABLE oBefore     AS System.ComponentModel.CancelEventArgs NO-UNDO .
        DEFINE VARIABLE cValue      AS CHARACTER                             NO-UNDO .
        DEFINE VARIABLE cColumnName AS CHARACTER                             NO-UNDO .

        DEFINE VARIABLE iPosition  AS INTEGER                                NO-UNDO .

        oBefore = NEW System.ComponentModel.CancelEventArgs () .

        OnBeforePerformLookup (oBefore) .
        
        IF oBefore:Cancel THEN 
            RETURN . 

        IF NOT plLostFocus THEN DO:
            IF NOT VALID-OBJECT (THIS-OBJECT:OwningGrid) THEN  
                ASSIGN iPosition = THIS-OBJECT:SelectionStart .
            ELSE IF THIS-OBJECT:OwningGrid:ActiveCell:IsInEditMode THEN DO: 
                ASSIGN iPosition = THIS-OBJECT:OwningGrid:ActiveCell:SelStart .
            END.
        END.
          
        ASSIGN oAdapter  = LaunchBusinessEntityAdapter() .

        IF NOT VALID-OBJECT (THIS-OBJECT:OwningGrid) THEN 
            ASSIGN cValue = UNBOX (THIS-OBJECT:Value) . 
        ELSE IF VALID-OBJECT (THIS-OBJECT:OwningGrid:ActiveCell) THEN DO: 
            ASSIGN cValue = THIS-OBJECT:OwningGrid:ActiveCell:Text .

            /* Mike Fechner, Consultingwerk Ltd. 04.07.2015
               SCL-888 - can't use ActiveCell:Value above as that's not yet assigned and 
               ActiveCell:Text and the lookup controls Value property behave differntly 
               on empty lookup */
            IF cValue = "":U THEN 
                ASSIGN cValue = ? . 
        END.

        oAdapter:QueryString  = SUBSTITUTE (THIS-OBJECT:LookupQueryString, 
                                            cValue) .

        oAdapter:QuerySort = THIS-OBJECT:LookupQuerySort .                                             
                                            
        oAdapter:RetrieveData () .                                            

        ASSIGN hBuffer    = oAdapter:Dataset:GET-BUFFER-HANDLE (THIS-OBJECT:LookupEntityTable) 
               lAvailable = hBuffer:AVAILABLE 
               oParent    = THIS-OBJECT:Parent .
        
        ParentLookup: DO WHILE VALID-OBJECT(oParent):
            IF TYPE-OF (oParent, SmartViewerControl) THEN 
                LEAVE ParentLookup .    
                
            oParent = oParent:Parent .                
        END.

        IF VALID-OBJECT(oParent) THEN 
            DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupFields):
                ASSIGN oControl = CAST(oParent, SmartViewerControl):GetViewerField (ENTRY(i, THIS-OBJECT:LookupControls)) .           
                
                /* Mike Fechner, Consultingwerk Ltd. 24.02.2010
                   When Nulling an UltraNumericEditor we cannot set Text = "", we need to 
                   set the Value property to 0 or ? */
                
                IF VALID-OBJECT(oControl) THEN DO:
                    IF lAvailable THEN DO:
                        cColumnName = QueryHelper:QualifiedColumnName (ENTRY(i, THIS-OBJECT:LookupFields), 
                                                                       oAdapter:QueryHandle) .
                        
                        IF TYPE-OF (oControl, UltraDateTimeEditor) THEN 
                            CAST (oControl, UltraDateTimeEditor):DateTime = DataTypeHelper:ToDateTime (oAdapter:GetFieldValues (cColumnName)) .
                        ELSE 
                            oControl:Text = oAdapter:GetFieldValues (cColumnName) .

                        IF THIS-OBJECT:LookupCascade AND THIS-OBJECT:LookupCascade AND TYPE-OF (oControl, SmartBusinessEntityLookup) THEN 
                            CAST (oControl, SmartBusinessEntityLookup):PerformLookup () . 
                    END. 
                    ELSE DO:
                        IF TYPE-OF (oControl, UltraNumericEditor) THEN DO:
                            IF CAST (oControl, UltraNumericEditor):Nullable THEN  
                                CAST (oControl, UltraNumericEditor):Value = ? . 
                            ELSE  
                                CAST (oControl, UltraNumericEditor):Value = BOX (0) .
                        END.  
                        IF TYPE-OF (oControl, UltraDateTimeEditor) THEN DO:
                            CAST (oControl, UltraDateTimeEditor):Value = BOX (NOW) .
                        END.   
                        ELSE 
                            oControl:Text = "":U .
                    END.                       
                END.                      
            END.
        ELSE 
            DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupFields):
                IF THIS-OBJECT:Parent:Controls:ContainsKey (ENTRY(i, THIS-OBJECT:LookupControls)) THEN DO:
                                        
                    ASSIGN oControl = THIS-OBJECT:Parent:Controls [ENTRY(i, THIS-OBJECT:LookupControls)] . 
                    
                    IF lAvailable THEN DO:
                        cColumnName = QueryHelper:QualifiedColumnName (ENTRY(i, THIS-OBJECT:LookupFields), 
                                                                       oAdapter:QueryHandle) .
                        
                        IF TYPE-OF (oControl, UltraDateTimeEditor) THEN 
                            CAST (oControl, UltraDateTimeEditor):DateTime = DataTypeHelper:ToDateTime (oAdapter:GetFieldValues (cColumnName)) .
                        ELSE 
                            oControl:Text = oAdapter:GetFieldValues (cColumnName) .
                        
                        IF THIS-OBJECT:LookupCascade AND THIS-OBJECT:LookupCascade AND TYPE-OF (oControl, SmartBusinessEntityLookup) THEN 
                            CAST (oControl, SmartBusinessEntityLookup):PerformLookup () . 
                    END. 
                    ELSE DO:
                        IF TYPE-OF (oControl, UltraNumericEditor) THEN DO:
                            IF CAST (oControl, UltraNumericEditor):Nullable THEN  
                                CAST (oControl, UltraNumericEditor):Value = ? . 
                            ELSE  
                                CAST (oControl, UltraNumericEditor):Value = BOX (0) .
                        END.    
                        IF TYPE-OF (oControl, UltraDateTimeEditor) THEN DO:
                            CAST (oControl, UltraDateTimeEditor):Value = BOX (NOW) .
                        END.   
                        ELSE 
                            oControl:Text = "":U .
                    END.                       
                END. 
            END.                    
    
        IF lAvailable THEN DO:
            cColumnName = QueryHelper:QualifiedColumnName (THIS-OBJECT:LookupKeyField, 
                                                           oAdapter:QueryHandle) .
            
            THIS-OBJECT:Value = oAdapter:GetFieldValues (cColumnName) .
            
            IF THIS-OBJECT:LookupKeyValueColumn > "":U THEN  DO:
                cColumnName = QueryHelper:QualifiedColumnName (THIS-OBJECT:LookupKeyValueColumn, 
                                                               oAdapter:QueryHandle) .

                THIS-OBJECT:LookupKeyValue = oAdapter:GetFieldValues (cColumnName) .
            END.
        END.                  
        ELSE DO:
            IF plLostFocus THEN DO: 
                THIS-OBJECT:Value = ? .
                THIS-OBJECT:LookupKeyValue = ? .
            END.
            ELSE IF VALID-OBJECT (THIS-OBJECT:OwningGrid) THEN DO:
                THIS-OBJECT:Value = cValue .                
            END.
            
            THIS-OBJECT:LookupClearPending = TRUE . 
        END.
    
        IF THIS-OBJECT:LookupAdditionalFields > "":U THEN 
            THIS-OBJECT:PopulateAdditionalFieldDictionary (oAdapter) .

        OnAfterPerformLookup (System.EventArgs:Empty) .

        IF NOT plLostFocus THEN DO:  
            IF NOT VALID-OBJECT (THIS-OBJECT:OwningGrid) THEN DO:
                IF THIS-OBJECT:TextLength >= iPosition THEN
                    ASSIGN THIS-OBJECT:SelectionStart  = iPosition
                           THIS-OBJECT:SelectionLength = THIS-OBJECT:TextLength - iPosition .
            END.
            ELSE DO:
                IF THIS-OBJECT:TextLength >= iPosition AND THIS-OBJECT:OwningGrid:ActiveCell:IsInEditMode THEN DO ON ERROR UNDO, THROW:
                    ASSIGN THIS-OBJECT:OwningGrid:ActiveCell:SelStart  = iPosition
                           THIS-OBJECT:OwningGrid:ActiveCell:SelLength = THIS-OBJECT:TextLength - iPosition .
                
                    CATCH sysex AS System.NotSupportedException:
                    	/* Ignore */	
                    END CATCH.
                    
                END.
            END.
        END.

        FINALLY:
            /* Mike Fechner, Consultingwerk Ltd. 01.04.2011
               Allows to suspend deletion of the adapter instance in an 
               overriding class */
            ShutdownBusinessEntityAdapter () .
        END FINALLY .                   

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs the lookup functioanlity, performs a call to the data source
                 to retrieve the record based on the field value the user entered
                 in the lookup control
        Notes:   Abstract method that needs to be overidden in a more specialized
                 Lookup class, public method, that always assumes that the lookup has 
                 no longer focus. Calls into PerformLookup (TRUE).
    ------------------------------------------------------------------------------*/   
	METHOD OVERRIDE PUBLIC VOID PerformLookup ():
		
		THIS-OBJECT:PerformLookup (TRUE) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates the dictionary with the additional lookup field values 
        Notes:   
        @param poAdapter The reference to the SmartBusinessEntityAdapter that returns the values
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateAdditionalFieldDictionary (poAdapter AS SmartBusinessEntityAdapter):
		
		DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
		DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
		
		THIS-OBJECT:AdditionalLookupFieldValues:Clear() .
		
		DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:LookupAdditionalFields):
		    ASSIGN cField = ENTRY (i, THIS-OBJECT:LookupAdditionalFields) .
		    
		    THIS-OBJECT:AdditionalLookupFieldValues:Add (cField,
		                                                 poAdapter:GetFieldValues (cField)) . 
		END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the dictionary with the additional lookup field values 
        Notes:   
        @param poDialog The reference to the ISmartBusinessEntityLookupDialog that returns the values
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateAdditionalFieldDictionary (poDialog AS ISmartBusinessEntityLookupDialog):
        
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
        
        THIS-OBJECT:AdditionalLookupFieldValues:Clear() .
        
        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:LookupAdditionalFields):
            ASSIGN cField = ENTRY (i, THIS-OBJECT:LookupAdditionalFields) .
            
            THIS-OBJECT:AdditionalLookupFieldValues:Add (cField,
                                                         poDialog:GetLookupFieldValue (cField)) . 
        END.

    END METHOD .
     /*------------------------------------------------------------------------------
        Purpose: A LookupDialog is shown with the Data Information specified with the
                 Properties LookupEntityName, LookupEntityTable and LookupEntityView.
        Notes:   This Method is called from the Base Class if a user presses the 
                 LookupEditorButton in RunTime.
                 A LookupDialog is set up with a Title and Data from the Backend 
                 defined by the Properties LookupEntityName, LookupEntityTable and 
                 LookupEntityView. The shown Columns of the resultset can be limited
                 by providing a comma delimeted list to the Property 
                 LookupBrowserFields. The LookupKeyField is the field used to find
                 the correct Data Row when comming back from the Dialog. When the 
                 OK Button was selected the related fields are recursively updated.
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID ShowLookupDialog ():
        
        DEFINE VARIABLE oLookupDialog AS ISmartBusinessEntityLookupDialog  NO-UNDO.
        DEFINE VARIABLE oForm         AS System.Windows.Forms.Form         NO-UNDO . 
        DEFINE VARIABLE oResult       AS System.Windows.Forms.DialogResult NO-UNDO.
        DEFINE VARIABLE i             AS INTEGER                           NO-UNDO.
        DEFINE VARIABLE oControl      AS System.Windows.Forms.Control      NO-UNDO.
        DEFINE VARIABLE oParent       AS System.Windows.Forms.Control      NO-UNDO.
        DEFINE VARIABLE oParentForm   AS System.Windows.Forms.Form         NO-UNDO.
        
        DEFINE VARIABLE oBefore    AS System.ComponentModel.CancelEventArgs NO-UNDO .

        oBefore = NEW System.ComponentModel.CancelEventArgs () .

        OnShowLookupDialog (oBefore) .
        
        IF oBefore:Cancel THEN 
            RETURN .        
        
        IF THIS-OBJECT:LookupDialogClassName > "":U THEN DO ON ERROR UNDO, THROW:
            oLookupDialog = DYNAMIC-NEW (THIS-OBJECT:LookupDialogClassName) (THIS-OBJECT) .
            
            /* Mike Fechner, Consultingwerk Ltd. 09.07.2012
               Handle error: DYNAMIC-NEW cannot instantiate class .... because the wrong 
               number of parameters were passed to the constructor, or the constructor 
               was not PUBLIC. (14758) */
            CATCH err AS Progress.Lang.SysError:
                IF err:GetMessageNum (1) = 14758 THEN 
                    oLookupDialog = DYNAMIC-NEW (THIS-OBJECT:LookupDialogClassName) () . 
                ELSE 
                    UNDO, THROW err .                    
            END CATCH.
        END.
        ELSE 
            oLookupDialog = NEW SmartBusinessEntityLookupDialog (THIS-OBJECT).
        
        oForm = CAST(CAST (oLookupDialog, Progress.Lang.Object), 
                     System.Windows.Forms.Form)  .
        
        IF VALID-OBJECT (oLookupDialog:LookupBrowser) THEN
            oLookupDialog:LookupBrowser:InitializeLayout:Subscribe (LookupBrowserInitializeLayoutHandler) .
            
        oLookupDialog:AfterInitializeBusinessEntityAdapter:Subscribe (AfterInitializeBusinessEntityAdapterHandler) .
        
        ASSIGN oLookupDialog:AdapterType                      = THIS-OBJECT:AdapterType 
               oLookupDialog:LookupSmartBusinessEntityAdapter = THIS-OBJECT:SmartBusinessEntityAdapter 
               .
        
        oLookupDialog:InitializeDataAdapter() .

        oLookupDialog:SetText (THIS-OBJECT:LookupDialogTitle) .

        ASSIGN 
            oLookupDialog:AppServerPartition          = THIS-OBJECT:AppServerPartition
            oLookupDialog:LookupBrowserExcludeFields  = THIS-OBJECT:LookupBrowserExcludeFields
            oLookupDialog:LookupBrowserFields         = THIS-OBJECT:LookupBrowserFields
            oLookupDialog:LookupDialogQueryString     = SUBSTITUTE(THIS-OBJECT:LookupDialogQueryString, 
                                                                   UNBOX (THIS-OBJECT:Value))
            oLookupDialog:LookupDialogQuerySort       = THIS-OBJECT:LookupDialogQuerySort
            oLookupDialog:LookupDialogFilterFields    = THIS-OBJECT:LookupDialogFilterFields
            oLookupDialog:LookupDialogFilterOperators = THIS-OBJECT:LookupDialogFilterOperators                                                               
            oLookupDialog:LookupEntityName            = THIS-OBJECT:LookupEntityName
            oLookupDialog:LookupEntityTable           = THIS-OBJECT:LookupEntityTable
            oLookupDialog:LookupEntityView            = THIS-OBJECT:LookupEntityView
            oLookupDialog:LookupKeyField              = THIS-OBJECT:LookupKeyField
            .
        
        IF TYPE-OF (oLookupDialog, SmartBusinessEntityLookupDialog) THEN 
            CAST (oLookupDialog, SmartBusinessEntityLookupDialog):LookupDialogOpenQuery = THIS-OBJECT:LookupDialogOpenQuery . 
        
        THIS-OBJECT:OnBeforeLookupDialogShown (NEW BeforeLookupDialogShownEventArgs (oLookupDialog)) .    
        
        oParentForm = THIS-OBJECT:FindForm () .
        
        oForm:FormClosed:Subscribe (LookupDialogFormClosedHandler) .
        
        IF VALID-OBJECT (oParentForm) THEN 
            WAIT-FOR oForm:ShowDialog (oParentForm) SET oResult.
        ELSE 
            WAIT-FOR oForm:ShowDialog () SET oResult.

        IF EnumHelper:AreEqual (oResult, System.Windows.Forms.DialogResult:Ok) AND VALID-OBJECT (oLookupDialogValueDictionary) THEN DO:
            THIS-OBJECT:Text = UNBOX (oLookupDialogValueDictionary[THIS-OBJECT:LookupKeyField]) .

            IF THIS-OBJECT:LookupKeyValueColumn > "":U THEN 
                THIS-OBJECT:LookupKeyValue = UNBOX (oLookupDialogValueDictionary[THIS-OBJECT:LookupKeyValueColumn]) .

            oParent = THIS-OBJECT:Parent .    

            ParentLookup: DO WHILE VALID-OBJECT(oParent):
                IF TYPE-OF (oParent, SmartViewerControl) THEN 
                    LEAVE ParentLookup .    
                    
                oParent = oParent:Parent .                
            END.

            IF VALID-OBJECT(oParent) THEN 
                DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupFields):
                    ASSIGN oControl = CAST(oParent, SmartViewerControl):GetViewerField (ENTRY(i, THIS-OBJECT:LookupControls)) .           
                    
                    IF VALID-OBJECT(oControl) THEN DO:
                        IF TYPE-OF (oControl, UltraDateTimeEditor) THEN 
                            CAST (oControl, UltraDateTimeEditor):DateTime = DataTypeHelper:ToDateTime (UNBOX (oLookupDialogValueDictionary[ENTRY(i, THIS-OBJECT:LookupFields)])) .
                        ELSE 
                            oControl:Text = UNBOX (oLookupDialogValueDictionary[ENTRY(i, THIS-OBJECT:LookupFields)]) .

                        IF TYPE-OF (oControl, SmartBusinessEntityLookup) THEN 
                            CAST (oControl, SmartBusinessEntityLookup):PerformLookup () . 
                    END.  
                END.
            ELSE 
                DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupFields):
                    IF THIS-OBJECT:Parent:Controls:ContainsKey(ENTRY(i, THIS-OBJECT:LookupControls)) THEN DO:
                        oControl = THIS-OBJECT:Parent:Controls[ENTRY(i, THIS-OBJECT:LookupControls)] . 

                        IF TYPE-OF (oControl, UltraDateTimeEditor) THEN 
                            CAST (oControl, UltraDateTimeEditor):DateTime = DataTypeHelper:ToDateTime (UNBOX (oLookupDialogValueDictionary[ENTRY(i, THIS-OBJECT:LookupFields)])) .
                        ELSE 
                            oControl:Text = UNBOX (oLookupDialogValueDictionary[ENTRY(i, THIS-OBJECT:LookupFields)]) .  
                                    
                        IF THIS-OBJECT:LookupCascade AND THIS-OBJECT:LookupCascade AND TYPE-OF (oControl, SmartBusinessEntityLookup) THEN 
                            CAST (oControl, SmartBusinessEntityLookup):PerformLookup () . 
                    END . 
                END.                    
        END.

        IF THIS-OBJECT:LookupAdditionalFields > "":U THEN 
            THIS-OBJECT:PopulateAdditionalFieldDictionary (oLookupDialog) .

        THIS-OBJECT:OnLookupCompleted (System.EventArgs:Empty) .
        THIS-OBJECT:SelectAll () .        

        FINALLY:
            IF VALID-OBJECT (oLookupDialog) THEN DO:
                IF VALID-OBJECT (oLookupDialog:LookupBrowser) THEN
                    oLookupDialog:LookupBrowser:InitializeLayout:Unsubscribe (LookupBrowserInitializeLayoutHandler) . 
                
                oLookupDialog:AfterInitializeBusinessEntityAdapter:Unsubscribe (AfterInitializeBusinessEntityAdapterHandler) .
            END.

            IF VALID-OBJECT (oForm) THEN 
                oForm:FormClosed:Unsubscribe (LookupDialogFormClosedHandler) .

            GarbageCollectorHelper:DeleteObject (oLookupDialog) .
            GarbageCollectorHelper:DeleteObject (oLookupDialogValueDictionary) . 
        END FINALLY.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: This Method returns a Registry Key where the Lookup size and position
                 is stored for each Lookup.
        Notes:   The Name of the Key is individual buildable.
        @return The registry key used by the Lookup
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER GetLookupRegistryKey ():
        
        RETURN SUBSTITUTE ("&1-&2-&3":U, 
                           LookupEntityName, 
                           LookupEntityTable, 
                           LookupEntityView).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shuts down (deletes) the SmartBusinessEntityAdapter instance                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShutdownBusinessEntityAdapter ():
        
        /* Marko Rüterbories, Consultingwerk Ltd. 22.04.2010
           Only delete the Object if it exists. This would cause an error to be 
           thrown if man cancels the PerformLookup by setting Cancel = TRUE inside 
           of the OnBeforePerformLookup EventHandler. */        
        IF VALID-OBJECT (THIS-OBJECT:SmartBusinessEntityAdapter) THEN 
            DELETE OBJECT THIS-OBJECT:SmartBusinessEntityAdapter . 

    END METHOD.

END CLASS.
