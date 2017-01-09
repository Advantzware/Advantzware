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
    File        : SmartDataObjectLookup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 24 23:01:03 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH . 
USING Consultingwerk.SmartComponents.Support.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.* FROM ASSEMBLY .
USING Progress.Lang.* FROM ASSEMBLY .
USING Progress.Util.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.SmartDataObjectLookup 
    INHERITS SmartLookup: 

    /*------------------------------------------------------------------------------
        Purpose: Name of the Type of SmartDataObjectAdapter used to lookup 
                 values from the backend.
        Notes:   Modified from a child class only   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AdapterType AS CHARACTER NO-UNDO
        INIT "Consultingwerk.SmartComponents.Implementation.SmartDataObjectAdapter":U 
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Fields that allow to Filter on the Lookup Browser Dialog
        Notes:   Comma-Delimited, first entry is default                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LookupDialogFilterFields AS CHARACTER NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Operators for the LookupDialogFilterFields 
        Notes:   Comma-Delimited, first entry is default                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LookupDialogFilterOperators AS CHARACTER NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Name of the SmartDataObject for the Lookup.
                 This Property is set by the Developer during DesignTime.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupSmartDataObjectName AS CHARACTER NO-UNDO 
    GET.
    SET.    

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartDataObjectLookup control                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDataObjectLookup ():
        SUPER ().
    
        IF THIS-OBJECT:DesignTime THEN     
            THIS-OBJECT:SetDesignerProperties () .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Selects a SmartDataObject during design time                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DesignerOpenSmartDataObject ():
        
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lOk       AS LOGICAL   NO-UNDO.
        
        IF NOT THIS-OBJECT:DesignTime THEN 
            RETURN .        
        
        ASSIGN 
            cFileName = THIS-OBJECT:LookupSmartDataObjectName 
            lOk       = SmartDataObjectSupport:FileOpenSmartDataObject (INPUT-OUTPUT cFileName).

        IF NOT lOk THEN RETURN.              
        
        IF cFileName > "":U THEN          
            ASSIGN THIS-OBJECT:LookupSmartDataObjectName = cFileName .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs                                                                       
        Notes:                            
        @param pcVerbText The text indicating the verb that was clicked                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID OnVerbClicked (pcVerbText AS CHARACTER):
            
        SUPER:OnVerbClicked(INPUT pcVerbText).

        CASE pcVerbText:
            WHEN "Open SmartDataObject (SDO)":U THEN 
                DesignerOpenSmartDataObject() .             
        END CASE . 

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
        Purpose: Performs lookup when user presses a key in the lookup field                                                                       
        Notes:
        @param plLostFocus Logical value indicating if the Lookup is made because the focus is lost                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD OVERRIDE PROTECTED VOID PerformLookup (plLostFocus AS LOGICAL):        

        DEFINE VARIABLE oAdapter   AS SmartDataObjectAdapter                NO-UNDO . 
        DEFINE VARIABLE hBuffer    AS HANDLE                                NO-UNDO .
        DEFINE VARIABLE lAvailable AS LOGICAL                               NO-UNDO .
        DEFINE VARIABLE i          AS INTEGER                               NO-UNDO .
        DEFINE VARIABLE oControl   AS System.Windows.Forms.Control          NO-UNDO .
        DEFINE VARIABLE oParent    AS System.Windows.Forms.Control          NO-UNDO .
        DEFINE VARIABLE oBefore    AS System.ComponentModel.CancelEventArgs NO-UNDO .

        oBefore = NEW System.ComponentModel.CancelEventArgs () .

        OnBeforePerformLookup (oBefore) .
        
        IF oBefore:Cancel THEN 
            RETURN . 

        IF THIS-OBJECT:AdapterType > "":U THEN 
            oAdapter = DYNAMIC-NEW (THIS-OBJECT:AdapterType) () .
        ELSE 
            oAdapter = NEW SmartDataObjectAdapter () .
        
        ASSIGN 
            oAdapter:BatchSize           = 1 
            oAdapter:SmartDataObjectName = THIS-OBJECT:LookupSmartDataObjectName 
            .

        oAdapter:InitializeSmartDataObject() .
        oAdapter:AssignQuerySelection (THIS-OBJECT:LookupKeyField, UNBOX (THIS-OBJECT:Value), "EQ") .
                                            
        oAdapter:RetrieveData () .                                            

        ASSIGN 
            hBuffer    = oAdapter:RowObject 
            lAvailable = hBuffer:AVAILABLE 
            oParent    = THIS-OBJECT:Parent .
        
        ParentLookup: DO WHILE VALID-OBJECT(oParent):
            IF TYPE-OF (oParent, SmartViewerControl) THEN 
                LEAVE ParentLookup .    
                
            oParent = oParent:Parent .                
        END.

        IF VALID-OBJECT(oParent) THEN 
        DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupFields):
            ASSIGN 
                oControl = CAST(oParent, SmartViewerControl):GetViewerField (ENTRY(i, THIS-OBJECT:LookupControls)) .           
                
            /* Mike Fechner, Consultingwerk Ltd. 24.02.2010
               When Nulling an UltraNumericEditor we cannot set Text = "", we need to 
               set the Value property to 0 or ? */
                
            IF VALID-OBJECT(oControl) THEN 
            DO:
                IF lAvailable THEN 
                DO:
                    oControl:Text = oAdapter:GetFieldValues (ENTRY(i, THIS-OBJECT:LookupFields)) .
                END. 
                ELSE 
                DO:
                    IF TYPE-OF (oControl, UltraNumericEditor) THEN 
                    DO:
                        IF CAST (oControl, UltraNumericEditor):Nullable THEN  
                            CAST (oControl, UltraNumericEditor):Value = ? . 
                        ELSE  
                            CAST (oControl, UltraNumericEditor):Value = BOX (0) .
                    END.    
                    ELSE 
                        oControl:Text = "":U .
                END.                       
            END.                      
        END.
        ELSE 
        DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupFields):
            IF THIS-OBJECT:Parent:Controls:ContainsKey (ENTRY(i, THIS-OBJECT:LookupControls)) THEN 
            DO:
                ASSIGN 
                    oControl = THIS-OBJECT:Parent:Controls[ENTRY(i, THIS-OBJECT:LookupControls)] . 
                    
                IF lAvailable THEN 
                DO:
                    oControl:Text = oAdapter:GetFieldValues (ENTRY(i, THIS-OBJECT:LookupFields)) .
                END. 
                ELSE 
                DO:
                    IF TYPE-OF (oControl, UltraNumericEditor) THEN 
                    DO:
                        IF CAST (oControl, UltraNumericEditor):Nullable THEN  
                            CAST (oControl, UltraNumericEditor):Value = ? . 
                        ELSE  
                            CAST (oControl, UltraNumericEditor):Value = BOX (0) .
                    END.    
                    ELSE 
                        oControl:Text = "":U .
                END.                       
            END. 
        END.                    
    
        IF lAvailable THEN
            THIS-OBJECT:Value = oAdapter:GetFieldValues (THIS-OBJECT:LookupKeyField) .                 
        ELSE 
        DO:
            IF plLostFocus THEN 
                THIS-OBJECT:Value = ? .
            THIS-OBJECT:LookupClearPending = TRUE . 
        END.
    
        OnAfterPerformLookup (System.EventArgs:Empty) .
    
        FINALLY:
            /* Marko Rüterbories, Consultingwerk Ltd. 22.04.2010
               Only delete the Object if it exists. This would cause an error to be 
               thrown if man cancels the PerformLookup by setting Cancel = TRUE inside 
               of the OnBeforePerformLookup EventHandler. */
            IF VALID-OBJECT (oAdapter) THEN 
                DELETE OBJECT oAdapter .
        END FINALLY .                   

    END METHOD.

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
    METHOD OVERRIDE PROTECTED VOID ShowLookupDialog (  ):
        
        DEFINE VARIABLE oLookupDialog AS SmartDataObjectLookupDialog           NO-UNDO.
        DEFINE VARIABLE oResult       AS System.Windows.Forms.DialogResult     NO-UNDO.
        DEFINE VARIABLE i             AS INTEGER                               NO-UNDO.
        DEFINE VARIABLE oControl      AS System.Windows.Forms.Control          NO-UNDO.
        DEFINE VARIABLE oParent       AS System.Windows.Forms.Control          NO-UNDO.
        DEFINE VARIABLE oParentForm   AS System.Windows.Forms.Form             NO-UNDO.
        
        DEFINE VARIABLE oBefore       AS System.ComponentModel.CancelEventArgs NO-UNDO .

        oBefore = NEW System.ComponentModel.CancelEventArgs () .

        OnShowLookupDialog (oBefore) .
        
        IF oBefore:Cancel THEN 
            RETURN .        
        
        oLookupDialog = NEW SmartDataObjectLookupDialog (THIS-OBJECT).
        
        oLookupDialog:LookupBrowser:InitializeLayout:Subscribe (LookupBrowserInitializeLayoutHandler) .
        
        oLookupDialog:AdapterType = THIS-OBJECT:AdapterType .
        
        oLookupDialog:InitializeDataAdapter() .
        
        ASSIGN 
            oLookupDialog:Text                        = THIS-OBJECT:LookupDialogTitle
            oLookupDialog:LookupBrowserFields         = THIS-OBJECT:LookupBrowserFields
            oLookupDialog:LookupDialogFilterFields    = THIS-OBJECT:LookupDialogFilterFields
            oLookupDialog:LookupDialogFilterOperators = THIS-OBJECT:LookupDialogFilterOperators                                                               
            oLookupDialog:LookupSmartDataObjectName   = THIS-OBJECT:LookupSmartDataObjectName
            oLookupDialog:LookupKeyField              = THIS-OBJECT:LookupKeyField
            .
        
        oParentForm = THIS-OBJECT:FindForm () .
        
        IF VALID-OBJECT (oParentForm) THEN 
            WAIT-FOR oLookupDialog:ShowDialog (oParentForm) SET oResult.
        ELSE 
            WAIT-FOR oLookupDialog:ShowDialog () SET oResult.

        IF EnumHelper:AreEqual (oResult, System.Windows.Forms.DialogResult:Ok) THEN 
        DO:
            THIS-OBJECT:TEXT = oLookupDialog:GetLookupFieldValue (THIS-OBJECT:LookupKeyField).

            oParent = THIS-OBJECT:Parent .    

            ParentLookup: DO WHILE VALID-OBJECT(oParent):
                IF TYPE-OF (oParent, SmartViewerControl) THEN 
                    LEAVE ParentLookup .    
                    
                oParent = oParent:Parent .                
            END.

            IF VALID-OBJECT(oParent) THEN 
            DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupFields):
                ASSIGN 
                    oControl = CAST(oParent, SmartViewerControl):GetViewerField (ENTRY(i, THIS-OBJECT:LookupControls)) .           
                    
                IF VALID-OBJECT(oControl) THEN 
                    oControl:Text = oLookupDialog:GetLookupFieldValue (ENTRY(i, THIS-OBJECT:LookupFields)) .  
            END.
            ELSE 
            DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupFields):
                IF THIS-OBJECT:Parent:Controls:ContainsKey(ENTRY(i, THIS-OBJECT:LookupControls)) THEN
                    THIS-OBJECT:Parent:Controls[ENTRY(i, THIS-OBJECT:LookupControls)]:Text =
                        oLookupDialog:GetLookupFieldValue (ENTRY(i, THIS-OBJECT:LookupFields)) .  
            END.                    
        END.

        THIS-OBJECT:OnLookupCompleted (System.EventArgs:Empty) .
        THIS-OBJECT:SelectAll () .        

        FINALLY:
            IF VALID-OBJECT (oLookupDialog) THEN 
                oLookupDialog:LookupBrowser:InitializeLayout:Unsubscribe (LookupBrowserInitializeLayoutHandler) . 
            
            DELETE OBJECT oLookupDialog.
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets Properties to customize Visual Designer behaviour                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        THIS-OBJECT:DesignerVerbs    = TRIM (THIS-OBJECT:DesignerVerbs + ",Open SmartDataObject (SDO)":U, ",") .

    END METHOD.    

    /*------------------------------------------------------------------------------
        Purpose: This Method returns a Registry Key where the Lookup size and position
                 is stored for each Lookup.
        Notes:   The Name of the Key is individual buildable.
        @return The registry key to use for the lookup
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER GetLookupRegistryKey ():
        
        RETURN SUBSTITUTE ("&1-&2":U, 
                           THIS-OBJECT:LookupSmartDataObjectName, 
                           THIS-OBJECT:LookupKeyField).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartDataObjectLookup class                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartDataObjectLookup ():

    END DESTRUCTOR.

END CLASS.