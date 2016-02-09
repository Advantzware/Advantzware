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
    File        : SmartWindowFormController
    Purpose     : Delegate for shared functionality between SmartWindowsForm 
                  and SmartRadWindowForm and SmartRadRibbonWindowForm
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 08 20:39:21 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                  FROM PROPATH .
USING Consultingwerk.Framework.Enum.*             FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*       FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*       FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Resources.*  FROM PROPATH .
USING Consultingwerk.Util.*                       FROM PROPATH .
USING Consultingwerk.Util.Forms.*                 FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*         FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*             FROM ASSEMBLY.
USING Progress.Lang.*                             FROM PROPATH .
USING Progress.Util.*                             FROM ASSEMBLY .
USING System.Windows.Forms.*                      FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Base.SmartWindowFormController: 

    DEFINE VARIABLE oFrameworkBootstrap AS IFrameworkBootstrap NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Form that owns this controller instance
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Form AS ISmartWindowForm NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of System.Type's that map enter to tab
        Notes:   Initialized with TextEditor, UltraTextEditor (through inheritance 
                 of the EmbeddableTextEditor from TextEditor), UltraComboEditor, 
                 UltraNumericEditor and UltraDateTimeEditor when MapEnterKeyToTab  
                 is set to true
                 Additional types can be added as needed
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY MapEnterKeyControlTypeList AS "System.Collections.Generic.List<System.Type>" NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Enter Key in all SmartWindowForms should be 
                 mapped to Tab
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY MapEnterKeyToTab AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET (arg AS LOGICAL):
	    IF arg = TRUE AND NOT VALID-OBJECT (SmartWindowFormController:MapEnterKeyControlTypeList) THEN DO:
            ASSIGN SmartWindowFormController:MapEnterKeyControlTypeList = NEW "System.Collections.Generic.List<System.Type>" () . 
            
            SmartWindowFormController:MapEnterKeyControlTypeList:Add (TypeHelper:GetType ("System.Windows.Forms.TextBox":U)) .
            SmartWindowFormController:MapEnterKeyControlTypeList:Add (TypeHelper:GetType ("Infragistics.Win.UltraWinEditors.UltraTextEditor":U)) . 
            SmartWindowFormController:MapEnterKeyControlTypeList:Add (TypeHelper:GetType ("Infragistics.Win.UltraWinEditors.UltraComboEditor":U)) .
            SmartWindowFormController:MapEnterKeyControlTypeList:Add (TypeHelper:GetType ("Infragistics.Win.UltraWinEditors.UltraNumericEditor":U)) .
            SmartWindowFormController:MapEnterKeyControlTypeList:Add (TypeHelper:GetType ("Infragistics.Win.UltraWinEditors.UltraDateTimeEditor":U)) .

	    END.
	    
	    SmartWindowFormController:MapEnterKeyToTab = arg . 
	END.

	/*------------------------------------------------------------------------------
	    Purpose: Constructor for the SmartWindowFormController class
	    Notes:
        @param poSmartWindowForm The reference to the ISmartWindowForm that owns this controller
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SmartWindowFormController (poSmartWindowForm AS ISmartWindowForm):
		
		DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .
		
		SUPER ().
		
		THIS-OBJECT:Form = poSmartWindowForm . 
		
		IF SmartWindowFormController:MapEnterKeyToTab = TRUE THEN DO:

		    oForm = CAST (CAST (poSmartWindowForm, Progress.Lang.Object), System.Windows.Forms.Form) . 
		    
            oForm:KeyPreview = TRUE . 
		    oForm:KeyDown:Subscribe (KeyDownHandler) .
		END.
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activates the Links between the active control and the 
                 SmartToolbarController                                                                          
        Notes:   When the ActiveControl is a read-only Browse, we attempt to activate
                 the TableIO Link on another Data-Target of the Browsers SmartDataAdapter
                 That could be a Viewer with is linked to the same adapter      
        @param poActiveControl The Control where the Links should be activated                                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateLinks (poActiveControl AS System.Windows.Forms.Control):
        
        DEFINE VARIABLE oSmartToolbarController AS ISmartToolbarControl                                   NO-UNDO .
        DEFINE VARIABLE oDataSource             AS ISmartDataSource                                       NO-UNDO .
        DEFINE VARIABLE oDataTarget             AS ISmartDataTarget                                       NO-UNDO .
        DEFINE VARIABLE oSmartDataAdapter       AS SmartDataAdapter                                       NO-UNDO .
        DEFINE VARIABLE lActiveTableIOTarget    AS LOGICAL                                                NO-UNDO INIT FALSE .
        DEFINE VARIABLE i                       AS INTEGER                                                NO-UNDO .
        
        oSmartToolbarController = THIS-OBJECT:Form:GetToolbar() .
        
        IF NOT VALID-OBJECT (oSmartToolbarController) THEN 
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to activate links in Form &1.&2There is no Toolbar available for this Form.",
                                                  THIS-OBJECT:GetClass():TypeName,
                                                  System.Environment:NewLine), 0) .
         
        /* Activate Navigation Link to the SmartControl's DataSource */
        IF TYPE-OF (poActiveControl, ISmartDataTarget) THEN DO:
            ASSIGN oDataSource = CAST (poActiveControl, ISmartDataTarget):SmartDataSource . 
            
            IF VALID-OBJECT (oDataSource) AND TYPE-OF (oDataSource, ISmartNavigationTarget) THEN DO:
                IF CAST (oDataSource, ISmartNavigationTarget):SmartNavigationSource = oSmartToolbarController THEN 
                    oSmartToolbarController:ActivateSmartNavigationTarget (CAST (oDataSource, ISmartNavigationTarget)) .
                ELSE
                    oSmartToolbarController:ActivateSmartNavigationTarget (?) .
            END.            
            ELSE 
                oSmartToolbarController:ActivateSmartNavigationTarget (?) .
        END. 
        ELSE 
            oSmartToolbarController:ActivateSmartNavigationTarget (?) .

        /* Activate TableIO Link to the SmartControl */
        IF TYPE-OF (poActiveControl, ISmartTableIOTarget) THEN 
            IF CAST (poActiveControl, ISmartTableIOTarget):SmartTableIOSource = oSmartToolbarController THEN DO:
                oSmartToolbarController:ActivateSmartTableIOTarget(CAST (poActiveControl, ISmartTableIOTarget)) .
                lActiveTableIOTarget = TRUE .     
            END.            
        
        /* If the Control itself is not a TableIOTarget (e.g. a Read-Only Browser) try the other Data-Target's of the 
           data source */
        IF NOT lActiveTableIOTarget THEN DO:
        
            /* oDataSource was set above */
            IF VALID-OBJECT (oDataSource) AND TYPE-OF (oDataSource, SmartDataAdapter) THEN DO:
                ASSIGN oSmartDataAdapter = CAST (oDataSource, SmartDataAdapter) .
                
                datatargets: 
                DO i = 1 TO oSmartDataAdapter:SmartDataTargets:Count:
                
                    ASSIGN oDataTarget = oSmartDataAdapter:SmartDataTargets:GetItem(i) .  
                
                    IF oDataTarget = poActiveControl THEN 
                        NEXT datatargets.
                
                    IF TYPE-OF (oDataTarget, ISmartTableIOTarget) THEN 
                        IF CAST (oDataTarget, ISmartTableIOTarget):SmartTableIOSource = oSmartToolbarController THEN DO:
                            oSmartToolbarController:ActivateSmartTableIOTarget(CAST (oDataTarget, ISmartTableIOTarget)) .
                            lActiveTableIOTarget = TRUE .     
                            LEAVE datatargets .
                        END.            
                END.
            END.
            
            IF NOT lActiveTableIOTarget THEN             
                oSmartToolbarController:ActivateSmartTableIOTarget (?) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Checks for pending transactions in a SmartBusinessEntityAdpater 
                 in the Form or UserControl in the Form
        Notes:   Invoked while closing the Form, dependent on the ComponentsCollection   
        @return Logical vlaue indicating if the Form can be closed
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED LOGICAL CheckForPendingTransactions ():
		
		DEFINE VARIABLE oComponents       AS "System.Collections.Generic.List<System.ComponentModel.Component>" NO-UNDO . 
		DEFINE VARIABLE oDialogResultEnum AS DialogResultEnum                                                   NO-UNDO . 
		
		DEFINE VARIABLE lHasPendingTransaction AS LOGICAL NO-UNDO INIT FALSE .
		
		ASSIGN oComponents = NEW "System.Collections.Generic.List<System.ComponentModel.Component>" () .

        THIS-OBJECT:FilterSmartBusinessEntityAdapter (THIS-OBJECT:Form:GetAllComponents(), oComponents) .
		
        THIS-OBJECT:GetAllSmartBusinessEntityAdapterInControls (CAST (CAST (THIS-OBJECT:Form, Progress.Lang.Object), 
                                                                      System.Windows.Forms.Form):Controls, oComponents) .

        {Consultingwerk/foreach.i System.ComponentModel.Component oComponent in oComponents}
        
            IF CAST (oComponent, ISmartCommitTarget):SmartTransactionState = TransactionStateEnum:PendingChanges THEN DO:
                ASSIGN lHasPendingTransaction = TRUE . 
                LEAVE . 
            END.
        END.

        IF NOT lHasPendingTransaction THEN 
            RETURN TRUE . 

        oDialogResultEnum = THIS-OBJECT:Form:PromptForSaveOfPendingChangesOnClose () . 

        CASE oDialogResultEnum:
            WHEN DialogResultEnum:DialogResultNo THEN 
                RETURN TRUE . 
            WHEN DialogResultEnum:DialogResultYes THEN DO:                

                {Consultingwerk/foreach.i System.ComponentModel.Component oComponent in oComponents nodefine}
        
                    CAST (oComponent, ISmartCommitTarget):CommitTransaction () .      
                
                    /* Still Pending Changes? - validation error etc.*/
                    IF CAST (oComponent, ISmartCommitTarget):SmartTransactionState = TransactionStateEnum:PendingChanges THEN 
                        RETURN FALSE . 
                END.
            END.
            WHEN DialogResultEnum:DialogResultCancel THEN 
                RETURN FALSE . 
        END CASE . 

        RETURN TRUE .         

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds all SmartBusinessEntityAdpater instances (based on the ISmartCommitTarget 
                 interface) from the Source list to the Target list
        Notes:   
        @param poSourceList The source list to filter for ISmartCommitTarget instances
        @param poTargetList The target list to add instances to
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID FilterSmartBusinessEntityAdapter (poSourceList AS "System.Collections.Generic.List<System.ComponentModel.Component>",
	                                                        poTargetList AS "System.Collections.Generic.List<System.ComponentModel.Component>"):
		
		{Consultingwerk/foreach.i System.ComponentModel.Component oComponent in poSourceList}
		
		    IF TYPE-OF (oComponent, ISmartCommitTarget) THEN 
		        poTargetList:Add (oComponent) . 
		END .

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Delegate for the FormClosing event handler
	    Notes:
        @param e The FormClosingEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID FormClosingHandler (e AS FormClosingEventArgs):
		
		DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO . 
		
		oForm = CAST (CAST (THIS-OBJECT:Form, Progress.Lang.Object),
		              System.Windows.Forms.Form)  .
		
        IF NOT RecurseCheckIsOkToClose (oForm:Controls,
                                        TRUE,
                                        e:CloseReason) THEN DO:
            ASSIGN e:Cancel = TRUE.
            
            RETURN . 
        END.

        IF NOT THIS-OBJECT:CheckForPendingTransactions () THEN DO:
            ASSIGN e:Cancel = TRUE . 

            RETURN . 
        END.

        IF SmartWindowFormController:MapEnterKeyToTab = TRUE THEN 
            oForm:KeyDown:Unsubscribe (KeyDownHandler) .

        CATCH err AS Progress.Lang.Error :
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loops recursively through Controls and Child-Controls and adds all 
                 SmartBusinessEntityAdapter instances to the target list  
        Notes:   
        @param poControls The current iteration of the Controls collections
        @param poTargetList The List<Component> to add the Components to
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID GetAllSmartBusinessEntityAdapterInControls (poControls AS System.Windows.Forms.Control+ControlCollection,
	                                                                  poTargetList AS "System.Collections.Generic.List<System.ComponentModel.Component>"):
		
		DEFINE VARIABLE oSourceList AS "System.Collections.Generic.List<System.ComponentModel.Component>" NO-UNDO . 
		
		{Consultingwerk/foreach.i System.Windows.Forms.Control oControl in poControls}
		
		    IF oControl:Controls:Count > 0 THEN 
		        THIS-OBJECT:GetAllSmartBusinessEntityAdapterInControls (oControl:Controls, poTargetList) .
		
		    IF TYPE-OF (oControl, SmartUserControl) THEN DO: 
		        oSourceList = CAST (oControl, SmartUserControl):GetAllComponents () . 
		        
		        THIS-OBJECT:FilterSmartBusinessEntityAdapter (oSourceList, poTargetList) .
		    END.
	    END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a List of Data Adapter instances in the Form
        Notes:   Returns the PrimaryDataTarget (when that's a Data Adapter) and 
                 the PrimaryDataSource as well as all their DataTarget instances 
        @return The List of Data Adapter instances
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListSmartDataAdapter GetLinkedDataAdapters ():

        RETURN THIS-OBJECT:GetLinkedDataAdapters (?) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns a List of Data Adapter instances in the Form
        Notes:   Returns the PrimaryDataTarget (when that's a Data Adapter) and 
                 the PrimaryDataSource or the poStart SmartDataAdapter instance 
                 as well as all their DataTarget instances 
        @param poStart An optional reference to a SmartDataAdapter to use as the starting point
        @return The List of Data Adapter instances
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListSmartDataAdapter GetLinkedDataAdapters (poStart AS SmartDataAdapter):
        
        DEFINE VARIABLE oList    AS ListSmartDataAdapter NO-UNDO .
        DEFINE VARIABLE oAdapter AS SmartDataAdapter     NO-UNDO .
        
        oList = NEW ListSmartDataAdapter () .
        
        IF VALID-OBJECT (poStart) THEN 
            THIS-OBJECT:GetLinkedDataAdapters (poStart, oList) .
        
        ELSE DO:
            IF VALID-OBJECT (THIS-OBJECT:Form:PrimaryDataTarget) AND 
               TYPE-OF (THIS-OBJECT:Form:PrimaryDataTarget, SmartDataAdapter) THEN DO:
        
                oAdapter = CAST (THIS-OBJECT:Form:PrimaryDataTarget, SmartDataAdapter) .  
        
                THIS-OBJECT:GetLinkedDataAdapters (oAdapter, oList) .
            END.
            
            IF VALID-OBJECT (THIS-OBJECT:Form:PrimaryDataSource) AND 
               TYPE-OF (THIS-OBJECT:Form:PrimaryDataSource, SmartDataAdapter) THEN DO:
        
                oAdapter = CAST (THIS-OBJECT:Form:PrimaryDataSource, SmartDataAdapter) .  
        
                IF NOT oList:Contains (oAdapter) THEN 
                    THIS-OBJECT:GetLinkedDataAdapters (oAdapter, oList) .   
            END.
        END.
        
        RETURN oList .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Add linked SmartDataAdapter instances to a List
        Notes:   Used by GetLinkedDataAdpaters (poStart) 
        @param poStart The reference to a SmartDataAdapter to use as the starting point
        @param poList The list of SmartDataAdapters to add to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetLinkedDataAdapters (poStart AS SmartDataAdapter,
                                                 poList AS ListSmartDataAdapter):
    
        IF poList:Contains (poStart) THEN 
            RETURN . 
            
         poList:Add (poStart) .
         
         {Consultingwerk/foreachABL.i ISmartDataTarget oTarget in poStart:SmartDataTargets}
         
              IF TYPE-OF (oTarget, SmartDataAdapter) THEN DO:

                  THIS-OBJECT:GetLinkedDataAdapters (CAST (oTarget, SmartDataAdapter),
                                                     poList) .
              END.
         END.           

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Initializes the Framework Bootstrap 
        Notes:   Allows overriding in a custom Form base class
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeFrameworkBootstrap ():
        
        /* Mike Fechner, Consultingwerk Ltd. 04.05.2013
           Support for Framework bootstrap integration when the first form
           is started */
        IF NOT VALID-OBJECT (oFrameworkBootstrap) THEN 
            oFrameworkBootstrap = {Consultingwerk/get-service.i Consultingwerk.Framework.IFrameworkBootstrap
                                                                "NEW Consultingwerk.Framework.FrameworkBootstrap()"} .
        
        IF NOT oFrameworkBootstrap:IsInitialized THEN 
            oFrameworkBootstrap:Initialize () . 

    END METHOD .
        
	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the KeyDown event of the Form
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The KeyEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID KeyDownHandler (sender AS System.Object,
	                                      e AS System.Windows.Forms.KeyEventArgs):
		
        DEFINE VARIABLE oForm           AS System.Windows.Forms.Form    NO-UNDO .
        DEFINE VARIABLE oCurrentControl AS System.Windows.Forms.Control NO-UNDO . 
		
		IF EnumHelper:AreEqual (e:KeyCode, Keys:Enter) AND NOT e:Shift AND NOT e:Control THEN DO:

            oForm = CAST (CAST (THIS-OBJECT:Form, Progress.Lang.Object), System.Windows.Forms.Form) . 
	
	        oCurrentControl = FormHelper:FindActiveChildControl(oForm) . 
	
	        IF VALID-OBJECT (oCurrentControl) THEN DO: 
    	        /* Mike Fechner, Consultingwerk Ltd. 06.07.2015
                   Don't apply Enter to Tab in a Browser-Cell */
                IF TYPE-OF (oCurrentControl, Infragistics.Win.EmbeddableTextBoxWithUIPermissions) AND 
                   VALID-OBJECT (CAST (oCurrentControl, Infragistics.Win.EmbeddableTextBoxWithUIPermissions):Parent) AND 
                   TYPE-OF (CAST (oCurrentControl, Infragistics.Win.EmbeddableTextBoxWithUIPermissions):Parent, UltraGrid) THEN 
                    RETURN . 	
    	        
    	        {Consultingwerk/foreach.i System.Type oType in SmartWindowFormController:MapEnterKeyControlTypeList}
    	        
    	            IF oType:IsAssignableFrom (oCurrentControl:GetType()) THEN DO:
    
                        SendKeys:Send ("~{TAB}":U) .
                                    
                        e:SuppressKeyPress = TRUE .
                        RETURN . 
                    
                    END.
    	        END. 
            END.
		END.

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Delegate for the Load event handler
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID LoadHandler ():
		
        DEFINE VARIABLE oNavigationSource AS ISmartNavigationSource NO-UNDO . 

        IF VALID-OBJECT (THIS-OBJECT:Form:ExternalDataSource) AND 
           VALID-OBJECT (THIS-OBJECT:Form:PrimaryDataTarget) THEN 
       
           CAST(CAST(THIS-OBJECT:Form:PrimaryDataTarget, Progress.Lang.Object),
                ISmartDataTarget):SmartDataSource = THIS-OBJECT:Form:ExternalDataSource . 

        IF VALID-OBJECT (THIS-OBJECT:Form:ExternalNavigationTarget) AND 
           VALID-OBJECT (THIS-OBJECT:Form:PrimaryNavigationSource) THEN DO:

            ASSIGN oNavigationSource = CAST (CAST (THIS-OBJECT:Form:PrimaryNavigationSource, Progress.Lang.Object),
                                             ISmartNavigationSource) .

            IF NOT VALID-OBJECT (THIS-OBJECT:Form:ExternalNavigationTarget:SmartNavigationSource) THEN 
                THIS-OBJECT:Form:ExternalNavigationTarget:SmartNavigationSource = oNavigationSource .  
                
            ELSE IF TYPE-OF (THIS-OBJECT:Form:ExternalNavigationTarget, SmartDataAdapter) THEN 
                CAST (THIS-OBJECT:Form:ExternalNavigationTarget, SmartDataAdapter):AddSmartNavigationSource (oNavigationSource) .
        END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shows the Prompt to save pending transactions to the backend while 
                 closing the Form
        Notes:   
        @return The DialogResultEnum value indicating "Yes" to save changes, "No" to not save changes and "Cancel" to not close the Form
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DialogResultEnum PromptForSaveOfPendingChangesOnClose ():

        DEFINE VARIABLE oResult AS DialogResultEnum NO-UNDO .  
        
        oResult = Consultingwerk.Util.MessageFormHelper:ShowMessage
            (SmartComponentLibraryCustomizer:SmartWindowForm_TransactionPendingWhileFormClosing,
             SmartComponentLibraryCustomizer:SmartWindowForm_TransactionPendingWhileFormClosing_Title,
             "":U,
             MessageFormImages:ImageQuestion,
             MessageFormDefaults:LabelYesNoCancel,
             MessageFormDefaults:GetDialogResultYesNoCancel()) .        
                                  
        RETURN oResult .

    END METHOD .

     /*------------------------------------------------------------------------------
        Purpose: Recursivly ask all Controls implementing the ICheckOkToClose Interface
                 wheather it is ok to close the form.
        Notes:
        @param poControls The System.Windows.Forms.Control+ControlCollection Collection of Controls to iterate
        @param plShowMessage When TRUE a message is Shown explaining why it is not ok to close the parent form.
        @param poCloseReason The System.Windows.Forms.CloseReason passed from then OnFormClosing Method of the parent form giving the reason who closes the form.
        @return Logical value indicating if the Form can be closed or not.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurseCheckIsOkToClose (INPUT poControls    AS System.Windows.Forms.Control+ControlCollection,
                                                   INPUT plShowMessage AS LOGICAL, 
                                                   INPUT poCloseReason AS System.Windows.Forms.CloseReason):
        
        IF poControls:Count > 0 THEN DO:
            
            {Consultingwerk/foreach.i Control oControl in poControls}    
                IF TYPE-OF (oControl, ICheckOkToClose) THEN DO:
                    IF NOT CAST (oControl, ICheckOkToClose):IsOkToClose (plShowMessage, poCloseReason) THEN
                        RETURN FALSE.
                END.
                ELSE 
                    IF NOT THIS-OBJECT:Form:RecurseCheckIsOkToClose (oControl:Controls,
                                                                     plShowMessage,
                                                                     poCloseReason) THEN
                        RETURN FALSE.
                        
                CATCH err AS Progress.Lang.Error:
                    IF err:GetMessageNum (1) = 15580 OR err:GetMessage(1) MATCHES "* (15580)*":U THEN 
                       /* ignore */ .
                    ELSE 
                        UNDO, THROW err .   
                END CATCH.                                                     
            END.
        END.

        RETURN TRUE.

    END METHOD.
        
    /*------------------------------------------------------------------------------
        Purpose: Recursively checks all contained controls to save changes on 
                 Deactivate of the Form 
        Notes:   
        @param poControls The System.Windows.Forms.Control+ControlCollection Collection of Controls to iterate
        @param plShowMessage When TRUE a message is Shown explaining why it is not ok to close the parent form.
        @return Logical value indicating if it's o.k. to move to the Activated window
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RecurseSaveChangesOnDeactivateForm (poControls    AS System.Windows.Forms.Control+ControlCollection,
                                                              plShowMessage AS LOGICAL):
        
        IF poControls:Count > 0 THEN DO:
            
            {Consultingwerk/foreach.i Control oControl in poControls}    

                IF TYPE-OF (oControl, ISaveChangesOnDeactivateForm) THEN  DO:
                    IF CAST (oControl, ISaveChangesOnDeactivateForm):SaveChangesOnDeactivateForm (plShowMessage) = FALSE THEN 

                    RETURN FALSE . 
                END . 
                ELSE 
                    IF THIS-OBJECT:RecurseSaveChangesOnDeactivateForm (oControl:Controls,
                                                                       plShowMessage) = FALSE THEN
                        RETURN FALSE . 
            END.
        END.
        
        RETURN TRUE . 

    END METHOD.        

	/*------------------------------------------------------------------------------
	    Purpose: Removes Data links from all contained SmartDataTargets when the Form
	             is closed
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RemoveDataLinks ():
		
        DEFINE VARIABLE oList    AS ListSmartDataAdapter NO-UNDO .
        DEFINE VARIABLE oAdapter AS SmartDataAdapter     NO-UNDO .
        DEFINE VARIABLE i        AS INTEGER              NO-UNDO .

        oList = THIS-OBJECT:GetLinkedDataAdapters() .

        DO i = oList:Count TO 1 BY -1:

            ASSIGN oAdapter = oList:GetItem (i) .

            IF VALID-OBJECT (oAdapter) THEN DO:
                
                IF VALID-OBJECT (oAdapter:SmartDataSource) THEN
                    oAdapter:SmartDataSource = ? .

                IF NOT THIS-OBJECT:Form:CanFindInComponentsCollection (oAdapter) THEN 
                    DELETE OBJECT oAdapter .
            END.
        END.

	END METHOD.
        
END CLASS.
