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
    File        : SmartComponentLibraryCustomizer
    Purpose     : This class is an entry point for customization and 
                  translation of strings used for messages or as f.e.
                  the caption of a toolbars button. To enable customization
                  all strings are represented as static properties and may
                  be overridden in f.e. an initialization procedure. 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 17 10:46:57 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Resources.SmartComponentLibraryCustomizer: 
        
    /*------------------------------------------------------------------------------
        Purpose: Delete prompf message for the SmartViewerControl class 
        Notes:   When this property is set, the delete prompt will be shown before 
                 deleting a record in the SmartViewerControl. 
                 There is no default value to this message. This message may be 
                 populated from the SmartViewerControl DeletePrompt translation
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SmartUpdatableBrowser_DeletePrompt AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY SmartUpdatableBrowser_SaveRecordPromt_On_RowDeactivate AS CHARACTER 
       INITIAL "Current values have not been saved.~n~nDo you wish to save current values before you select another row?" NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC STATIC PROPERTY SmartUpdatableBrowser_UnableToEnableFields_NoActiveRow AS CHARACTER 
       INITIAL 'Unable to enable fields for add/update. There is no active row.' NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Delete prompf message for the SmartViewerControl class 
        Notes:   When this property is set, the delete prompt will be shown before 
                 deleting a record in the SmartViewerControl. 
                 There is no default value to this message. This message may be 
                 populated from the SmartViewerControl DeletePrompt translation
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC STATIC PROPERTY SmartViewerControl_DeletePrompt AS CHARACTER NO-UNDO 
	GET.
	SET. 

    DEFINE PUBLIC STATIC PROPERTY SmartViewerControl_UpdateActiveWhileFormClosing AS CHARACTER 
        INITIAL "Current values have not been saved.~n~nDo you wish to save current values before you close the window?" NO-UNDO 
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartViewerControl_UpdateActiveWhileFormClosing_Title AS CHARACTER 
        INITIAL "Close Window" NO-UNDO 
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartViewerControl_UpdateActiveWhileFormDeactivate AS CHARACTER 
        INITIAL "Current values have not been saved.~n~nDo you wish to save current values?" NO-UNDO 
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartViewerControl_UnableToCreateRecord AS CHARACTER 
        INITIAL "Unable to create a new record." NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartViewerControl_DisplayError_Title AS CHARACTER 
        INITIAL "Error" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartBindingSource_AddControlsForChosenFieldsCatchBlock_Title AS CHARACTER 
        INITIAL "SmartComponent Library - SmartBindingSource" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartBindingSource_DesignerAddFieldsCatchBlock_Title AS CHARACTER 
        INITIAL "SmartComponent Library - SmartBindingSource" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartBindingSource_DesignerClearSchemaCatchBlock_Title AS CHARACTER 
        INITIAL "SmartComponent Library - SmartBindingSource" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartBindingSource_DesignerImportSchemaCatchBlock_Title AS CHARACTER 
        INITIAL "SmartComponent Library - SmartBindingSource" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartBusinessEntityAdapter_RetrieveDataError AS CHARACTER 
        INITIAL "Error during RetrieveData:" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartBusinessEntityAdapter_CommitRejectWithAutoCommit AS CHARACTER 
        INITIAL "Unable to commit or reject transaction when AutoCommit is turned on." NO-UNDO 
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartBusinessEntityAdapter_DeleteNoRecordError AS CHARACTER 
        INITIAL "Unable to delete - no row is available." NO-UNDO 
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartBusinessEntityAdapter_PromptCommitTransactionBeforeRetrieveData AS CHARACTER 
        INITIAL "Commit pending changes before retrieving data?" NO-UNDO 
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartBusinessEntityAdapter_UnableToCommitTransaction AS CHARACTER 
        INITIAL "Unable to commit transaction." NO-UNDO 
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartBusinessEntityAdapter_UnableToRejectTransaction AS CHARACTER 
        INITIAL "Unable to reject transaction." NO-UNDO 
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartBusinessEntityBindingSource_DesignerImportSchemaNoValidSchema AS CHARACTER 
        INITIAL "Backend returned no valid schema information. Cancelling." NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartBusinessEntityBindingSource_DesignerImportSchemaNewSchemaImported AS CHARACTER 
        INITIAL "New Schema imported into" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartBusinessEntityBindingSource_DesignerImportSchema_Title AS CHARACTER 
        INITIAL "SmartBusinessEntityBindingSource" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartDataObjectAdapter_UpdateRowSysError AS CHARACTER 
        INITIAL "Progress runtime" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartDataObjectBindingSource_DesignerImportSchemaUnableToLaunchSDO AS CHARACTER 
        INITIAL "Unable to launch SmartDataObject!" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartDataObjectBindingSource_DesignerImportSchemaSuccess AS CHARACTER 
        INITIAL "RowObject schema of SmartDataObject &1 imported." NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartDataObjectBindingSource_DesignerImportSchemaSuccess_Title AS CHARACTER 
        INITIAL "SmartComponent Library" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartGenericLookup_ShowLookupDialogNumberOfControlsAndFieldsNotEqual_Part1 AS CHARACTER 
        INITIAL "The number of LookupControls is different than the number of LookupFields." NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartGenericLookup_ShowLookupDialogNumberOfControlsAndFieldsNotEqual_Part2 AS CHARACTER 
        INITIAL "Please remember to fill those properties in pairs." NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartGenericLookup_ShowLookupDialogNoSmartDataSource AS CHARACTER 
        INITIAL "There is currently no valid SmartDataSource defined." NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_AddNavigationTools AS CHARACTER 
        INITIAL "Adding default navigation Tools and Toolbar to the design time instance." NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_AddNavigationTools_Title AS CHARACTER 
        INITIAL "SmartComponent Library" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_AddTableIOTools AS CHARACTER 
        INITIAL "Adding default TableIO Tools and Toolbar to the design time instance." NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_AddTableIOTools_Title AS CHARACTER 
        INITIAL "SmartComponent Library" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolBar_Text AS CHARACTER 
        INITIAL "Navigation Toolbar" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationFirst_ButtonCaption AS CHARACTER 
        INITIAL "First record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationFirst_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "First" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationFirst_ButtonToolTipText AS CHARACTER 
        INITIAL "First record" NO-UNDO
    GET.
    SET.
                                                     
    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationPrev_ButtonCaption AS CHARACTER 
        INITIAL "Previous record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationPrev_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "Previous" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationPrev_ButtonToolTipText AS CHARACTER 
        INITIAL "Previous record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationNext_ButtonCaption AS CHARACTER 
        INITIAL "Next record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationNext_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "Next" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationNext_ButtonToolTipText AS CHARACTER 
        INITIAL "Next record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationLast_ButtonCaption AS CHARACTER 
        INITIAL "Last record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationLast_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "Last" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_NavigationToolNavigationLast_ButtonToolTipText AS CHARACTER 
        INITIAL "Last record" NO-UNDO
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolBar_Text AS CHARACTER 
        INITIAL "TableIO Toolbar" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOAdd_ButtonCaption AS CHARACTER 
        INITIAL "Add record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOAdd_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "Add" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOAdd_ButtonToolTipText AS CHARACTER 
        INITIAL "Add record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOCopy_ButtonCaption AS CHARACTER 
        INITIAL "Copy record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOCopy_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "Copy" NO-UNDO
    GET.
    SET.
                                                     
    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOCopy_ButtonToolTipText AS CHARACTER 
        INITIAL "Copy record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOUpdate_ButtonCaption AS CHARACTER 
        INITIAL "Update record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOUpdate_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "Update" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOUpdate_ButtonToolTipText AS CHARACTER 
        INITIAL "Update record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOSave_ButtonCaption AS CHARACTER 
        INITIAL "Save changes" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOSave_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "Save" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOSave_ButtonToolTipText AS CHARACTER 
        INITIAL "Save changes" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOCancel_ButtonCaption AS CHARACTER 
        INITIAL "Cancel update" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOCancel_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "Cancel" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIOCancel_ButtonToolTipText AS CHARACTER 
        INITIAL "Cancel update" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIODelete_ButtonCaption AS CHARACTER 
        INITIAL "Delete record" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIODelete_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "Delete" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TableIOToolTableIODelete_ButtonToolTipText AS CHARACTER 
        INITIAL "Delete record" NO-UNDO
    GET.
    SET.
        
    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TransactionToolBar_Text AS CHARACTER 
        INITIAL "Transaction Toolbar" NO-UNDO
    GET.
    SET.
        
    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TransactionToolTransactionCommit_ButtonCaption AS CHARACTER 
        INITIAL "Commit" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TransactionToolTransactionCommit_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "Commit" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TransactionToolTransactionCommit_ButtonToolTipText AS CHARACTER 
        INITIAL "Commit transaction" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TransactionToolTransactionUndo_ButtonCaption AS CHARACTER 
        INITIAL "Revert" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TransactionToolTransactionUndo_ButtonCustomizerCaption AS CHARACTER 
        INITIAL "Revert" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_TransactionToolTransactionUndo_ButtonToolTipText AS CHARACTER 
        INITIAL "Revert transaction" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_Navigation_GroupCaption AS CHARACTER 
        INITIAL "Navigation" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_Transaction_GroupCaption AS CHARACTER 
        INITIAL "Transaction" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartToolbarController_Maintenance_GroupCaption AS CHARACTER 
        INITIAL "Maintenance" NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC STATIC PROPERTY SmartWindowForm_TransactionPendingWhileFormClosing AS CHARACTER 
        INITIAL "Pending transactions have not been committed.~n~nDo you wish to commit those transactions before you close the window?" NO-UNDO 
    GET.
    SET.
    
    DEFINE PUBLIC STATIC PROPERTY SmartWindowForm_TransactionPendingWhileFormClosing_Title AS CHARACTER 
        INITIAL "Close Window" NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                   
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE SmartComponentLibraryCustomizer ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a translated string
        Notes:   
        @param pcScope The translation scope
        @param pcKey The translation key
        @param pcDefault The default text - when no translation is available
        @return The Translated text
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED STATIC CHARACTER GetTranslation (pcScope AS CHARACTER, 
	                                                  pcKey AS CHARACTER,
	                                                  pcDefault AS CHARACTER):
		
        DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
        
        cText = {Consultingwerk/translate.i &scope=pcScope &key=pcKey} .
        
        IF cText > "":U THEN 
            RETURN cText . 
        ELSE 
            RETURN pcDefault .  

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Message Form strings from the TranslationProvider
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID InitializeFromTranslationProvider ():
	
        SmartUpdatableBrowser_DeletePrompt = GetTranslation ("SmartUpdatableBrowser":U, "DeletePrompt":U, '':U) .
        SmartUpdatableBrowser_SaveRecordPromt_On_RowDeactivate = GetTranslation ("SmartUpdatableBrowser":U, "SaveRecordPromt_On_RowDeactivate":U, "Current values have not been saved.~n~nDo you wish to save current values before you select another row?") .
        SmartUpdatableBrowser_UnableToEnableFields_NoActiveRow = GetTranslation ("SmartUpdatableBrowser":U, "UnableToEnableFields_NoActiveRow":U, 'Unable to enable fields for add/update. There is no active row.') .
        SmartViewerControl_UpdateActiveWhileFormClosing = GetTranslation ("SmartViewerControl":U, "UpdateActiveWhileFormClosing":U, "Current values have not been saved.~n~nDo you wish to save current values before you close the window?") .
        SmartViewerControl_UpdateActiveWhileFormClosing_Title = GetTranslation ("SmartViewerControl":U, "UpdateActiveWhileFormClosing_Title":U, "Close Window") .
        SmartViewerControl_UnableToCreateRecord = GetTranslation ("SmartViewerControl":U, "UnableToCreateRecord":U, "Unable to create a new record.") .
        SmartViewerControl_DisplayError_Title = GetTranslation ("SmartViewerControl":U, "DisplayError_Title":U, "Error") .
        SmartViewerControl_DeletePrompt = GetTranslation ("SmartViewerControl":U, "DeletePrompt":U, '':U) .
        SmartBindingSource_AddControlsForChosenFieldsCatchBlock_Title = GetTranslation ("SmartBindingSource":U, "AddControlsForChosenFieldsCatchBlock_Title":U, "SmartComponent Library - SmartBindingSource") .
        SmartBindingSource_DesignerAddFieldsCatchBlock_Title = GetTranslation ("SmartBindingSource":U, "DesignerAddFieldsCatchBlock_Title":U, "SmartComponent Library - SmartBindingSource") .
        SmartBindingSource_DesignerClearSchemaCatchBlock_Title = GetTranslation ("SmartBindingSource":U, "DesignerClearSchemaCatchBlock_Title":U, "SmartComponent Library - SmartBindingSource") .
        SmartBindingSource_DesignerImportSchemaCatchBlock_Title = GetTranslation ("SmartBindingSource":U, "DesignerImportSchemaCatchBlock_Title":U, "SmartComponent Library - SmartBindingSource") .
        SmartBusinessEntityAdapter_RetrieveDataError = GetTranslation ("SmartBusinessEntityAdapter":U, "RetrieveDataError":U, "Error during RetrieveData:") .
        SmartBusinessEntityAdapter_CommitRejectWithAutoCommit = GetTranslation ("SmartBusinessEntityAdapter":U, "CommitRejectWithAutoCommit":U, "Unable to commit or reject transaction when AutoCommit is turned on.") .
        SmartBusinessEntityAdapter_DeleteNoRecordError = GetTranslation ("SmartBusinessEntityAdapter":U, "DeleteNoRecordError":U, "Unable to delete - no row is available.") .
        SmartBusinessEntityAdapter_UnableToCommitTransaction = GetTranslation ("SmartBusinessEntityAdapter":U, "UnableToCommitTransaction":U, "Unable to commit transaction.") .
        SmartBusinessEntityAdapter_UnableToRejectTransaction = GetTranslation ("SmartBusinessEntityAdapter":U, "UnableToRejectTransaction":U, "Unable to reject transaction.") .
        SmartBusinessEntityBindingSource_DesignerImportSchemaNoValidSchema = GetTranslation ("SmartBusinessEntityBindingSource":U, "DesignerImportSchemaNoValidSchema":U, "Backend returned no valid schema information. Cancelling.") .
        SmartBusinessEntityBindingSource_DesignerImportSchemaNewSchemaImported = GetTranslation ("SmartBusinessEntityBindingSource":U, "DesignerImportSchemaNewSchemaImported":U, "New Schema imported into") .
        SmartBusinessEntityBindingSource_DesignerImportSchema_Title = GetTranslation ("SmartBusinessEntityBindingSource":U, "DesignerImportSchema_Title":U, "SmartBusinessEntityBindingSource") .
        SmartDataObjectAdapter_UpdateRowSysError = GetTranslation ("SmartDataObjectAdapter":U, "UpdateRowSysError":U, "Progress runtime") .
        SmartDataObjectBindingSource_DesignerImportSchemaUnableToLaunchSDO = GetTranslation ("SmartDataObjectBindingSource":U, "DesignerImportSchemaUnableToLaunchSDO":U, "Unable to launch SmartDataObject!") .
        SmartDataObjectBindingSource_DesignerImportSchemaSuccess = GetTranslation ("SmartDataObjectBindingSource":U, "DesignerImportSchemaSuccess":U, "RowObject schema of SmartDataObject &1 imported.") .
        SmartDataObjectBindingSource_DesignerImportSchemaSuccess_Title = GetTranslation ("SmartDataObjectBindingSource":U, "DesignerImportSchemaSuccess_Title":U, "SmartComponent Library") .
        SmartGenericLookup_ShowLookupDialogNumberOfControlsAndFieldsNotEqual_Part1 = GetTranslation ("SmartGenericLookup":U, "ShowLookupDialogNumberOfControlsAndFieldsNotEqual_Part1":U, "The number of LookupControls is different than the number of LookupFields.") .
        SmartGenericLookup_ShowLookupDialogNumberOfControlsAndFieldsNotEqual_Part2 = GetTranslation ("SmartGenericLookup":U, "ShowLookupDialogNumberOfControlsAndFieldsNotEqual_Part2":U, "Please remember to fill those properties in pairs.") .
        SmartGenericLookup_ShowLookupDialogNoSmartDataSource = GetTranslation ("SmartGenericLookup":U, "ShowLookupDialogNoSmartDataSource":U, "There is currently no valid SmartDataSource defined.") .
        SmartToolbarController_AddNavigationTools = GetTranslation ("SmartToolbarController":U, "AddNavigationTools":U, "Adding default navigation Tools and Toolbar to the design time instance.") .
        SmartToolbarController_AddNavigationTools_Title = GetTranslation ("SmartToolbarController":U, "AddNavigationTools_Title":U, "SmartComponent Library") .
        SmartToolbarController_AddTableIOTools = GetTranslation ("SmartToolbarController":U, "AddTableIOTools":U, "Adding default TableIO Tools and Toolbar to the design time instance.") .
        SmartToolbarController_AddTableIOTools_Title = GetTranslation ("SmartToolbarController":U, "AddTableIOTools_Title":U, "SmartComponent Library") .
        SmartToolbarController_NavigationToolBar_Text = GetTranslation ("SmartToolbarController":U, "NavigationToolBar_Text":U, "Navigation Toolbar") .
	    SmartToolbarController_NavigationToolNavigationFirst_ButtonCaption = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationFirst_ButtonCaption":U, "First Record") . 
        SmartToolbarController_NavigationToolNavigationFirst_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationFirst_ButtonCustomizerCaption":U, "First") .
        SmartToolbarController_NavigationToolNavigationFirst_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationFirst_ButtonToolTipText":U, "First record") .
        SmartToolbarController_NavigationToolNavigationPrev_ButtonCaption = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationPrev_ButtonCaption":U, "Previous record") .
        SmartToolbarController_NavigationToolNavigationPrev_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationPrev_ButtonCustomizerCaptio":U, "Previous") .
        SmartToolbarController_NavigationToolNavigationPrev_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationPrev_ButtonToolTipText":U, "Previous record") .
        SmartToolbarController_NavigationToolNavigationNext_ButtonCaption = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationNext_ButtonCaption":U, "Next record") .
        SmartToolbarController_NavigationToolNavigationNext_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationNext_ButtonCustomizerCaption":U, "Next") .
        SmartToolbarController_NavigationToolNavigationNext_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationNext_ButtonToolTipText":U, "Next record") .
        SmartToolbarController_NavigationToolNavigationLast_ButtonCaption = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationLast_ButtonCaption":U, "Last record") .
        SmartToolbarController_NavigationToolNavigationLast_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationLast_ButtonCustomizerCaption":U, "Last") .
        SmartToolbarController_NavigationToolNavigationLast_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "NavigationToolNavigationLast_ButtonToolTipText":U, "Last record") .
        SmartToolbarController_TableIOToolBar_Text = GetTranslation ("SmartToolbarController":U, "TableIOToolBar_Text":U, "TableIO Toolbar") .
        SmartToolbarController_TableIOToolTableIOAdd_ButtonCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOAdd_ButtonCaption":U, "Add record") .
        SmartToolbarController_TableIOToolTableIOAdd_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOAdd_ButtonCustomizerCaption":U, "Add") .
        SmartToolbarController_TableIOToolTableIOAdd_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOAdd_ButtonToolTipText":U, "Add record") .
        SmartToolbarController_TableIOToolTableIOCopy_ButtonCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOCopy_ButtonCaption":U, "Copy record") .
        SmartToolbarController_TableIOToolTableIOCopy_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOCopy_ButtonCustomizerCaption":U, "Copy") .
        SmartToolbarController_TableIOToolTableIOCopy_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOCopy_ButtonToolTipText":U, "Copy record") .
        SmartToolbarController_TableIOToolTableIOUpdate_ButtonCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOUpdate_ButtonCaption":U, "Update record") .
        SmartToolbarController_TableIOToolTableIOUpdate_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOUpdate_ButtonCustomizerCaption":U, "Update") .
        SmartToolbarController_TableIOToolTableIOUpdate_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOUpdate_ButtonToolTipText":U, "Update record") .
        SmartToolbarController_TableIOToolTableIOSave_ButtonCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOSave_ButtonCaption":U, "Save changes") .
        SmartToolbarController_TableIOToolTableIOSave_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOSave_ButtonCustomizerCaption":U, "Save") .
        SmartToolbarController_TableIOToolTableIOSave_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOSave_ButtonToolTipText":U, "Save changes") .
        SmartToolbarController_TableIOToolTableIOCancel_ButtonCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOCancel_ButtonCaption":U, "Cancel update") .
        SmartToolbarController_TableIOToolTableIOCancel_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOCancel_ButtonCustomizerCaption":U, "Cancel") .
        SmartToolbarController_TableIOToolTableIOCancel_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIOCancel_ButtonToolTipText":U, "Cancel update") .
        SmartToolbarController_TableIOToolTableIODelete_ButtonCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIODelete_ButtonCaption":U, "Delete record") .
        SmartToolbarController_TableIOToolTableIODelete_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIODelete_ButtonCustomizerCaption":U, "Delete") .
        SmartToolbarController_TableIOToolTableIODelete_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "TableIOToolTableIODelete_ButtonToolTipText":U, "Delete record") .
        SmartToolbarController_TransactionToolBar_Text = GetTranslation ("SmartToolbarController":U, "TransactionToolBar_Text":U, "Transaction Toolbar") .
        SmartToolbarController_TransactionToolTransactionCommit_ButtonCaption = GetTranslation ("SmartToolbarController":U, "TransactionToolTransactionCommit_ButtonCaption":U, "Commit") .
        SmartToolbarController_TransactionToolTransactionCommit_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "TransactionToolTransactionCommit_ButtonCustomizerCaption":U, "Commit") .
        SmartToolbarController_TransactionToolTransactionCommit_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "TransactionToolTransactionCommit_ButtonToolTipText":U, "Commit transaction") .
        SmartToolbarController_TransactionToolTransactionUndo_ButtonCaption = GetTranslation ("SmartToolbarController":U, "TransactionToolTransactionUndo_ButtonCaption":U, "Revert") .
        SmartToolbarController_TransactionToolTransactionUndo_ButtonCustomizerCaption = GetTranslation ("SmartToolbarController":U, "TransactionToolTransactionUndo_ButtonCustomizerCaption":U, "Revert") .
        SmartToolbarController_TransactionToolTransactionUndo_ButtonToolTipText = GetTranslation ("SmartToolbarController":U, "TransactionToolTransactionUndo_ButtonToolTipText":U, "Revert transaction") .

        SmartToolbarController_Navigation_GroupCaption = GetTranslation ("SmartToolbarController":U, "Navigation_GroupCaption":U, "Navigation":U).
        SmartToolbarController_Transaction_GroupCaption = GetTranslation ("SmartToolbarController":U, "Transaction_GroupCaption":U, "Transaction":U).
        SmartToolbarController_Maintenance_GroupCaption = GetTranslation ("SmartToolbarController":U, "Maintenance_GroupCaption":U, "Maintenance":U).
		
        SmartWindowForm_TransactionPendingWhileFormClosing = GetTranslation ("SmartWindowForm":U, "TransactionPendingWhileFormClosing":U, "Pending transactions have not been committed.~n~nDo you wish to commit those transactions before you close the window?") .
        SmartWindowForm_TransactionPendingWhileFormClosing_Title = GetTranslation ("SmartWindowForm":U, "TransactionPendingWhileFormClosing_Title":U, "Close Window") .
		
		
	END METHOD .
        
 END CLASS.