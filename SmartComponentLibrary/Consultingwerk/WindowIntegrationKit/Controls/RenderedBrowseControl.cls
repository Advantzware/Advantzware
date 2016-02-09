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
    File        : RenderedBrowseControl
    Purpose     : Grid Control that can replace a Progress Browse Widget
    Syntax      : 
    Description : Class inheritings from the Infragistics UltraGrid
                  This class is part of the WinKit
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 16 22:51:27 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*                     FROM PROPATH .
USING Consultingwerk.Framework.*                     FROM PROPATH .
USING Consultingwerk.Framework.Collections.*         FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH . 
USING Consultingwerk.WindowIntegrationKit.*          FROM PROPATH . 
USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH . 
USING Consultingwerk.WindowIntegrationKit.Forms.*    FROM PROPATH .
USING Consultingwerk.Windows.*                       FROM PROPATH . 
USING Consultingwerk.Windows.Util.*                  FROM PROPATH .
USING Infragistics.Win.UltraWinScrollBar.*           FROM ASSEMBLY . 
USING Infragistics.Win.UltraWinGrid.*                FROM ASSEMBLY .
USING Progress.Lang.*                                FROM PROPATH .
USING System.Windows.Forms.*                         FROM ASSEMBLY . 

CLASS Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseControl 
    INHERITS UltraGrid
    IMPLEMENTS IOverlayControl
    USE-WIDGET-POOL: 
    
    DEFINE PROTECTED VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE VARIABLE hBrowse                 AS HANDLE                           NO-UNDO .
    DEFINE VARIABLE oEmbeddedWindowForm     AS IEmbeddedWindowForm              NO-UNDO .
    DEFINE VARIABLE oParent                 AS System.Windows.Forms.Control     NO-UNDO .
    DEFINE VARIABLE oWindowContainer        AS Progress.Windows.WindowContainer NO-UNDO .
    DEFINE VARIABLE hQuery                  AS HANDLE                           NO-UNDO .    
    DEFINE VARIABLE hAlternativeQuery       AS HANDLE                           NO-UNDO .
    DEFINE VARIABLE hAlternativeQueryBuffer AS HANDLE                           NO-UNDO EXTENT .    
    
    DEFINE VARIABLE lRowCreated                            AS LOGICAL NO-UNDO INIT FALSE .
    DEFINE VARIABLE lProcedureHasInitializeRowEventHandler AS LOGICAL NO-UNDO INIT FALSE .
    DEFINE VARIABLE lProcedureHasDisplayFieldsHook         AS LOGICAL NO-UNDO INIT FALSE . 
    DEFINE VARIABLE hEventHandlerProcedure                 AS HANDLE  NO-UNDO.
    /* ROW-ENTRY is surpressed from after it's raised the first time for a row until that 
       row is left (BeforeRowDeactivate) */
    DEFINE VARIABLE lSupressROW-ENTRY                      AS LOGICAL NO-UNDO INIT FALSE .  
    DEFINE VARIABLE lSupressAfterUpdateCell                AS LOGICAL NO-UNDO INIT FALSE .
    DEFINE VARIABLE lFirstTimeInitializeLayout             AS LOGICAL NO-UNDO INIT TRUE .
    
    DEFINE VARIABLE oLogMessageHandler                     AS ILogMessageHandler  NO-UNDO .

    DEFINE VARIABLE hEventHandlerProcedure2                AS HANDLE              NO-UNDO .
    DEFINE VARIABLE lFirstVisibleChanged                   AS LOGICAL             NO-UNDO INIT TRUE . 
    DEFINE VARIABLE lFormDisposedSubscribed                AS LOGICAL             NO-UNDO INIT FALSE .

    /* Mike Fechner, Consultingwerk Ltd. 03.09.2013
       Static only on 10.2B to workaround too-many-temp-table issue
       Not static on 11.x to avoid issues with the combination of static
       and hybrids on 11.2 and 11.3, OE defect OE00240707  
       http://knowledgebase.progress.com/articles/Article/000042229?popup=true */
    DEFINE 
    &IF PROVERSION BEGINS "10.2":U &THEN 
    STATIC 
    &ENDIF
        TEMP-TABLE ttBufferInfo NO-UNDO 
        FIELD RecordOwner  AS Progress.Lang.Object 
        FIELD BufferHandle AS HANDLE
        FIELD UniqueKeys   AS CHARACTER 
        FIELD UniqueFind   AS CHARACTER 
        INDEX BufferHandle IS UNIQUE RecordOwner BufferHandle .  

    /*------------------------------------------------------------------------------
        Purpose: EVent raised when the user double clicks a row or presses Enter                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC EVENT DefaultAction DELEGATE System.EventHandler .

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the user double clicks a row or presses Enter                                                                      
        Notes:     
        @param sender The reference to the object that raised the event
        @param e The FindRecordsForCalculatedFieldsEventArgs with the data for this event                                                                    
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC EVENT FindRecordsForCalculatedFields SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                       e AS FindRecordsForCalculatedFieldsEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised when an attempt is made to scroll beyond the first row
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC EVENT OffHome DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Grid is anchored to the Bottom of the Form                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY AnchoredBottom AS LOGICAL NO-UNDO INIT FALSE 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the LEAVE Event should be APPLY'd to the browse widget
                 and if the leave event should be published on Deactivate or EndUpdate
                 of the cell
        Notes:   EndUpdate is only fired, when the cell value actually has changed
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ApplyCellLeaveToBrowse AS ApplyCellLeaveToBrowseEnum NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Enter event of the RenderedBrowserControl will 
                 APPLY ENTRY to the Browse widget
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ApplyEntryOnEnter AS LOGICAL NO-UNDO INIT FALSE  
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets where Function Key events will be APPLY'd to
        Notes:   Controls if F1..F12 events on the Grid will result in an APPLY "Fn" 
                 TO the CURRENT-COLUMN or the BROWSE widget
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ApplyFunctionKeyEvents AS ApplyFunctionKeyEventsEnum NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Logical value indicating if the Grid is currently applying ENTRY
                 to the browse widget
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ApplyingEntry AS LOGICAL NO-UNDO INIT FALSE
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the RenderedBrowseControl will APPLY the ITERATION-CHANGED
                 event together with the VALUE-CHANGED Event
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ApplyIterationChanged AS LOGICAL NO-UNDO INIT FALSE 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the instance to the RenderedBrowseControl is should
                 Apply the ROW-DISPLAY Evnt to the Browser before (TRUE) or after (FALSE)
                 running the WindowDisplayFieldsIn... hook procedure
        Notes:   In programs, where the ROW-DISPLAY Trigger of a browse widget changes
                 the value of fields (e.g. a linked temp-table) or variables and expects 
                 the automatic display of those the ROW-DISPLAY Trigger is expected 
                 to execute before the WinKitDisplayFieldsIn... hook.
                 In programs, where the ROW-DISPLAY Trigger of a browse modifies the 
                 SCREEN-VALUE of certain columns ten the WinKitDisplayFieldsIn... hook
                 needs to run before
                 This property is mainly relevant when using the CustomCalculatedColumnNames
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY ApplyRowDisplayBeforeDisplayFields AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    PRIVATE SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the instance of the RenderedBrowseControl should 
                 APPLY ROW-ENTRY to the Progress Browse OnBeforeEnterEditMode
        Notes:   Typically applying ROW-ENTRY during OnBeforeEnterEditMode may cause 
                 issues with ADM1 based updatable browser
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY ApplyRowEntryOnBeforeEnterEditMode AS LOGICAL INITIAL TRUE NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the instance of the RenderedBrowseControl should 
                 APPLY ROW-ENTRY to the Progress Browse OnRowActivate
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY ApplyRowEntryOnRowActivate AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the instance of the RenderedBrowseControl should 
                 APPLY ROW-LEAVE to the Progress browse OnLeave 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ApplyRowLeaveOnLeave AS LOGICAL NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the instance of the RenderedBrowseControl should 
                 APPLY ROW-LEAVE to the Progress Browse OnBeforeRowDeactivate
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY ApplyRowLeaveOnRowDeactivate AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the BindingSource fields should be Assign-ed to the 
                 buffer during handling the OnRowDeactivate
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AssignBindingSourceOnRowDeactivate AS LOGICAL NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the bound Progress.Data.BindingSource instance                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY BindingSource AS Progress.Data.BindingSource NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the BindingSourceFieldsEnum value indicating which 
                 fields will be added to the ProBindingSource of the RenderedBrowseControl
        Notes:   With AllFields (default) all fields that are available though the 
                 browsers query will be added to the ProBindingSource, with BrowserFields 
                 only those fields that are displayed in the original Progerss Browse
                 widget will be shown, with UseList The BindingSourceFieldList property
                 will be used to determine which fields should be exposed to the 
                 GUI for .NET
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSourceFields AS BindingSourceFieldsEnum NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the List of fields that will be exposed to the 
                 ProBindingSource used by the RenderedBrowseControl
        Notes:   See BindingSourceFields property for details
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSourceFieldList AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if the instance to the RenderedBrowseControl is using Batching
        Notes:   Tied to the MaxDataGuess property of the bound Progress.Data.BindingSource
                 components
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Batching AS LOGICAL NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Return the handle of the original browse widget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BrowseHandle AS WIDGET-HANDLE NO-UNDO
    GET.
    PROTECTED SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the Procedure to use for call-backs                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY CallbackProcedure AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns a list of the browse column handles matching the 
                 ColorCodedColumnNames, to be used for the automatic processing
                 of color coding (ROW-DISPLAY Trigger)                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY ColorCodedColumnHandles AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns a list of column keys to be used for the automatic processing
                 of color coding (ROW-DISPLAY Trigger)                                                                     
        Notes:   Pass in "*" to process all columns                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY ColorCodedColumnNames AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of column keys to be used for the additional 
                 automatic processing of of calculated fields (ROW-DISPLAY Trigger)                                                                     
        Notes:   Pass in "*" to process all columns          
                 By default only "unbound" columns in the UltraGrid are handled as 
                 calcuolated columns. Use this property when regular query columns
                 for database or temp-tables are used as a placeholder for a calculated
                 field
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY CustomCalculatedColumnNames AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the DEFAULT-ACTION event has been handled by an 
                 event handled from the recend APPLY "DEFAULT-ACTION"
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DefaultActionHandled AS LOGICAL INITIAL ? NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the InitializeRow and ListChanged events should 
                 execute code to handle ABL calculated fields and value based color 
                 based on synchronizing the ROW-DISPLAY trigger                                                                     
        Notes:   Turning these features off may greatly improve the performance of 
                 the RenderedBrowseControl
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY DisableCalculatedFieldsAndColor AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the RenderedBrowseControl should disable the 
                 OutlookGroupBy even when Batching is not used 
        Notes:   By default Grids that do not use batching to allow the OutlookGroupBy
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DisableOutlookGroupBy AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets of the Browser has dynamic columns
        Notes:   Dynamic columns need to be stored and restored by the RenderedBrowseControl
                 because the browser will loose those columns when the browsers 
                 Query is replaced
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DynamicColumns AS LOGICAL NO-UNDO INIT FALSE 
    GET.
    PRIVATE SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the IColorTableConverted used by 
                 this control                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY ColorTableConverter AS Consultingwerk.Windows.IColorTableConverter NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the labels of the Visible Columns as an override to 
                 the currnetly visible browse columns
        Notes:   Used in RenderedBrowseControl:ViewHideColumns in combination with  
                 VisibleColumns  
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnLabels AS CHARACTER NO-UNDO INIT ?
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the widths of the Visible Columns as an override to 
                 the currnetly visible browse columns
        Notes:   Used in RenderedBrowseControl:ViewHideColumns in combination with  
                 VisibleColumns                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnWidths AS CHARACTER NO-UNDO INIT ?
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the current browse column                                                                     
        Notes:   Used during the START-SEARCH trigger
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY CURRENT-COLUMN AS HANDLE NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Get's and sets if the CursorKeyNavigation during update is used
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CursorKeyNavigationInEdit AS CursorKeyNavigationInEditEnum NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if the RenderedBrowseControl is still within the execution
                 of the CONSTRUCTOR 
        Notes:   Used to avoid raising the VALUE-CHANGED event too early
    ------------------------------------------------------------------------------*/
	DEFINE PROTECTED PROPERTY InConstructor AS LOGICAL INITIAL TRUE NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Key of the last activated Grid cell  
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY LastActiveCell AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the MaxDataGuess value for the RenderedBrowseControl 
                 instance, used only when batching                                                                     
        Notes:   Default value is set by WinKitControls:DefaultMaxDataGuess
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MaxDataGuess AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the RenderedBrowseControl should subscribe to the 
                 ENTRY event of the Browse Cells
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MonitorBrowseCellEntry AS LOGICAL NO-UNDO INIT FALSE 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: The number of visible leading columns locked in a browse widget.  
        Notes:   If a locked column is hidden, the next visible non-locked column in 
                 the browse will then become locked.   
                 When you use the horizontal scrollbar to scroll columns in the browse, 
                 locked columns do not move. For example, if NUM-LOCKED-COLUMNS is 3, 
                 then the three leftmost columns in the browse are locked.
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY NUM-LOCKED-COLUMNS AS INTEGER NO-UNDO 
	GET:
	    DEFINE VARIABLE i AS INTEGER NO-UNDO.
	    
	    {Consultingwerk/foreach.i UltraGridColumn oColumn in this-object:DisplayLayout:Bands[0]:Columns}
	    
	       IF NOT oColumn:Hidden AND oColumn:Header:Fixed THEN 
	           i = i + 1 . 
	    END.
	    
	    RETURN i. 
	END GET . 
	SET (arg AS INTEGER):
        DEFINE VARIABLE oColumns AS "System.Collections.Generic.Dictionary<System.Int32,UltraGridColumn>" NO-UNDO . 	
	    DEFINE VARIABLE i        AS INTEGER NO-UNDO.

        THIS-OBJECT:DisplayLayout:UseFixedHeaders = TRUE.
        
	    oColumns = NEW "System.Collections.Generic.Dictionary<System.Int32,UltraGridColumn>" () .
	     
        {Consultingwerk/foreach.i UltraGridColumn oColumn in this-object:DisplayLayout:Bands[0]:Columns}
        
           oColumns:Add (oColumn:Header:VisiblePosition + 1, oColumn) .
        END.
	    
	    DO i = 1 TO THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Count:
	        
	        oColumn = oColumns[i] .
	        
	        IF oColumn:Hidden THEN NEXT .
	          
	        IF arg > 0 THEN 
	           ASSIGN oColumn:Header:Fixed = TRUE 
	                  arg                  = arg - 1 .
	        ELSE
	           ASSIGN oColumn:Header:Fixed = FALSE .    
	    END. 
	    
	END SET . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the parameters for the   
        Notes:   Currently only supported as a holder class. The reposition methods 
                 to not yet use these values
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY RepositionedRow AS RepositionedRow NO-UNDO 
	GET.
	PROTECTED SET . 

    /*------------------------------------------------------------------------------
        Purpose: The key used as the base key when getting user settings from the 
                 ISettingsService implementation
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY SettingsKey AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if this Control should suspend APPLY'ing evens to the 
                 Progress Browse Control                                                 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY SuspendEvents AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: A comma-delimited list of unbound column names                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY UnboundColumnNames AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: A comma-delimited list of unbound column handles                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY UnboundColumnHandles AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Query handle of the browse                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY QUERY AS HANDLE NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns if grid column names are prefixed with the buffer name 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY UseFullFieldNames AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns of the ValueChanged event is currently suspended
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY ValueChangedSuspended AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the names of the browse columns that are supposed to
                 be visible as an override to the currently visible browse columns
        Notes:   See RenderedBrowseControl:ViewHideColumns                                                                       
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY VisibleColumns AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the widget handle this Control is replacing
                 on the screen                                                                      
        Notes:   Required by the Consultingwerk.WindowIntegrationKit.Controls.IOverlayControl 
                 Interface                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY WidgetHandle AS HANDLE NO-UNDO 
    GET.
    PROTECTED SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RenderedBrowseControl class                                                                      
        Notes:               
        @param poContext An instance of a Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseContext that contains the settings for this RenderedBrowseControl instance
    ------------------------------------------------------------------------------*/    
    CONSTRUCTOR PUBLIC RenderedBrowseControl (poContext AS RenderedBrowseContext):
        
        DEFINE VARIABLE phBrowse                      AS HANDLE                       NO-UNDO . 
        DEFINE VARIABLE poEmbeddedWindowForm          AS IEmbeddedWindowForm          NO-UNDO . 
        DEFINE VARIABLE poParent                      AS System.Windows.Forms.Control NO-UNDO . 
        DEFINE VARIABLE plUseBatching                 AS LOGICAL                      NO-UNDO . 
        DEFINE VARIABLE phCallBackProcedure           AS HANDLE                       NO-UNDO . 
        DEFINE VARIABLE pcColorCodingColumnNames      AS CHARACTER                    NO-UNDO . 
        DEFINE VARIABLE plAutoProcessCalculatedFields AS LOGICAL                      NO-UNDO . 
    
        DEFINE VARIABLE oBrowseCustomizer             AS IRenderedBrowseCustomizer    NO-UNDO . 
    
        SUPER().

        ASSIGN phBrowse                          = poContext:BrowseHandle
               poEmbeddedWindowForm              = poContext:EmbeddedWindowForm 
               poParent                          = poContext:Parent
               plUseBatching                     = poContext:Batching 
               phCallBackProcedure               = poContext:CallBackProcedure
               pcColorCodingColumnNames          = poContext:ColorCodingColumnNames 
               THIS-OBJECT:RepositionedRow       = NEW RepositionedRow (1, RepositionModeEnum:Conditional)
            .

        /* Mike Fechner, Consultingwerk Ltd. 16.04.2015
           WinKit-15: Allow to assign Name to this instnace for UltraCalcManager */
        IF poContext:GridName > "":U THEN 
            THIS-OBJECT:Name = poContext:GridName .  

        ASSIGN oLogMessageHandler = {Consultingwerk/get-service.i Consultingwerk.Framework.ILogMessageHandler} 
               oBrowseCustomizer  = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IRenderedBrowseCustomizer} .

        IF plUseBatching = ? THEN ASSIGN plUseBatching = FALSE.

        /* Default to true */
        IF plAutoProcessCalculatedFields = ? THEN ASSIGN plAutoProcessCalculatedFields = TRUE . 

        IF VALID-OBJECT (poContext:BindingSourceFields) THEN 
            THIS-OBJECT:BindingSourceFields = poContext:BindingSourceFields . 
        ELSE 
            poContext:BindingSourceFields = BindingSourceFieldsEnum:AllFields .

        ASSIGN THIS-OBJECT:SettingsKey                 = poContext:SettingsKey 
               THIS-OBJECT:CustomCalculatedColumnNames = poContext:CustomCalculatedColumnNames
               THIS-OBJECT:BindingSourceFieldList      = poContext:BindingSourceFieldList 
               THIS-OBJECT:MaxDataGuess                = poContext:MaxDataGuess .

        IF VALID-OBJECT (poContext:ApplyCellLeaveToBrowse) THEN 
            THIS-OBJECT:ApplyCellLeaveToBrowse = poContext:ApplyCellLeaveToBrowse . 
        ELSE
            THIS-OBJECT:ApplyCellLeaveToBrowse = ApplyCellLeaveToBrowseEnum:Never .

        IF poContext:ApplyEntryOnEnter = TRUE THEN 
            THIS-OBJECT:ApplyEntryOnEnter = TRUE .  

        IF VALID-OBJECT (poContext:ApplyFunctionKeyEvents) THEN 
            THIS-OBJECT:ApplyFunctionKeyEvents = poContext:ApplyFunctionKeyEvents .
        ELSE 
            THIS-OBJECT:ApplyFunctionKeyEvents = ApplyFunctionKeyEventsEnum:None .

        IF poContext:ApplyIterationChanged = TRUE THEN 
            THIS-OBJECT:ApplyIterationChanged = TRUE . 

        IF poContext:ApplyRowDisplayBeforeDisplayFields = TRUE THEN 
            THIS-OBJECT:ApplyRowDisplayBeforeDisplayFields = TRUE  .

        IF poContext:ApplyRowEntryOnRowActivate = TRUE THEN 
            THIS-OBJECT:ApplyRowEntryOnRowActivate = TRUE . 

        IF poContext:ApplyRowEntryOnBeforeEnterEditMode = FALSE THEN 
            THIS-OBJECT:ApplyRowEntryOnBeforeEnterEditMode = FALSE . 

        IF poContext:ApplyRowLeaveOnLeave = TRUE THEN 
            THIS-OBJECT:ApplyRowLeaveOnLeave = TRUE . 

        IF poContext:ApplyRowLeaveOnRowDeactivate = TRUE THEN 
            THIS-OBJECT:ApplyRowLeaveOnRowDeactivate = TRUE . 

        IF poContext:AssignBindingSourceOnRowDeactivate = TRUE THEN 
            THIS-OBJECT:AssignBindingSourceOnRowDeactivate = TRUE . 

        IF VALID-OBJECT (poContext:CursorKeyNavigationInEdit) THEN 
            THIS-OBJECT:CursorKeyNavigationInEdit = poContext:CursorKeyNavigationInEdit .
        ELSE 
            THIS-OBJECT:CursorKeyNavigationInEdit = CursorKeyNavigationInEditEnum:None .

        IF poContext:MonitorBrowseCellEntry = TRUE THEN 
            THIS-OBJECT:MonitorBrowseCellEntry = TRUE . 

        IF poContext:VisibleColumns > "":U THEN 
            THIS-OBJECT:VisibleColumns = poContext:VisibleColumns . 

        IF poContext:ColumnLabels > "":U THEN 
            THIS-OBJECT:ColumnLabels = poContext:ColumnLabels . 
            
        IF poContext:ColumnWidths > "":U THEN 
            THIS-OBJECT:ColumnWidths = poContext:ColumnWidths . 
    
        IF poContext:DynamicColumns = TRUE THEN 
            THIS-OBJECT:DynamicColumns = TRUE . 
    
        IF poContext:DisableOutlookGroupBy = TRUE THEN 
            THIS-OBJECT:DisableOutlookGroupBy = TRUE . 
    
        IF poContext:DisableCalculatedFieldsAndColor = TRUE THEN 
            THIS-OBJECT:DisableCalculatedFieldsAndColor = TRUE . 
    
        IF VALID-OBJECT (poContext:Dock) THEN 
            THIS-OBJECT:Dock = poContext:Dock . 
    
        IF NOT VALID-HANDLE (phBrowse) OR
           phBrowse:TYPE <> "BROWSE":U OR
           NOT VALID-OBJECT (poEmbeddedWindowForm) THEN

           UNDO, THROW NEW AppError ("RenderedBrowseControl requires a valid browser and IEmbeddedWindowForm reference."{&TRAN}, 0) .

        /* Mike Fechner, Consultingwerk Ltd. 09.08.2011
           Get the current default Color Table Converter and use it 
           If there is no default (yet), create that instance */
        ASSIGN THIS-OBJECT:ColorTableConverter = {Consultingwerk/get-service.i Consultingwerk.Windows.IColorTableConverter} .             

        IF THIS-OBJECT:DisableCalculatedFieldsAndColor = FALSE THEN 
            THIS-OBJECT:InitializeRow:Subscribe (InitializeRowHandler) .

        InitializeComponent().
        
        IF THIS-OBJECT:DisableCalculatedFieldsAndColor = TRUE THEN DO:
            THIS-OBJECT:bindingSource1:ListChanged:Unsubscribe (ListChangedHandler) .
            THIS-OBJECT:bindingSource1:ListChanged:Subscribe (ListChangedHandler2) .
        END.
        
        THIS-OBJECT:Batching = plUseBatching.
        IF THIS-OBJECT:Batching THEN 
            THIS-OBJECT:DisplayLayout:Override:HeaderClickAction = HeaderClickAction:ExternalSortMulti.
        ELSE 
            THIS-OBJECT:DisplayLayout:Override:HeaderClickAction = HeaderClickAction:Default.
            
        THIS-OBJECT:BindingSource = THIS-OBJECT:bindingSource1 .
        
        /* Mike Fechner, Consultingwerk Ltd. 20.12.2011
           On 10.205 + use the UseFullFieldNames attribute when query is joined */
        IF phBrowse:QUERY:NUM-BUFFERS > 1 AND 
           (PROVERSION > Consultingwerk.ProVersionEnum:P102B OR
           (PROVERSION = Consultingwerk.ProVersionEnum:P102B AND 
            INTEGER (SessionHelper:GetPatchLevel()) >= 5)) THEN DO:

            ReflectionHelper:SetPropertyValue (THIS-OBJECT:BindingSource,
                                               "UseFullFieldNames":U,
                                               BOX(TRUE)) .
                                                                   
            ASSIGN THIS-OBJECT:UseFullFieldNames = TRUE .                                                                    
        END.
        
        /* Mike Fechner, Consultingwerk Ltd. 08.07.2013
           Provide automatic event subscription for the FindRecordsForCalculatedFields
           event before returning from the constructor. May be required when the Query
           would otherwise have to be opened twice */
            IF VALID-HANDLE (phBrowse:INSTANTIATING-PROCEDURE) THEN 
                IF Consultingwerk.Util.ProcedureHelper:HasEntry (phBrowse:INSTANTIATING-PROCEDURE,
                                                                 "FindRecordsForDisplay":U,
                                                                 Consultingwerk.EntryTypeEnum:Procedure) THEN DO:
        
                    THIS-OBJECT:FindRecordsForCalculatedFields:Subscribe (phBrowse:INSTANTIATING-PROCEDURE,
                                                                          "FindRecordsForDisplay":U) .
                END.

        /* Mike Fechner, Consultingwerk Ltd. 14.02.2012
           The use of PreloadRows or LoadOnDemand seems to the 
           responsible for issues seen at CCE during scrolling 
           in the Grid: System.IndexOutOfRangeException: Index was outside the bounds of the array.
                           at Infragistics.Shared.SparseArray.ValidateIndex(Int32 index)
                           at Infragistics.Shared.SparseArray.GetItem(Int32 index, ICreateItemCallback createItemCallback)
                           at Infragistics.Shared.SparseArray.get_Item(Int32 index)
                           at Infragistics.Win.UltraWinGrid.RowsCollection.RowsSparseArray.GetItem(Int32 index, Boolean create, Boolean calledFromScrollCountManagerSparseArray, Boolean& newRowCreated) 
            Further investigation has shown that these issues are related to 
            using LoadOnDemand when the query only returns a small set of records (less then 200?)
            this can be cured by setting the MaxDataGuess of the BindingSource to a very small
            value such as 2. Using PreloadRows with MaxDataGuess of 200 does cause a huge performance
            degradation */
        THIS-OBJECT:DisplayLayout:LoadStyle = Infragistics.Win.UltraWinGrid.LoadStyle:PreloadRows /*LoadOnDemand | PreloadRows*/ .

        THIS-OBJECT:CallbackProcedure = phCallBackProcedure .
        
        THIS-OBJECT:QUERY = phBrowse:QUERY.

        THIS-OBJECT:ColorCodedColumnNames = pcColorCodingColumnNames .             
               
        IF VALID-HANDLE (THIS-OBJECT:CallbackProcedure) THEN                              
        DO:
            ASSIGN lProcedureHasInitializeRowEventHandler = ProcedureHelper:HasEntry (THIS-OBJECT:CallbackProcedure,
                                                                                      "InitializeRowEventHandler":U, 
                                                                                      "PROCEDURE":U)
                   lProcedureHasDisplayFieldsHook  = ProcedureHelper:HasEntry (THIS-OBJECT:CallbackProcedure,
                                                                               SUBSTITUTE ("WinKitDisplayFieldsIn&1":U, phBrowse:NAME), 
                                                                               "PROCEDURE":U) .             
        END.             
        ELSE 
            ASSIGN lProcedureHasInitializeRowEventHandler = FALSE 
                   lProcedureHasDisplayFieldsHook         = FALSE . 
                                  
        ASSIGN THIS-OBJECT:BrowseHandle = phBrowse 
               THIS-OBJECT:WidgetHandle = phBrowse .                                  
             
        
        /* Mike Fechner, Consultingwerk Ltd. 11.06.2012
           Allow for Grid customization */
        IF lFirstTimeInitializeLayout AND VALID-OBJECT (oBrowseCustomizer) THEN
            oBrowseCustomizer:AssignBrowseProperties (THIS-OBJECT:CallbackProcedure, 
                                                      THIS-OBJECT:BrowseHandle, 
                                                      THIS-OBJECT).
             
        InitializeUniqueKeys () .   
   
        InitializeGrid (phBrowse, poEmbeddedWindowForm, poParent, plUseBatching) . 
        
        SynchronizeVisibleColumns ().
                                  
        THIS-OBJECT:DoubleClickRow:Subscribe (DefaultActionHandler).
                  
        /* Mike Fechner, Consultingwerk Ltd. 06.07.2011
           Add to List of Overlay Controls */
        poEmbeddedWindowForm:OverlayControls:Add (STRING (phBrowse), THIS-OBJECT) .           
                          
        IF NOT VALID-OBJECT (THIS-OBJECT:ColorTableConverter) THEN DO:
            THIS-OBJECT:ColorTableConverter = NEW Consultingwerk.Windows.DefaultColorTableConverter () .
            
            Consultingwerk.Framework.FrameworkSettings:ServiceContainer:AddService
                    (Progress.Lang.Class:GetClass ("Consultingwerk.Windows.IColorTableConverter":U),
                     THIS-OBJECT:ColorTableConverter) .
        END . 
                                
        /* Mike Fechner, Consultingwerk Ltd. 25.11.2011
           prepare for column-sorting and row-display */
        DO ON ERROR UNDO, THROW:
            hBrowse:READ-ONLY = FALSE .               
/*            hBrowse:INSERT-ROW () .*/
             
            CATCH err AS Progress.Lang.Error :
                /* Do nothing, ignore errors from the above two statements */   
            END CATCH.
        END .               
            
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

        FINALLY:
            ASSIGN InConstructor = FALSE . 
        END FINALLY.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activates the current browse column in the Grid
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateCurrentBrowseColumn ():
        
        DEFINE VARIABLE cColumn   AS CHARACTER NO-UNDO .
                            
        ASSIGN cColumn = THIS-OBJECT:ColumnKeyFromColumnHandle (THIS-OBJECT:BrowseHandle:CURRENT-COLUMN) .                                    
                                            
        IF Consultingwerk.CharacterType:IsNotNullOrEmpty (cColumn) THEN DO:
            THIS-OBJECT:Focus () .

            THIS-OBJECT:ActiveRow:Cells[cColumn]:Activate () .

            Consultingwerk.Util.UserInterfaceHelper:ProcessEvents () . 

            THIS-OBJECT:PerformAction (UltraGridAction:EnterEditMode) .
            THIS-OBJECT:ActiveCell:SelectAll () .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates an alternative query that contains alternative buffers to 
                 the tables used by the original query 
        Notes:   Used in the RenderedBrowseControl to assign to the query handle of
                 the original browse widget
        @param phQuery The original browse handle
        @return The handle to the alternative query created by this routine                                                                      
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED HANDLE AlternativeQuery (phQuery AS HANDLE):
        
        DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hBuffer      AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
        
        ASSIGN EXTENT (hAlternativeQueryBuffer) = phQuery:NUM-BUFFERS .

        CREATE QUERY hQuery . 

        DO i = 1 TO phQuery:NUM-BUFFERS:
            CREATE BUFFER hAlternativeQueryBuffer[i]
                FOR TABLE phQuery:GET-BUFFER-HANDLE (i)
                BUFFER-NAME phQuery:GET-BUFFER-HANDLE (i):NAME .

            hQuery:ADD-BUFFER (hAlternativeQueryBuffer[i]) .

            ASSIGN cQueryString = cQueryString +
                                  (IF i > 1 THEN ", ":U ELSE "":U) + 
                                  SUBSTITUTE ("EACH &1 WHERE ROWID (&1) = TO-ROWID ('?') NO-LOCK":U, 
                                              hAlternativeQueryBuffer[i]:NAME) . 
        END.                

        hQuery:QUERY-PREPARE ("FOR ":U + cQueryString) .
        hQuery:QUERY-OPEN () .
                
        RETURN hQuery . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Anchors the Grid at the bottom of it's container and takes all remaining space                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AnchorBottom ():
        
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        
        THIS-OBJECT:Left = 0 .
        THIS-OBJECT:Width = THIS-OBJECT:Parent:Width .
        THIS-OBJECT:Height = THIS-OBJECT:Parent:Height - THIS-OBJECT:Top .
        
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Bottom, 
                                                      System.Windows.Forms.AnchorStyles:Left), 
                          System.Windows.Forms.AnchorStyles).
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(nestedvar0, 
                                                      System.Windows.Forms.AnchorStyles:Top), 
                          System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, 
                                                              System.Windows.Forms.AnchorStyles:Right), 
                                  System.Windows.Forms.AnchorStyles).

        THIS-OBJECT:AnchoredBottom = TRUE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the VALUE-CHANGED Event to the Browse Widget 
        Notes:   Conditionally raises the obsolete ITERATION-CHANGED Event as well
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ApplyValueChanged ():
    
         /* Mike Fechner, Consultingwerk Ltd. 11.08.2014
            Do not apply the VALUE-CHANGED event while still in the constructor */
         
        IF THIS-OBJECT:InConstructor THEN   
            RETURN . 
    
        APPLY "VALUE-CHANGED":U TO hBrowse .    
        
        IF THIS-OBJECT:ApplyIterationChanged = TRUE THEN         
            APPLY "ITERATION-CHANGED":U TO hBrowse .        

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the PositionChanged event of the Progress.Data.BindingSource                                                                        
        Notes:                                                                       
        @param sender The reference to the Component that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID bindingSource1_PositionChanged (sender AS System.Object, 
                                                        e AS System.EventArgs):
        
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 23.06.2014
           WINKIT-7: Do not launch the row-entry-handler.p when color 
           coding and calculated fields are completely disabled. The
           .p file may be co-responsible for the issues with one-way 
           buttons (buttons that can only be clicked once) */
        IF THIS-OBJECT:DisableCalculatedFieldsAndColor = TRUE THEN DO:
            ASSIGN hBuffer = THIS-OBJECT:QUERY:GET-BUFFER-HANDLE (1) .

            IF NOT hBuffer:AVAILABLE THEN
                RETURN .

            IF VALID-HANDLE (hBrowse) AND NOT THIS-OBJECT:ValueChangedSuspended AND NOT THIS-OBJECT:SuspendEvents THEN
                THIS-OBJECT:ApplyValueChanged () .
          
          RETURN . 
        END.           

        /* Mike Fechner, Consultingwerk Ltd. 02.08.2012
           INSERT-ROW Method only applicable when the Browse Widget
           has been realized */
        IF NOT lRowCreated AND THIS-OBJECT:BrowseHandle:HWND > 0 THEN 
        DO ON ERROR UNDO, THROW:
            /* Mike Fechner, Consultingwerk Ltd. 10.08.2011
               Temporarily override the ROW-ENTRY event handler */
            RUN Consultingwerk/WindowIntegrationKit/Controls/row-entry-handler.p
                PERSISTENT SET hEventHandlerProcedure (hBrowse) .

            IF NOT THIS-OBJECT:BrowseHandle:READ-ONLY THEN
                THIS-OBJECT:BrowseHandle:INSERT-ROW () .

            ASSIGN lRowCreated = TRUE .
        END.

        ASSIGN hBuffer = THIS-OBJECT:QUERY:GET-BUFFER-HANDLE (1) .

        IF NOT hBuffer:AVAILABLE THEN
            RETURN .

        DisplayCurrentRow () .

        IF VALID-HANDLE (hBrowse) AND NOT THIS-OBJECT:ValueChangedSuspended AND NOT THIS-OBJECT:SuspendEvents THEN
            THIS-OBJECT:ApplyValueChanged () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the browse column related to the UltraGridColumn                                                                        
        Notes:
        @param pcColumnKey The key of the UltraGridColumn 
        @return The handle of the browse column                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC FINAL HANDLE BrowseColumnHandleFromGridColumnKey (pcColumnKey AS CHARACTER):
        
        DEFINE VARIABLE cColumnName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cBufferName AS CHARACTER NO-UNDO.

        DEFINE VARIABLE hColumn     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
        
        IF NUM-ENTRIES (pcColumnKey, ":":U) = 1 THEN 
            ASSIGN cColumnName = pcColumnKey
                   cBufferName = "":U .  
        ELSE 
            ASSIGN cColumnName = ENTRY (2, pcColumnKey, ":":U)
                   cBufferName = ENTRY (1, pcColumnKey, ":":U) .

        DO i = 1 TO THIS-OBJECT:BrowseHandle:NUM-COLUMNS:       
            hColumn = THIS-OBJECT:BrowseHandle:GET-BROWSE-COLUMN (i) .

            IF cBufferName > "":U THEN 
                IF hColumn:NAME = cColumnName AND hColumn:BUFFER-FIELD:BUFFER-NAME = cBufferName THEN 
                    RETURN hColumn .
                     
            IF hColumn:NAME = cColumnName THEN 
                RETURN hColumn .   
        END.

        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Closes the query by opening it with a query string that matches
                 no records                                                                        
        Notes:   Closing a query that is attached to a binding source results in
                 runtime errors.                                                                           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseQuery ():
        
        QueryHelper:OpenNonMatchingQuery (hQuery) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the key of the UltraGridColumn that represents the given 
                 buffer field                                                                         
        Notes:
        @param pcBufferName The name of the buffer in the query              
        @param pcFieldName The nae of the buffer field                            
        @return The UltraGridColumn Key name (CHARACTER)           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC FINAL CHARACTER ColumnKeyFromFieldName (pcBufferName AS CHARACTER, 
                                                          pcFieldName AS CHARACTER):

        IF THIS-OBJECT:UseFullFieldNames THEN 
            RETURN SUBSTITUTE ("&1:&2":U,
                               pcBufferName,
                               pcFieldName) .
        ELSE 
            RETURN pcFieldName . 

    END METHOD . 
 
    /*------------------------------------------------------------------------------
        Purpose: Returns the key of the UltraGridColumn that represents the given 
                 column of the browse                                                                        
        Notes:
        @param phColumn The handle of a browse column              
        @return The UltraGridColumn Key name (CHARACTER)                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC FINAL CHARACTER ColumnKeyFromColumnHandle (phColumn AS HANDLE):

        DEFINE VARIABLE cKey   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iIndex AS INTEGER   NO-UNDO INIT 0.
        
        IF phColumn:INDEX > 0 THEN 
            ASSIGN iIndex = phColumn:INDEX .  
        
        {Consultingwerk/Assertion/HandleAssert/ValidHandle.i phColumn """Browse Column"":U"} . 

        IF NOT VALID-HANDLE (phColumn:BUFFER-FIELD) THEN DO:
            IF LOOKUP (STRING (phColumn), THIS-OBJECT:UnboundColumnHandles) > 0 THEN
                ASSIGN cKey = ENTRY (LOOKUP (STRING (phColumn), THIS-OBJECT:UnboundColumnHandles),
                                     THIS-OBJECT:UnboundColumnNames) .
            ELSE
                ASSIGN cKey = phColumn:NAME .    
        END.    
        ELSE DO: 
            IF THIS-OBJECT:UseFullFieldNames THEN 
                ASSIGN cKey = SUBSTITUTE ("&1:&2":U,
                                          phColumn:BUFFER-FIELD:BUFFER-HANDLE:NAME,
                                          phColumn:NAME) .
            ELSE
                ASSIGN cKey = phColumn:NAME .
        END.

        IF phColumn:INDEX > 0 THEN
            ASSIGN cKey = cKey + "[":U + STRING (iIndex) + "]":U .

        RETURN cKey .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Call all registered DefaultActionSubscriber to perform their action.
                 Event handler for the DoubleClickRow event of the base class 
                 (UltraGrid). 
        Notes:   
        @param sender System.Object reference to the Object that raised the event (usually THIS-OBJECT)
        @param e System.EventArgs, the default .NET empty parameter object 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID DefaultActionHandler (sender AS System.Object, e AS System.EventArgs):
        
        /* Mike Fechner, Consultingwerk Ltd. 08.07.2014
           WINKIT-9: Don't perform "Default Action" when double clicking on Group-By or Filter row */
        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveRow) OR 
           THIS-OBJECT:ActiveRow:IsGroupByRow OR 
           THIS-OBJECT:ActiveRow:IsFilterRow THEN 
            RETURN .   
        
        ASSIGN THIS-OBJECT:DefaultActionHandled = FALSE . 
        
        OnDefaultAction (e) . 
        
        /* Mike Fechner, Consultingwerk Ltd. 30.10.2012
           Don't handle RETURN key or MOUSE-SELECT-DBLCLICK when the DEFAULT-ACTION 
           event has been handled in the .w file */
        IF THIS-OBJECT:DefaultActionHandled = TRUE THEN 
            RETURN . 
        
        IF TYPE-OF (e, System.Windows.Forms.KeyEventArgs) THEN DO:
            /* Only perform the action if the enter key has been pressed */
            IF NOT Progress.Util.EnumHelper:AreEqual (CAST (e, System.Windows.Forms.KeyEventArgs):KeyCode,
                                                      Keys:Return) THEN  
                RETURN.
            ELSE 
                CAST (e, System.Windows.Forms.KeyEventArgs):Handled = TRUE.
        END.
        ELSE IF TYPE-OF (e, DoubleClickRowEventArgs) THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 30 okt 2012 WinKit
               When the Grid is not visible (most likely because the Form has already
               been closed from whatever happened in the MOUSE-SELECT-DBL-CLICK handler
               we shouldn't raise the DEFAULT-ACTION event to the ABL Browse which is
               also being closed */
            IF NOT THIS-OBJECT:SuspendEvents THEN 
                APPLY "MOUSE-SELECT-DBLCLICK":U TO hBrowse .  
        END.
                       
        FINALLY:
            ASSIGN THIS-OBJECT:DefaultActionHandled = ? .       
        END FINALLY.
                                                            
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Deselects the row with current focus.                                                                    
        Notes:   Returns TRUE for compability with the ABL built in method
        @return Logical value, always TRUE                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DESELECT-FOCUSED-ROW ():
        
        IF VALID-OBJECT (THIS-OBJECT:ActiveRow) THEN 
            THIS-OBJECT:ActiveRow:Selected = FALSE . 
        
        RETURN TRUE.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Re-Display's the current row                                                                      
        Notes:                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayCurrentRow ():
        
        DEFINE VARIABLE iCol       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hCol       AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cColumnKey AS CHARACTER NO-UNDO.

        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:BrowseHandle:SELECT-ROW (1) .

            CATCH err AS Progress.Lang.Error:
                RETURN . /* if we can't select a row, we can't display */     
            END CATCH.
        END.

        DO iCol = 1 TO hBrowse:NUM-COLUMNS ON ERROR UNDO, THROW:
            ASSIGN hCol       = hBrowse:GET-BROWSE-COLUMN (iCol) 
                   cColumnKey = THIS-OBJECT:ColumnKeyFromColumnHandle (hCol) .

            ASSIGN hCol:SCREEN-VALUE = STRING (UNBOX (bindingSource1:InputValue [cColumnKey])) .

            CATCH err AS Progress.Lang.Error :
                /* ignore */
            END CATCH.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Displays the current browse values on the Grid's current row
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayFromBrowse ():
        
        DEFINE VARIABLE iCol       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hCol       AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cColumnKey AS CHARACTER NO-UNDO.

        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:BrowseHandle:SELECT-ROW (1) .

            CATCH err AS Progress.Lang.Error:
                RETURN . /* if we can't select a row, we can't display */     
            END CATCH.
        END.

        DO iCol = 1 TO hBrowse:NUM-COLUMNS ON ERROR UNDO, THROW:
            ASSIGN hCol       = hBrowse:GET-BROWSE-COLUMN (iCol) 
                   cColumnKey = THIS-OBJECT:ColumnKeyFromColumnHandle (hCol) .

            IF Consultingwerk.CharacterType:IsNotNullOrEmpty (cColumnKey) THEN 
                IF hCol:INPUT-VALUE <> ? THEN 
                    THIS-OBJECT:ActiveRow:Cells [cColumnKey]:Value = hCol:INPUT-VALUE . 

            CATCH err AS Progress.Lang.Error :
                /* ignore */
            END CATCH.
        END.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Displays the current Grid values on the BROWSE widgets current row
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayFromGrid ():
        
        DEFINE VARIABLE iCol       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hCol       AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cColumnKey AS CHARACTER NO-UNDO.

        DO ON ERROR UNDO, THROW:
            THIS-OBJECT:BrowseHandle:SELECT-ROW (1) .

            /* Mike Fechner / Consultingwerk Ltd. / WinKit 15 Aug 2013
               When the SELECT-ROW above has failed with "Browse widget 
               must contain at least one record before SELECT-ROW method is used. (2108)"
               some existing code may have removed that one (unbound) row in the Browse 
               widget that we require for the calculated fieldscommunication. It should 
               be o.k. to just create a new one. */
            CATCH err AS Progress.Lang.Error:
                IF err:GetMessageNum (1) = 2108 THEN                       
                    DO ON ERROR UNDO, THROW:
                        THIS-OBJECT:BrowseHandle:INSERT-ROW () .
                        THIS-OBJECT:BrowseHandle:SELECT-ROW (1) .

                        CATCH err2 AS Progress.Lang.Error:
                            RETURN .
                        END CATCH.
                    END.
                ELSE 
                    RETURN .
            END CATCH.
        END.

        DO iCol = 1 TO hBrowse:NUM-COLUMNS ON ERROR UNDO, THROW:
            ASSIGN hCol       = hBrowse:GET-BROWSE-COLUMN (iCol) 
                   cColumnKey = THIS-OBJECT:ColumnKeyFromColumnHandle (hCol) .

            IF Consultingwerk.CharacterType:IsNotNullOrEmpty (cColumnKey) THEN 
                hCol:SCREEN-VALUE = STRING (UNBOX (THIS-OBJECT:ActiveRow:Cells [cColumnKey]:Value)) . 

            CATCH err AS Progress.Lang.Error :
                /* ignore */
            END CATCH.
        END.
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Enables and Disables the Columns in the RenderedBrowseControl based
                 on the current state of the browser columns                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableDisableColumns ():
        
        DEFINE VARIABLE iCol       AS INTEGER         NO-UNDO .
        DEFINE VARIABLE hCol       AS HANDLE          NO-UNDO .
        DEFINE VARIABLE oCol       AS UltraGridColumn NO-UNDO .
        DEFINE VARIABLE cColumnKey AS CHARACTER       NO-UNDO. 

        DEFINE VARIABLE lEditable AS LOGICAL NO-UNDO INIT FALSE .

        DO iCol = 1 TO hBrowse:NUM-COLUMNS ON ERROR UNDO, THROW:
            ASSIGN hCol       = hBrowse:GET-BROWSE-COLUMN (iCol) 
                   cColumnKey = THIS-OBJECT:ColumnKeyFromColumnHandle (hCol) .

            /* Calculated_ columns for CustomCalculatedColumnNames */
            IF LOOKUP (STRING (hCol), THIS-OBJECT:UnboundColumnHandles) > 0 THEN
                ASSIGN cColumnKey = ENTRY (LOOKUP (STRING (hCol), THIS-OBJECT:UnboundColumnHandles),
                                           THIS-OBJECT:UnboundColumnNames) .

            IF NOT THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Exists (cColumnKey) THEN
                NEXT .
                
            oCol = THIS-OBJECT:DisplayLayout:Bands[0]:Columns [cColumnKey].
            
            IF hCol:READ-ONLY OR hBrowse:READ-ONLY THEN 
                oCol:CellActivation = Activation:NoEdit . 
            ELSE 
                ASSIGN oCol:CellActivation = Activation:AllowEdit 
                       lEditable           = TRUE .
                
            CATCH err AS Progress.Lang.Error :
                /* ignore */
            END CATCH.
        END.

        IF lEditable THEN DO:
            THIS-OBJECT:DisplayLayout:Override:AllowUpdate     = Infragistics.Win.DefaultableBoolean:True.
            THIS-OBJECT:DisplayLayout:Override:CellClickAction = CellClickAction:EditAndSelectText . 
            
            bindingSource1:AllowEdit = TRUE . 
        END.
        ELSE DO: 
            THIS-OBJECT:DisplayLayout:Override:AllowUpdate     = Infragistics.Win.DefaultableBoolean:False.
            THIS-OBJECT:DisplayLayout:Override:CellClickAction = CellClickAction:RowSelect .
            
            bindingSource1:AllowEdit = FALSE . 
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Call back procedure for the ENTRY event of the Browse Cells
        Notes:   Invoked from the browse-event-handler.p Procedure
        @param phColumn The Handle to the Browse Column
    ------------------------------------------------------------------------------*/
  METHOD PUBLIC VOID EnterBrowseCell (phColumn AS HANDLE):

        DEFINE VARIABLE cColumn   AS CHARACTER NO-UNDO .

        IF THIS-OBJECT:ApplyingEntry = TRUE THEN
            RETURN .

        ASSIGN cColumn = THIS-OBJECT:ColumnKeyFromColumnHandle (THIS-OBJECT:BrowseHandle:CURRENT-COLUMN) .

        IF Consultingwerk.CharacterType:IsNotNullOrEmpty (cColumn) THEN DO:
            
            /* Mike Fechner, Consultingwerk Ltd. 04.12.2012
               Create an instance of the asnyc handler class that 
               can apply entry to a Grid cell after the ENTRY trigger on 
               the ABL widget has completed*/
            NEW UltraGridAsyncActivateCellHandler (THIS-OBJECT, cColumn) .
     END.        

  END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Fetches a row/record by a ROWID                                                                       
        Notes:   Creates an ROWID EXTENT 1 value and uses the FetchByRowid (ROWID EXTENT)
                 implementation                                  
        @param poRowid The ROWID to reposition to                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchByRowid (poRowid AS ROWID):
        
        DEFINE VARIABLE oRowid AS ROWID EXTENT 1 . 
        
        ASSIGN oRowid [1] = poRowid .        

        FetchByRowid (oRowid) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetches a row/record by a ROWID Array                                                                      
        Notes:                                    
        @param poRowid The Array of ROWIDs to reposition to                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchByRowid (poRowid AS ROWID EXTENT):

        DEFINE VARIABLE cPrepareString AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iResultRow     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lDone          AS LOGICAL   NO-UNDO INIT FALSE .
        
        THIS-OBJECT:Selected:Rows:Clear () .

        /* Carl Verbiest
           Attempt the reposition, if it succeeds we're OK, if not executed Original code */
        
        DO ON ERROR UNDO, LEAVE:
            lDone = QueryHelper:RepositionToRowidArray (THIS-OBJECT:QUERY,
                                                        poRowid) .
            CATCH e AS Progress.Lang.Error :
                lDone = NO.
            END CATCH.
        END.
        IF lDone THEN 
            RETURN.

        ASSIGN cPrepareString = THIS-OBJECT:QUERY:PREPARE-STRING .

        IF (cPrepareString = ? OR cPrepareString = "":U) AND THIS-OBJECT:BindingSource:MaxDataGuess > 0 THEN
            UNDO, THROW NEW AppError ("Unable to call FetchByRowid when query is not dynamic and batching is used."{&TRAN}, 0) .
        
        IF cPrepareString MATCHES "* INDEXED-REPOSITION*":U THEN DO:
            ASSIGN cPrepareString = REPLACE (cPrepareString, " INDEXED-REPOSITION":U, "":U) .

            /* Debug Info */
            IF VALID-OBJECT (oLogMessageHandler) THEN
                oLogMessageHandler:LogMessage (SUBSTITUTE ("FetchByRowid - cPrepareString: &1":U, cPrepareString)) .

            THIS-OBJECT:QUERY:QUERY-PREPARE (cPrepareString) .
            THIS-OBJECT:QUERY:QUERY-OPEN () .
        END.
        
        IF THIS-OBJECT:BindingSource:MaxDataGuess > 0 THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 13.12.2011
               Need to Suspend Binding to be able to query the exact query position */
            THIS-OBJECT:BindingSource:SuspendBinding() .
            
            QueryHelper:RepositionToRowidArray (THIS-OBJECT:QUERY, 
                                                poRowid) .
            
            ASSIGN iResultRow = THIS-OBJECT:QUERY:CURRENT-RESULT-ROW .
    
            THIS-OBJECT:BindingSource:ResumeBinding() . 
            
            /* Mike Fechner, Consultingwerk Ltd. 13.12.2011
               Need to fill more data before reposition bindingSource*/
            IF THIS-OBJECT:BindingSource:Count < iResultRow THEN DO:
                DO WHILE THIS-OBJECT:BindingSource:Count < iResultRow:
                    THIS-OBJECT:BindingSource:MoveLast () .
                END.
            END.
            
            THIS-OBJECT:BindingSource:Position = iResultRow - 1 . 
        END.
        ELSE DO:
            QueryHelper:RepositionToRowidArray (THIS-OBJECT:QUERY, 
                                                poRowid) .
        END .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetches the first row                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchFirst ():

        IF THIS-OBJECT:Batching
        THEN DO:
            THIS-OBJECT:Selected:Rows:Clear () .
            THIS-OBJECT:BindingSource:MoveFirst () .
        END.
        ELSE 
            THIS-OBJECT:PerformAction(UltraGridAction:FirstRowInGrid).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetches the last row                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchLast ():
    
        DEFINE VARIABLE hBrowser AS HANDLE NO-UNDO.

        IF THIS-OBJECT:Batching
        THEN DO:
            Consultingwerk.Framework.FrameworkSettings:WaitStateManager:SetWaitState() .

            THIS-OBJECT:Selected:Rows:Clear () .

            THIS-OBJECT:ValueChangedSuspended = TRUE .
    
            THIS-OBJECT:BindingSource:MoveLast () .
    
            /* Mike Fechner, Consultingwerk Ltd. 12.12.2011
               We may not have all data yet. In that case the binding source
               will be located at Count - 1 */
            IF THIS-OBJECT:BindingSource:MaxDataGuess > 0 THEN DO:
    
                THIS-OBJECT:BeginUpdate () .
                
                DO WHILE THIS-OBJECT:BindingSource:Position < THIS-OBJECT:BindingSource:Count - 1:
                    THIS-OBJECT:BindingSource:MoveLast () .
                END.            
    
                THIS-OBJECT:BindingSource:RefreshAll () .
                THIS-OBJECT:EndUpdate () .
                
                THIS-OBJECT:BindingSource:MoveLast () .
                THIS-OBJECT:ScrollToCurrentRow (FALSE) .
            END.
    
            ASSIGN hBrowser = THIS-OBJECT:BrowseHandle .
    
            IF NOT THIS-OBJECT:SuspendEvents THEN 
                THIS-OBJECT:ApplyValueChanged () .
        
            Consultingwerk.Framework.FrameworkSettings:WaitStateManager:ClearWaitState() .
        END.
        ELSE DO:
            THIS-OBJECT:PerformAction(UltraGridAction:LastRowInGrid).
        END.
        
        FINALLY:
            THIS-OBJECT:ValueChangedSuspended = FALSE .         
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetches the next row                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchNext ():

        IF THIS-OBJECT:Batching
        THEN DO:
            THIS-OBJECT:Selected:Rows:Clear () .
            THIS-OBJECT:BindingSource:MoveNext () .
        END.
        ELSE 
            THIS-OBJECT:PerformAction(UltraGridAction:BelowRow).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetches the previous row                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchPrevious ():

        IF THIS-OBJECT:Batching
        THEN DO:
            THIS-OBJECT:Selected:Rows:Clear () .
            THIS-OBJECT:BindingSource:MovePrevious () .
        END.
        ELSE 
            THIS-OBJECT:PerformAction(UltraGridAction:AboveRow).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Moves a query object’s result list pointer to the row corresponding 
                 to the specified sequence number.
        Notes:   
        @param piRow An integer expression representing the sequence number.
        @return Logical value, indicating the success of the operation
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL FetchRow (piRow AS INTEGER):

        THIS-OBJECT:BindingSource:Position = piRow - 1 . 

        RETURN TRUE . 
        
        CATCH err AS Progress.Lang.Error:
            RETURN FALSE .  
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Dispose event of the containing Form                                                                       
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FormDisposedHandler (sender AS System.Object,
                                               e AS System.EventArgs):
        
        THIS-OBJECT:SuspendEvents = TRUE . 
        
        CAST (sender, System.ComponentModel.Component):Disposed:Unsubscribe (FormDisposedHandler) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an array describing every column of the browse widget      
        Notes:   Returns per Column Buffername.Fieldname or CALC:DATA-TYPE,FORMAT,INITIAL,LABEL                                            
        @return A dynamic array with the current browse columns                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER EXTENT GetBrowseColumns ():
        
        DEFINE VARIABLE cColumns AS CHARACTER NO-UNDO EXTENT.
        DEFINE VARIABLE i        AS INTEGER   NO-UNDO .
        DEFINE VARIABLE hColumn  AS HANDLE    NO-UNDO .

        ASSIGN EXTENT (cColumns) = THIS-OBJECT:BrowseHandle:NUM-COLUMNS .

        DO i = 1 TO EXTENT (cColumns):

            ASSIGN hColumn = THIS-OBJECT:BrowseHandle:GET-BROWSE-COLUMN (i) . 
            
            IF VALID-HANDLE (hColumn:BUFFER-FIELD) THEN DO:
                IF hColumn:INDEX = 0 THEN  
                    ASSIGN cColumns[i] = SUBSTITUTE ("&1.&2&3&4&3":U, 
                                                     hColumn:BUFFER-FIELD:BUFFER-NAME,
                                                     hColumn:BUFFER-FIELD:NAME,
                                                     CHR(1),
                                                     hColumn:WIDTH-PIXELS) .
                ELSE 
                    ASSIGN cColumns[i] = SUBSTITUTE ("&1.&2&3&4&3&5":U, 
                                                     hColumn:BUFFER-FIELD:BUFFER-NAME,
                                                     hColumn:BUFFER-FIELD:NAME,
                                                     CHR(1),
                                                     hColumn:WIDTH-PIXELS,
                                                     hColumn:INDEX) .                
            END.
            ELSE 
                ASSIGN cColumns[i] = SUBSTITUTE ("CALC&4&1&4&2&4&3&4&5":U,
                                                 hColumn:DATA-TYPE,
                                                 hColumn:FORMAT,
                                                 hColumn:LABEL,
                                                 CHR(1),
                                                 hColumn:WIDTH-PIXELS) .
        END .
    
        RETURN cColumns .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the row index of the browse viewport where the REPOSITION 
                 TO ROWID (or RECID) statement displays a repositioned record.
        Notes:   
        @return The row index of the browse viewport where the REPOSITION TO ROWID (or RECID) statement displays a repositioned record. 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC INTEGER GetRepositionedRow ():

        IF VALID-OBJECT (THIS-OBJECT:RepositionedRow) THEN 
            RETURN THIS-OBJECT:RepositionedRow:RowIndex . 
        ELSE 
            RETURN 1 .  

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent ():
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:AllowEdit = FALSE.
        THIS-OBJECT:bindingSource1:AllowNew = FALSE.
        THIS-OBJECT:bindingSource1:AllowRemove = FALSE.
        THIS-OBJECT:bindingSource1:ListChanged:Subscribe(THIS-OBJECT:ListChangedHandler) .
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:TableSchema = ?.
        THIS-OBJECT:bindingSource1:PositionChanged:Subscribe(THIS-OBJECT:bindingSource1_PositionChanged).
        /*  */
        /* RenderedBrowseControl */
        /*  */
        THIS-OBJECT:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:DisplayLayout:LoadStyle = Infragistics.Win.UltraWinGrid.LoadStyle:LoadOnDemand.
        THIS-OBJECT:DisplayLayout:Override:AllowGroupBy = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:DisplayLayout:Override:AllowRowFiltering = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Grid with the layout from the Progress Browse widget                                                                       
        Notes:                                                                       
        @param phBrowse The handle of the original browse widget
        @param poEmbeddedWindowForm The reference to the IEmbeddedWindowForm hosting the Window of the Progress browse widget
        @param poParent The parent .NET Control
        @param plUseBatching Controls the use of Batching of the ProBindingSource     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeGrid (phBrowse AS HANDLE,
                                          poEmbeddedWindowForm AS IEmbeddedWindowForm,
                                          poParent AS System.Windows.Forms.Control,
                                          plUseBatching AS LOGICAL):
        
        /* Create unbound columns */
        DEFINE VARIABLE hColumn            AS HANDLE             NO-UNDO .
        DEFINE VARIABLE oColumn            AS UltraGridColumn    NO-UNDO .
        DEFINE VARIABLE oBand              AS UltraGridBand      NO-UNDO . 
        DEFINE VARIABLE i                  AS INTEGER            NO-UNDO .
        DEFINE VARIABLE iCalcIndex         AS INTEGER            NO-UNDO .
        DEFINE VARIABLE cPrepareString     AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cQueryString       AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cExceptFields      AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cUnboundColumns    AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cUnboundHandles    AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE iIndex             AS INTEGER            NO-UNDO .
        DEFINE VARIABLE cHandles           AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE lAllColorCoded     AS LOGICAL            NO-UNDO INIT FALSE .
        DEFINE VARIABLE lAllCalculated     AS LOGICAL            NO-UNDO INIT FALSE .
        DEFINE VARIABLE lMissingColumns    AS LOGICAL            NO-UNDO INIT FALSE .
        DEFINE VARIABLE cColumnName        AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cMessage           AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cCalcColumnKey     AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE oLogMessageHandler AS ILogMessageHandler NO-UNDO . 
        DEFINE VARIABLE cColumns           AS CHARACTER          NO-UNDO EXTENT .
        
        DEFINE VARIABLE arrayvar AS System.Object EXTENT NO-UNDO.

        IF THIS-OBJECT:ColorCodedColumnNames > "":U THEN
            THIS-OBJECT:VerifyColorCodedColumnNames () .

        IF THIS-OBJECT:CustomCalculatedColumnNames > "":U THEN
            THIS-OBJECT:VerifyCustomCalculatedColumnNames () .
        
        IF THIS-OBJECT:ColorCodedColumnNames = "*":U THEN  
            ASSIGN THIS-OBJECT:ColorCodedColumnHandles = FILL (",":U, phBrowse:NUM-COLUMNS - 1)
                   THIS-OBJECT:ColorCodedColumnNames   = FILL (",":U, phBrowse:NUM-COLUMNS - 1) 
                   lAllColorCoded                      = TRUE .
        ELSE 
            IF THIS-OBJECT:ColorCodedColumnNames > "":U THEN 
                ASSIGN THIS-OBJECT:ColorCodedColumnHandles = FILL (",":U, 
                                                                   NUM-ENTRIES (THIS-OBJECT:ColorCodedColumnNames) - 1) .
        
        EXTENT (arrayvar) = phBrowse:NUM-COLUMNS .

        DO i = 1 TO phBrowse:NUM-COLUMNS:
            ASSIGN hColumn = phBrowse:GET-BROWSE-COLUMN (i) .

            ASSIGN cColumnName = THIS-OBJECT:ColumnKeyFromColumnHandle (hColumn) . 

            IF NOT VALID-HANDLE (hColumn:BUFFER-FIELD) OR 
               LOOKUP (cColumnName, THIS-OBJECT:CustomCalculatedColumnNames) > 0 THEN DO:
                
                IF LOOKUP (cColumnName, THIS-OBJECT:CustomCalculatedColumnNames) > 0 THEN 
                    cCalcColumnKey = "Calculated_":U + cColumnName . 
                ELSE 
                    cCalcColumnKey = cColumnName . 
                   
                 /* Calculated column without @ syntax will be named ? */
                 IF cCalcColumnKey = ? THEN 
                    ASSIGN cCalcColumnKey = SUBSTITUTE ("unknown_&1":U, i) . 
                       
                oColumn = NEW UltraGridColumn (cCalcColumnKey, iCalcIndex) .
                
                IF NOT hColumn:DATA-TYPE BEGINS "DATE":U THEN DO ON ERROR UNDO, THROW:
                    oColumn:DataType = Consultingwerk.Util.DataTypeHelper:ToSystemType (hColumn:DATA-TYPE) .
                
                    CATCH err2 AS Progress.Lang.Error :
                        Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err2) .   
                    END CATCH.
                
                END.
                
                ASSIGN iCalcIndex      = iCalcIndex + 1 
                       cUnboundColumns = cUnboundColumns + cCalcColumnKey + ",":U 
                       cUnboundHandles = cUnboundHandles + STRING (hColumn) + ",":U . 
            END.
            ELSE                 
                oColumn = NEW UltraGridColumn (cColumnName) .
            
            IF lAllColorCoded THEN 
            DO:
                ENTRY (i, ColorCodedColumnNames)   = cColumnName .
                ENTRY (i, ColorCodedColumnHandles) = STRING (hColumn) .
            END.
            ELSE IF THIS-OBJECT:ColorCodedColumnNames > "":U THEN DO:
                ASSIGN iIndex = LOOKUP (cColumnName, THIS-OBJECT:ColorCodedColumnNames) . 
                
                IF iIndex > 0 THEN 
                    ENTRY (iIndex, /* THIS-OBJECT:*/ ColorCodedColumnHandles) = STRING (hColumn) .
            END.
            
            oColumn:WIDTH = 100.
            
            THIS-OBJECT:NewUltraGridColumn (oColumn) .
            
            arrayvar[i] = oColumn .
        END.

        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:ColorCodedColumnNames):
            IF ENTRY (i, THIS-OBJECT:ColorCodedColumnHandles) = "":U THEN DO: 
                IF NOT VALID-OBJECT (oLogMessageHandler) THEN 
                    oLogMessageHandler = {Consultingwerk/get-service.i Consultingwerk.Framework.ILogMessageHandler} .
                
                ASSIGN cMessage = SUBSTITUTE ("ColorCoded Column &1 not found in browse &2!":U,
                                              ENTRY (i, THIS-OBJECT:ColorCodedColumnNames), 
                                              phBrowse:NAME) .
                
                IF VALID-OBJECT (oLogMessageHandler) THEN 
                    oLogMessageHandler:LogMessage (cMessage) .
                ELSE 
                    MessageFormHelper:ShowMessage (cMessage,
                                                   THIS-OBJECT:GetClass():TypeName,
                                                   Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageWarning).
            END.                                                       
                                                                   
            ASSIGN lMissingColumns = TRUE . 
        END.

        IF lMissingColumns THEN REPEAT:
            
            iIndex = LOOKUP ("":U, ColorCodedColumnHandles) .
            
            IF iIndex > 0 THEN DO:
                ColorCodedColumnHandles = ListHelper:RemoveEntry (ColorCodedColumnHandles, iIndex) .
                ColorCodedColumnNames   = ListHelper:RemoveEntry (ColorCodedColumnNames, iIndex) .
            END.
            ELSE LEAVE . 
        END. 

        ASSIGN THIS-OBJECT:UnboundColumnNames   = TRIM (cUnboundColumns, ",":U)
               THIS-OBJECT:UnboundColumnHandles = TRIM (cUnboundHandles, ",":U).

        oBand = NEW UltraGridBand (phBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME, -1) .
        oBand:Columns:AddRange(arrayvar).

        THIS-OBJECT:DisplayLayout:BandsSerializer:Add (oBand) .
         
         /* Mike Fechner, Consultingwerk Ltd. 25.05.2010
            Bug 2293: Workaround for issue Work Request Number: W005240110
                      As of 10.2B Service Pack 1 the use of MaxDataGuess and 
                      the Outlook Group by leads to a crash of the prowin32.exe
                      (infinitive loop) */
        IF plUseBatching THEN DO: 
            bindingSource1:MaxDataGuess = THIS-OBJECT:MaxDataGuess .
             
            THIS-OBJECT:DisplayLayout:ViewStyleBand = ViewStyleBand:Vertical.
        END.
        ELSE DO:
            bindingSource1:MaxDataGuess = 0 .
             
            IF THIS-OBJECT:DisableOutlookGroupBy = TRUE THEN 
                THIS-OBJECT:DisplayLayout:ViewStyleBand = ViewStyleBand:Vertical.
            ELSE 
                THIS-OBJECT:DisplayLayout:ViewStyleBand = ViewStyleBand:OutlookGroupBy.
        END. 

        ASSIGN hQuery = phBrowse:QUERY .  
         
         /* Mike Fechner, Consultingwerk Ltd. 01.08.2010
           When Query is not opened, then open it.  */
         IF NOT hQuery:IS-OPEN THEN DO:
             ASSIGN cPrepareString = hQuery:PREPARE-STRING 
                    cQueryString   = "FOR ":U  .
             
             DO i = 1 TO hQuery:NUM-BUFFERS:
                 ASSIGN cQueryString = cQueryString + 
                                       (IF i > 1 THEN ",":U ELSE "":U) + 
                                       " EACH ":U + hQuery:GET-BUFFER-HANDLE (i):NAME + " ":U +
                                       " WHERE ROWID(":U + hQuery:GET-BUFFER-HANDLE (i):NAME + ") = ? NO-LOCK ":U .
             END.     
             
             hQuery:QUERY-PREPARE (cQueryString) .
             hQuery:QUERY-OPEN () .
             
             IF cPrepareString <> ? THEN 
                hQuery:QUERY-PREPARE (cPrepareString) .
         END.
         
         IF NOT THIS-OBJECT:UseFullFieldNames THEN 
             ASSIGN cExceptFields = QueryExceptList (hQuery) . 

        /* Mike Fechner, Consultingwerk Ltd. 11.06.2012
           Set ProBindingSource's TableSchema before assining the Handle */
        CASE THIS-OBJECT:BindingSourceFields:
            WHEN BindingSourceFieldsEnum:UseList THEN
                bindingSource1:SetFields (THIS-OBJECT:BindingSourceFieldList, "":U, "":U)  .
            WHEN BindingSourceFieldsEnum:BrowserFields THEN
                bindingSource1:SetFields (SetTableSchemaColumnsFromBrowse (), "":U, "":U)  .
            WHEN BindingSourceFieldsEnum:ListAndFields THEN 
                bindingSource1:SetFields (SetTableSchemaColumnsFromBrowseAndFields (), "":U, "":U)  .
            OTHERWISE 
                IF cExceptFields > "":U THEN
                    bindingSource1:SetFields ("*":U, cExceptFields, "":U) .
        END CASE.
         
        IF THIS-OBJECT:BrowseHandle:DYNAMIC OR THIS-OBJECT:DynamicColumns THEN
            ASSIGN cColumns = THIS-OBJECT:GetBrowseColumns () .
         
        ASSIGN hBrowse               = phBrowse
               oEmbeddedWindowForm   = poEmbeddedWindowForm 
               oParent               = poParent
               hAlternativeQuery     = AlternativeQuery (hQuery) 
               hBrowse:QUERY         = hAlternativeQuery NO-ERROR . 
  
        IF THIS-OBJECT:BrowseHandle:DYNAMIC OR THIS-OBJECT:DynamicColumns THEN 
            THIS-OBJECT:RestoreDynamicBrowseColumns (cColumns) .

        /* Mike Fechner, Consultingwerk Ltd. 07.09.2012
           Callback to allow for dynamic modification of column properties
           after the browse had been attached to the new/alternativ query */
        IF VALID-HANDLE (THIS-OBJECT:CallbackProcedure) AND    
           Consultingwerk.Util.ProcedureHelper:HasEntry (THIS-OBJECT:CallbackProcedure,
                                                         "InitializeBrowseForGridCallback":U,
                                                         Consultingwerk.EntryTypeEnum:Procedure) THEN 
            RUN InitializeBrowseForGridCallback IN (THIS-OBJECT:CallbackProcedure) (hBrowse) .

        ASSIGN bindingSource1:HANDLE = hQuery NO-ERROR .
                             
        THIS-OBJECT:DisplayLayout:Override:SelectTypeRow          = SelectType:SingleAutoDrag . 
        THIS-OBJECT:DisplayLayout:Bands[0]:Override:SelectTypeRow = SelectType:SingleAutoDrag.
                    
        THIS-OBJECT:Parent = oParent .                                
                    
        RepositionControl () .    
          
        /* Mike Fechner, Consultingwerk Ltd. 28.08.2012
           In cases where Browsers are stacked on each other and only a single one is visible
           th BringtoFront should only be executed for those browsers that are visible */  
        IF hBrowse:VISIBLE THEN
            THIS-OBJECT:BringToFront()  .   

        /* Mike Fechner, Consultingwerk Ltd. 30.01.2013 WinKit
           TITLE am Browser? */
        IF hBrowse:TITLE > "":U AND hBrowse:TITLE <> "?":U THEN 
                THIS-OBJECT:Text = hBrowse:TITLE . 
        
        /* Mike Fechner, Consultingwerk Ltd. 11.07.2013
           Multi Selection Browser */
        IF hBrowse:MULTIPLE THEN 
            THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Extended.
        ELSE        
            THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = SelectType:None .               

        /* Mike Fechner, Consultingwerk Ltd. 20.08.2013
           Fixed columns */
        IF hBrowse:NUM-LOCKED-COLUMNS > 0 THEN 
            THIS-OBJECT:NUM-LOCKED-COLUMNS = hBrowse:NUM-LOCKED-COLUMNS . 

        /* Mike Fechner, Consultingwerk Ltd. 19.07.2013
           Support for FIT-LAST-COLUMN */
        IF hBrowse:FIT-LAST-COLUMN = TRUE THEN 
            THIS-OBJECT:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn .


    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the UniqueKeyDictionary                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeUniqueKeys ():
        
        DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE j          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cKeys      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
        
        DO i = 1 TO THIS-OBJECT:QUERY:NUM-BUFFERS:
            ASSIGN hBuffer = THIS-OBJECT:QUERY:GET-BUFFER-HANDLE (i) .
            
            CREATE ttBufferInfo.
            
            ASSIGN cKeys = THIS-OBJECT:KeyFieldsForBuffer (hBuffer) .
            
            ASSIGN ttBufferInfo.RecordOwner  = THIS-OBJECT 
                   ttBufferInfo.BufferHandle = hBuffer 
                   ttBufferInfo.UniqueKeys   = cKeys .

            DO j = 1 TO NUM-ENTRIES (cKeys):
                ASSIGN cFieldName = ENTRY (j, cKeys)
                       ttBufferInfo.UniqueFind = ttBufferInfo.UniqueFind + 
                                                 (IF j > 1 THEN " AND ":U ELSE "WHERE ":U) +
                                                 SUBSTITUTE ("&1 = ~"&&&2~"":U,
                                                             ENTRY (NUM-ENTRIES (cFieldName, ":":U),
                                                                    cFieldName, ":":U),
                                                             j) .  
            END.    
            
            IF Consultingwerk.CharacterType:IsNullOrEmpty(ttBufferInfo.UniqueKeys) THEN DO:
                Consultingwerk.Util.MessageFormHelper:ShowMessage
                      (SUBSTITUTE ("There is no unique key defined for the table &1.&2&3The rendered browse control will not function properly."{&TRAN},
                                   hBuffer:TABLE,
                                   System.Environment:NewLine)) .
            END.            
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of unqiue key fields for a buffer
        Notes:   This is a seperate method to allow customer specific overrides
        @param phBuffer The handle of the buffer 
        @return The comma delimited list of field column names
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER KeyFieldsForBuffer (phBuffer AS HANDLE):
    
        DEFINE VARIABLE cKeys AS CHARACTER NO-UNDO .
		DEFINE VARIABLE cKey  AS CHARACTER NO-UNDO.
		DEFINE VARIABLE j     AS INTEGER   NO-UNDO .
		
        IF THIS-OBJECT:UseFullFieldNames THEN DO:
            ASSIGN cKeys = BufferHelper:UniqueKeyFields (phBuffer, FALSE) .
            
            IF cKeys > "":U THEN . 
            ELSE 
                cKeys = BufferHelper:AllFieldNames (phBuffer) .
            
            DO j = 1 TO NUM-ENTRIES (cKeys):        
                ASSIGN cKey = SUBSTITUTE ("&1:&2":U,
                                          phBuffer:NAME, 
                                          ENTRY (j, cKeys)) 
                       ENTRY (j, cKeys) = cKey.
            END.
        END.
        ELSE DO: 
            cKeys = BufferHelper:UniqueKeyFields (phBuffer, FALSE) .
            
            IF cKeys > "":U THEN . 
            ELSE 
                cKeys = BufferHelper:AllFieldNames (phBuffer) .
        END.

        RETURN cKeys . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ListChanged event of the ProBindingSource 
        Notes:   The ListChangedType of Reset is typically raised after opening the
                 query. Calling ProcessColorCodingAndCalculatedFields makes sure that
                 the original Progress browser shows the first row. This might be 
                 releavent if the ROW-ENTRY event handler requires the first row to
                 be displayed.
        @param sender The reference to the object that rased the event
        @param e The ListChangedEventArgs with the data for this event
      ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ListChangedHandler (sender AS System.Object,
                                            e AS System.ComponentModel.ListChangedEventArgs):

        IF Progress.Util.EnumHelper:AreEqual (e:ListChangedType,
                                              System.ComponentModel.ListChangedType:Reset) THEN DO:

            THIS-OBJECT:BindingSource1:SuspendBinding() .

            {Consultingwerk/foreach.i UltraGridRow oRow in THIS-OBJECT:Rows}
                IF oRow:IsGroupByRow = FALSE THEN
                   ProcessColorCodingAndCalculatedFields (oRow) .
            END.
            
            THIS-OBJECT:BindingSource1:ResumeBinding() .
            
            PROCESS EVENTS .
            
            THIS-OBJECT:ReGroupBy () .            
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Alternative event handler for the ListChanged event of the ProBindingSource
        Notes:   Re-Evaluate GroupBy when reopeninig the Query    
        @param sender The reference to the object that rased the event
        @param e The ListChangedEventArgs with the data for this event
      ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ListChangedHandler2 (sender AS System.Object,
                                             e AS System.ComponentModel.ListChangedEventArgs):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        IF Progress.Util.EnumHelper:AreEqual (e:ListChangedType,
                                              System.ComponentModel.ListChangedType:Reset) THEN DO:
            PROCESS EVENTS .

            THIS-OBJECT:ReGroupBy () .
           
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads a browsers column settings from the Registry                                                                        
        Notes:   
        @return The current user settings 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER LoadSettings ():

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        
        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        RETURN oSettingsService:GetSetting (THIS-OBJECT:SettingsKey, 
                                            "GridSettings":U) .   

        CATCH err AS System.Exception:
            /* ignore */    
        END CATCH.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Overloadable method, invoked whenever the Grid creates a new Column
        Notes:   Suitable place to attach ValueBasedAppearance's to the Grid Column
        @param poColumn The reference to the new UltraGridColumn
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID NewUltraGridColumn (poColumn AS UltraGridColumn):
		
		/* NOOP */

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Called after a row is activated
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnAfterRowActivate ():

        DEFINE VARIABLE hBrowse AS HANDLE NO-UNDO.

        SUPER:OnAfterRowActivate().

        IF THIS-OBJECT:ApplyRowEntryOnRowActivate <> TRUE THEN 
            RETURN . 

        ASSIGN hBrowse = THIS-OBJECT:BrowseHandle .

        DO ON ERROR UNDO, THROW:
            hBrowse:SELECT-ROW (1) .

            CATCH err AS Progress.Lang.Error:
                RETURN . 
            END CATCH.
        END.        
        
        IF THIS-OBJECT:ActiveRow:IsGroupByRow = FALSE THEN 
            ProcessColorCodingAndCalculatedFields (THIS-OBJECT:ActiveRow) .

        IF NOT THIS-OBJECT:SuspendEvents THEN 
            APPLY "ROW-ENTRY":U TO hBrowse .

        THIS-OBJECT:EnableDisableColumns () .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Raises the Enter event                                                                                                                                                                                                                                                                                                        
       Notes:   Applies ENTRY to the Progress Browse Widget when ApplyEntryOnEnter is 
                true                                  
       @param e The System.EventArgs that contains the event data                                                                                                                                                                                                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnEnter (e AS System.EventArgs):
       
       SUPER:OnEnter (e) .
       
       IF VALID-HANDLE (hBrowse) AND THIS-OBJECT:ApplyEntryOnEnter THEN DO:
           APPLY "ENTRY":U TO hBrowse .
    
            THIS-OBJECT:Focus () .
       END. 
     
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FindRecordsForCalculatedFields event 
        Notes:   
        @param e The FindRecordsForCalculatedFieldsEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID OnFindRecordsForCalculatedFields (e AS FindRecordsForCalculatedFieldsEventArgs):
		
		EventArgsAssert:IsValid (e, "FindRecordsForCalculatedFields":U) .
		
		THIS-OBJECT:FindRecordsForCalculatedFields:Publish (THIS-OBJECT, e) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes the GotFocus event.                                                                      
        Notes:                                                                        
        @param e An System.EventArgs instance that contains the event data.                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnGotFocus (e AS System.EventArgs):

        SUPER:OnGotFocus (e) .
        
        WinKitControls:OnRenderedBrowseControlActivated (THIS-OBJECT, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Called when a row is initialized
        Notes:   Best match for the ROW-DISPLAY Event of the Progress Browser
                 Previously we used to override the OnInitializeRow method which is
                 generally best practice for handling events of the Control itself. 
                 However in this situation, subscribing to the Event itself allows to
                 conditionally subscribe to the event which is not possible with the 
                 OnInitializeRow method override  
        @param sender The reference to the object that raised the event
        @param e An InitializeRowEventArgs instance that contains the event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeRowHandler (sender AS System.Object,
                                              e AS InitializeRowEventArgs):

        DEFINE VARIABLE hBuffer AS HANDLE  NO-UNDO.
        DEFINE VARIABLE i       AS INTEGER NO-UNDO.
        DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBrowse AS HANDLE  NO-UNDO.

        IF Consultingwerk.CharacterType:IsNullOrEmpty (THIS-OBJECT:ColorCodedColumnNames) AND
           Consultingwerk.CharacterType:IsNullOrEmpty (THIS-OBJECT:UnboundColumnNames) AND
           Consultingwerk.CharacterType:IsNullOrEmpty (THIS-OBJECT:CustomCalculatedColumnNames) THEN
            RETURN .                                

        /* Mike Fechner, Consultingwerk Ltd. 09.08.2011
           avoid display after cell-update */
        IF e:ReInitialize THEN RETURN .

        IF e:Row:IsDataRow = TRUE AND e:Row:Index >= 0 THEN DO:

            IF lProcedureHasInitializeRowEventHandler THEN
                RUN InitializeRowEventHandler IN (THIS-OBJECT:CallbackProcedure)
                          (THIS-OBJECT, e) .

            THIS-OBJECT:ProcessColorCodingAndCalculatedFields (e:Row) .
        END.

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Called before a row is deactivated                                                                        
        Notes: 
        @param e An System.ComponentModel.CancelEventArgs instance that contains the event data.                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnBeforeRowDeactivate (e AS System.ComponentModel.CancelEventArgs):

        DEFINE VARIABLE hBrowser   AS HANDLE          NO-UNDO.

        DEFINE VARIABLE iCol       AS INTEGER         NO-UNDO .
        DEFINE VARIABLE hCol       AS HANDLE          NO-UNDO .
        DEFINE VARIABLE oCol       AS UltraGridColumn NO-UNDO .
        DEFINE VARIABLE cColumnKey AS CHARACTER       NO-UNDO.

        DEFINE VARIABLE lEditable  AS LOGICAL         NO-UNDO INIT FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 07/03/2014
           Only handle Data Rows (not the filter row, not group by rows) */
        IF NOT THIS-OBJECT:ActiveRow:IsDataRow THEN 
            RETURN . 

        IF THIS-OBJECT:ApplyRowLeaveOnRowDeactivate = TRUE THEN DO:

            ASSIGN hBrowser = THIS-OBJECT:BrowseHandle .
            
            /* Mike Fechner / Consultingwerk Ltd. 15.01.2014
               If the Browse widget is no longer valid, there's no need to APPLY ROW-LEAVE */
            IF NOT VALID-HANDLE (hBrowser) THEN 
                RETURN . 
                            
            /* Mike Fechner, Consultingwerk Ltd. 24.01.2013
               Ensure, that the browser has a row selected */
            DO ON ERROR UNDO, THROW:
                hBrowse:SELECT-ROW (1) .

            /* Mike Fechner / Consultingwerk Ltd. / WinKit 15 Aug 2013
               When the SELECT-ROW above has failed with "Browse widget 
               must contain at least one record before SELECT-ROW method is used. (2108)"
               some existing code may have removed that one (unbound) row in the Browse 
               widget that we require for the calculated fieldscommunication. It should 
               be o.k. to just create a new one. */
                CATCH err AS Progress.Lang.Error:
                    IF err:GetMessageNum (1) = 2108 THEN                       
                        DO ON ERROR UNDO, THROW:
                            THIS-OBJECT:BrowseHandle:INSERT-ROW () .
                            hBrowse:SELECT-ROW (1) .
    
                            CATCH err2 AS Progress.Lang.Error:
                                    RETURN .
                            END CATCH.
                        END.
                    ELSE 
                        RETURN .
                END CATCH.
            END.
    
            DO iCol = 1 TO hBrowse:NUM-COLUMNS ON ERROR UNDO, THROW:
                ASSIGN hCol       = hBrowse:GET-BROWSE-COLUMN (iCol)
                       cColumnKey = THIS-OBJECT:ColumnKeyFromColumnHandle (hCol) .
    
                IF hCol:READ-ONLY THEN NEXT .
    
                IF LOOKUP (STRING (hCol), THIS-OBJECT:UnboundColumnHandles) > 0 THEN
                    ASSIGN cColumnKey = ENTRY (LOOKUP (STRING (hCol), THIS-OBJECT:UnboundColumnHandles),
                                               THIS-OBJECT:UnboundColumnNames) .
    
                IF NOT THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Exists (cColumnKey) THEN
                    NEXT .
    
                oCol = THIS-OBJECT:DisplayLayout:Bands[0]:Columns [cColumnKey].
    
                hCol:SCREEN-VALUE = UNBOX (THIS-OBJECT:ActiveRow:Cells[oCol:Key]:Value) .
                hCol:MODIFIED = TRUE .
    
                ASSIGN lEditable = TRUE  .
            END.

            IF THIS-OBJECT:AssignBindingSourceOnRowDeactivate = TRUE AND             
               THIS-OBJECT:ActiveRow:DataChanged AND 
               THIS-OBJECT:ActiveRow:IsGroupByRow = FALSE THEN

                THIS-OBJECT:BindingSource:Assign () .
    
            IF lEditable THEN DO:
                IF NOT THIS-OBJECT:SuspendEvents THEN DO:
                    APPLY "ROW-LEAVE":U TO hBrowse .
    
                    /* Mike Fechner, Consultingwerk Ltd. WINKIT 05/03/2014
                       Turn RETURN NO-APPLY into RETURN "NO-APPLY" so that we can
                       detect this using the RETURN-VALUE */
                    IF RETURN-VALUE = "NO-APPLY":U THEN DO:
                        e:Cancel = TRUE . 
                        RETURN .  
                    END . 
                END.
    
                THIS-OBJECT:BindingSource:Refresh () .
    
                IF THIS-OBJECT:ActiveRow:IsGroupByRow = FALSE THEN 
                    THIS-OBJECT:ProcessColorCodingAndCalculatedFields (THIS-OBJECT:ActiveRow) .
            END.
        END.
        ELSE 
            IF THIS-OBJECT:AssignBindingSourceOnRowDeactivate = TRUE AND             
               THIS-OBJECT:ActiveRow:DataChanged AND 
               THIS-OBJECT:ActiveRow:IsGroupByRow = FALSE THEN

                THIS-OBJECT:BindingSource:Assign () .
        
        FINALLY:
            SUPER:OnBeforeRowDeactivate (e) .
    
            ASSIGN lSupressROW-ENTRY = FALSE . 
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Leave event 
        Notes:   
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnLeave (e AS System.EventArgs):
		
	    IF VALID-HANDLE (hBrowse) AND 
	       THIS-OBJECT:ApplyRowLeaveOnLeave AND 
	       NOT THIS-OBJECT:SuspendEvents THEN 
            
            APPLY "ROW-LEAVE":U TO hBrowse .

        SUPER:OnLeave (e) .
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MouseClick event 
        Notes:   
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnMouseClick (e AS System.Windows.Forms.MouseEventArgs):
        
        DEFINE VARIABLE oUIElement            AS Infragistics.Win.UIElement NO-UNDO .
        DEFINE VARIABLE oScrollArrowUIElement AS ScrollArrowUIElement       NO-UNDO . 

        DEFINE VARIABLE iOffset               AS INTEGER                    NO-UNDO.

        oUIElement = THIS-OBJECT:DisplayLayout:UIElement:ElementFromPoint(e:Location) . 

        DO WHILE VALID-OBJECT (oUIElement):
            IF TYPE-OF (oUIElement, ScrollArrowUIElement) THEN DO:

                ASSIGN oScrollArrowUIElement = CAST (oUIElement, ScrollArrowUIElement) .

                IF Progress.Util.EnumHelper:AreEqual (oScrollArrowUIElement:Direction,
                                                      System.Windows.Forms.ScrollButton:Up) THEN DO:
                                                       
                    IF THIS-OBJECT:Rows:FilterRow:Hidden THEN
                        ASSIGN iOffset = 0 .
                    ELSE 
                        ASSIGN iOffset = 1 .                                                        
                                                          
                    IF THIS-OBJECT:ActiveRowScrollRegion:ScrollPosition = iOffset THEN 
                        THIS-OBJECT:OnOffHome (System.EventArgs:Empty) .                                    
                END.                                                          

                RETURN .
            END.
            
            oUIElement = oUIElement:Parent .
        END.

        SUPER:OnMouseClick (e) .
        
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the MouseWheel Event                                                                       
        Notes:   Used implement backwards-batching  
        @param e System.Windows.Forms.MouseEventArgs                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnMouseWheel (e AS System.Windows.Forms.MouseEventArgs):

        DEFINE VARIABLE iOffset AS INTEGER NO-UNDO.

        SUPER:OnMouseWheel(e) .
        
        IF THIS-OBJECT:Rows:FilterRow:Hidden THEN
            ASSIGN iOffset = 0 .
        ELSE 
            ASSIGN iOffset = 1 . 
        
        IF e:Delta > 0 AND THIS-OBJECT:ActiveRowScrollRegion:ScrollPosition = iOffset THEN 
            THIS-OBJECT:OnOffHome (System.EventArgs:Empty) .

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the OffHome event
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnOffHome (e AS System.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:Empty .
        
        THIS-OBJECT:OffHome:Publish (THIS-OBJECT, e) .

    END METHOD .

	/*------------------------------------------------------------------------------
		Purpose: Raises the VisibleChanged event
		Notes:
		@param e The System.EventArgs with the data for this event  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnVisibleChanged (e AS System.EventArgs):
		
		SUPER:OnVisibleChanged (e) .
		
		/* Mike Fechner / Consultingwerk Ltd. / WinKit 7 Jun 2013
           Fix for calc-fields of first row being replicated in 2nd row on
           some (???) browser instances */
		IF THIS-OBJECT:Visible AND lFirstVisibleChanged AND THIS-OBJECT:DisplayLayout:Rows:Count > 1 THEN DO: 
    
    		THIS-OBJECT:OnInitializeRow
    		  (NEW InitializeRowEventArgs (THIS-OBJECT:DisplayLayout:Rows[1], FALSE)) .
    
            ASSIGN lFirstVisibleChanged = FALSE .	
    	END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Processes color coding and calculted fields for a single grid row
        Notes:   Invoked from OnInitializeRow
        @param poRow The currently initialized Grid Row                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ProcessColorCodingAndCalculatedFields (poRow AS UltraGridRow):
        
        DEFINE VARIABLE hBuffer     AS HANDLE                                  NO-UNDO.
        DEFINE VARIABLE hFindBuffer AS HANDLE                                  NO-UNDO.
        DEFINE VARIABLE hColumn     AS HANDLE                                  NO-UNDO.
        DEFINE VARIABLE i           AS INTEGER                                 NO-UNDO.
        DEFINE VARIABLE e           AS FindRecordsForCalculatedFieldsEventArgs NO-UNDO .
        DEFINE VARIABLE oRowids     AS ROWID EXTENT                            NO-UNDO .

        IF THIS-OBJECT:DisableCalculatedFieldsAndColor = TRUE
            THEN RETURN .

        IF poRow:IsGroupByRow THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 15.08.2013
           The Grid may initialize more records then there are records.
           This will result in rows with just null fields. Those rows
           should not processed for calculated fields and color coding */
        nullValueLoop:
        DO:
          {Consultingwerk/foreach.i UltraGridCell oColumn in poRow:Cells}
               IF UNBOX (oColumn:Value) <> ? THEN LEAVE nullValueLoop .
          END.

          RETURN .
        END.

        Consultingwerk.Util.QueryHelper:GetCurrentRowids (THIS-OBJECT:QUERY, OUTPUT oRowids) .

        ASSIGN hBrowse          = THIS-OBJECT:BrowseHandle .

        ASSIGN hBuffer = THIS-OBJECT:QUERY:GET-BUFFER-HANDLE (1) .

        DO ON ERROR UNDO, THROW:

            e = NEW FindRecordsForCalculatedFieldsEventArgs (poRow) .

            OnFindRecordsForCalculatedFields (e) .

            IF NOT e:Handled THEN DO:
                DO i = 1 TO THIS-OBJECT:QUERY:NUM-BUFFERS ON ERROR UNDO, NEXT:
                    ASSIGN hFindBuffer = THIS-OBJECT:QUERY:GET-BUFFER-HANDLE (i) .

                    hFindBuffer:FIND-FIRST (THIS-OBJECT:UniqueFindPredicate (hFindBuffer,
                                                                             poRow),
                                            NO-LOCK) NO-ERROR .
                END.
            END.

            ERROR-STATUS:ERROR = FALSE NO-ERROR .

            IF hBuffer:AVAILABLE THEN DO:

                DO ON ERROR UNDO, THROW:
                    hBrowse:SELECT-ROW (1) .

                    /* Mike Fechner / Consultingwerk Ltd. / WinKit 15 Aug 2013
                       When the SELECT-ROW above has failed with "Browse widget
                       must contain at least one record before SELECT-ROW method is used. (2108)"
                       some existing code may have removed that one (unbound) row in the Browse
                       widget that we require for the calculated fieldscommunication. It should
                       be o.k. to just create a new one. */
                    CATCH err AS Progress.Lang.Error:
                        IF err:GetMessageNum (1) = 2108 THEN
                            DO ON ERROR UNDO, THROW:
                                THIS-OBJECT:BrowseHandle:INSERT-ROW () .

                                CATCH err2 AS Progress.Lang.Error:
                                		RETURN .
                                END CATCH.
                            END.
                        ELSE
                            RETURN .
                    END CATCH.
                END.

                /* Mike Fechner, Consultingwerk Ltd. 15.02.2012
                   Reset all column colors first - this is required when the
                   original code is just setting colors as exceptions and not
                   resetting it in the normal case */
                IF VALID-OBJECT (THIS-OBJECT:ColorTableConverter) AND THIS-OBJECT:ColorCodedColumnHandles > "":U THEN
                    DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:ColorCodedColumnHandles):
                        ASSIGN hColumn = WIDGET-HANDLE (ENTRY (i, THIS-OBJECT:ColorCodedColumnHandles))
                               hColumn:BGCOLOR = ?
                               hColumn:FGCOLOR = ?.
                    END.

                /* See property comments for ApplyRowDisplayBeforeDisplayFields */
                IF THIS-OBJECT:ApplyRowDisplayBeforeDisplayFields = TRUE AND NOT THIS-OBJECT:SuspendEvents THEN
                    APPLY "ROW-DISPLAY":U TO hBrowse.

                IF lProcedureHasDisplayFieldsHook THEN
                    RUN VALUE (SUBSTITUTE ("WinKitDisplayFieldsIn&1":U, hBrowse:NAME)) IN THIS-OBJECT:CallbackProcedure .

                /* See property comments for ApplyRowDisplayBeforeDisplayFields */
                IF THIS-OBJECT:ApplyRowDisplayBeforeDisplayFields = FALSE AND NOT THIS-OBJECT:SuspendEvents THEN
                    APPLY "ROW-DISPLAY":U TO hBrowse.

                IF VALID-OBJECT (THIS-OBJECT:ColorTableConverter) THEN
                    DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:ColorCodedColumnHandles):
                        ASSIGN hColumn = WIDGET-HANDLE (ENTRY (i, THIS-OBJECT:ColorCodedColumnHandles)) .

                        IF hColumn:BGCOLOR <> ? AND poRow:Cells:Exists(ENTRY (i, THIS-OBJECT:ColorCodedColumnNames)) THEN
                            poRow:Cells[ENTRY (i, THIS-OBJECT:ColorCodedColumnNames)]:Appearance:BackColor =
                                THIS-OBJECT:ColorTableConverter:ConvertColor (hColumn:BGCOLOR) .
                        IF hColumn:FGCOLOR <> ? AND poRow:Cells:Exists(ENTRY (i, THIS-OBJECT:ColorCodedColumnNames)) THEN DO:
                            poRow:Cells[ENTRY (i, THIS-OBJECT:ColorCodedColumnNames)]:Appearance:ForeColor =
                                THIS-OBJECT:ColorTableConverter:ConvertColor (hColumn:FGCOLOR) .
                        END.
                    END.

                ERROR-STATUS:ERROR = FALSE NO-ERROR .

                DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:UnboundColumnNames):
                    ASSIGN hColumn = WIDGET-HANDLE (ENTRY (i, THIS-OBJECT:UnboundColumnHandles)) .

                    IF poRow:Cells:Exists(ENTRY (i, THIS-OBJECT:UnboundColumnNames)) THEN
                        ASSIGN poRow:Cells [ENTRY (i, THIS-OBJECT:UnboundColumnNames)]:Value = BOX(hColumn:INPUT-VALUE)  .
                END.
            END.

            /* Mike Fechner, Consultingwerk Ltd. WINKIT 05/03/2014
               When calc-fields have beeen processed, the Grid may indicate the
               Update state of a row (pencil in front of the row). This fixes that */
            IF THIS-OBJECT:UnboundColumnNames > "":U THEN
                poRow:CancelUpdate() .

          /* Mike Fechner / Consultingwerk Ltd. / WinKit 14 Aug 2013
             Restore the records that were active prior to attemting
             to find the records for processing the colors and calc
             fields  */
          FINALLY:
              /* Dummy statement, otherwise proparse seems to have issues 
                 parsing the END FINALLY statement below */
              ASSIGN i = ? .
              
              DO i = 1 TO EXTENT (oRowids):
                  ASSIGN hFindBuffer = THIS-OBJECT:QUERY:GET-BUFFER-HANDLE (i) .

                  IF oRowids[i] = ? THEN
                    hFindBuffer:BUFFER-RELEASE () .
                  ELSE
                    hFindBuffer:FIND-BY-ROWID (oRowids[i]) .
              END.

          END FINALLY.
        END.
	
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Builds a list of Columns that need to be excluded from the BindingSource
        Notes:   Prior to 10.2B Service Pack 5 the BindingSource was not able to 
                 distinguish between columns with the same name from two joined 
                 buffers. This method was used to build a list of columns that had
                 to be excluded from the binding source to avoid issues with the 
                 UltraGrid caused by duplicate column key values           
        @param phQuery The handle of the Query widget 
        @return The list of columns to be excluded from the Query                                                              
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER QueryExceptList (phQuery AS HANDLE):
        
        /* Create Query */
        DEFINE VARIABLE i                AS INTEGER   NO-UNDO .
        DEFINE VARIABLE j                AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cQueryFields     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hBuffer          AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cExceptList      AS CHARACTER NO-UNDO . 
    
        IF THIS-OBJECT:UseFullFieldNames THEN 
            RETURN "":U . 
    
        ASSIGN hBuffer = phQuery:GET-BUFFER-HANDLE (1) .

        /* Add buffer fields of first buffer to the list of fields (to prevent duplicates) */
        DO j = 1 TO hBuffer:NUM-FIELDS:
            cQueryFields = cQueryFields + ",":U + hBuffer:BUFFER-FIELD(j):NAME .                                  
        END. 
        
        cQueryFields = TRIM(cQueryFields, ",":U) .
        
        DO i = 2 TO phQuery:NUM-BUFFERS:
            ASSIGN hBuffer = phQuery:GET-BUFFER-HANDLE (i) .
            
            DO j = 1 TO hBuffer:NUM-FIELDS:
                IF ListHelper:EntryIsInList (hBuffer:BUFFER-FIELD(j):NAME, cQueryFields) THEN 
                    ASSIGN cExceptList = cExceptList + ",":U + hBuffer:NAME + ".":U + hBuffer:BUFFER-FIELD(j):NAME .
                ELSE
                    ASSIGN cQueryFields = cQueryFields + ",":U + hBuffer:BUFFER-FIELD(j):NAME .  
            END.                         
        END.

        ASSIGN cExceptList = TRIM(cExceptList, ",":U) .

        RETURN cExceptList.    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterCellUpdate event                                                                        
        Notes:                                   
        @param e The CellEventArgs with the data for this event                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnAfterCellUpdate (e AS CellEventArgs):
        
        DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
        
        IF e:Cell:Row:IsFilterRow = FALSE THEN DO:
        IF THIS-OBJECT:ApplyCellLeaveToBrowse = ApplyCellLeaveToBrowseEnum:OnAfterCellUpdate AND
           lSupressAfterUpdateCell = FALSE AND 
           e:Cell:IsInEditMode AND NOT e:Cell:IsFilterRowCell THEN DO ON ERROR UNDO, THROW:
          
            ASSIGN lSupressAfterUpdateCell = TRUE . 
          
            hColumn = THIS-OBJECT:BrowseColumnHandleFromGridColumnKey (e:Cell:Column:Key) .

            IF VALID-HANDLE (hColumn) THEN DO ON ERROR UNDO, THROW:     
                IF NOT THIS-OBJECT:SuspendEvents THEN DO ON ERROR UNDO, THROW:           
                    ASSIGN THIS-OBJECT:ApplyingEntry = TRUE  .
                    
                    APPLY "ENTRY":U TO hColumn . 
                    
                    FINALLY:
                        ASSIGN THIS-OBJECT:ApplyingEntry = FALSE .    
                    END FINALLY.
                END.
                
                THIS-OBJECT:DisplayFromGrid () .
                
                IF NOT THIS-OBJECT:SuspendEvents THEN 
                    APPLY "LEAVE":U TO hColumn . 
                    
                FINALLY:
                    THIS-OBJECT:DisplayFromBrowse () .      
                END FINALLY.
            END.
        
            FINALLY:
                ASSIGN lSupressAfterUpdateCell = FALSE .    
            END FINALLY.
        END.
        END.

        SUPER:OnAfterCellUpdate (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Called after a cell is activated 
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnAfterCellActivate ():

        SUPER:OnAfterCellActivate () .

        IF VALID-OBJECT (THIS-OBJECT:ActiveCell) THEN 
            THIS-OBJECT:LastActiveCell = THIS-OBJECT:ActiveCell:Column:Key .
        ELSE  
            THIS-OBJECT:LastActiveCell = "":U .

    END.

    /*------------------------------------------------------------------------------
        Purpose: Called before a cell is deactivated 
        Notes:                                                                           
        @param e An System.ComponentModel.CancelEventArgs instance that contains the event data.                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnBeforeCellDeactivate (e AS System.ComponentModel.CancelEventArgs):

        DEFINE VARIABLE hColumn AS HANDLE NO-UNDO.

        IF THIS-OBJECT:ActiveRow:IsFilterRow = FALSE THEN DO:
            IF VALID-OBJECT (THIS-OBJECT:ActiveCell) THEN 
                THIS-OBJECT:LastActiveCell = THIS-OBJECT:ActiveCell:Column:Key .
            ELSE  
                THIS-OBJECT:LastActiveCell = "":U .
    
        IF Progress.Util.EnumHelper:AreEqual (THIS-OBJECT:ActiveCell:Column:CellActivation,
                                              Activation:AllowEdit) AND  
           THIS-OBJECT:ApplyCellLeaveToBrowse = ApplyCellLeaveToBrowseEnum:OnDeactivate THEN DO:
            
            hColumn = THIS-OBJECT:BrowseColumnHandleFromGridColumnKey (THIS-OBJECT:ActiveCell:Column:Key) .

            IF VALID-HANDLE (hColumn) THEN DO ON ERROR UNDO, THROW:     
                IF NOT THIS-OBJECT:SuspendEvents THEN DO ON ERROR UNDO, THROW:          
                    ASSIGN THIS-OBJECT:ApplyingEntry = TRUE  .
                    
                    APPLY "ENTRY":U TO hColumn . 
                    
                    FINALLY:
                        ASSIGN THIS-OBJECT:ApplyingEntry = FALSE .         
                    END FINALLY.
                END.
                                
                THIS-OBJECT:DisplayFromGrid () .
                
                IF NOT THIS-OBJECT:SuspendEvents THEN DO ON ERROR UNDO, THROW:  
                    ASSIGN THIS-OBJECT:ApplyingEntry = TRUE  .

                    IF VALID-HANDLE (hColumn:NEXT-COLUMN) THEN 
                        APPLY "TAB":U TO hColumn . 
                    ELSE 
                        APPLY "BACK-TAB":U TO hColumn . 

                    FINALLY:
                        ASSIGN THIS-OBJECT:ApplyingEntry = FALSE .         
                    END FINALLY.
                END.

                /* Mike Fechner, Consultingwerk Ltd. 04.12.2012
                   As we've used the TAB event, the LEAVE event will be 
                   fired and RETURN NO-APPLY can be detected from comparing
                   the current column */
                IF THIS-OBJECT:BrowseHandle:CURRENT-COLUMN = hColumn THEN DO: 
                    e:Cancel = TRUE . 
                    
                    THIS-OBJECT:PerformAction (UltraGridAction:EnterEditMode) .                
                END.
                                    
                FINALLY:
                    THIS-OBJECT:DisplayFromBrowse () .      
                END FINALLY.
            END.
        END.
        END.

        SUPER:OnBeforeCellDeactivate (e) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Called before entering edit mode
        Notes:                                                                           
        @param e An System.ComponentModel.CancelEventArgs instance that contains the event data.                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnBeforeEnterEditMode (e AS System.ComponentModel.CancelEventArgs):
        
        DEFINE VARIABLE hBrowse AS HANDLE NO-UNDO.
            
        SUPER:OnBeforeEnterEditMode (e) .
            
        IF THIS-OBJECT:ActiveRow:IsFilterRow = FALSE THEN DO:
        IF THIS-OBJECT:ApplyRowEntryOnBeforeEnterEditMode = FALSE THEN 
            RETURN . 
            
        IF e:Cancel THEN 
            RETURN .     
            
        IF THIS-OBJECT:ActiveCell:IsFilterRowCell THEN 
            RETURN .             
            
        ASSIGN hBrowse = THIS-OBJECT:BrowseHandle .     
        
        IF VALID-HANDLE (hEventHandlerProcedure) THEN
            DELETE OBJECT hEventHandlerProcedure .

        IF NOT lSupressROW-ENTRY THEN DO:
            IF NOT THIS-OBJECT:SuspendEvents THEN 
                APPLY "ROW-ENTRY":U TO hBrowse .
            
            ASSIGN lSupressROW-ENTRY = TRUE . 
        END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeSortChange event                                                                       
        Notes:                                    
        @param e The BeforeSortChangeEventArgs with the data for this event                                    
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnBeforeSortChange (e AS BeforeSortChangeEventArgs):
    
       DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
       DEFINE VARIABLE i       AS INTEGER NO-UNDO.

       IF hBrowse:ALLOW-COLUMN-SEARCHING = FALSE THEN 
           RETURN .        

       IF e:SortedColumns:Count > 0 THEN DO:
    
           ASSIGN hColumn = THIS-OBJECT:BrowseColumnHandleFromGridColumnKey (e:SortedColumns[0]:Key) .
           
           IF VALID-HANDLE (hColumn) THEN DO:

              IF NOT THIS-OBJECT:SuspendEvents THEN DO ON ERROR UNDO, THROW:
                   ASSIGN THIS-OBJECT:ApplyingEntry = TRUE  .
                    
                   APPLY "ENTRY":U TO hColumn . 
                    
                   FINALLY:
                       ASSIGN THIS-OBJECT:ApplyingEntry = FALSE .         
                   END FINALLY.
               END.
                
               ASSIGN hBrowse:CURRENT-COLUMN     = hColumn
                      THIS-OBJECT:CURRENT-COLUMN = hColumn .
    
               PROCESS EVENTS . 
    
               RETURN-VALUE = "". 
               IF NOT THIS-OBJECT:SuspendEvents THEN 
                   APPLY "START-SEARCH":U TO hBrowse .
    
               /* Mike Fechner, Consultingwerk Ltd. 05.04.2014
                  Ability to cancel BeforeSortChange event with RETURN "CANCEL" from 
                  the START-SEARCH Trigger of the Browse widget*/
               IF RETURN-VALUE = "CANCEL":U THEN 
                   e:Cancel = TRUE . 
    
               THIS-OBJECT:CURRENT-COLUMN = ? .
           END.    
    
/*           e:Cancel = TRUE .*/
       END.     

       SUPER:OnBeforeSortChange (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the DefaultAction event                                                                       
        Notes: 
        @param e An System.EventArgs that contains the event data.                                                                                   
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID OnDefaultAction (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN 
            ASSIGN e = NEW System.EventArgs () .

        THIS-OBJECT:DefaultAction:Publish (THIS-OBJECT, e) .
        
        /* Mike Fechner, Consultingwerk Ltd. 30 okt 2012 WinKit
           When the Grid is not visible (most likely because the Form has already
           been closed from whatever happened in the MOUSE-SELECT-DBL-CLICK handler
           we shouldn't raise the DEFAULT-ACTION event to the ABL Browse which is
           also being closed */
        IF NOT THIS-OBJECT:SuspendEvents THEN
            APPLY "DEFAULT-ACTION":U TO hBrowse .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Called when the layout is first initialized after the datasource 
                 has been set                                                                       
        Notes:   Used to update the Grid's layout with the layout of the underlying 
                 Progress Browser after the Grid has build Grid Columns for the 
                 Binding Source (query) columns                                                                  
        @param e An System.EventArgs that contains the event data.
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnInitializeLayout (e AS InitializeLayoutEventArgs ):
    
        DEFINE VARIABLE oCustomizer       AS IRenderedBrowseColumnCustomizer NO-UNDO . 
        DEFINE VARIABLE oBrowseCustomizer AS IRenderedBrowseCustomizer       NO-UNDO .
        DEFINE VARIABLE oInt32Type        AS System.Type                     NO-UNDO.
        DEFINE VARIABLE oDecimalType      AS System.Type                     NO-UNDO.
        DEFINE VARIABLE oDateType         AS System.Type                     NO-UNDO.
        
        /* Format numeric columns */
        ASSIGN oInt32Type   = Progress.Util.TypeHelper:GetType("System.Int32":U) 
               oDecimalType = Progress.Util.TypeHelper:GetType("System.Decimal":U)            
                  .
              
        SUPER:OnInitializeLayout (e) .
        
        ViewHideColumns() .

        oCustomizer = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IRenderedBrowseColumnCustomizer} .
        oBrowseCustomizer = {Consultingwerk/get-service.i Consultingwerk.WindowIntegrationKit.IRenderedBrowseCustomizer} .
    
        IF VALID-OBJECT (THIS-OBJECT:DataSource) THEN DO:
            {Consultingwerk/foreach.i UltraGridColumn oColumn in this-object:DisplayLayout:Bands[0]:Columns}

                /* Formatting of Integer Colums */
                IF oColumn:DataType:Equals(oInt32Type) THEN DO:
                    IF NOT VALID-OBJECT(oColumn:CellAppearance) THEN
                        oColumn:CellAppearance = NEW Infragistics.Win.Appearance() .

                    oColumn:CellAppearance:TextHAlign = Infragistics.Win.HAlign:Right .
                    oColumn:CellDisplayStyle = CellDisplayStyle:FormattedText.
                    oColumn:Style = ColumnStyle:Integer.    
                END.

                /* Formatting of Decimal Columns */
                ELSE IF oColumn:DataType:Equals(oDecimalType) THEN DO:
                    IF NOT VALID-OBJECT(oColumn:CellAppearance) THEN
                        oColumn:CellAppearance = NEW Infragistics.Win.Appearance() .

                    oColumn:CellAppearance:TextHAlign = Infragistics.Win.HAlign:Right .
                    oColumn:CellDisplayStyle = CellDisplayStyle:FormattedText.
                    oColumn:Style = ColumnStyle:Double. 
                END.            
                
                IF VALID-OBJECT (oCustomizer) THEN 
                    oCustomizer:AssignColumnProperties (THIS-OBJECT:CallbackProcedure,
                                                        THIS-OBJECT:BrowseHandle,
                                                        THIS-OBJECT,
                                                        oColumn) .
            END .
        END.
    
        /* Mike Fechner, Consultingwerk Ltd. 11.06.2012
           Allow for Grid customization */
        IF lFirstTimeInitializeLayout AND VALID-OBJECT (oBrowseCustomizer) THEN
            oBrowseCustomizer:InitializeLayout (THIS-OBJECT:CallbackProcedure, 
                                                THIS-OBJECT:BrowseHandle, 
                                                THIS-OBJECT).
        
        IF lFirstTimeInitializeLayout AND THIS-OBJECT:SettingsKey > "":U THEN 
            THIS-OBJECT:RestoreSettings () .
        
        

        IF THIS-OBJECT:MonitorBrowseCellEntry THEN
            RUN Consultingwerk/WindowIntegrationKit/Controls/browse-event-handler.p
                PERSISTENT SET hEventHandlerProcedure2
                (THIS-OBJECT, THIS-OBJECT:BrowseHandle).


        FINALLY:
            lFirstTimeInitializeLayout = FALSE .         
        END FINALLY.
        
     END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Key Down Event
        Notes:   Bug 2640: Enable Excel-like navigation during update
        @param e The reference to the KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnKeyDown (e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE cKeyLabel AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hColumn   AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hBrowse   AS HANDLE    NO-UNDO .

        IF VALID-OBJECT (THIS-OBJECT:ActiveRow) AND THIS-OBJECT:ActiveRow:IsFilterRow = FALSE THEN DO: 
        ASSIGN hBrowse = THIS-OBJECT:BrowseHandle .

        IF NOT e:Handled THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 29.04.2011
               Bug 2640: When a cell is on EditMode, enable cursor keys and Enter/Return
               for navigation to neighboring cells. e:Handled will be set so that the
               Grid itself will not perform it's default action. */
            IF VALID-OBJECT (THIS-OBJECT:ActiveCell) AND THIS-OBJECT:ActiveCell:IsInEditMode THEN DO:

                IF THIS-OBJECT:CursorKeyNavigationInEdit = CursorKeyNavigationInEditEnum:UpDownLeftRight OR 
                   THIS-OBJECT:CursorKeyNavigationInEdit = CursorKeyNavigationInEditEnum:UpDown THEN DO:  

                    IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Down) OR
                       Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Enter) OR
                       Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Return) THEN DO:
    
                        THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:BelowCell) .
                        THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode) .
                        e:Handled = TRUE .
                    END.
                    ELSE IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Up) THEN DO:
    
                        THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:AboveCell) .
                        THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode) .
                        e:Handled = TRUE .
                    END.
                END.

                IF THIS-OBJECT:CursorKeyNavigationInEdit = CursorKeyNavigationInEditEnum:UpDownLeftRight THEN DO:

                    IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Left) THEN DO:
    
                        THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:PrevCell) .
                        THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode) .
                        e:Handled = TRUE .
                    END.
                    ELSE IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Right) THEN DO:
    
                        THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:NextCell) .
                        THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:EnterEditMode) .
                        e:Handled = TRUE .
                    END.
                END.
            END.
            
            /* Mike Fechner, Consultingwerk Ltd. 06.09.2012
               OffHome Event */
            IF VALID-OBJECT (THIS-OBJECT:BindingSource) AND THIS-OBJECT:BindingSource:Position = 0 AND 
               (Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Up) OR 
                Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:PageUp)) THEN DO:
            
                THIS-OBJECT:OnOffHome (System.EventArgs:Empty) .   
                e:Handled = TRUE .
            END.
        END.
        
        /* Mike Fechner, Consultingwerk Ltd. 03.12.2012
           Control if/where to APPLY function keys to */
        IF NOT e:Handled AND VALID-OBJECT (THIS-OBJECT:ApplyFunctionKeyEvents) AND 
           THIS-OBJECT:ApplyFunctionKeyEvents <> ApplyFunctionKeyEventsEnum:None THEN DO:
        
            ASSIGN cKeyLabel = KeyboardHelper:SpecialKeyToAblEventLabel (e:KeyCode) .
            
            IF cKeyLabel BEGINS "F":U THEN DO:
                IF THIS-OBJECT:ApplyFunctionKeyEvents = ApplyFunctionKeyEventsEnum:ToColumn OR 
                   THIS-OBJECT:ApplyFunctionKeyEvents = ApplyFunctionKeyEventsEnum:ToColumnAndBrowse THEN DO:
                    
                    ASSIGN hColumn = THIS-OBJECT:BrowseColumnHandleFromGridColumnKey (THIS-OBJECT:ActiveCell:Column:Key) .
                    
                    IF VALID-HANDLE (hColumn) THEN DO:
                        ASSIGN THIS-OBJECT:BrowseHandle:CURRENT-COLUMN = hColumn .
                        
                        THIS-OBJECT:DisplayCurrentRow () .
                                                
                        APPLY cKeyLabel TO hColumn .

                        THIS-OBJECT:DisplayFromBrowse () .

                        /* Moved to different cell ? */
                        IF THIS-OBJECT:BrowseHandle:CURRENT-COLUMN <> hColumn THEN                                                  
                            THIS-OBJECT:ActivateCurrentBrowseColumn () .
                                
                        e:Handled = TRUE .                         
                    END.
                END.

                IF THIS-OBJECT:ApplyFunctionKeyEvents = ApplyFunctionKeyEventsEnum:ToBrowse OR 
                   THIS-OBJECT:ApplyFunctionKeyEvents = ApplyFunctionKeyEventsEnum:ToColumnAndBrowse THEN DO:
                    
                    APPLY cKeyLabel TO BrowseHandle .
                    e:Handled = TRUE .   
                END.  
            END.
        END.
        
        /* Mike Fechner, Consultingwerk Ltd. 04.12.2012
           Enter/Return may raise DefaultAction */
        IF NOT e:Handled AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Return) THEN 
            THIS-OBJECT:DefaultActionHandler (THIS-OBJECT, e) .
        
        IF NOT e:Handled AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, Keys:Delete) THEN
            APPLY "DELETE-CHARACTER":U TO hBrowse .
        END.
        
        /* Mike Fechner, Consultingwerk Ltd. 11.06.2012
           If the above code has handled the keys - don't process any further
           This is especially usefull in numeric columns because the cursor keys 
           usually flip the values there */
        IF NOT e:Handled THEN DO:
            SUPER:OnKeyDown (e) .
        END.        
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the ParentChanged Event
        Notes:   
        @param e The System.EventArgs instance that contains the event data                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnParentChanged (e AS System.EventArgs):
        
        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .
        
        SUPER:OnParentChanged (e) .
        
        IF NOT lFormDisposedSubscribed THEN DO:
        oForm = THIS-OBJECT:FindForm () .

            IF VALID-OBJECT (oForm) THEN DO ON ERROR UNDO, THROW:
            oForm:Disposed:Subscribe (FormDisposedHandler) .
                FINALLY:
                    ASSIGN lFormDisposedSubscribed = TRUE . 		
                END FINALLY. 
        END.
    END. 
    END. 

    /*------------------------------------------------------------------------------
        Purpose: Forces the Grid to update the row grouping (outlook group by)                                                                        
        Notes:   Performs re-GroupBy only when there are GroupBy Columns                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ReGroupBy ():
        
        {Consultingwerk/foreach.i Infragistics.Win.UltraWinGrid.UltraGridColumn oColumn in THIS-OBJECT:DisplayLayout:Bands[0]:SortedColumns}
        
            IF oColumn:IsGroupByColumn THEN DO:
                THIS-OBJECT:DisplayLayout:Bands[0]:SortedColumns:RefreshSort (TRUE) .
                RETURN . 
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Positions the RenderedBrowseControl on top of the Progress Browse                                                                       
        Notes:   Set's Location and Size properties                                                                       
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID RepositionControl ():
        
        DEFINE VARIABLE iX AS INTEGER NO-UNDO.
        DEFINE VARIABLE iY AS INTEGER NO-UNDO.

        WidgetHelper:AbsolutePositionOnWindow (hBrowse, OUTPUT iX, OUTPUT iY).

        /* Mike Fechner, Consultingwerk Ltd. 06.09.2012
           When the RenderedBrowseControl is parented directly by the EmbeddedWindowForm 
           it's position should also be adjusted by the WindowContainerRowOffset and 
           WindowContainerColOffset. Previously that was only happening when it was 
           parented by the Form's client area */
        IF THIS-OBJECT:Parent = oEmbeddedWindowForm:ClientArea OR
           THIS-OBJECT:Parent = oEmbeddedWindowForm THEN 
           
           ASSIGN iY = iY - oEmbeddedWindowForm:WindowContainerRowOffset 
                  iX = iX - oEmbeddedWindowForm:WindowContainerColOffset.
        
        ASSIGN THIS-OBJECT:Size     = NEW System.Drawing.Size (hBrowse:WIDTH-PIXELS,
                                                               hBrowse:HEIGHT-PIXELS) 
               THIS-OBJECT:Location = NEW System.Drawing.Point (iX, 
                                                                iY). 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Restores the columns of a dynamic browse widget after the query has
                 been replaced with the alternative query                                                                       
        Notes:   The column description has been built by GetBrowseColumns                                                                     
        @param pcColumns An array of browse column descriptions  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RestoreDynamicBrowseColumns (pcColumns AS CHARACTER EXTENT):
        
        DEFINE VARIABLE i       AS INTEGER NO-UNDO .
        DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO .

        DO i = 1 TO EXTENT (pcColumns):
            IF ENTRY (1, pcColumns[i], CHR(1)) = "CALC":U THEN DO:
                hColumn = THIS-OBJECT:BrowseHandle:ADD-CALC-COLUMN (ENTRY (2, pcColumns[i], CHR(1)), 
                                                                    ENTRY (3, pcColumns[i], CHR(1)),
                                                                    ?,
                                                                    ENTRY (4, pcColumns[i], CHR(1))) .
                hColumn:WIDTH-PIXELS = INTEGER (ENTRY (5, pcColumns[i], CHR(1))) .
            END.
            ELSE DO:
                /* Mike Fechner, Consultingwerk Ltd. 28.02.2012 - WinKit PoC
                   ADD-LIKE-COLUMN should be used with NO-ERROR to suppress 
                   error messages, like issues with VALIDATE expressions that 
                   do not work in browsers */
                IF ENTRY (3, pcColumns[i], CHR (1)) > "":U THEN 
                    hColumn = THIS-OBJECT:BrowseHandle:ADD-LIKE-COLUMN (SUBSTITUTE ("&1[&2]":U,
                                                                                    ENTRY (1, pcColumns [i], CHR(1)),
                                                                                    ENTRY (3, pcColumns [i], CHR(1)))) NO-ERROR  .
                ELSE 
                    hColumn = THIS-OBJECT:BrowseHandle:ADD-LIKE-COLUMN (ENTRY (1, pcColumns [i], CHR(1))) NO-ERROR  .

                hColumn:WIDTH-PIXELS = INTEGER (ENTRY (2, pcColumns[i], CHR(1))) .
            END.
        END .
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Restores the stored User settings for the RenderedBrowseControl instance                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RestoreSettings ():
        
        DEFINE VARIABLE cSettings AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE oSettings AS UltraGridSettings NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 12.10.2010
           Save Grid Column settings, when enabled for the session and the SettingsKey is set
           and different from the GUID created in the constrcutor. */
        IF THIS-OBJECT:SettingsKey > "":U THEN .
        ELSE RETURN .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataSource) THEN RETURN .

        ASSIGN cSettings = LoadSettings ()
               oSettings = NEW UltraGridSettings (THIS-OBJECT) .

        oSettings:ApplyStoredSettings (cSettings) .

        DELETE OBJECT oSettings .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the User Settings string for the RenderedBrowseControl instance                                                                      
        Notes:                                                                        
        @return The serialized user settings of the current RenderedBrowseControl
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ReturnGridSettings ():
        
        DEFINE VARIABLE cSettings AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE oSettings AS UltraGridSettings NO-UNDO .
        
        /* Mike Fechner, Consultingwerk Ltd. 12.10.2010
           Save Grid Column settings, when enabled for the session and the SettingsKey is set */
        IF THIS-OBJECT:SettingsKey > "":U THEN .
        ELSE RETURN "":U.   
                
        IF NOT VALID-OBJECT (THIS-OBJECT:DataSource) THEN RETURN "":U . 
        
        ASSIGN oSettings = NEW UltraGridSettings (THIS-OBJECT) 
               cSettings = oSettings:StoreColumnSettings() .

        DELETE OBJECT oSettings . 
        
        RETURN cSettings . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the current row 
        Notes:   Defaults to APPLY ROW-LEAVE to the ABL Browse Widget
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveCurrentRow ():

        THIS-OBJECT:SaveCurrentRow (TRUE) .

    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Saves the current row 
        Notes:   
        @param plApplyRowLeave Logical value indicating if ROW-LEAVE should be applied to the ABL Browse Widget
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SaveCurrentRow (plApplyRowLeave AS LOGICAL):
		
        DEFINE VARIABLE hBrowser   AS HANDLE          NO-UNDO.

        DEFINE VARIABLE iCol       AS INTEGER         NO-UNDO .
        DEFINE VARIABLE hCol       AS HANDLE          NO-UNDO .
        DEFINE VARIABLE oCol       AS UltraGridColumn NO-UNDO .
        DEFINE VARIABLE cColumnKey AS CHARACTER       NO-UNDO.

        DEFINE VARIABLE lEditable  AS LOGICAL         NO-UNDO INIT FALSE .

        IF VALID-OBJECT (THIS-OBJECT:ActiveCell) AND THIS-OBJECT:ActiveCell:IsInEditMode THEN 
            THIS-OBJECT:PerformAction (Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode) .

        ASSIGN hBrowser = THIS-OBJECT:BrowseHandle .
        
        THIS-OBJECT:SelectBrowseRow() .
        PROCESS EVENTS.

        DO iCol = 1 TO hBrowse:NUM-COLUMNS ON ERROR UNDO, THROW:
            ASSIGN hCol       = hBrowse:GET-BROWSE-COLUMN (iCol)
                   cColumnKey = THIS-OBJECT:ColumnKeyFromColumnHandle (hCol) .

            IF hCol:READ-ONLY THEN NEXT .

            IF LOOKUP (STRING (hCol), THIS-OBJECT:UnboundColumnHandles) > 0 THEN
                ASSIGN cColumnKey = ENTRY (LOOKUP (STRING (hCol), THIS-OBJECT:UnboundColumnHandles),
                                           THIS-OBJECT:UnboundColumnNames) .

            IF NOT THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Exists (cColumnKey) THEN
                NEXT .

            oCol = THIS-OBJECT:DisplayLayout:Bands[0]:Columns [cColumnKey].

            hCol:SCREEN-VALUE = STRING (UNBOX (THIS-OBJECT:ActiveRow:Cells[oCol:Key]:Value),
                                        hCol:FORMAT) .
            
            hCol:MODIFIED = TRUE .

            ASSIGN lEditable = TRUE  .
        END.

        IF plApplyRowLeave AND lEditable THEN DO:
            APPLY "ROW-LEAVE":U TO hBrowse .

            THIS-OBJECT:BindingSource:Refresh () .

            IF THIS-OBJECT:ActiveRow:IsGroupByRow = FALSE THEN 
                THIS-OBJECT:ProcessColorCodingAndCalculatedFields (THIS-OBJECT:ActiveRow) .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Scrolls to the BindingSource's position                                                                      
        Notes:          
        @param plAllRegions Controls if all RowScrollRegions will be positioned to the current row of just the first one.                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ScrollToCurrentRow (plAllRegions AS LOGICAL):
 
        DEFINE VARIABLE oBindingSource AS Progress.Data.BindingSource NO-UNDO . 
        DEFINE VARIABLE i              AS INTEGER                     NO-UNDO .
        
        oBindingSource = THIS-OBJECT:BindingSource .  
 
        IF NOT VALID-OBJECT (oBindingSource) THEN 
            UNDO, THROW NEW Progress.Lang.AppError ("Unable to ScrollToCurrentRow without BindingSource.":U) . 
 
        IF plAllRegions = FALSE THEN
            THIS-OBJECT:DisplayLayout:RowScrollRegions[0]:ScrollPosition = THIS-OBJECT:BindingSource:Position . 
        ELSE DO:
            DO i = 0 TO THIS-OBJECT:DisplayLayout:RowScrollRegions:Count - 1:
                THIS-OBJECT:DisplayLayout:RowScrollRegions[i]:ScrollPosition = THIS-OBJECT:BindingSource:Position .
            END.
        END.

        THIS-OBJECT:Selected:Rows:Clear() .
        
        THIS-OBJECT:Rows[oBindingSource:Position]:Activate() .

    END METHOD.

    /*------------------------------------------------------------------------------
		Purpose: Selects the (temporary row in the browse widget)   																	  
		Notes:  																	  
	------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SelectBrowseRow ():
		
        DEFINE VARIABLE hBrowser   AS HANDLE          NO-UNDO.

        ASSIGN hBrowser = THIS-OBJECT:BrowseHandle .
        /* Mike Fechner, Consultingwerk Ltd. 24.01.2013
           Ensure, that the browser has a row selected */
        IF NOT hBrowser:READ-ONLY THEN DO ON ERROR UNDO, THROW:
            hBrowser:SELECT-ROW (1) .

            /* Mike Fechner / Consultingwerk Ltd. / WinKit 15 Aug 2013
               When the SELECT-ROW above has failed with "Browse widget 
               must contain at least one record before SELECT-ROW method is used. (2108)"
               some existing code may have removed that one (unbound) row in the Browse 
               widget that we require for the calculated fieldscommunication. It should 
               be o.k. to just create a new one. */
            CATCH err AS Progress.Lang.Error:
                IF err:GetMessageNum (1) = 2108 THEN DO:                       
                        
                    hBrowser:INSERT-ROW () .
                    hBrowser:SELECT-ROW (1) .
                        
                END.
                ELSE 
                    UNDO, THROW err .
            END CATCH.
        END. 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the row index where records positioned with the REPOSITION TO 
                 ROWID (or RECID) statement are displayed.
        Notes:   Currently not yet supports   
        @param piRowIndex Indicates the row number where the new record is displayed, 1 being the first row.
        @param pcRepositionMode ALWAYS or CONDITIONAL 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SetRepositionedRow (piRowIndex AS INTEGER,
                                           pcRepositionMode AS CHARACTER):
		
		IF NOT VALID-OBJECT (THIS-OBJECT:RepositionedRow) THEN 
		    THIS-OBJECT:RepositionedRow = NEW RepositionedRow ()  .

        CASE pcRepositionMode:
            WHEN "ALWAYS":U      THEN THIS-OBJECT:RepositionedRow:RepositionMode = RepositionModeEnum:Always .
            WHEN "CONDITIONAL":U THEN THIS-OBJECT:RepositionedRow:RepositionMode = RepositionModeEnum:Conditional .
          
            OTHERWISE 
                UNDO, THROW NEW Consultingwerk.Exceptions.InvalidValueException (pcRepositionMode, "Reposition Mode":U) .
        
        END CASE .
        
        ASSIGN THIS-OBJECT:RepositionedRow:RowIndex = piRowIndex. 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the comma delimited list of all field names in the browse 
                 widget
        Notes:   
        @return The comma delimited list of all field names in the browse
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER SetTableSchemaColumnsFromBrowse ():
        
        DEFINE VARIABLE hColumn AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hField  AS HANDLE    NO-UNDO .
        DEFINE VARIABLE iIndex  AS INTEGER   NO-UNDO.

        DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE j       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE n       AS INTEGER   NO-UNDO.

        DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

        DO i = 1 TO THIS-OBJECT:BrowseHandle:NUM-COLUMNS:
            
            ASSIGN hColumn = THIS-OBJECT:BrowseHandle:GET-BROWSE-COLUMN (i) 
                   hField = hColumn:BUFFER-FIELD  
                   iIndex = hColumn:INDEX . 
            
            IF NOT VALID-HANDLE (hField) THEN 
                NEXT . 
            
            IF iIndex > 0 THEN 
                ASSIGN cReturn = cReturn + SUBSTITUTE ("&1.&2[&3],":U,
                                                       hField:BUFFER-HANDLE:NAME,
                                                       hField:NAME,
                                                       iIndex) .
            ELSE     
                ASSIGN cReturn = cReturn + SUBSTITUTE ("&1.&2,":U,
                                                       hField:BUFFER-HANDLE:NAME,
                                                       hField:NAME) .
        END.
            
        RETURN TRIM (cReturn, ",":U) .    
          
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds a list of fields for the BindingSource when the BindingSourceFields
                 property is set to BindingSourceFieldsEnum:ListAndFields
        Notes:   
        @return The list of BindingSource fields 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER SetTableSchemaColumnsFromBrowseAndFields ():
        
        DEFINE VARIABLE cSourceFields AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i             AS INTEGER   NO-UNDO .

        ASSIGN cSourceFields = SetTableSchemaColumnsFromBrowse ().
          
        DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:BindingSourceFieldList):
            
            IF LOOKUP(ENTRY (i,THIS-OBJECT:BindingSourceFieldList), cSourceFields) = 0 THEN
                ASSIGN cSourceFields = cSourceFields + ",":U + 
                                       ENTRY (i,  THIS-OBJECT:BindingSourceFieldList).  
        END.
        
        RETURN cSourceFields . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stores the Column settings to the ISettingsService
        Notes:   This method should be called when settings should be explicitly 
                 stored or when te Parent Forms closes 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID StoreSettings ():
		
        DEFINE VARIABLE cSettings        AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE oSettings        AS UltraGridSettings NO-UNDO .
        DEFINE VARIABLE oSettingsService AS ISettingsService  NO-UNDO .
        
        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        /* Mike Fechner, Consultingwerk Ltd. 12.10.2010
           Save Grid Column settings, when enabled for the session and the SettingsKey is set
           and different from the GUID created in the constrcutor. */
        IF THIS-OBJECT:SettingsKey > "":U THEN .
        ELSE RETURN .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataSource) THEN RETURN .

        ASSIGN oSettings = NEW UltraGridSettings (THIS-OBJECT) 
               cSettings = oSettings:StoreColumnSettings() .

        oSettingsService:StoreSetting (THIS-OBJECT:SettingsKey, 
                                       "GridSettings":U,
                                       cSettings) .   

        CATCH err AS System.Exception:
            /* ignore */    
        END CATCH.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oSettings) .		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the current browse cell with the values from the Grid                                                                       
        Notes:   Used to make sure that the updatable ABL browse widget is up 
                 to date during event handlers                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SynchronizeCurrentColumn ():
        
        IF VALID-OBJECT (THIS-OBJECT:ActiveCell) AND 
           NOT THIS-OBJECT:ActiveCell:IsFilterRowCell AND
           THIS-OBJECT:ActiveCell:IsInEditMode THEN DO:
    
            THIS-OBJECT:PerformAction (UltraGridAction:ExitEditMode) .           
        END.
               
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Views and hides columns in the Grid based on the HIDDEN attribute
                 of the browsers columns 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SynchronizeVisibleColumns ():
		
		DEFINE VARIABLE hColumn    AS HANDLE    NO-UNDO .
		DEFINE VARIABLE i          AS INTEGER   NO-UNDO .
		DEFINE VARIABLE cColumnKey AS CHARACTER NO-UNDO .
		
		DO i = 1 TO hBrowse:NUM-COLUMNS:
		    ASSIGN hColumn = hBrowse:GET-BROWSE-COLUMN (i) 
		           cColumnKey = THIS-OBJECT:ColumnKeyFromColumnHandle (hColumn).
		    
		    IF THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Exists (cColumnKey) THEN DO:

                THIS-OBJECT:DisplayLayout:Bands[0]:Columns[cColumnKey]:Hidden = NOT hColumn:VISIBLE .  
		    END.
		END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds a QueryString predicate that can be used to (re)fetch a 
                 record based on equality of primary unique columns 
                 Based on the UniqueKeyDictionary values of predetermined unique key
                 columns
        @param phBuffer The Handle to the buffer to return the FIND predicate for
        @param poRow The reference to the Row collection that contains the values
        @return The FIND predicate
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER UniqueFindPredicate (phBuffer AS HANDLE,
                                                    poRow AS UltraGridRow):
        
        DEFINE VARIABLE iField       AS INTEGER       NO-UNDO .
        DEFINE VARIABLE cColumn      AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oValue       AS System.Object NO-UNDO . 
        DEFINE VARIABLE cValue       AS CHARACTER     NO-UNDO EXTENT .
        DEFINE VARIABLE cReturn      AS CHARACTER     NO-UNDO .
        
        FIND ttBufferInfo WHERE ttBufferInfo.RecordOwner  = THIS-OBJECT 
                            AND ttBufferInfo.BufferHandle = phBuffer
            NO-ERROR . 
        
        EXTENT (cValue) = MAX (9, NUM-ENTRIES (ttBufferInfo.UniqueKeys)) .
 
        DO iField = 1 TO NUM-ENTRIES (ttBufferInfo.UniqueKeys) ON ERROR UNDO, THROW:
            ASSIGN cColumn = ENTRY(iField, ttBufferInfo.UniqueKeys)   . 
                   
            IF poRow:Cells:Exists (cColumn) THEN
                ASSIGN oValue = poRow:Cells[cColumn]:Value
                       cValue[iField] = UNBOX (oValue).
            ELSE
                cValue[iField] = "?":U .
        END.  

        ASSIGN cReturn = ttBufferInfo.UniqueFind . 

        DO iField = 10 TO EXTENT (cValue):
            ASSIGN cReturn = REPLACE (cReturn, "&":U + STRING (iField), cValue[iField]) .
        END .
        
        ASSIGN cReturn = SUBSTITUTE (cReturn,
                                     cValue[1],
                                     cValue[2],
                                     cValue[3],
                                     cValue[4],
                                     cValue[5],
                                     cValue[6],
                                     cValue[7],
                                     cValue[8],
                                     cValue[9]) .     

        RETURN cReturn .
 
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Updates the Text property of the Grid based on the TITLE property 
                 of the Progress browser                                                                      
        Notes:   May be used in the triggerend.i include files.                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateTitle ():
        
        CASE hBrowse:TITLE:
            WHEN "":U OR WHEN ? OR WHEN "?":U THEN 
                THIS-OBJECT:Text = "":U . 
            OTHERWISE 
                THIS-OBJECT:Text = hBrowse:TITLE . 
        END CASE .  

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Verifies that the value of the Property ColorCodedColumnNames
                 matches the setting of UseFullFieldNames
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID VerifyColorCodedColumnNames ():

        DEFINE VARIABLE lUseFullFieldNames AS LOGICAL   NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iEntries           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cEntry             AS CHARACTER NO-UNDO .

        lUseFullFieldNames = BindingSourceHelper:UseFullFieldNames (THIS-OBJECT:BindingSource) .

        DO i = 1 TO NUM-ENTRIES (ColorCodedColumnNames):

            ASSIGN cEntry = ENTRY (i, ColorCodedColumnNames) .

            /* Mike Fechner, Consultingwerk Ltd. 13.02.2012
               When we are not prefixing the BindingSource fields with the
               buffer name, then all fields in the list should be referenced
               by the field nam only */
            IF lUseFullFieldNames = FALSE THEN DO:
                ASSIGN iEntries = NUM-ENTRIES (cEntry, ":":U) .

                IF iEntries > 1 THEN
                    ASSIGN cEntry = ENTRY (iEntries, cEntry, ":":U)
                           ENTRY (i, ColorCodedColumnNames) = cEntry .

                ASSIGN iEntries = NUM-ENTRIES (cEntry, ".":U) .

                IF iEntries > 1 THEN
                    ASSIGN cEntry = ENTRY (iEntries, cEntry, ".":U)
                           ENTRY (i, ColorCodedColumnNames) = cEntry .
            END.
            ELSE DO:
                /* Mike Fechner, Consultingwerk Ltd. 13.02.2012
                   Avoid typical mistake/pitfall: Column Name in 
                   ProBindingSource should be using : and not . */
                IF INDEX (cEntry, ".":U) > 0 THEN                 
                    ASSIGN cEntry                           = REPLACE (cEntry, ".":U, ":":U) 
                           ENTRY (i, ColorCodedColumnNames) = cEntry .
                
            END.
        END.

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Verifies that the value of the Property CustomCalculatedColumnNames 
                 matches the setting of UseFullFieldNames
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID VerifyCustomCalculatedColumnNames ():

        DEFINE VARIABLE lUseFullFieldNames AS LOGICAL   NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iEntries           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cEntry             AS CHARACTER NO-UNDO .

        lUseFullFieldNames = BindingSourceHelper:UseFullFieldNames (THIS-OBJECT:BindingSource) .

        DO i = 1 TO NUM-ENTRIES (CustomCalculatedColumnNames):

            ASSIGN cEntry = ENTRY (i, CustomCalculatedColumnNames) .

            /* Mike Fechner, Consultingwerk Ltd. 13.02.2012
               When we are not prefixing the BindingSource fields with the
               buffer name, then all fields in the list should be referenced
               by the field nam only */
            IF lUseFullFieldNames = FALSE THEN DO:
                ASSIGN iEntries = NUM-ENTRIES (cEntry, ":":U) .

                IF iEntries > 1 THEN
                    ASSIGN cEntry = ENTRY (iEntries, cEntry, ":":U)
                           ENTRY (i, CustomCalculatedColumnNames) = cEntry .

                ASSIGN iEntries = NUM-ENTRIES (cEntry, ".":U) .

                IF iEntries > 1 THEN
                    ASSIGN cEntry = ENTRY (iEntries, cEntry, ".":U)
                           ENTRY (i, CustomCalculatedColumnNames) = cEntry .
            END.
            ELSE DO:
                /* Mike Fechner, Consultingwerk Ltd. 13.02.2012
                   Avoid typical mistake/pitfall: Column Name in 
                   ProBindingSource should be using : and not . */
                IF INDEX (cEntry, ".":U) > 0 THEN                 
                    ASSIGN cEntry                           = REPLACE (cEntry, ".":U, ":":U) 
                           ENTRY (i, CustomCalculatedColumnNames) = cEntry .
                
            END.
        END.

    END METHOD.    
    
    /*------------------------------------------------------------------------------
        Purpose: Views and hides columns based on the browsers columns and sets
                 the column header captions                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ViewHideColumns ():
        
        DEFINE VARIABLE hColumn      AS HANDLE          NO-UNDO .
        DEFINE VARIABLE iHeaderLines AS INTEGER         NO-UNDO .
        DEFINE VARIABLE cColumnName  AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER         NO-UNDO .
        DEFINE VARIABLE iColumns     AS INTEGER         NO-UNDO .
        DEFINE VARIABLE cLabel       AS CHARACTER       NO-UNDO .    
        DEFINE VARIABLE iWidth       AS INTEGER         NO-UNDO .    
            
        {Consultingwerk/foreach.i UltraGridColumn oColumn in THIS-OBJECT:DisplayLayout:Bands[0]:Columns}

            IF LOOKUP (oColumn:Key, THIS-OBJECT:UnboundColumnNames) > 0 THEN
                oColumn:Hidden = FALSE .
            ELSE
                oColumn:Hidden = TRUE .
/*            oColumn:ExcludeFromColumnChooser = ExcludeFromColumnChooser:True .*/
        
        END.
        
        IF THIS-OBJECT:VisibleColumns > "":U THEN 
            ASSIGN iColumns = NUM-ENTRIES (THIS-OBJECT:VisibleColumns) . 
        ELSE 
            ASSIGN iColumns = hBrowse:NUM-COLUMNS .
        
        DO i = 1 TO iColumns:
            IF THIS-OBJECT:VisibleColumns > "":U THEN DO:
                ASSIGN cColumnName = ENTRY (i, THIS-OBJECT:VisibleColumns) 
                       cLabel      = ENTRY (i, THIS-OBJECT:ColumnLabels)
                       iWidth      = INTEGER (ENTRY (i, THIS-OBJECT:ColumnWidths))
                       NO-ERROR .
            END.
            ELSE DO:
                ASSIGN hColumn = hBrowse:GET-BROWSE-COLUMN (i) 
                       cLabel  = hColumn:LABEL
                       iWidth  = hColumn:WIDTH-PIXELS .
    
                ASSIGN cColumnName = THIS-OBJECT:ColumnKeyFromColumnHandle (hColumn) .
    
                IF LOOKUP (STRING (hColumn), THIS-OBJECT:UnboundColumnHandles) > 0 THEN
                    cColumnName = ENTRY (LOOKUP (STRING (hColumn), THIS-OBJECT:UnboundColumnHandles),
                                         THIS-OBJECT:UnboundColumnNames) .
            END.

            IF NOT THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Exists (cColumnName) THEN
                NEXT .

            oColumn = THIS-OBJECT:DisplayLayout:Bands[0]:Columns [cColumnName] .

            oColumn:Header:Caption = REPLACE(cLabel, "!":U, "~r~n":U) .

            IF iWidth <> ? THEN 
                oColumn:Width = iWidth .
                   
            oColumn:Header:VisiblePosition = i - 1 .
            oColumn:Hidden = FALSE .
/*            oColumn:ExcludeFromColumnChooser = ExcludeFromColumnChooser:False .*/

            iHeaderLines = MAX (iHeaderLines, NUM-ENTRIES (cLabel, "!":U)) .
        END.

        IF iHeaderLines > 1 THEN 
            THIS-OBJECT:DisplayLayout:Bands[0]:ColHeaderLines = iHeaderLines .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the RenderedBrowseControl class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC RenderedBrowseControl ():

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        FOR EACH ttBufferInfo WHERE ttBufferInfo.RecordOwner = THIS-OBJECT:
            DELETE ttBufferInfo . 
        END.

        /* Mike Fechner, Consultingwerk Ltd. 06.07.2011
           Remove from List of Overlay Controls*/
        IF VALID-OBJECT (oEmbeddedWindowForm) AND VALID-OBJECT (oEmbeddedWindowForm:OverlayControls) THEN    
            IF oEmbeddedWindowForm:OverlayControls:ContainsKey (STRING (THIS-OBJECT:BrowseHandle)) THEN 
                oEmbeddedWindowForm:OverlayControls:Remove (STRING (THIS-OBJECT:BrowseHandle)) .

        IF VALID-HANDLE (hAlternativeQuery) THEN 
            DELETE OBJECT hAlternativeQuery . 

        IF EXTENT (hAlternativeQueryBuffer) > 0 THEN 
            DO i = 1 TO EXTENT (hAlternativeQueryBuffer):
                IF VALID-HANDLE (hAlternativeQueryBuffer[i]) THEN 
                    DELETE OBJECT hAlternativeQueryBuffer[i] .
            END. 

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

        IF VALID-HANDLE (hEventHandlerProcedure) THEN 
            DELETE PROCEDURE hEventHandlerProcedure .

        IF VALID-HANDLE (hEventHandlerProcedure2) THEN 
            DELETE PROCEDURE hEventHandlerProcedure2 .

    END DESTRUCTOR.

END CLASS.
