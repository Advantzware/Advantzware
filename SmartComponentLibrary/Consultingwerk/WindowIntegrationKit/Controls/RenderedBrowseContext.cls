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
    File        : RenderedBrowseContext
    Purpose     : Parameter class for the constructor of the 
                  RenderedBrowseControl class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 14 12:24:09 CET 2012
    Notes       : Simplifies extension and customization as only a single 
                  parameter is requred to the RenderedBrowseControl constructor    
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Forms.*    FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseContext: 
        
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
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the RenderedBrowseControl will APPLY the ITERATION-CHANGED
                 event together with the VALUE-CHANGED Event
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ApplyIterationChanged AS LOGICAL NO-UNDO INIT FALSE 
	GET.
	SET. 

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
    SET. 

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
        Purpose: Gets and sets if the instance to the RenderedBrowseControl is using Batching
        Notes:   Tied to the MaxDataGuess property of the bound Progress.Data.BindingSource
                 components
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY Batching AS LOGICAL NO-UNDO 
    GET.
    SET. 
            
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
        Purpose: Gets and sets the handle of the original browse widget
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY BrowseHandle AS HANDLE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the handle of the Procedure to use for call-backs 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY CallBackProcedure AS HANDLE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets a list of column keys to be used for the automatic processing
                 of color coding (ROW-DISPLAY Trigger)                                                                     
        Notes:   Pass in "*" to process all columns                                                                     
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY ColorCodingColumnNames AS CHARACTER NO-UNDO 
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
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the widths of the Visible Columns as an override to 
                 the currnetly visible browse columns
        Notes:   Used in RenderedBrowseControl:ViewHideColumns in combination with  
                 VisibleColumns                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnWidths AS CHARACTER NO-UNDO INIT ?
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Get's and sets if the CursorKeyNavigation during update is used
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY CursorKeyNavigationInEdit AS CursorKeyNavigationInEditEnum NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets a list of column keys to be used for the additional 
                 automatic processing of of calculated fields (ROW-DISPLAY Trigger)                                                                     
        Notes:   "*" to process all columns IS NOT SUPPORTED!      
                 By default only "unbound" columns in the UltraGrid are handled as 
                 calcuolated columns. Use this property when regular query columns
                 for database or temp-tables are used as a placeholder for a calculated
                 field
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY CustomCalculatedColumnNames AS CHARACTER NO-UNDO 
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
        Purpose: Gets and sets the inital value for the Dock property of the grid 
        Notes:   Allows to avoid need for repositioning the Grid during initalization
                 Only applied, when a valid value is assigned 
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Dock AS System.Windows.Forms.DockStyle NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets of the Browser has dynamic columns
        Notes:   Dynamic columns need to be stored and restored by the RenderedBrowseControl
                 because the browser will loose those columns when the browsers 
                 Query is replaced
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DynamicColumns AS LOGICAL NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the IEmbeddedWindowForm that should
                 contain the RenderedBrowseControl
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY EmbeddedWindowForm AS IEmbeddedWindowForm NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name that should be set to the RenderedBrowseControl
                 instance 
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY GridName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if search capabilities should be enabled for the 
                 current browse instance. 
        Notes:   This property is not passed to the actual RenderedBrowseControl 
                 instance. However it does allow the calling program to know if for 
                 the current browser an RenderedBrowseControl or 
                 RenderedBrowseWithSearchControl instance should be created. Typically
                 assigned by a callback in the actual ABL GUI program. 
    ------------------------------------------------------------------------------*/    
	DEFINE PUBLIC PROPERTY EnableSearch AS LOGICAL NO-UNDO INIT FALSE  
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: MaxDataGuess value for the RenderedBrowseControl instance, used only
                 when batching                                                                     
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
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the .NET Control that should be used
                 as the parent for the RenderedBrowseControl instance
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY Parent AS System.Windows.Forms.Control NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets key used as the base key when getting user settings 
                 from the ISettingsService implementation
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY SettingsKey AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the names of the browse columns that are supposed to
                 be visible as an override to the currently visible browse columns
        Notes:   See RenderedBrowseControl:ViewHideColumns                                                                       
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY VisibleColumns AS CHARACTER NO-UNDO INIT ?
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RenderedBrowseContext class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RenderedBrowseContext ():
		SUPER ().
		
        ASSIGN  
            THIS-OBJECT:ApplyFunctionKeyEvents    = ApplyFunctionKeyEventsEnum:ToBrowse 
            THIS-OBJECT:CursorKeyNavigationInEdit = CursorKeyNavigationInEditEnum:UpDownLeftRight 
            THIS-OBJECT:MaxDataGuess              = WinKitControls:DefaultMaxDataGuess 
            .
        
	END CONSTRUCTOR.

END CLASS.