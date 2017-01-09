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
   File        : SmartBrowserControl
   Purpose     : Base class for the Browsers/Grids in the SmartComponent 
                 Library framework
   Syntax      : 
   Description : This class is not intended to be used as an instance on
                 any screen. There are classes deriving from this class 
                 (SmartDataBrowser) which should be used
   Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
   Created     : Mon Feb 16 15:26:16 CET 2009
   Notes       : 
 ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                /* FROM PROPATH */ .
USING Consultingwerk.SmartComponents.Base.*              FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* .   /* FROM PROPATH is not possible here, then the .Interfaces.Design would not be accessible */
USING Consultingwerk.SmartComponents.Interfaces.Design.* FROM ASSEMBLY . 
USING Consultingwerk.SmartComponents.Implementation.*    FROM PROPATH .
USING Consultingwerk.SmartComponents.Resources.*         FROM PROPATH .  
USING Consultingwerk.Util.*                              FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                    FROM ASSEMBLY .
USING System.ComponentModel.*                            FROM ASSEMBLY .
USING System.ComponentModel.Design.*                     FROM ASSEMBLY .
USING Progress.Lang.*                                    FROM PROPATH .
 
{Consultingwerk/products.i} 
 
CLASS Consultingwerk.SmartComponents.Base.SmartBrowserControl 
    INHERITS UltraGrid  
    IMPLEMENTS ISmartDataTarget, ICheckOkToClose, IAfterRetrieveDataSubscriber,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataTarget:

    DEFINE VARIABLE oBindingSource                     AS Progress.Data.BindingSource                          NO-UNDO . 
    DEFINE VARIABLE oSmartDataSource                   AS ISmartDataSource                                     NO-UNDO .
    DEFINE VARIABLE lBindingSourceEventsSubscribed     AS LOGICAL                                              NO-UNDO INIT FALSE . 
    
    DEFINE VARIABLE oColumnsList                       AS "System.Collections.Generic.List<UltraGridColumn>":U NO-UNDO . 

    DEFINE VARIABLE lInited                            AS LOGICAL                                              NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the viewer has successfully deleted a record.
        Notes:   This event is defined using a .NET delegate, so it will show in 
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterDeleteRecord DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired when the Viewer starts deleting a record.
                 This event is cancellable
        Notes:   This event is defined using a .NET delegate, so it will show in 
                 the PropertyGrid of the VisualDesigner
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeDeleteRecord DELEGATE System.ComponentModel.CancelEventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Raised when the ListChanged event of the BindingSource of the SmartBrowserControl
                 is raised
        Notes:   
        @param sender The object that raised the SmartBrowserListChanged event
        @param e The SmartBrowserListChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SmartBrowserListChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                e AS SmartBrowserListChangedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Propery flags if the currently selected Rows will be deselected 
                 before activating a new row. The default is TRUE. 
        Notes:   See BeforeRowActivateHandler for details                                                                     
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DeselectRowsOnActivate AS LOGICAL NO-UNDO INIT TRUE 
    GET.
    SET.
     
    /*------------------------------------------------------------------------------
        Purpose: Propery flags if the current instance of the SmartBindingSource
                 is running at design time or runtime.                                                                        
        Notes:   Set during constructor using a check at the 
                 System.ComponentModel.LicenseManager                                                                     
    ------------------------------------------------------------------------------*/        
    DEFINE PROTECTED PROPERTY DesignTime AS LOGICAL NO-UNDO 
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
       Purpose: Indicated that the SmartDataAdapter is currrently being 
                Initialized
       Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY Initializing AS LOGICAL NO-UNDO INIT FALSE  
    GET.
    PRIVATE SET.        
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBrowserControl classs 
        Notes:   Subscribes handlers for events.   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBrowserControl ():
        
        SUPER().

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
            System.ComponentModel.LicenseUsageMode:Designtime) .         

        THIS-OBJECT:InitializeLayout:Subscribe(InitializeLayoutHandler).
        THIS-OBJECT:BeforeRowActivate:Subscribe(BeforeRowActivateHandler).

        /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
           Only subscribe to Disposed event when not in Visual Designer 
           to avoid conflicts when re-opening layouts. */        
        IF NOT THIS-OBJECT:DesignTime THEN 
            THIS-OBJECT:Disposed:Subscribe (DisposedHandler) .        
                
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartDataSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
                SCL-725 - code moved from LinkDataSource.i directly into class file
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkDataSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource NO-UNDO 
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) THEN 
            RETURN CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) . 
    END GET . 
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartDataSource) THEN 
            ASSIGN THIS-OBJECT:SmartDataSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartDataSource) .         
    END.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the FormDestroyed evengt of the containing Form has been
                 subscribed                                                                
        Notes:                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PROTECTED PROPERTY ParentFormDestroyed AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the FormDestroyed evengt of the containing Form has been
                 subscribed                                                                
        Notes:                                                                        
    ------------------------------------------------------------------------------*/       
    DEFINE PROTECTED PROPERTY ParentFormDestroyedSubscribed AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Get/Set the SmartDataSource of the current Class. The SmartDataSource 
                 can be set to any class implementing the Interface ISmartDataSource.
                 This Property is set by the developer when initializing a Control 
                 implementing the Interface ISmartDataTarget.
                 Registers the SmartDataBrowser instance with the SmartDataSource 
                 (using the RegisterSmartDataTarget method in ISmartDataSource). 
                 When the browser is already attached to a BindingSource (in the Form 
                 Design), the bindingSource is introduced to the SmartDataSource by 
                 assigning it to the BindingSource property.
        Notes:   Enforced by Interface ISmartDataTarget.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataSource AS ISmartDataSource NO-UNDO 
    GET():
        RETURN oSmartDataSource .        
    END GET.
    SET (INPUT arg AS ISmartDataSource):
        DEFINE VARIABLE lAttachToAdapterBindingSource AS LOGICAL NO-UNDO INIT FALSE .

        /* Mike Fechner, Consultingwerk Ltd. 27.12.2009
           During Initialization of the Container (Form), just keep the 
           reference to the SmartDataSource, EndInit will set this property again
           to invoke binding etc. */           
        IF THIS-OBJECT:Initializing THEN 
            ASSIGN oSmartDataSource = arg . 

        ELSE 
        DO:    
            IF VALID-OBJECT (arg) THEN 
            DO:   
                oSmartDataSource = arg.        
            
                IF VALID-OBJECT (THIS-OBJECT:DataSource) THEN 
                DO:
                    IF TYPE-OF (THIS-OBJECT:DataSource, Progress.Data.BindingSource) THEN 
                        oBindingSource = CAST(THIS-OBJECT:DataSource, Progress.Data.BindingSource).
                END.
                ELSE 
                    /* set flag that we will be using the BindingSource of the DataAdapter (if present) */ 
                    ASSIGN lAttachToAdapterBindingSource = TRUE .         
    
                oSmartDataSource:RegisterSmartDataTarget(THIS-OBJECT).

                oSmartDataSource:AfterQueryPositionChanged:Subscribe (AfterQueryPositionChangedHandler) .    
                oSmartDataSource:BeginUpdateState:Subscribe (BeginUpdateStateHandler) .
                oSmartDataSource:EndUpdateState:Subscribe (EndUpdateStateHandler) .
        
                IF TYPE-OF (oSmartDataSource, ISmartFilterTarget) THEN 
                    CAST (oSmartDataSource, ISmartFilterTarget):CollectFilterValues:Subscribe (CollectFilterValuesHandler) .
        
                /* use the bindingsource of the DataAdapter if there was no design time bindingsource */
                IF lAttachToAdapterBindingSource = TRUE THEN DO: 
                    THIS-OBJECT:DataSource = oSmartDataSource:BindingSource .
                    
                    THIS-OBJECT:RegisterBindingSourceEvents () .
                END.
                
                IF TYPE-OF (arg, SmartDataAdapter) THEN 
                    CAST(arg, SmartDataAdapter):SubscribeAfterRetrieveData (THIS-OBJECT) .    
                
            END.
            ELSE DO:
                IF VALID-OBJECT (oSmartDataSource) THEN DO: 
                    oSmartDataSource:DeregisterSmartDataTarget (THIS-OBJECT) . 
            
                    oSmartDataSource:AfterQueryPositionChanged:Unsubscribe (AfterQueryPositionChangedHandler) .
                    oSmartDataSource:BeginUpdateState:Unsubscribe (BeginUpdateStateHandler) .                           
                    oSmartDataSource:EndUpdateState:Unsubscribe (EndUpdateStateHandler) .      
                    
                    IF TYPE-OF (oSmartDataSource, ISmartFilterTarget) THEN 
                        CAST (oSmartDataSource, ISmartFilterTarget):CollectFilterValues:Unsubscribe (CollectFilterValuesHandler) .
                END.
        
                oSmartDataSource = ? . 
            END.
        END.
    END SET.    

    /*------------------------------------------------------------------------------
        Purpose: Activates the current query row (on add and delete)                                                                         
        Notes:   Event handler for the AfterQueryPositionChanged event of the ISmartDataSource
        @param sender The reference to object that raised this event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterQueryPositionChangedHandler (sender AS System.Object,
                                                            e AS System.EventArgs):
        
        DEFINE VARIABLE iRow AS INTEGER NO-UNDO.
        
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND 
           VALID-OBJECT (THIS-OBJECT:SmartDataSource:BindingSource) AND 
           THIS-OBJECT:SmartDataSource:BindingSource:Position >= 0 THEN  

             /* Mike Fechner, Consultingwerk Ltd. 11.05.2015
                SCL-806: When group-by is used, the Rows[Position] may fail, because it
                would refer to the summary rows only */
             THIS-OBJECT:Rows:GetRowWithListIndex (THIS-OBJECT:SmartDataSource:BindingSource:Position, TRUE):Activate() .
     
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked as a callback from DataAdapter, after Data has been retrieve.                                                                                
        Notes:   Overridden in SmartDataBrowser to call FormatColumns ()
        @param sender The reference to the ISmartDataSource that invoked this callback                                                                         
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID AfterRetrieveDataCallback (sender AS Consultingwerk.SmartComponents.Interfaces.ISmartDataSource):
        
        /* NOOP */

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeginUpdateState event of the data source                                                                         
        Notes:                                                                  
        @param sender The reference to the object that raised this event
        @param e The DataSourceUpdateStateEventArgs instance with the data for this event       
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BeginUpdateStateHandler (sender AS System.Object,
                                                   e AS DataSourceUpdateStateEventArgs):
        
        DisableForUpdate (e:UpdatingDataTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handles the ListChanged event of the BindingSource 
        Notes:   
        @param sender The reference to the object that raised the ListChanged event
        @param e The ListChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID BindingSourceListChangedHandler (sender AS System.Object,
	                                                       e AS System.ComponentModel.ListChangedEventArgs):

        IF VALID-OBJECT (THIS-OBJECT:DataSource) THEN 
	
    	    OnSmartBrowserListChanged (NEW SmartBrowserListChangedEventArgs (CAST (THIS-OBJECT:DataSource, Progress.Data.BindingSource):Count,
    	                                                                     FALSE)) .	

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CollectFilterValues event of the ISmartFilterTarget
        Notes:   
        @param sender The object that raised the CollectFilterValues event
        @param e The CollectFilterValuesEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CollectFilterValuesHandler (sender AS Progress.Lang.Object, 
                                                      e AS CollectFilterValuesEventArgs):
        
        /* NOOP */

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Dummy method invoked from ICustomTypeDescriptor                                                                        
        Notes:   Overridden in the SmartDataBrowser class                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID CreateVerbs ():
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deregisters events of the BindingSource 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID DeregisterBindingSourceEvents ():
		
        IF lBindingSourceEventsSubscribed AND VALID-OBJECT (THIS-OBJECT:DataSource) THEN 
            CAST (THIS-OBJECT:DataSource, Progress.Data.BindingSource):ListChanged:UNSUBSCRIBE (BindingSourceListChangedHandler) .

        ASSIGN lBindingSourceEventsSubscribed = FALSE . 

	END METHOD .

    /*------------------------------------------------------------------------------
       Purpose: Disable the Controls during an Update.
                This Method is called by the SmartDataAdapter by a call to 
                BeginUpdateState () which is defined in the Interface ISmartDataSource.
       Notes:   Enforced by Interface ISmartDataTarget.
       @param poSmartDataTarget The ISmartDataTarget instance that is in update mode
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID DisableForUpdate (poSmartDataTarget AS ISmartDataTarget):
    
        THIS-OBJECT:Enabled = FALSE.
        
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Disposed event of the SmartBrowserControl                                                                      
        Notes:                                                                  
        @param sender The reference to the object that raised the Disposed event
        @param e The System.EventArgs with the data for this event       
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID DisposedHandler (sender AS System.Object, 
                                           e AS System.EventArgs):            
                   
        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        IF VALID-OBJECT (oColumnsList) THEN 
        DO:
            DO i = oColumnsList:Count - 1 TO 0 BY -1:
                IF VALID-OBJECT (oColumnsList[i]) THEN
                    DELETE OBJECT oColumnsList[i] .
            END.

            DELETE OBJECT oColumnsList .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Enable the Controls after an Update.
                 This Method is called by the SmartDataAdapter when setting the 
                 SmartDataSourceState Property to Navigatable.
                 Enforced by Interface ISmartDataTarget.
        Notes:   Use this only in a Browser!
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID EnableAfterUpdate():
        
        THIS-OBJECT:Enabled = TRUE.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EndUpdateState event of the data source
        Notes:                                                                
        @param sender The object that raised the EndupdateState event
        @param e The DataSourceUpdateStateEventArgs with the data for this event          
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EndUpdateStateHandler (sender AS System.Object,
                                                 e AS DataSourceUpdateStateEventArgs):
        
        EnableAfterUpdate () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Updateable Browsers are not supported by this class but the Method 
                 is enforced by the Interface ISmartDataTarget.
        Notes:   
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID EvaluateTableIOState ():        
        /* NOOP */
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the InizializeLayout event of the UltraGrid (parent
                 class). 
        Notes:   This Method hides columns where the name starts with _ from Grid and 
                 ColumnChooser. All Bands and Columns are processed.
        @param sender The reference to the object that raised the event
        @param e The InitializeLayoutEventArgs with the data for this event   
    ------------------------------------------------------------------------------*/    
    METHOD PRIVATE VOID InitializeLayoutHandler (sender AS System.Object, 
                                                 e AS InitializeLayoutEventArgs):

        DEFINE VARIABLE i     AS INTEGER       NO-UNDO.
        DEFINE VARIABLE j     AS INTEGER       NO-UNDO.

        DEFINE VARIABLE cBindingSourceCaption AS CHARACTER NO-UNDO.
        DEFINE VARIABLE oTableDesc            AS Progress.Data.TableDesc      NO-UNDO . 
        DEFINE VARIABLE oColumnDesc           AS Progress.Data.ColumnPropDesc NO-UNDO . 
        
        /* Mike Fechner, Consultingwerk Ltd. 22.12.2015
           SCL-1055: Workaround malicius code / .resx file generation for grid column header
           (labels). When the Grid columns header caption is not changed and thus is the same
           as the BindingSource's ColumnLabel for the given column, the source code and the .resx
           file would not spell out the label for the grid column. Consequence is, that unmodified
           grid column labels would not be translatable using the resource file based 
           localization. To work around this we add a space to unmodified column header captions 
           when in the visual designer and thus causing the property label to be written to the 
           .resx file and be available for translation. */
        &IF DEFINED (AddSpaceToDefaultGridColumnHeaders) NE 0 &THEN       
        IF THIS-OBJECT:DesignTime AND 
           VALID-OBJECT (THIS-OBJECT:DataSource) AND 
           TYPE-OF (THIS-OBJECT:DataSource, Progress.Data.BindingSource) THEN DO:
               
            {Consultingwerk/foreach.i UltraGridBand oBand in this-object:DisplayLayout:Bands}
                
                oTableDesc = BindingSourceHelper:FindTableDesc (CAST (THIS-OBJECT:DataSource, Progress.Data.BindingSource),
                                                                oBand:Key) .
            
                IF NOT VALID-OBJECT (oTableDesc) THEN 
                    NEXT . 
            
                {Consultingwerk/foreach.i UltraGridColumn oColumn in oBand:Columns}
                
                    oColumnDesc = BindingSourceHelper:FindColumnPropDesc (oTableDesc, oColumn:Key) .
                
                    ASSIGN cBindingSourceCaption = oColumnDesc:ColumnLabel .
        
                    IF COMPARE (cBindingSourceCaption, 
                                "=":U, 
                                oColumn:Header:Caption, 
                                CompareStrengthEnum:Raw:ToString()) THEN
                                 
                        oColumn:Header:Caption = SUBSTITUTE ("&1 ":U, cBindingSourceCaption) .
                END.
            END.
        END.
        &ENDIF

        DO i = 0 TO THIS-OBJECT:DisplayLayout:Bands:Count - 1:
            DO j = 0 TO THIS-OBJECT:DisplayLayout:Bands[i]:Columns:Count - 1:
                IF THIS-OBJECT:DisplayLayout:Bands[i]:Columns[j]:Key BEGINS "_":U THEN
                    ASSIGN
                        THIS-OBJECT:DisplayLayout:Bands[i]:Columns[j]:Hidden                   = TRUE
                        THIS-OBJECT:DisplayLayout:Bands[i]:Columns[j]:ExcludeFromColumnChooser = ExcludeFromColumnChooser:True 
                        .
            END.
        END.
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeRowActivateHandler 
        Notes:   Deselect all rows before a new one is selected. 
                 Event handler for the BeforeRowActivate event of the UltraGrid 
                 (parent class).
        @param sender The reference to the object that raised the event
        @param e The RowEventArgs with the data for this event   
    ------------------------------------------------------------------------------*/    
    METHOD PRIVATE VOID BeforeRowActivateHandler (sender AS System.Object, 
                                                  e AS Infragistics.Win.UltraWinGrid.RowEventArgs):

        IF THIS-OBJECT:DeselectRowsOnActivate THEN 
            THIS-OBJECT:Selected:Rows:Clear().
            
    END METHOD.
      
    /*------------------------------------------------------------------------------
        Purpose: This Method is a dispatcher to call DeleteRow inside of the 
                 SmartDataSource of the current Class.
                 It is called by the SmartTableIOSources SmartToolbarController or
                 SmartToolStripContainer if the tool to delete a record is pressed
                 and by the SmartDataBrowser if the AfterRowsDeleted Event fires. 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DeleteRecord ():

        DEFINE VARIABLE oDialogResult AS Consultingwerk.Framework.Enum.DialogResultEnum NO-UNDO . 
        DEFINE VARIABLE cTitle        AS CHARACTER                                      NO-UNDO .
        
        /* Marko Rüterbories, Consultingwerk Ltd. 19.08.2010
           Bug 2412: SmartUpdatableBrowser: When deleting a record the
                     BeforeDeleteRecord and AfterDeleteRecord Events are not
                     published
           
           Publish the BeforeDeleteRecord Event */
        /* Mike Fechner, Consultingwerk Ltd. 19.08.2010
           Bug 2412: Moved from SmartUpdatableBrowser to SmartBrowserControl.
                     more generic use here */
        DEFINE VARIABLE e AS System.ComponentModel.CancelEventArgs NO-UNDO .
        e = NEW System.ComponentModel.CancelEventArgs () .
    
        OnBeforeDeleteRecord (e) .
                
        IF VALID-OBJECT (e) AND e:Cancel THEN 
            RETURN .  

        IF SmartComponentLibraryCustomizer:SmartViewerControl_DeletePrompt > "":U THEN DO:
            ASSIGN cTitle = THIS-OBJECT:FindForm():Text NO-ERROR . 
                    
            IF cTitle > "":U THEN . 
            ELSE 
                ASSIGN cTitle = "":U .                        
                    
            oDialogResult = Consultingwerk.Util.MessageFormHelper:AskQuestion (SmartComponentLibraryCustomizer:SmartViewerControl_DeletePrompt,
                                                                               cTitle) .
            IF oDialogResult <> Consultingwerk.Framework.Enum.DialogResultEnum:DialogResultYes THEN 
                RETURN .                                                                 
        END.

        IF VALID-OBJECT(oSmartDataSource) THEN
            oSmartDataSource:DeleteRow ().
            
        /* Marko Rüterbories, Consultingwerk Ltd. 19.08.2010
           Bug 2412: SmartUpdatableBrowser: When deleting a record the
                     BeforeDeleteRecord and AfterDeleteRecord Events are not
                     published
           
           Publish the AfterDeleteRecord Event */
        /* Mike Fechner, Consultingwerk Ltd. 19.08.2010
           Bug 2412: Moved from SmartUpdatableBrowser to SmartBrowserControl.
                     more generic use here */
        OnAfterDeleteRecord (System.EventArgs:Empty) .
            
    END METHOD.
 
    /*------------------------------------------------------------------------------
        Purpose: This Method attaches a new BindingSource given as the Parameter 
                 poBindingSource to the Objects of the current Class. The new 
                 BindingSource may be created by a DataAdapter or borrowed from a 
                 Browser.
                 It is called by the SmartDataAdapter Class when creating a new 
                 BindingSource or when a new SmartDataSource is registered to the
                 Class implementing the Interface ISmartDataTarget.
        Notes:   Enforced by Interface ISmartDataTarget.
        @param poBindingSource The BindingSource to be attached to the Grids DataSource Property.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AttachBindingSource (poBindingSource AS Progress.Data.BindingSource):
        
        /* Mike Fechner, Consultingwerk Ltd. 10.02.2011
           Bug 2583: Only use DataAdapter's bindingSource, if valid object*/
        IF NOT VALID-OBJECT (THIS-OBJECT:DataSource) AND VALID-OBJECT (poBindingSource) THEN DO: 
            THIS-OBJECT:DataSource = poBindingSource.
        
            RegisterBindingSourceEvents () .
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Check if it is ok to close the parent form.
        Notes:   This class does always return TRUE, as a read-only browser has no
                 reason to prevent closing the parent form
        @param plShowMessage When TRUE a message is Shown explaining why it is not ok to close the parent form
        @param poCloseReason The System.Windows.Forms.CloseReason passed from then OnFormClosing Method of the parent form giving the reason why the form is closed
        @return Logical value indicating if the parent Form may be closed
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC LOGICAL IsOkToClose (plShowMessage AS LOGICAL, 
                                       poCloseReason AS System.Windows.Forms.CloseReason):
        
        RETURN TRUE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:  Raises the AfterDeleteRecord event                                                                    
        Notes:                                      
        @param e The System.EventArgs with the data for this event                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterDeleteRecord (e AS System.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:Empty .
            
        THIS-OBJECT:AfterDeleteRecord:Publish (THIS-OBJECT, e) .     

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeDeleteRecord event                                                                     
        Notes:                                      
        @param e The CancelEventArgs with the data for this event                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBeforeDeleteRecord (e AS System.ComponentModel.CancelEventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            UNDO, THROW NEW AppError ("This event needs a valid CancelEventArgs object.", 0) .
            
        THIS-OBJECT:BeforeDeleteRecord:Publish (THIS-OBJECT, e) .     

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Invoked at the beginning of a Containers InitializeComponents 
                 method                                                       
        Notes:   Enforced by System.ComponentModel.ISupportInitialize           
                 http://msdn.microsoft.com/de-de/library/system.componentmodel.isupportinitialize.begininit(VS.80).aspx                                                   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnBeginInit ():
        
        ASSIGN THIS-OBJECT:Initializing = TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked at the end of a Containers InitializeComponents method                                                       
        Notes:   Enforced by System.ComponentModel.ISupportInitialize           
                 http://msdn.microsoft.com/de-de/library/system.componentmodel.isupportinitialize.endinit(VS.80).aspx                                                 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnEndInit ():
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO. 
        DEFINE VARIABLE j AS INTEGER NO-UNDO. 
    
        SUPER:OnEndInit () .
        
        /* Prevent multiple execuitions on OnEndInit when Visual Validation is used */
        IF NOT lInited THEN 
            ASSIGN lInited = TRUE . 
        ELSE
            RETURN .  
        
        oColumnsList = NEW "System.Collections.Generic.List<UltraGridColumn>":U () . 

        DO i = 0 TO THIS-OBJECT:DisplayLayout:BandsSerializer:Count - 1:
            DO j = 0 TO THIS-OBJECT:DisplayLayout:BandsSerializer[i]:Columns:Count - 1:
                oColumnsList:Add (THIS-OBJECT:DisplayLayout:BandsSerializer[i]:Columns[j]) .
            END.    
        END.
        
        ASSIGN 
            THIS-OBJECT:Initializing = FALSE . 
       
        IF VALID-OBJECT (oSmartDataSource) THEN 
            ASSIGN THIS-OBJECT:SmartDataSource = oSmartDataSource .
    
        THIS-OBJECT:RegisterBindingSourceEvents () .
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Enter event                                                                        
        Notes:   
        @param e The System.EventArgs with the data for this event                                                                     
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnEnter (e AS System.EventArgs):
        
        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO . 
        
        SUPER:OnEnter (INPUT e).
        
        oForm = THIS-OBJECT:FindForm() .
        
        IF VALID-OBJECT (oForm) AND TYPE-OF (oForm, ISmartWindowForm) THEN  
            CAST (oForm, ISmartWindowForm):ActivateSmartControl (THIS-OBJECT) .
        
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the HandleCreated event
        Notes:   
        @param e The System.EventArgs with the data for this control
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnHandleCreated (e AS System.EventArgs):
		
		DEFINE VARIABLE oParentForm AS System.Windows.Forms.Form NO-UNDO .
		
		SUPER:OnHandleCreated (e) .

        oParentForm = THIS-OBJECT:FindForm() .

        IF NOT THIS-OBJECT:ParentFormDestroyedSubscribed AND
           VALID-OBJECT (oParentForm) AND
           TYPE-OF (oParentForm, ISmartWindowForm) THEN DO:

            /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
               Only subscribe to ParentFormDestroyedHandler event when not in Visual Designer
               to avoid conflicts when re-opening layouts. */
            IF NOT THIS-OBJECT:DesignTime THEN
                CAST (oParentForm, ISmartWindowForm):FormDestroyed:Subscribe (ParentFormDestroyedHandler) .

            ASSIGN ParentFormDestroyedSubscribed = TRUE .
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the SmartBrowserListChanged
        Notes:   
        @param e The SmartBrowserListChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSmartBrowserListChanged (e AS SmartBrowserListChangedEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "SmartBrowserListChanged":U) .
     
        THIS-OBJECT:SmartBrowserListChanged:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the FormDestroyed event of the owning Form
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParentFormDestroyedHandler (sender AS System.Object,
                                                      e AS System.EventArgs):

        IF VALID-OBJECT (sender) THEN 
            CAST (sender, ISmartWindowForm):FormDestroyed:Unsubscribe (ParentFormDestroyedHandler) .

        ASSIGN THIS-OBJECT:ParentFormDestroyed = TRUE . 

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Registers events of the BindingSource 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID RegisterBindingSourceEvents ():

		IF NOT lBindingSourceEventsSubscribed AND VALID-OBJECT (THIS-OBJECT:DataSource) THEN 
		    CAST (THIS-OBJECT:DataSource, Progress.Data.BindingSource):ListChanged:Subscribe (BindingSourceListChangedHandler) .

        ASSIGN lBindingSourceEventsSubscribed = TRUE . 

	END METHOD .

END CLASS.
