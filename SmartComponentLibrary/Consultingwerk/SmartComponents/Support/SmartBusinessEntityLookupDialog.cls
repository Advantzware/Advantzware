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
    File        : SmartBusinessEntityLookupDialog
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Apr 07 08:34:51 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.* FROM PROPATH . 
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.* FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY .
USING Progress.Lang.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Support.SmartBusinessEntityLookupDialog 
    INHERITS SmartBusinessEntityLookupDialogDesigner
    IMPLEMENTS IDefaultActionSubscriber, ISmartBusinessEntityLookupDialog: 

    DEFINE PROTECTED VARIABLE oAdapter AS SmartBusinessEntityAdapter NO-UNDO.

    DEFINE PRIVATE VARIABLE cLookupKeyFieldValue AS CHARACTER                 NO-UNDO .
                                                                                      
    DEFINE PRIVATE VARIABLE cLookupColumnWidth   AS CHARACTER                 NO-UNDO .
    DEFINE PRIVATE VARIABLE cLookupRegistryKey   AS CHARACTER                 NO-UNDO .
                                                                                      
    DEFINE PRIVATE VARIABLE cQuerySort           AS CHARACTER                 NO-UNDO .
    DEFINE PRIVATE VARIABLE cQueryString         AS CHARACTER                 NO-UNDO .
    DEFINE PRIVATE VARIABLE oSmartLookup         AS SmartBusinessEntityLookup NO-UNDO .
    
    DEFINE VARIABLE lFirstShown AS LOGICAL NO-UNDO INIT TRUE .
    
    DEFINE VARIABLE oFullWindowDragManager AS Consultingwerk.Windows.Util.Forms.FullWindowDragManager NO-UNDO . 
    
    /*------------------------------------------------------------------------------
        Purpose: Event raised when the lookups SmartBusinessEntityAdapter is initialized
        Notes:      
        @param sender The reference to the object that raised the event
        @param e The AfterInitializeBusinessEntityAdapterEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterInitializeBusinessEntityAdapter SIGNATURE VOID 
       (sender AS System.Object, e AS AfterInitializeBusinessEntityAdapterEventArgs).
        
    /*------------------------------------------------------------------------------
        Purpose: Name of the Type of SmartBusinessEntityAdapter used to lookup 
                 values from the backend.
        Notes:      
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AdapterType AS CHARACTER NO-UNDO
    INIT "Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter":U 
    GET.
    SET.    
    
    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the name of the AppServer Partition to be used
                 by this SmartDataAdapter instance
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AppServerPartition AS CHARACTER NO-UNDO INIT "":U 
    GET:
        RETURN oAdapter:AppServerPartition .
    END GET.
    SET (arg AS CHARACTER):
        oAdapter:AppServerPartition = arg.
    END SET.
        
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Lookup Browser                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowser AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO 
    GET:
        RETURN smartDataBrowser1 . 
    END GET . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Lookup Entity Name
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityName AS CHARACTER NO-UNDO 
    GET:
        RETURN oAdapter:EntityName.
    END GET.
    SET (arg AS CHARACTER):
        oAdapter:EntityName = arg.
    END SET.    

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Lookup Entity Table 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityTable AS CHARACTER NO-UNDO 
    GET:
        RETURN oAdapter:EntityTable.
    END GET.
    SET (arg AS CHARACTER):
        oAdapter:EntityTable = arg.
    END SET.    

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Lookup Entity View
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityView AS CHARACTER NO-UNDO 
    GET:
        RETURN oAdapter:EntityView.
    END GET.
    SET (arg AS CHARACTER):
        oAdapter:EntityView = arg.
        oAdapter:EntityJoin = TRIM(FILL ("YES,":U, NUM-ENTRIES(arg)), ",":U) . 
    END SET.    

    /*------------------------------------------------------------------------------
        Purpose: Fields to be shown in the SmartDataBrowser
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowserFields AS CHARACTER NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Fields not to be shown in the SmartDataBrowser
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowserExcludeFields AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Option to open the Lookup Dialog Query when the dialog is opened
        Notes:   SCL-673                                                                       
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LookupDialogOpenQuery AS LOGICAL NO-UNDO INIT TRUE
    GET.
    SET.     
    
    /*------------------------------------------------------------------------------
        Purpose: QuerySort used when opening the Lookup Browser Dialog
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LookupDialogQuerySort AS CHARACTER NO-UNDO 
    GET:
        RETURN cQuerySort.
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN cQuerySort = arg . 
        
        oAdapter:QuerySort = cQuerySort .
    END SET.  
    /*------------------------------------------------------------------------------
        Purpose: QueryString used when opening the Lookup Browser Dialog
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LookupDialogQueryString AS CHARACTER NO-UNDO 
    GET:
        RETURN cQueryString.
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN cQueryString = arg . 
        
        oAdapter:QueryString = cQueryString .
    END SET.  
    
    /*------------------------------------------------------------------------------
        Purpose: Sets and sets a reference to the SmartBusinessEntityAdapter instance
                 used by the SmartBusinessEntityAdapter
        Notes:   Not currently used in this class. Allows custom ISmartBusinessEntityLookupDialog
                 implementations to access the BusinessEntityAdapter of the Lookup
                 control itself   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupSmartBusinessEntityAdapter AS SmartBusinessEntityAdapter NO-UNDO 
    GET.
    SET. 
    
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
        Purpose: Field that is returned by the SmartLookup
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupKeyField AS CHARACTER NO-UNDO 
    GET.
    SET.    

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityLookupDialog class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityLookupDialog ():
        THIS-OBJECT ("":U).
        
    END CONSTRUCTOR . 
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityLookupDialog class
        Notes:   This constructor calls back into the Lookup Control to receive the 
                 Lookup Dialog Settings Registrykey
        @param poSmartLookup The reference to the SmartLookup instance for this LookupDialog
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityLookupDialog (poSmartLookup AS SmartLookup):

        THIS-OBJECT ((IF VALID-OBJECT (poSmartLookup) THEN poSmartLookup:GetLookupRegistryKey() ELSE "":U)).

        IF VALID-OBJECT (poSmartLookup) AND TYPE-OF (poSmartLookup, SmartBusinessEntityLookup) THEN                 
            ASSIGN oSmartLookup = CAST (poSmartLookup, SmartBusinessEntityLookup) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityLookupDialog class
        Notes:   
        @param pcLookupRegistryKey The settings key to use for the Lookup Dialog to store grid and dialog settings
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityLookupDialog (pcLookupRegistryKey AS CHARACTER):
        SUPER ().

        IF pcLookupRegistryKey > "":U THEN DO:
            cLookupRegistryKey = pcLookupRegistryKey . 
            RestoreLookupSettings ().
        END.

        THIS-OBJECT:smartDataBrowser1:RegisterDefaultActionSubscriber (THIS-OBJECT).
        THIS-OBJECT:tbSearchField:EditorButtonClick:Subscribe (tbSearchField_EditorButtonClick).
        THIS-OBJECT:tbSearchField:KeyDown:Subscribe (tbSearchField_KeyDown) .
            
        THIS-OBJECT:Closed:Subscribe (StoreLookupSettings).
        
        /* Mike Fechner, Consultingwerk Ltd. 12.10.2010
           Lookups have their own way of storing settings */
        THIS-OBJECT:smartDataBrowser1:SettingsKey = "":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Applys the filter to the query                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ApplyFilter ():
        
        DEFINE VARIABLE cFilter   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cOperator AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cValue    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iField    AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cWhereAnd AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iIndex    AS INTEGER   NO-UNDO .
        
        IF tbSearchField:Text > "":U THEN DO: 
            IF THIS-OBJECT:LookupDialogQueryString > "":U THEN DO: 
                oAdapter:QueryString = THIS-OBJECT:LookupDialogQueryString .
                
                IF THIS-OBJECT:LookupDialogQueryString MATCHES "* WHERE *":U THEN 
                    ASSIGN cWhereAnd = "AND":U .
                ELSE 
                    ASSIGN cWhereAnd = "WHERE":U .
            END.
            ELSE     
                ASSIGN oAdapter:QueryString = SUBSTITUTE ("FOR EACH &1":U,
                                                          oAdapter:EntityTable) 
                       cWhereAnd            = "WHERE":U.             
        
            iField = LOOKUP (UNBOX(cbFilterField:Value), 
                             THIS-OBJECT:LookupDialogFilterFields) .
        
            IF NUM-ENTRIES (THIS-OBJECT:LookupDialogFilterOperators) >= iField THEN
                ASSIGN cOperator = ENTRY (iField, THIS-OBJECT:LookupDialogFilterOperators) . 
            ELSE
                ASSIGN cOperator = ">=":U . 
        
            ASSIGN cValue = tbSearchField:Text .
        
            /* Mike Fechner, Consultingwerk Ltd. 28.10.2015
               SCL-1085 - Special handling for BEGINSMATCHES */
            IF cOperator = "BEGINSMATCHES":U THEN DO:
        
                IF cValue = "":U THEN 
                    ASSIGN cOperator = "BEGINS":U .
                ELSE DO: 
                    ASSIGN iIndex = INDEX (cValue, "*":U) . 
            
                    CASE iIndex:
                        WHEN 0 THEN 
                            ASSIGN cOperator = "BEGINS":U .
                        WHEN LENGTH (cValue) THEN 
                            ASSIGN cValue    = SUBSTRING (cValue, 1, iIndex - 1) 
                                   cOperator = "BEGINS":U .
                        OTHERWISE DO:
                            IF SUBSTRING (cValue, LENGTH (cValue), 1) <> "*":U THEN
                                ASSIGN cValue = cValue + "*":U .
                               
                            ASSIGN cOperator = "MATCHES":U . 
                        END.
                    END CASE . 
                END.
            END.

            oAdapter:QueryString = SUBSTITUTE ("&1 &2 &3.&4 &5 &6":U, 
                                               oAdapter:QueryString,
                                               cWhereAnd,
                                               oAdapter:EntityTable, 
                                               UNBOX(cbFilterField:Value),
                                               cOperator,
                                               QUOTER(cValue)) .        
        END.
        ELSE 
            oAdapter:QueryString = THIS-OBJECT:LookupDialogQueryString .        
        
        oAdapter:RetrieveData () . 
        
        THIS-OBJECT:button2:Enabled = oAdapter:QueryHandle:GET-BUFFER-HANDLE (1):AVAILABLE .
 
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a Field Value from the Business Entity Adapter                                                                       
        Notes:                                                                        
        @param pcFieldName The name of the Field to return
        @return The value of the field                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC CHARACTER GetLookupFieldValue (pcFieldName AS CHARACTER):
        
        DEFINE VARIABLE hDataHandle AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hField      AS HANDLE    NO-UNDO.
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cField      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iExtent     AS INTEGER   NO-UNDO.
        
        ASSIGN hDataHandle = oAdapter:BindingSource:Handle .
        
        Consultingwerk.Util.BufferHelper:GetFieldNameAndExtent (pcFieldName, OUTPUT cField, OUTPUT iExtent) .
        
        IF VALID-HANDLE(hDataHandle) THEN 
            DO i = 1 TO hDataHandle:NUM-BUFFERS:
                ASSIGN hField = hDataHandle:GET-BUFFER-HANDLE(i):BUFFER-FIELD(cField) 
                    NO-ERROR .                  
                
                IF VALID-HANDLE(hField) THEN DO:
                    IF iExtent > 0 THEN 
                        RETURN hField:BUFFER-VALUE [iExtent] .
                    ELSE 
                        RETURN hField:BUFFER-VALUE .
                END.                 
            END. 
        
        RETURN "":U.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the DataAdapter used by the SmartBusinessEntityLookupDialog 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeDataAdapter ():
        
        IF THIS-OBJECT:AdapterType > "":U THEN 
            oAdapter = DYNAMIC-NEW (THIS-OBJECT:AdapterType) () .
        ELSE 
            oAdapter = NEW SmartBusinessEntityAdapter () .         
            
        ASSIGN oAdapter:BatchSize                     = 50       
               oAdapter:RetrieveDataOnChangeSortOrder = FALSE 
               oAdapter:FetchLastOnServer             = TRUE .
        
        OnAfterInitializeBusinessEntityAdapter (NEW AfterInitializeBusinessEntityAdapterEventArgs (oAdapter, 
                                                                                                   AdapterOriginEnum:LookupDialog)) .
    END.    

     /*------------------------------------------------------------------------------
        Purpose: Initializes the Data Binding
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeDataBinding ():

        DEFINE VARIABLE i       AS INTEGER         NO-UNDO.
        DEFINE VARIABLE oColumn AS UltraGridColumn NO-UNDO.

        IF NOT VALID-OBJECT (oAdapter) THEN 
            UNDO, THROW NEW AppError ("No DataAdapter has been initialized so far.", 0) .
        
        smartDataBrowser1:SmartDataSource = oAdapter.

        oAdapter:SmartNavigationSource = THIS-OBJECT:smartNavigationPanel1 .

        IF THIS-OBJECT:LookupEntityView > "":U THEN 
            THIS-OBJECT:InitializeExceptFields () .

        IF THIS-OBJECT:LookupDialogOpenQuery THEN 
            THIS-OBJECT:InitialRetrieveData () . 
        ELSE 
            oAdapter:InitializeAdapter () . 

        oAdapter:RetrieveDataOnChangeSortOrder = TRUE . 

        THIS-OBJECT:button2:Enabled = oAdapter:QueryHandle:GET-BUFFER-HANDLE (1):AVAILABLE .

        BrowserInitializeLayout(?, ?).

        IF THIS-OBJECT:LookupDialogFilterFields > "":U THEN DO:
            DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:LookupDialogFilterFields):
                oColumn = smartDataBrowser1:DisplayLayout:Bands[0]:Columns[ENTRY(i, THIS-OBJECT:LookupDialogFilterFields)] 
                    NO-ERROR . 
                    
                IF NOT VALID-OBJECT (oColumn) THEN 
                    UNDO, THROW NEW AppError (SUBSTITUTE("Column &1 not valid in LookupDialogFilterFields.",
                                                         ENTRY(i, THIS-OBJECT:LookupDialogFilterFields)), 0) .

                cbFilterField:ValueList:ValueListItems:Add (ENTRY(i, THIS-OBJECT:LookupDialogFilterFields),
                                                            oColumn:Header:Caption) .
                
                IF i = 1 THEN 
                    cbFilterField:Value = ENTRY(i, THIS-OBJECT:LookupDialogFilterFields) . 
            END.
        END.
        ELSE DO:
            THIS-OBJECT:cbFilterField:Visible = FALSE .
            THIS-OBJECT:tbSearchField:Visible = FALSE . 
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the InitializeLayout event of the Lookup Grid Control
        Notes:   Make sure that only the given list of LookupBrowserFields and none
                 of the fields in LookupBrowserExcludeFields are visible.  
                 If LookupBrowserFields is an empty string all columns will be shown
        @param sender The reference to the object that raised the event
        @param e The InitializeLayoutEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
     METHOD PRIVATE VOID BrowserInitializeLayout (sender AS System.Object, 
                                                  e      AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs):

        DEFINE VARIABLE oBand AS UltraGridBand NO-UNDO.
        DEFINE VARIABLE oGrid AS UltraGrid     NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER       NO-UNDO.
        DEFINE VARIABLE j     AS INTEGER       NO-UNDO. 
        
        DEFINE VARIABLE iColumnWidthIndex AS INTEGER NO-UNDO. 
        
        oGrid = THIS-OBJECT:smartDataBrowser1.
        
        IF LookupBrowserFields > "":U OR LookupBrowserExcludeFields > "":U THEN
            DO i = 1 TO oGrid:DisplayLayout:Bands:Count:
                oBand = oGrid:DisplayLayout:Bands[i - 1] .
                
                DO j = 1 TO oBand:Columns:Count:
                    iColumnWidthIndex = LOOKUP(oBand:Columns[j - 1]:Key, cLookupColumnWidth).
                    IF iColumnWidthIndex > 0 THEN 
                        oBand:Columns[j - 1]:Width = INTEGER(ENTRY(iColumnWidthIndex + 1, cLookupColumnWidth)).
                        
                    IF (LookupBrowserFields > "":U AND NOT ListHelper:EntryIsInList (oBand:Columns[j - 1]:Key, LookupBrowserFields)) OR
                       (ListHelper:EntryIsInList (oBand:Columns[j - 1]:Key, LookupBrowserExcludeFields)) THEN
                        oBand:Columns[j - 1]:Hidden = TRUE.
                END.
                
                DO j = 1 TO NUM-ENTRIES (LookupBrowserFields):
                    oBand:Columns [ENTRY (j, LookupBrowserFields)]:Header:VisiblePosition = j .
                END.             
            END.
    END METHOD.    

    /*------------------------------------------------------------------------------
        Purpose: Initializes the ExceptField Lists of the Data Adapter 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InitializeExceptFields ():

        DEFINE VARIABLE hQuery   AS HANDLE NO-UNDO.
        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
		DEFINE VARIABLE cExcept  AS CHARACTER NO-UNDO.

        ASSIGN hDataset = oAdapter:DataSet
               hQuery   = SmartBusinessEntityQuerySupport:PrepareBindingQuery (hDataset, 
                                                                               THIS-OBJECT:LookupEntityTable,
                                                                               THIS-OBJECT:LookupEntityView,
                                                                               TRIM (FILL("YES,":U, NUM-ENTRIES(THIS-OBJECT:LookupEntityView)), ",":U),
                                                                               TRUE,
                                                                               OUTPUT cExcept) .

        IF cExcept > "":U THEN 
            oAdapter:BindingSourceExcludeFields = cExcept .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves the initial data in the Lookup Dialog 
        Notes:   Performs a FindRowWhere call instead of RetrieveData when the lookup
                 control contains a value. This is to avoid an extra backend call
                 (SCL-960)
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InitialRetrieveData ():
	
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iIndex       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lThrow       AS LOGICAL   NO-UNDO.
	    
	    IF VALID-OBJECT (oSmartLookup) THEN DO:
	        
	        IF UNBOX (oSmartLookup:Value) = ? OR 
	           oSmartLookup:Text          = "":U THEN DO:
	               
                 oAdapter:RetrieveData ().
                 
                 RETURN . 
             END.	          
             ELSE DO:
                ASSIGN cQueryString = SUBSTITUTE (oSmartLookup:LookupQueryString, 
                                                  UNBOX (oSmartLookup:Value)) 
                       iIndex       = INDEX (cQueryString, " where ":U). 

                IF iIndex > 0 THEN DO:
                    cQueryString = SUBSTRING (cQueryString, iIndex) .
                    
                        DO ON ERROR UNDO, THROW:
                            ASSIGN lThrow = oAdapter:ThrowErrorsFromRetrieveData
                                   oAdapter:ThrowErrorsFromRetrieveData = TRUE . 
            
                            oAdapter:FindRowWhere (cQueryString,
                                                   FindRowWhereModeEnum:FindOnServer) .
            
                            CATCH err AS Progress.Lang.Error:
                                oAdapter:ThrowErrorsFromRetrieveData = lThrow .                            
                                oAdapter:RetrieveData () . 
                            END CATCH.
            
                            FINALLY:
                                oAdapter:ThrowErrorsFromRetrieveData = lThrow .                            
                            END FINALLY.            
                        END. /* do on error undo, throw */
                        
                        RETURN . 
                    END. /* iIndex > 0 */    
	         END. /* else do: */
	    END. /* valid-object oSmartLookup */

        oAdapter:RetrieveData ().

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
        Purpose: Raises the Load event                                                                        
        Notes:      
        @param e The System.EventArgs with the data for this event                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
        
        SUPER:OnLoad (e) .

        oFullWindowDragManager = NEW Consultingwerk.Windows.Util.Forms.FullWindowDragManager (THIS-OBJECT) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event                                                                        
        Notes:                   
        @param e The System.EventArgs with the data for this event                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
    
        THIS-OBJECT:InitializeDataBinding (). 
        
        SUPER:OnShown (e) .
    
        IF lFirstShown AND THIS-OBJECT:tbSearchField:Visible THEN 
            THIS-OBJECT:tbSearchField:Focus () .   
    
        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.
    
        FINALLY:
            ASSIGN lFirstShown = FALSE .     
        END FINALLY.    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Resets the filter of the query 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ResetFilter ():
		        
        ASSIGN tbSearchField:Text = "":U .
        
        IF THIS-OBJECT:LookupDialogQueryString > "":U THEN 
            oAdapter:QueryString = THIS-OBJECT:LookupDialogQueryString .
        ELSE     
            ASSIGN oAdapter:QueryString = SUBSTITUTE ("FOR EACH &1":U,
                                                      oAdapter:EntityTable) .  
    
        oAdapter:RetrieveData () . 
        
        THIS-OBJECT:button2:Enabled = oAdapter:QueryHandle:GET-BUFFER-HANDLE (1):AVAILABLE .
 
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Restores the stored settings for this lookup
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID RestoreLookupSettings ():
   
        DEFINE VARIABLE cPosition   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSize       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cColumnSize AS CHARACTER NO-UNDO.
        
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        
        IF cLookupRegistryKey > "":U THEN .
        ELSE RETURN . 
        
        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .
        
        ASSIGN 
            cPosition   = oSettingsService:GetSetting (cLookupRegistryKey, 
                                                      "Position":U)
            cSize       = oSettingsService:GetSetting (cLookupRegistryKey, 
                                                       "Size":U)
            cColumnSize = oSettingsService:GetSetting (cLookupRegistryKey, 
                                                       "ColumnSize":U) .

        IF NUM-ENTRIES(cPosition) = 2 THEN                              
            ASSIGN THIS-OBJECT:Top  = INTEGER(ENTRY(1, cPosition))
                   THIS-OBJECT:Left = INTEGER(ENTRY(2, cPosition)) NO-ERROR . 

        IF NUM-ENTRIES(cSize) = 2 THEN                              
            ASSIGN THIS-OBJECT:Width  = INTEGER(ENTRY(1, cSize))
                   THIS-OBJECT:Height = INTEGER(ENTRY(2, cSize)) NO-ERROR . 
        
        cLookupColumnwidth = cColumnSize.
        
        CATCH ple AS Progress.Lang.Error :
            /* Ignore Errors 
               the registry key will not exist on first run of a dialog */
        END CATCH.
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Sets the Title of the Lookup Dialog                                                                       
        Notes:
        @param pcText The Text to be used as the Forms Title                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID SetText (pcText AS CHARACTER):
        
        THIS-OBJECT:Text = pcText .  

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Closed event of the Lookup Dialog
        Notes:   Stores the Lookup Settings in the Settings Service
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/    
    METHOD PRIVATE VOID StoreLookupSettings (sender AS System.Object, 
                                             e AS System.EventArgs):
        
        DEFINE VARIABLE cPosition   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSize       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cColumnSize AS CHARACTER NO-UNDO.
        
        DEFINE VARIABLE oGrid AS UltraGrid     NO-UNDO.
        DEFINE VARIABLE oBand AS UltraGridBand NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER       NO-UNDO.
        DEFINE VARIABLE j     AS INTEGER       NO-UNDO. 
    
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        
        IF cLookupRegistryKey > "":U THEN .
        ELSE RETURN . 

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .
    
        ASSIGN 
            cPosition = STRING(THIS-OBJECT:Top) + ",":U + STRING(THIS-OBJECT:Left)
            cSize     = STRING(THIS-OBJECT:Width) + ",":U + STRING(THIS-OBJECT:Height)
            .
        
        oGrid = THIS-OBJECT:smartDataBrowser1.
        
        IF LookupBrowserFields > "":U THEN 
            DO i = 1 TO oGrid:DisplayLayout:Bands:Count:
                oBand = oGrid:DisplayLayout:Bands[i - 1] .
                
                DO j = 1 TO oBand:Columns:Count:
                    IF ListHelper:EntryIsInList (oBand:COLUMNS[j - 1]:Key, LookupBrowserFields) THEN DO:
                        cColumnSize = TRIM(SUBSTITUTE("&1,&2,&3":U, cColumnSize, oBand:COLUMNS[j - 1]:Key, oBand:COLUMNS[j - 1]:Width), ",":U). 
                    END.
                END.             
        END.

        oSettingsService:StoreSetting (cLookupRegistryKey, 
                                       "Position":U, 
                                       cPosition) .
        oSettingsService:StoreSetting (cLookupRegistryKey, 
                                       "Size":U, 
                                       cSize) .
        oSettingsService:StoreSetting (cLookupRegistryKey, 
                                       "ColumnSize":U, 
                                          cColumnSize) .

        CATCH ple AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (ple) .
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Method to be executed inside the class which implements the Interface
                 IDefaultActionSubscriber.
        Notes:   Invoked by the SmartDataBrowser Instance if the DefaultActionHandler 
                 Method is executed.
        @param sender The reference to the Instance of the SmartDataBrowser which has invoked the Method
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SmartDataBrowserDefaultActionHandler (sender AS SmartDataBrowser, 
                                                             e AS System.EventArgs):
        
        THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:OK.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbSearchField                                                                 
        Notes:         
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID tbSearchField_EditorButtonClick (sender AS System.Object, 
                                                           e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
        
        CASE e:Button:Key:
            WHEN "apply":U THEN 
                ApplyFilter () .
            WHEN "reset":U THEN 
                ResetFilter () .
        END CASE . 

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown event of the tbSearchField                                                                 
        Notes:         
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID tbSearchField_KeyDown (sender AS System.Object,
                                                 e AS System.Windows.Forms.KeyEventArgs):
        
        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode,
                                              System.Windows.Forms.Keys:Enter) OR
           Progress.Util.EnumHelper:AreEqual (e:KeyCode,
                                              System.Windows.Forms.Keys:Return) THEN 
                                              
            ApplyFilter () .                                                                                               

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:  Destructor of the SmartBusinessEntityLookupDialog class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartBusinessEntityLookupDialog ():

        IF VALID-OBJECT (oAdapter) THEN 
            DELETE OBJECT (oAdapter) .

    END DESTRUCTOR.

END CLASS.
