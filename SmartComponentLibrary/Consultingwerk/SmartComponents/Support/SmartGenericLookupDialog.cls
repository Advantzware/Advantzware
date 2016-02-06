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
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.* FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY .
USING Progress.Lang.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Support.SmartGenericLookupDialog 
    INHERITS SmartGenericLookupDialogDesigner
    IMPLEMENTS IDefaultActionSubscriber : 

    DEFINE PRIVATE VARIABLE cLookupKeyFieldValue AS CHARACTER NO-UNDO.
    
    DEFINE PRIVATE VARIABLE cLookupColumnWidth   AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE cLookupRegistryKey   AS CHARACTER NO-UNDO.
    
    DEFINE PRIVATE VARIABLE oDataAdapter         AS SmartDataAdapter NO-UNDO.
    
    /*------------------------------------------------------------------------------
        Purpose: Get's and set's the reference to the SmartDataAdapter that is the 
                 data source to the SmartGenericLookupDialog
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataSource AS SmartDataAdapter NO-UNDO 
    GET:
        RETURN oDataAdapter .
    END GET.
    SET (arg AS SmartDataAdapter):
        oDataAdapter = arg .
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
        Purpose: Fields to be shown in the SmartDataBrowser
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowserFields AS CHARACTER NO-UNDO 
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
        Purpose: Constructor for the SmartGenericLookupDialog class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartGenericLookupDialog ():
        SUPER ().

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartGenericLookupDialog class
        Notes:  
        @param poSmartLookup The reference to the SmartLookup instance 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartGenericLookupDialog (poSmartLookup AS SmartLookup):
        SUPER ().
        
        cLookupRegistryKey = poSmartLookup:GetLookupRegistryKey ().
        RestoreLookupSettings ().

        THIS-OBJECT:smartDataBrowser1:RegisterDefaultActionSubscriber (THIS-OBJECT).
            
        THIS-OBJECT:Shown:Subscribe (InitializeDatabinding).
        
        THIS-OBJECT:Closed:Subscribe (StoreLookupSettings).
        
        /* Mike Fechner, Consultingwerk Ltd. 12.10.2010
           Looks have their own way of storing settings */
        THIS-OBJECT:smartDataBrowser1:SettingsKey = "":U .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a Field Value from the Data Adapter                                                                       
        Notes:                                                                        
        @param pcFieldName The name of the Field to return
        @return The value of the field                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC CHARACTER GetLookupFieldValue (pcFieldName AS CHARACTER):
        
        DEFINE VARIABLE hDataHandle AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hField      AS HANDLE  NO-UNDO.
        DEFINE VARIABLE i           AS INTEGER NO-UNDO.
        
        ASSIGN hDataHandle = oDataAdapter:BindingSource:Handle .
        
        IF VALID-HANDLE(hDataHandle) THEN 
            DO i = 1 TO hDataHandle:NUM-BUFFERS:
                ASSIGN hField = hDataHandle:GET-BUFFER-HANDLE(i):BUFFER-FIELD(pcFieldName) 
                    NO-ERROR .                  
                
                IF VALID-HANDLE(hField) THEN 
                    RETURN hField:BUFFER-VALUE .                 
            END. 
        
        RETURN "".

    END METHOD.


     /*------------------------------------------------------------------------------
        Purpose: Event handler for the Shown event of the Dialog
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeDatabinding (sender AS CLASS System.Object, 
                                               e      AS CLASS System.EventArgs):
                                                   
        smartDataBrowser1:SmartDataSource = oDataAdapter .
        oDataAdapter:SmartNavigationSource = smartToolbarController1.

        oDataAdapter:RetrieveData ().
        
        BrowserInitializeLayout(?, ?).
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the InitializeLayout event of the UltraGrid instance
        Notes:   Make sure that only the given list of LookupBrowserFields are visible
                 If LookupBrowserFields is an empty string all columns will bwe shown
        @param sender The reference to the object that raised the event
        @param e The InitializeLayoutEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
     METHOD PRIVATE VOID BrowserInitializeLayout (INPUT sender AS System.Object, 
                                                  INPUT e      AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs):

        DEFINE VARIABLE oBand AS UltraGridBand NO-UNDO.
        DEFINE VARIABLE oGrid AS UltraGrid     NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER       NO-UNDO.
        DEFINE VARIABLE j     AS INTEGER       NO-UNDO. 
        
        DEFINE VARIABLE iColumnWidthIndex AS INTEGER NO-UNDO. 
        
        oGrid = THIS-OBJECT:smartDataBrowser1.
        
        IF LookupBrowserFields > "":U THEN 
            DO i = 1 TO oGrid:DisplayLayout:Bands:Count:
                oBand = oGrid:DisplayLayout:Bands[i - 1] .
                
                DO j = 1 TO oBand:Columns:Count:
                    iColumnWidthIndex = LOOKUP(oBand:Columns[j - 1]:Key, cLookupColumnWidth).
                    IF iColumnWidthIndex > 0 THEN 
                        oBand:Columns[j - 1]:Width = INTEGER(ENTRY(iColumnWidthIndex + 1, cLookupColumnWidth)).
                        
                    IF NOT ListHelper:EntryIsInList (oBand:COLUMNS[j - 1]:Key, LookupBrowserFields) THEN 
                        oBand:Columns[j - 1]:Hidden = TRUE.
                END.
                
                DO j = 1 TO NUM-ENTRIES (LookupBrowserFields):
                    oBand:Columns [ENTRY (j, LookupBrowserFields)]:Header:VisiblePosition = j .
                END.             
        END.
    END METHOD.    

     /*------------------------------------------------------------------------------
        Purpose: Restores the Lookup Settings from the Setting Service 
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
        Purpose: Event handler for the Close event of the Dialog
        Notes:   
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
                        cColumnSize = TRIM(SUBSTITUTE("&1,&2,&3":U, cColumnSize, oBand:COLUMNS[j - 1]:Key, oBand:COLUMNS[j - 1]:Width), ","). 
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
            ErrorHelper:ShowErrorMessage (ple). 
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

END CLASS.