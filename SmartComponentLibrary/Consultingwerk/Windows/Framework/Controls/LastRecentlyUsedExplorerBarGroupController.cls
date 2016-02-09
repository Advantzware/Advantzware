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
    File        : LastRecentlyUsedExplorerBarGroupController
    Purpose     : Renders the last recently used menu items in an Ultra Explorer Bar Group
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Feb 08 23:31:21 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                            FROM PROPATH .
USING Consultingwerk.Assertion.*                  FROM PROPATH . 
USING Consultingwerk.Framework.*                  FROM PROPATH . 
USING Consultingwerk.OERA.*                       FROM PROPATH . 
USING Consultingwerk.SmartFramework.*             FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.*        FROM PROPATH .
USING Consultingwerk.Util.*                       FROM PROPATH .
USING Consultingwerk.Windows.Framework.*          FROM PROPATH .  
USING Consultingwerk.Windows.Framework.Controls.* FROM PROPATH .  
USING Infragistics.Win.UltraWinExplorerBar.*      FROM ASSEMBLY .
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Controls.LastRecentlyUsedExplorerBarGroupController: 

&IF DEFINED (SmartFramework) NE 0 &THEN
    {Consultingwerk/Windows/Framework/ttMenuStructure.i &ACCESS=PROTECTED}
    {Consultingwerk/SmartFramework/Menu/dsMenu.i}

    DEFINE           VARIABLE oExplorerBarGroup AS UltraExplorerBarGroup  NO-UNDO .
    DEFINE PROTECTED VARIABLE oMenuProvider     AS IMenuStructureProvider NO-UNDO .
    DEFINE PROTECTED VARIABLE oImageProvider    AS IImageProvider         NO-UNDO .
    DEFINE PROTECTED VARIABLE oGroup            AS UltraExplorerBarGroup  NO-UNDO .
    DEFINE           VARIABLE oLauncher         AS IMenuFunctionLauncher  NO-UNDO . 


    /*------------------------------------------------------------------------------
        Purpose: The maximum number of items 
        Notes:   Constant property 
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY MAX_ITEMS AS INTEGER INITIAL 10 NO-UNDO 
	GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the owning object 
        Notes:   The owner is the object in which to invoke methods 
                 (depending on the type of menu item) 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Owner AS Progress.Lang.Object NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraToolbarsManager that renders
                 the Ribbon Tab 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ExplorerBar AS UltraExplorerBar NO-UNDO 
    GET.
    PROTECTED SET. 
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LastRecentlyUsedExplorerBarGroupController class
        Notes:   
        @param poExplorerBar The UltraExplorerBar that owns the Explorer Bar Group 
        @param poOwner The owner in which to invoke methodes (depending on the type of Menu Item)
        @param pcGroupKey The key of the Group to initialize
        @param pcGroupTitle The title of the group to initialize
        @param pcSmallImageKey The key of the small Group Image
        @param pcLargeImageKey The key of the large Group Image
        @param piPosition The position of the Explorer Bar Group or ?
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LastRecentlyUsedExplorerBarGroupController (poExplorerBar AS UltraExplorerBar,
                                                                poOwner AS Progress.Lang.Object,
                                                                pcGroupKey AS CHARACTER,
                                                                pcGroupTitle AS CHARACTER,
                                                                pcSmallImageKey AS CHARACTER,
                                                                pcLargeImageKey AS CHARACTER,
                                                                piPosition AS INTEGER):

        ObjectAssert:IsValid (poExplorerBar, "UltraExplorerBar":U) .
        ObjectAssert:IsValid (poOwner, "Owner":U) . 
        
        ASSIGN THIS-OBJECT:ExplorerBar      = poExplorerBar
               THIS-OBJECT:Owner            = poOwner .

        IF piPosition = ? THEN
            oGroup = poExplorerBar:Groups:Add (pcGroupKey, pcGroupTitle) .
        ELSE
            oGroup = poExplorerBar:Groups:Insert (piPosition, pcGroupKey, pcGroupTitle) .

        oGroup:Settings:Style = GroupStyle:LargeImagesWithText .

        oLauncher = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IMenuFunctionLauncher} .
        oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .
        oMenuProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IMenuStructureProvider} .

        IF VALID-OBJECT (oImageProvider) THEN DO:
            oGroup:Settings:AppearancesSmall:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcSmallImageKey) .
            oGroup:Settings:AppearancesLarge:HeaderAppearance:Image = oImageProvider:ImageForKey
                        (pcLargeImageKey) .
        END.

        poExplorerBar:ItemClick:Subscribe (ItemClickHandler) .
        oLauncher:MenuFunctionLaunched:Subscribe (MenuFunctionLaunchedHandler) .
        
        IF VALID-OBJECT (FrameworkSettings:MdiContainer) THEN 
            FrameworkSettings:MdiContainer:FormClosing:Subscribe (MdiContainerClosingHandler) .
            
        THIS-OBJECT:RestoreSettings () .             
    END. 

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemClick Event of the UltraExplorerBar 
        Notes:   
        @param sender The reference to object that raised the event
        @param e The ItemEventArgs instance with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ItemClickHandler (sender AS System.Object, 
                                          e AS ItemEventArgs):
        
        DEFINE VARIABLE cKey      AS CHARACTER             NO-UNDO.

        ASSIGN cKey = e:Item:Key .

        IF e:Item:Group <> oGroup THEN 
            RETURN . 

        IF VALID-OBJECT (oLauncher) THEN 
            oLauncher:LaunchMenuFunction (e:Item:Key, 
                                          NEW MenuContext (e:Item:Key, e:Item:Text, THIS-OBJECT:Owner)) .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

	/*------------------------------------------------------------------------------
	   Purpose: Event handle for the FormClosing event of the main MDI Container
	   Notes:
	   @param sender The reference to the object that raised the event
	   @param e The FormClosingEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID MdiContainerClosingHandler (sender AS System.Object,
	                                                  e AS System.Windows.Forms.FormClosingEventArgs):
		
		THIS-OBJECT:StoreLastRecentlyUsedFunctions () .

	END METHOD.

	/*------------------------------------------------------------------------------
	   Purpose: Event handler for the MenuFunctionLaunched event of the MenuFunctionLauncher 
	   Notes:
	   @param sender The reference to the object that raised the event
	   @param e The MenuFunctionLaunchedEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID MenuFunctionLaunchedHandler (sender AS Progress.Lang.Object,
	                                                   e AS MenuFunctionLaunchedEventArgs):
		
        DEFINE VARIABLE oItem          AS UltraExplorerBarItem NO-UNDO . 
        DEFINE VARIABLE oRequest       AS FetchDataRequest     NO-UNDO .
        DEFINE VARIABLE iItemsToRemove AS INTEGER              NO-UNDO .
	    DEFINE VARIABLE i              AS INTEGER              NO-UNDO .
	
		IF oGroup:Items:Exists (e:MenuFunctionKey) THEN DO:
		    oItem = oGroup:Items [e:MenuFunctionKey] .

            oItem:Reposition (oGroup:Items[0], Infragistics.Win.UltraWinExplorerBar.ItemPosition:First) .
		    
		    RETURN .
		END.
		
		ASSIGN iItemsToRemove = oGroup:Items:Count - MAX_ITEMS + 1 . 

        IF iItemsToRemove > 0 THEN 
            DO i = 1 TO iItemsToRemove:
                oGroup:Items:Remove (oGroup:Items[MAX_ITEMS - 1]) . 
            END.
        
        oRequest = NEW FetchDataRequest ("eSmartFunction":U,
                                         SUBSTITUTE ("FOR EACH eSmartFunction WHERE eSmartFunction.FunctionGuid = ~"&1~"":U, e:MenuFunctionKey),
                                         1) .
        
        FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                       "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U,
                                                       oRequest,
                                                       OUTPUT DATASET dsMenu) .    
                                                               
        oItem = oGroup:Items:Insert (0, e:MenuFunctionKey, e:MenuFunctionTitle) .
        
        FIND FIRST eSmartFunction NO-ERROR .
        
        IF AVAILABLE (eSmartFunction) THEN DO:
            oItem:Settings:AppearancesLarge:Appearance:Image = oImageProvider:ImageForKey (eSmartFunction.FunctionLargeImage) .
            oItem:Settings:AppearancesSmall:Appearance:Image = oImageProvider:ImageForKey (eSmartFunction.FunctionSmallImage) .
        END. 
        
	END METHOD.

	/*------------------------------------------------------------------------------
	   Purpose: Restores the last currently used menu functions
	   Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RestoreSettings ():

        DEFINE VARIABLE cKeys            AS CHARACTER                                 EXTENT NO-UNDO . 
        DEFINE VARIABLE oSettingsService AS Consultingwerk.Framework.ISettingsService NO-UNDO . 
        DEFINE VARIABLE i                AS INTEGER                                   NO-UNDO .
        DEFINE VARIABLE oItem            AS UltraExplorerBarItem                      NO-UNDO . 

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .
		
		oGroup:Items:Clear () .

        EXTENT (cKeys) = MAX_ITEMS .

        DO i = 1 TO MAX_ITEMS:

            cKeys[i] = oSettingsService:GetSetting ("Menu Function LRU":U, 
                                                    SUBSTITUTE ("LRU_&1":U, i)) .
                                                    
        END .        

        oMenuProvider:GetMenuItems (cKeys, 
                                    OUTPUT TABLE ttMenuStructure BY-REFERENCE) .

        DO i = 1 TO MAX_ITEMS:

            IF cKeys[i] > "":U THEN . 
            ELSE 
                NEXT . 

            FIND ttMenuStructure WHERE ttMenuStructure.MenuKey = cKeys[i] NO-ERROR . 
            
            IF AVAILABLE ttMenuStructure THEN DO:
                
                oItem = oGroup:Items:Add (ttMenuStructure.MenuKey,
                                          ttMenuStructure.MenuName) .
    
                oItem:Settings:AppearancesLarge:Appearance:Image = oImageProvider:ImageForKey (ttMenuStructure.MenuLargeImage) .
                oItem:Settings:AppearancesSmall:Appearance:Image = oImageProvider:ImageForKey (ttMenuStructure.MenuSmallImage) .
            END.
        END.        

        ERROR-STATUS:ERROR = FALSE NO-ERROR . 

	END METHOD.

	/*------------------------------------------------------------------------------
	   Purpose: Stores the last recently used menu functions using the Settings Service
	   Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID StoreLastRecentlyUsedFunctions ():
		
		DEFINE VARIABLE oSettingsService AS Consultingwerk.Framework.ISettingsService NO-UNDO . 
        DEFINE VARIABLE i                AS INTEGER                                   NO-UNDO .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF NOT VALID-OBJECT (oSettingsService) THEN 
            RETURN . 

        DO i = 1 TO MAX_ITEMS:
            IF oGroup:Items:Count >= i THEN 
                oSettingsService:StoreSetting ("Menu Function LRU":U, 
                                               SUBSTITUTE ("LRU_&1":U, i), 
                                               oGroup:Items[i - 1]:Key) . 
            ELSE 
                oSettingsService:StoreSetting ("Menu Function LRU":U, 
                                               SUBSTITUTE ("LRU_&1":U, i), 
                                               "":U) . 
        END. 

	END METHOD.
&ENDIF
END CLASS.
