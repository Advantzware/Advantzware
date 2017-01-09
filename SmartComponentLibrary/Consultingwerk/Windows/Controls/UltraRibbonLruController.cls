/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : UltraRibbonLruController
    Purpose     : Provides generic functionality for managing a LRU list in 
                  the Ribbon Application menu (right)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 22 00:11:06 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*          FROM PROPATH . 
USING Consultingwerk.Windows.Controls.*   FROM PROPATH .  
USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY . 
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Windows.Controls.UltraRibbonLruController: 
 
    /*------------------------------------------------------------------------------
        Purpose: Gets the ISettingsService Base Key to use to store/retrieve the 
                 settings from
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BaseKey AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets the ISettingsService Key pattern to use to store/retrieve the 
                 settings from 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY KeyPattern AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets the maximum number of LRU items
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MaxItems AS INTEGER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets the reference to the UltraToolbarsManager
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ToolbarsManager AS UltraToolbarsManager NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraRibbonLruController class
        Notes:   
        @param poToolbarsManager The reference to the UltraToolbarsManager
        @param pcBaseKey The ISettingsService Base Key to use to store/retrieve the settings from
        @param pcKeyPattern The ISettingsService Key pattern to use to store/retrieve the settings from
        @param piMaxItems The maximum number of LRU items
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraRibbonLruController (poToolbarsManager AS UltraToolbarsManager,
                                                 pcBaseKey AS CHARACTER,
                                                 pcKeyPattern AS CHARACTER,
                                                 piMaxItems AS INTEGER):
        SUPER ().
        
        ObjectAssert:IsValid (poToolbarsManager) . 
        Assert:NotNullOrEmpty (pcBaseKey, "pcBaseKey":U) .
        Assert:NotNullOrEmpty (pcKeyPattern, "pcKeyPattern":U) .
        Assert:GE (piMaxItems, 2) .
        
        ASSIGN THIS-OBJECT:ToolbarsManager = poToolbarsManager
               THIS-OBJECT:BaseKey         = pcBaseKey
               THIS-OBJECT:KeyPattern      = pcKeyPattern
               THIS-OBJECT:MaxItems        = piMaxItems . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds an item at the beginning of the LRU
        Notes:   
        @param pcDisplayText The display text of the item
        @param pcItemKey The key of the item
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddItemToLru (pcDisplayText AS CHARACTER, 
                                     pcItemKey AS CHARACTER):
    
        DEFINE VARIABLE oButtonTool AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO .
        DEFINE VARIABLE oEnumerator AS System.Collections.IEnumerator               NO-UNDO .
        
        oEnumerator = THIS-OBJECT:ToolbarsManager:Ribbon:ApplicationMenu:ToolAreaRight:Tools:GetEnumerator() .
        oEnumerator:Reset() .
        
        /* Remove items with the same display text */
        DO WHILE oEnumerator:MoveNext():
            oButtonTool = CAST (oEnumerator:Current,
                                Infragistics.Win.UltraWinToolbars.ButtonTool) .
            
            IF oButtonTool:SharedProps:TooltipText = pcDisplayText THEN DO:
                THIS-OBJECT:ToolbarsManager:Tools:Remove (oButtonTool:SharedProps:RootTool) .
            END.
        END.

        DO WHILE THIS-OBJECT:ToolbarsManager:Ribbon:ApplicationMenu:ToolAreaRight:Tools:Count >= THIS-OBJECT:MaxItems:
            oButtonTool = CAST (THIS-OBJECT:ToolbarsManager:Ribbon:ApplicationMenu:ToolAreaRight:Tools [THIS-OBJECT:MaxItems - 1],
                                Infragistics.Win.UltraWinToolbars.ButtonTool) . 
                                           
            THIS-OBJECT:ToolbarsManager:Tools:Remove (oButtonTool:SharedProps:RootTool) .
        END.

        oButtonTool = NEW Infragistics.Win.UltraWinToolbars.ButtonTool ("lru_":U + GUID) .
        oButtonTool:SharedProps:Caption = (IF LENGTH (pcDisplayText) > 40 THEN "... ":U ELSE "":U) + 
                                          SUBSTRING (pcDisplayText, MAX (1, LENGTH (pcDisplayText) - 40)) .
        oButtonTool:SharedProps:ToolTipText = pcDisplayText .
        oButtonTool:Tag = BOX (pcItemKey) . 
        THIS-OBJECT:ToolbarsManager:Tools:Add (oButtonTool) .
        
        THIS-OBJECT:ToolbarsManager:Ribbon:ApplicationMenu:ToolAreaRight:Tools:InsertTool(0, oButtonTool:Key) . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Loads the LRU list from the ISettingsService                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadLru ():
        
        DEFINE VARIABLE i                AS INTEGER                                   NO-UNDO.
        DEFINE VARIABLE cDisplayText     AS CHARACTER                                 NO-UNDO.        
        DEFINE VARIABLE cKey             AS CHARACTER                                 NO-UNDO.
        DEFINE VARIABLE cValue           AS CHARACTER                                 NO-UNDO.

        DEFINE VARIABLE oSettingsService AS Consultingwerk.Framework.ISettingsService NO-UNDO . 

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        DO i = THIS-OBJECT:MaxItems TO 0 BY -1 ON ERROR UNDO, THROW:
            cValue = oSettingsService:GetSetting (THIS-OBJECT:BaseKey,
                                                  SUBSTITUTE (THIS-OBJECT:KeyPattern, i)) . 
         
            IF cValue > "":U AND NUM-ENTRIES (cValue, CHR (1)) > 1 THEN DO:

                ASSIGN cDisplayText = ENTRY (1, cValue, CHR (1))
                       cKey         = ENTRY (2, cValue, CHR (1)) .
                
                THIS-OBJECT:AddItemToLru (cDisplayText, cKey) .
            END.                                                    
                                                  
            CATCH err AS Progress.Lang.Error :
                /* noop */    
            END CATCH.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the LRU list in the ISettingsService                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreLru ():
        
        DEFINE VARIABLE oSettingsService AS Consultingwerk.Framework.ISettingsService NO-UNDO . 
        DEFINE VARIABLE i                AS INTEGER                                   NO-UNDO INIT 1.

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .
    
        {Consultingwerk/foreach.i ButtonTool oButtonTool in THIS-OBJECT:ToolbarsManager:Ribbon:ApplicationMenu:ToolAreaRight:Tools}
                            
            IF oButtonTool:Key BEGINS "lru_":U AND oButtonTool:SharedProps:TooltipText > "":U THEN     
                oSettingsService:StoreSetting (THIS-OBJECT:BaseKey,
                                               SUBSTITUTE (THIS-OBJECT:KeyPattern, i),
                                               SUBSTITUTE ("&1&2&3":U,
                                                           oButtonTool:SharedProps:TooltipText,
                                                           CHR (1),
                                                           UNBOX (oButtonTool:SharedProps:RootTool:Tag))) .                                
            i = i + 1 .
        END.

    END METHOD.


END CLASS.
