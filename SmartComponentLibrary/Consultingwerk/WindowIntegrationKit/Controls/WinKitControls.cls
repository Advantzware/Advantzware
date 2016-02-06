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
    File        : WinKitControls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 20 13:46:12 CEST 2013
    Notes       : Avoid the usage of certain static members in hybrid 
                  classes on OpenEdge 11.2 and 11.3 due to OE defect OE00240707 
                  http://knowledgebase.progress.com/articles/Article/000042229?popup=true
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .  
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.Controls.WinKitControls: 

    /*------------------------------------------------------------------------------
        Purpose: Static event raised whenever a RenderedBrowseControl is activated. This
                 event is useful to control a central UltraGridColumnChooser etc.                                                                 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC STATIC EVENT RenderedBrowseControlActivated DELEGATE System.EventHandler.
    
    /*------------------------------------------------------------------------------
        Purpose: Default MaxDataGuess for RenderedBrowseControl instances that are using 
                 batching                                                                     
        Notes:   Static property because it defines the default for all classes
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DefaultMaxDataGuess AS INTEGER INITIAL 2 NO-UNDO
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the RenderedBrowseControl that is associated to the given browse widget handle                                                                       
        Notes:   Static method                                                                  
        @param phBrowseHandle The Browse Widget Handle to search the associated RenderedBrowseControl for
        @return The reference to the RenderedBrowseControl or unknown value 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC RenderedBrowseControl FromBrowseHandle (phBrowseHandle AS HANDLE):
        
        DEFINE VARIABLE oObject AS Progress.Lang.Object  NO-UNDO .
        DEFINE VARIABLE oBrowse AS RenderedBrowseControl NO-UNDO . 
        
        oObject = SESSION:FIRST-OBJECT .  
        
        DO WHILE VALID-OBJECT (oObject):
            IF TYPE-OF (oObject, RenderedBrowseControl) THEN DO:
            
                oBrowse = CAST (oObject, RenderedBrowseControl) .                
            
                IF oBrowse:BrowseHandle = phBrowseHandle THEN 
                    RETURN oBrowse . 
            END . 

            oObject = oObject:Next-Sibling .
        END.
                                                         
        RETURN ? .
                                                         
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the RenderedBrowseControl that is associated to the given query handle                                                                        
        Notes:   Static method                                                                     
        @param phQueryHandle The Query Handle to search the associated RenderedBrowseControl for
        @return The reference to the RenderedBrowseControl or unknown value  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC RenderedBrowseControl FromQueryHandle (phQueryHandle AS HANDLE):
        
        DEFINE VARIABLE oObject AS Progress.Lang.Object  NO-UNDO .
        DEFINE VARIABLE oBrowse AS RenderedBrowseControl NO-UNDO . 
        
        oObject = SESSION:FIRST-OBJECT .  
        
        DO WHILE VALID-OBJECT (oObject):
            IF TYPE-OF (oObject, RenderedBrowseControl) THEN DO:
            
                oBrowse = CAST (oObject, RenderedBrowseControl) .                
            
                IF oBrowse:QUERY = phQueryHandle THEN 
                    RETURN oBrowse . 
            END . 

            oObject = oObject:Next-Sibling .
        END.
                                                         
        RETURN ? .
                
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the SmartDataBrowserActivated event                                                                       
        Notes:   Static event raised whenever a SmartDataBrowser is activated. This
                 event is useful to control a central UltraGridColumnChooser etc.                                                                 
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs instance that contains the event data                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID OnRenderedBrowseControlActivated (sender AS System.Object,
                                                                e AS System.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:Empty .
            
        WinKitControls:RenderedBrowseControlActivated:Publish (sender, e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Flags that the browse has handled the DEFAULT-ACTION event
        Notes:   
        @param phBrowser The BROWSE widget that has handled a DEFAULT-ACTION event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetDefaultActionHandled (phBrowser AS HANDLE):
        
        DEFINE VARIABLE oGrid AS RenderedBrowseControl NO-UNDO .

        ASSIGN oGrid = WinKitControls:FromBrowseHandle (phBrowser) .

        IF VALID-OBJECT (oGrid) THEN 
            ASSIGN oGrid:DefaultActionHandled = TRUE . 

    END METHOD .



END CLASS.
