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
    File        : IStatusManager
    Purpose     : Describes Methods required by classes that can
                  handle status updates on the UI 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 15 12:07:47 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

INTERFACE Consultingwerk.Framework.IStatusManager:  

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the generic status has changed                                                                        
        Notes:    
        @param sender The IStatusManager that raised this event
        @param pcStatus The generic status value                                                                    
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT GenericStatusChanded SIGNATURE VOID 
          (sender AS IStatusManager, pcStatus AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Event raised when a scoped status has updated                                                                      
        Notes:                                                                        
        @param sender The IStatusManager that raised this event
        @param pcStatus The generic status value                                                                    
        @param pcScope The name of the scope
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ScopedStatusChanded SIGNATURE VOID 
          (sender AS IStatusManager, pcStatus AS CHARACTER, pcScope AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Clears the generic status display (i.e. main window status bar)                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClearStatus ().

    /*------------------------------------------------------------------------------
        Purpose: Clears the status display for the given scope 
                 (i.e. a specific application window referenced by a GUID)                                                                      
        Notes:                                                                        
        @param pcScope The name of the scope
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClearStatus (pcScope AS CHARACTER).
  
    /*------------------------------------------------------------------------------
        Purpose: Sets the generic status (i.e. main window status bar)                                                                       
        Notes:                                                                        
        @param pcStatus The generic status value                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayStatus (pcStatus AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Sets the status for the given scope 
                 (i.e. a specific application window referenced by a GUID)                                                                      
        Notes:                                                                        
        @param pcStatus The generic status value                                                                    
        @param pcScope The name of the scope
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayStatus (pcStatus AS CHARACTER, 
                                      pcScope AS CHARACTER).

END INTERFACE.