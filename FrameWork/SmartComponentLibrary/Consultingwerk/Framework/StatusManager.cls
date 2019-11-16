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
    File        : StatusManager
    Purpose     : Default IStatusManager implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 15 12:12:46 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* .
USING Progress.Lang.* .

CLASS Consultingwerk.Framework.StatusManager
    IMPLEMENTS IStatusManager: 

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
    METHOD PUBLIC VOID ClearStatus ():
        
        OnGenericStatusChanged ("":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Clears the status display for the given scope 
                 (i.e. a specific application window referenced by a GUID)                                                                      
        Notes:         
        @param pcScope The name of the scope
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClearStatus (INPUT pcScope AS CHARACTER):
        
        OnScopedStatusChanged ("":U, pcScope) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the generic status (i.e. main window status bar)                                                                    
        Notes: 
        @param pcStatus The generic status value                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayStatus (INPUT pcStatus AS CHARACTER):
        
        OnGenericStatusChanged (pcStatus) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the status for the given scope 
                 (i.e. a specific application window referenced by a GUID)                                                                      
        Notes:       
        @param pcStatus The generic status value                                                                    
        @param pcScope The name of the scope
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayStatus (INPUT pcStatus AS CHARACTER, 
                                      INPUT pcScope AS CHARACTER):
        
        OnScopedStatusChanged (pcStatus, pcScope) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the GenericStatusChanged event                                                                       
        Notes:        
        @param pcStatus The generic status value                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnGenericStatusChanged (pcStatus AS CHARACTER):
        
        THIS-OBJECT:GenericStatusChanded:Publish (THIS-OBJECT, pcStatus) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the ScopedStatusChanged event                                                                      
        Notes:                                                                        
        @param pcStatus The generic status value                                                                    
        @param pcScope The name of the scope
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnScopedStatusChanged (pcStatus AS CHARACTER,
                                                 pcScope  AS CHARACTER):
        
        THIS-OBJECT:ScopedStatusChanded:Publish (THIS-OBJECT, pcStatus, pcScope) .

    END METHOD.

END CLASS.
