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
    File        : IProgressManager
    Purpose     : Describes Methods required by classes that can
                  handle Progress updates on the UI 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 15 12:07:47 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* .
USING Progress.Lang.* .

INTERFACE Consultingwerk.Framework.IProgressManager:  

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the generic progress has changed                                                                        
        Notes:                                                 
        @param sender The reference to the IProgressManager that raised the event
        @param pcLabel The label for the current progress
        @param piValue The current progress value
        @param piMax The Maximum value for the current progress                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT GenericProgressChanded SIGNATURE VOID (sender AS IProgressManager, 
                                                               pcLabel AS CHARACTER, 
                                                               piValue AS INTEGER, 
                                                               piMax AS INTEGER).

    /*------------------------------------------------------------------------------
        Purpose: Event raised when a scoped scoped progress has updated                                                                      
        Notes:                                                                        
        @param sender The reference to the IProgressManager that raised the event
        @param pcLabel The label for the current progress
        @param piValue The current progress value
        @param piMax The Maximum value for the current progress                        
        @param pcScope The name of the scope this progress event belongs to
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ScopedProgressChanded SIGNATURE VOID (sender AS IProgressManager, 
                                                              pcLabel AS CHARACTER, 
                                                              piValue AS INTEGER, 
                                                              piMax AS INTEGER, 
                                                              pcScope AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Clears the generic progress display (i.e. main window progress bar)                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClearProgress ().

    /*------------------------------------------------------------------------------
        Purpose: Clears the progress display for the given scope 
                 (i.e. a specific application window referenced by a GUID)                                                                      
        Notes:          
        @param pcScope The name of the scope for which the progress display should be cleared
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClearProgress (pcScope AS CHARACTER).
  
    /*------------------------------------------------------------------------------
        Purpose: Sets the generic progress (i.e. main window progress bar)                                                                       
        Notes:                                                                        
        @param pcLabel The label for the progress to display
        @param piValue The current value to display
        @param piMax The Maximum value for the progress to display                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayProgress (pcLabel AS CHARACTER, 
                                        piValue AS INTEGER, 
                                        piMax AS INTEGER).

    /*------------------------------------------------------------------------------
        Purpose: Sets the progress for the given scope 
                 (i.e. a specific application window referenced by a GUID)                                                                      
        Notes:           
        @param pcLabel The label for the progress to display
        @param piValue The current value to display
        @param piMax The Maximum value for the progress to display                         
        @param pcScope The name of the scope for the current progress to display                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayProgress (pcLabel AS CHARACTER, 
                                        piValue AS INTEGER, 
                                        piMax AS INTEGER,
                                        pcScope AS CHARACTER).

END INTERFACE.