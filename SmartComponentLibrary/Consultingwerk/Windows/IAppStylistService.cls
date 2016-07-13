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
    File        : IAppStylistService
    Purpose     : Service Interface for switching and editing Infragistics
                  Style Libraries
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 22 00:31:38 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Windows.IAppStylistService:  

    /*------------------------------------------------------------------------------
        Purpose: Raised when the Style Library has changed
        Notes:   
        @param sender The object that raised the StyleLibraryChanged event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT StyleLibraryChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                            e AS Consultingwerk.EventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the current style library 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StyleLibrary AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the designer for the loaded style library 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID EditStyleLibrary ().

    /*------------------------------------------------------------------------------
        Purpose: Loads a Style Library
        Notes:   
        @param pcStyleLibrary The name of the style library file, without the .isl extension
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID LoadStyleLibrary (pcStyleLibrary AS CHARACTER).

END INTERFACE.
