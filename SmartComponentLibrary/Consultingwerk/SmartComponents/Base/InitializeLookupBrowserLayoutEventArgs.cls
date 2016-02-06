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
    File        : InitializeLookupBrowserLayoutEventArgs
    Purpose     : Event argument class for the InitializeLookupBrowserLayout
                  event of the SmartLookup
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 05 14:31:35 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY .
USING Progress.Lang.*                 FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Base.InitializeLookupBrowserLayoutEventArgs 
    INHERITS InitializeLayoutEventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Grid control used as the Lookup Browser                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Grid AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InitializeLookupBrowserLayoutEventArgs class
        Notes:                                                                        
        @param layout The reference to the UltraGridLayout instance associated with this event
        @param poGrid The reference to the UltraGrid that is initializing it's layout                                                                    
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InitializeLookupBrowserLayoutEventArgs (layout AS UltraGridLayout,
                                                               poGrid AS UltraGrid):
        SUPER (INPUT layout).
        
        THIS-OBJECT:Grid = poGrid . 
        
    END CONSTRUCTOR.

END CLASS.
