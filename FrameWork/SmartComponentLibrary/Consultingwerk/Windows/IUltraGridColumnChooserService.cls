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
    File        : IUltraGridColumnChooserService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Mon Dec 12 22:28:03 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY . 
USING Progress.Lang.*                 FROM ASSEMBLY .

INTERFACE Consultingwerk.Windows.IUltraGridColumnChooserService:  

    /*------------------------------------------------------------------------------
        Purpose: Raised when the ColumnChooser was closed
        Notes:                                     
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ColumnChooserClosed SIGNATURE VOID (sender AS Progress.Lang.Object, 
                                                            e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Closes the ColumnChooser 
        Notes:                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseColumnChooser ().

    /*------------------------------------------------------------------------------
        Purpose: Invokes the ColumnChooser for an UltraGrid instance
        Notes:                                      
        @param poGrid The reference to the UltraGridBase instance to invoke the ColumnChooser for                                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InvokeColumnChooser (poGrid AS UltraGridBase).

END INTERFACE.