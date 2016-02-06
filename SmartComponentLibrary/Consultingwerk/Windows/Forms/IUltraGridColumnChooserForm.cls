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
    File        : IUltraGridColumnChooserForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 13 09:00:51 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Windows.Forms.IUltraGridColumnChooserForm:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Column Chooser Control
        Notes:                                      
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnChooser AS Infragistics.Win.UltraWinGrid.UltraGridColumnChooser NO-UNDO 
    GET.  


END INTERFACE.