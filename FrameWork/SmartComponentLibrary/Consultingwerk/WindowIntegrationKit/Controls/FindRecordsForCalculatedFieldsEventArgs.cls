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
    File        : FindRecordsForCalculatedFieldsEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jun 07 13:57:22 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.EventArgs.


USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.Controls.FindRecordsForCalculatedFieldsEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Indicates if the Event was handled
        Notes:   The subscribed of this event is expected to set this to true 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Handled AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraGridRow 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Row AS Infragistics.Win.UltraWinGrid.UltraGridRow NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FindRecordsForCalculatedFieldsEventArgs class
        Notes:   
        @param poRow The reference to the current UltraGridRow
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FindRecordsForCalculatedFieldsEventArgs (poRow AS Infragistics.Win.UltraWinGrid.UltraGridRow):
        SUPER ().
        
        THIS-OBJECT:Row = poRow . 
        
    END CONSTRUCTOR.

END CLASS.
