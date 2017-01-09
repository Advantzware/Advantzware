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
    File        : SmartDataCombo
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Feb 25 16:20:58 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.SmartComboControl FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.SmartDataCombo 
    INHERITS SmartComboControl : 

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Instance of SmartDataCombo and subscribes Handlers to
                 Events.
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDataCombo ():
        
        SUPER ().
        
        THIS-OBJECT:InitializeLayout:Subscribe (InitializeLayoutHandler).        
        
    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: This Method hides _ columns from Grid and ColumnChooser. All Bands and 
                 Columns are tested.
        Notes:   Event handler for the InizializeLayout event of the UltraGrid (parent
                 class).
        @param sender The reference to the object that raised the event
        @param e The InitializeLayoutEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeLayoutHandler (sender AS System.Object, 
                                                 e AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs):

        DEFINE VARIABLE i     AS INTEGER       NO-UNDO.
        DEFINE VARIABLE j     AS INTEGER       NO-UNDO.

        DO i = 0 TO THIS-OBJECT:DisplayLayout:Bands:Count - 1:
            DO j = 0 TO THIS-OBJECT:DisplayLayout:Bands[i]:Columns:Count - 1:
                IF THIS-OBJECT:DisplayLayout:Bands[i]:Columns[j]:Key BEGINS "_" THEN
                    ASSIGN
                        THIS-OBJECT:DisplayLayout:Bands[i]:Columns[j]:Hidden = TRUE .
            END.
        END.          
    END METHOD.

END CLASS.
