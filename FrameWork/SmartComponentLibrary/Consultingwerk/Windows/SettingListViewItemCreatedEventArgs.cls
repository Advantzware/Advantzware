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
    File        : SettingListViewItemCreatedEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Aug 19 22:57:02 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.EventArgs.

USING Consultingwerk.Windows.*            FROM PROPATH .  
USING Infragistics.Win.UltraWinListView.* FROM ASSEMBLY .
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Windows.SettingListViewItemCreatedEventArgs INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the settings object
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SettingsInstance AS Progress.Lang.Object NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraListViewItem
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UltraListViewItem AS UltraListViewItem NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SettingListViewItemCreatedEventArgs class
        Notes:   
        @param poItem The UltraListViewItem that has been created 
        @param poSetting The settings instance that has been created 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SettingListViewItemCreatedEventArgs (poItem AS UltraListViewItem,
                                                            poSetting AS Progress.Lang.Object):
        SUPER ().
        
        THIS-OBJECT:UltraListViewItem = poItem . 
        THIS-OBJECT:SettingsInstance  = poSetting .
        
    END CONSTRUCTOR.

END CLASS.
