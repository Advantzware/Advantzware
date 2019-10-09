/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IAblFrameMigrationFormCustomizer
    Purpose     : Interface for service that allow customization of the 
                  AblFrameMigrationForm
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 24 20:48:32 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Implementation.*  FROM PROPATH . 
USING Consultingwerk.Studio.LegacyGuiMigration.Frame.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

INTERFACE Consultingwerk.Studio.LegacyGuiMigration.Frame.IAblFrameMigrationFormCustomizer:  

    {Consultingwerk/Studio/LegacyGuiMigration/Frame/ttFrameWidget.i &REFERENCE-ONLY=REFERENCE-ONLY}
    {Consultingwerk/Studio/LegacyGuiMigration/Frame/dsTrigger.i &REFERENCE-ONLY=REFERENCE-ONLY}

    DEFINE TEMP-TABLE ttSelectedFrameWidget NO-UNDO REFERENCE-ONLY
        LIKE ttFrameWidget
        FIELD IsSelected             AS LOGICAL   INIT TRUE 
        FIELD AlternativeControlType AS CHARACTER INIT "":U.

    /*------------------------------------------------------------------------------
        Purpose: Method called when the AblFrameMigrationForm has started and initializes
                 the customization service                                                                   
        Notes:   Passes references to some key components of the tool to 
                 the plugin, use this to initialize the plugin, subscribe to events                                                                     
        @param ttSelectedFrameWidget Temp-Table with the list of widgets BIND
        @param dsTrigger The dataset with the trigger information BIND
        @param poForm The main Form of the BusinessEntityDesigner
        @param poToolbar The reference to the SmartToolbarController used by the BusinessEntityDesigner                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Startup (TABLE ttSelectedFrameWidget BIND,
                                DATASET dsTrigger BIND,
                                poForm AS AblFrameMigrationForm,
                                poToolbar AS SmartToolbarController) .
  
END INTERFACE.
