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
    File        : IAblBrowseMigrationFormCustomizer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 24 21:01:13 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Implementation.*  FROM PROPATH . 
USING Consultingwerk.Studio.LegacyGuiMigration.Browse.* FROM PROPATH .  
USING Progress.Lang.*                                   FROM PROPATH .

INTERFACE Consultingwerk.Studio.LegacyGuiMigration.Browse.IAblBrowseMigrationFormCustomizer:  

    {Consultingwerk/Studio/LegacyGuiMigration/Browse/ttBrowseColumn.i &REFERENCE-ONLY=REFERENCE-ONLY}
    {Consultingwerk/Studio/LegacyGuiMigration/Browse/dsTrigger.i &REFERENCE-ONLY=REFERENCE-ONLY}

    DEFINE TEMP-TABLE ttSelectedBrowseColumn NO-UNDO REFERENCE-ONLY
        LIKE ttBrowseColumn
        FIELD IsSelected             AS LOGICAL   INIT TRUE
        FIELD AlternativeControlType AS CHARACTER INIT "":U.
  
    /*------------------------------------------------------------------------------
        Purpose: Method called when the AblBrowseMigrationForm has started and initializes
                 the customization service                                                                   
        Notes:   Passes references to some key components of the tool to 
                 the plugin, use this to initialize the plugin, subscribe to events                                                                     
        @param ttSelectedBrowseColumn Temp-Table with the list of widgets BIND
        @param dsTrigger The dataset with the trigger information BIND
        @param poForm The main Form of the BusinessEntityDesigner
        @param poToolbar The reference to the SmartToolbarController used by the BusinessEntityDesigner                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Startup (TABLE ttSelectedBrowseColumn BIND,
                                DATASET dsTrigger BIND,
                                poForm AS AblBrowseMigrationForm,
                                poToolbar AS SmartToolbarController) .
  
  
END INTERFACE.