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
    File        : IAblBrowseMigrationPreProcessor
    Purpose     : Service interface for classes that allow to pre-process
                  the BrowseParser output before presenting it to the user
                  This service allows to preassign the IsSelected and AltrenativeControlType
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 10 12:09:28 CET 2014
    Notes       : Service Interface with no default implementation
                  {Consultingwerk/Studio/LegacyGuiMigration/Browse/ttBrowseColumn.i}
                    DEFINE TEMP-TABLE ttSelectedBrowseColumn NO-UNDO
                        LIKE ttBrowseColumn
                        FIELD IsSelected             AS LOGICAL   INIT TRUE
                        FIELD AlternativeControlType AS CHARACTER INIT "":U.
  ----------------------------------------------------------------------*/

USING Consultingwerk.Studio.LegacyGuiMigration.Browse.* FROM PROPATH .  
USING Progress.Lang.*                                   FROM PROPATH .

INTERFACE Consultingwerk.Studio.LegacyGuiMigration.Browse.IAblBrowseMigrationPreProcessor:  

    {Consultingwerk/Studio/LegacyGuiMigration/Browse/ttBrowseColumn.i}

    DEFINE TEMP-TABLE ttSelectedBrowseColumn NO-UNDO
        LIKE ttBrowseColumn
        FIELD IsSelected             AS LOGICAL   INIT TRUE
        FIELD AlternativeControlType AS CHARACTER INIT "":U.
        
    /*------------------------------------------------------------------------------
        Purpose: Allows post-processing of the Browse parser result 
        Notes:   Temp-Table ttSelectedBrowseColumn passed BY-REFERENCE from the BrowseParser
        @param pcFileName The name of the file to parse
        @param pcBrowseName The name of the Browse to return the widgets for
        @param ttSelectedBrowseColumn INPUT-OUTPUT TABLE The temp-table with the Browse widget information
        @param poBrowseSettings INPUT-OUTPUT The reference to the BrowseSettings instance with the parameters related to the BindingSource settings and browse or form name  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PreProcessBrowseColumns (pcFileName AS CHARACTER,
                                                pcBrowseName AS CHARACTER,
                                                INPUT-OUTPUT TABLE ttSelectedBrowseColumn,
                                                INPUT-OUTPUT poBrowseSettings AS BrowseSettings).

END INTERFACE.
