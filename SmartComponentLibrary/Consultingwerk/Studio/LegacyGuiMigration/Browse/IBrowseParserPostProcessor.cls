/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IBrowseParserPostProcessor
    Purpose     : Service interface for classes that allow to post-process
                  the FrameParser output before presenting it to the user
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 10 12:09:28 CET 2014
    Notes       : Service Interface with no default implementation
  ----------------------------------------------------------------------*/

USING Consultingwerk.Studio.LegacyGuiMigration.Frame.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

INTERFACE Consultingwerk.Studio.LegacyGuiMigration.Browse.IBrowseParserPostProcessor:  

    {Consultingwerk/Studio/LegacyGuiMigration/Browse/ttBrowseColumn.i}

    /*------------------------------------------------------------------------------
        Purpose: Allows post-processing of the browse parser result 
        Notes:   Temp-Table ttBrowseColumn passed BY-REFERENCE from the BrowseParser
        @param pcFileName The name of the file to parse
        @param pcBrowseName The name of the browse to return the widgets for
        @param ttBrowseColumn INPUT-OUTPUT TABLE The temp-table with the browse widget information (from Consultingwerk/Studio/LegacyGuiMigration/Browse/ttBrowseColumn.i) 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PostProcessBrowseColumn (pcFileName AS CHARACTER,
                                                pcBrowseName AS CHARACTER,
                                                INPUT-OUTPUT TABLE ttBrowseColumn).

END INTERFACE.
