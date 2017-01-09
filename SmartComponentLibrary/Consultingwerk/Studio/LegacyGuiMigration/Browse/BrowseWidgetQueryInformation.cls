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
    File        : BrowseWidgetQueryInformation
    Purpose     : Holder class for BROWSE Widget Query information (query
                  name and open query statements)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Mar 22 11:04:15 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.*            FROM PROPATH . 
USING Consultingwerk.Studio.LegacyGuiMigration.Browse.* FROM PROPATH .  
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.Studio.LegacyGuiMigration.Browse.BrowseWidgetQueryInformation: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the browse query buffers
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferNames AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Open query statements for the browse query
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OpenQueryStatements AS CharacterList NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the browse query
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY QueryName AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BrowseWidgetQueryInformation class
        Notes:
        @param pcQueryName The QueryName value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BrowseWidgetQueryInformation (pcQueryName AS CHARACTER):
        THIS-OBJECT ().
        
        THIS-OBJECT:QueryName = pcQueryName . 
        
    END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Constructor for the BrowseWidgetQueryInformation class
	    Notes:
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC BrowseWidgetQueryInformation ():
		SUPER ().
		
		THIS-OBJECT:OpenQueryStatements = NEW CharacterList ("":U, CHR(1)) . 
		
	END CONSTRUCTOR.

END CLASS.
