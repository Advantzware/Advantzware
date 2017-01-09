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
    File        : ResourceFileHelper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 01 19:01:22 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .  
USING Consultingwerk.Util.Support.*          FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Util.ResourceFileHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ResourceFileHelper class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PROTECTED ResourceFileHelper ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList with the data element names in the resx file
        Notes:   
        @param pcFileName The name of the resx file
        @return The CharacterList of data element names
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC CharacterList GetElementNames (pcFileName AS CHARACTER):
		
		DEFINE VARIABLE oParser AS ResxFileParser NO-UNDO . 
		
		oParser = NEW ResxFileParser () .
		
		RETURN oParser:ParseResxFile (pcFileName) .
		
		FINALLY:
            GarbageCollectorHelper:DeleteObject (oParser) .		
        END FINALLY.

	END METHOD .

END CLASS.
