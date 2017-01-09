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
    File        : ImageProvider
    Purpose     : Default IImageProvider implementation. Retrieves Images
                  using a PROPATH lookup on the local file system
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 21:33:13 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Framework.IImageProvider.


USING Consultingwerk.Windows.Framework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.ImageProvider IMPLEMENTS IImageProvider: 

    DEFINE VARIABLE oImages AS "System.Collections.Generic.Dictionary<System.String,System.Drawing.Bitmap>":U NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ImageProvider class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ImageProvider ():
		SUPER ().
		
		/* Mike Fechner, Consultingwerk Ltd. 08.01.2013
           no :U here to avoid crash of Compiler on OpenEdge 10.2B */
		ASSIGN oImages = NEW "System.Collections.Generic.Dictionary<System.String,System.Drawing.Bitmap>" /*:U*/ () .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Image object for the specified key
        Notes:   Should return ? when the image cound not be loaded, not throw an
                 error
        @param pcKey The key to return the Image for
        @return The Image object for the specified key or ?
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC System.Drawing.Bitmap ImageForKey (pcKey AS CHARACTER):
		
		DEFINE VARIABLE oImage AS System.Drawing.Bitmap NO-UNDO.

        IF oImages:ContainsKey (pcKey) THEN 
            RETURN oImages [pcKey] .
            
        DO ON ERROR UNDO, THROW:            
            oImage = Consultingwerk.Util.ImageHelper:Load (pcKey) .

            CATCH err AS Progress.Lang.Error :
            	RETURN ? .	
            END CATCH.
        END.

        IF NOT VALID-OBJECT (oImage) THEN 
            RETURN ? . 

        oImages:Add (pcKey, oImage) .

		RETURN oImage .
		
	END METHOD .

END CLASS.
