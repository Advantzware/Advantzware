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
    File        : IImageProvider
    Purpose     : Interface for classes that can serve images to visual 
                  Controls and may serve as an Image List / Cache
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 21:30:45 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.Framework.* FROM PROPATH .  
USING Progress.Lang.*                    FROM PROPATH .

INTERFACE Consultingwerk.Windows.Framework.IImageProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the Image object for the specified key
        Notes:   Should return ? when the image cound not be loaded, not throw an
                 error
        @param pcKey The key to return the Image for
        @return The Image object for the specified key or ?
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Drawing.Bitmap ImageForKey (pcKey AS CHARACTER).

END INTERFACE.