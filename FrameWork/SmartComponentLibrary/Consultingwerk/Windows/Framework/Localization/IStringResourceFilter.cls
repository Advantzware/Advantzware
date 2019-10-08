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
    File        : IStringResourceFilter
    Purpose     : Interface for classes that provide filter functionality
                  for the String resources for the translation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat May 11 17:23:09 CEST 2013
    Notes       : This Interface does not actually enforce any members,
                  it's just a logical flag for the service container
                  access
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.Framework.Localization.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

INTERFACE Consultingwerk.Windows.Framework.Localization.IStringResourceFilter:  
  
END INTERFACE.
