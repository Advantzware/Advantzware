/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IUserInterfaceLanguageManager
    Purpose     : Interface for Services managing the user interface
                  language
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 15 22:10:59 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.Framework.* FROM PROPATH .
USING Progress.Lang.*                    FROM PROPATH .

INTERFACE Consultingwerk.Windows.Framework.IUserInterfaceLanguageManager:

    /*------------------------------------------------------------------------------
        Purpose: Initializes a Culture for the UserInterface
        Notes:   http://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.cultureinfo(v=vs.80).aspx
        @param pcCulture The name of the culture to initialize
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeUiCulture (pcCulture AS CHARACTER) .

END INTERFACE.
