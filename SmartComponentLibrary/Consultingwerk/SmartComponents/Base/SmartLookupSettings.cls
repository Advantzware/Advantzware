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
    File        : SmartLookupSettings
    Purpose     : Default Settings for the SmartLookupSettings class  
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 17 22:05:24 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.SmartLookupSettings: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Key used as the Shortcut for opening the Lookup 
                 Dialog
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ShortcutKey AS System.Windows.Forms.Keys NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Static Constructor for the SmartLookupSettings class
        Notes:   Assigns the default for the ShortcutKey setting    
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR STATIC SmartLookupSettings ():

        SmartLookupSettings:ShortcutKey = System.Windows.Forms.Keys:F3 .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartLookupSettings class
        Notes:   Disallow instance creation   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE SmartLookupSettings ():
        SUPER ().
        
    END CONSTRUCTOR.

END CLASS.
