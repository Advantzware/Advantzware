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
    File        : SmartToolbarControllerSettings
    Purpose     : Defaults for the SmartToolbarController class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 23 18:21:11 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.SmartToolbarControllerSettings: 

    /*------------------------------------------------------------------------------
        Purpose: Controls if SmartToolbarController instances automatically close 
                 their Form when the "Exit" tool is clicked
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SmartToolbarControllerDefaultExit AS LOGICAL NO-UNDO INIT FALSE  
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartToolbarControllerSettings class
        Notes:   Disable instance creation   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE SmartToolbarControllerSettings ():
        SUPER ().
        
    END CONSTRUCTOR.




 

END CLASS.
