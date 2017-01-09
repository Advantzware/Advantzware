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
    File        : AppServerConfigurationException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 30 11:24:04 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*           FROM PROPATH .  
USING Consultingwerk.Framework.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Framework.Exceptions.AppServerConfigurationException
    INHERITS Exception
    {&SERIALIZABLE}:         

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AppServerConfigurationException class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AppServerConfigurationException ():
        SUPER ("Unable to start application due to an issue with the AppServer configuration."{&TRAN}, 0).
        
    END CONSTRUCTOR.

END CLASS.
