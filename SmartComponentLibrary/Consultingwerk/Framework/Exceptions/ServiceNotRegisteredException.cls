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
    File        : ServiceNotRegisteredException
    Purpose     : Error thrown from ServiceContainer:GetMandatoryService 
                  when the requested service type is notregistered with 
                  the Service Container
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 15 22:32:24 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.Exception.


USING Consultingwerk.Framework.Exceptions.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Framework.Exceptions.ServiceNotRegisteredException
    INHERITS Exception:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the class or interface of the service that 
                 is not registered
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ServiceClass AS Progress.Lang.Class NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ServiceAlreadyRegisteredException class
        Notes:   
        @param poServiceClass The reference to the Class or interface of the service that cannot be added
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ServiceNotRegisteredException (poServiceClass AS Progress.Lang.Class):
        SUPER (SUBSTITUTE ("The requested service type for ~"&1~" is not registered."{&TRAN}, 
                           poServiceClass:TypeName), 
               0) .
        
        THIS-OBJECT:ServiceClass = poServiceClass .
        
    END CONSTRUCTOR.

END CLASS.
