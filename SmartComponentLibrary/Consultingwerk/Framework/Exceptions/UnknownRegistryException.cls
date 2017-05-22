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
    File        : UnknownRegistryException
    Purpose     : Error class thrown from the Consultingwerk.Framework.Registry
                  class when the requested registry is not known
    Syntax      : 
    Description : Thrown by Consultingwerk.Dynamics.Framework.Registry
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed May 21 14:59:15 CEST 2008
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW . 

{ Consultingwerk/products.i }

USING Consultingwerk.Exceptions.Exception FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Framework.Exceptions.UnknownRegistryException 
    INHERITS Exception
    {&SERIALIZABLE}: 
        
    /*------------------------------------------------------------------------------
        Purpose: Returns the RegistryName of the UnknownRegistryException class
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RegistryName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UnknownRegistryException class
        Notes:   
        @param pcRegistryName The name of the registry that is unknown
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnknownRegistryException (pcRegistryName AS CHARACTER):
        SUPER (SUBSTITUTE ("The Registry &1 is not know."{&TRAN}, pcRegistryName), 0).    
        
        THIS-OBJECT:RegistryName = pcRegistryName . 
            
    END CONSTRUCTOR.
    
END CLASS.