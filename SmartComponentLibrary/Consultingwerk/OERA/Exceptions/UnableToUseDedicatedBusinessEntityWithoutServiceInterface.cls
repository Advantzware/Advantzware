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
    File        : UnableToUseDedicatedBusinessEntityWithoutServiceInterface
    Purpose     : Exception thrown by the worker methods of the 
                  DatasetModel Performer class when trying to use a dedicated
                  Business Entity (on the backend) from the frontend
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 06 06:52:39 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.Exceptions.UnableToUseDedicatedBusinessEntityWithoutServiceInterface 
    INHERITS NotSupportedException {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UnableToUseDedicatedBusinessEntityWithoutServiceInterface 
                 class
        Notes:
        @param pcMethodName The name of the method that is not implemented
        @param pcClassName The name of the class that caused this NotSupportedException
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnableToUseDedicatedBusinessEntityWithoutServiceInterface (pcMethodName   AS CHARACTER,
                                                                                  pcClassName    AS CHARACTER):
        
        SUPER ((IF pcMethodName > "":U THEN pcMethodName ELSE "UseDedicatedBusinessEntity":U),
               pcClassName,
               "The use of UseDedicatedBusinessEntity is only supported when using the ServiceInterface."{&TRAN}).
        
    END CONSTRUCTOR.

END CLASS.
