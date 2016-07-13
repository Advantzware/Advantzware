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
    File        : MethodNotImplementedException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Aug 09 22:31:12 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.* FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartComponents.Implementation.MethodNotImplementedException
    INHERITS AppError
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: The Constructor initializes a new instance of MethodNotImplementedException.
        Notes:   
        @param pcMethodName Name of the Method where the error occured
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MethodNotImplementedException (pcMethodName AS CHARACTER):
        
        SUPER (SUBSTITUTE ("Method &1 is not implemented by the class.", 
                           pcMethodName), 0).
        
    END CONSTRUCTOR.

END CLASS.