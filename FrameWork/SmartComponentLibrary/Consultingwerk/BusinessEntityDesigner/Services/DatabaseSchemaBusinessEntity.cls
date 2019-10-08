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
    File        : BusinessEntityBusinessEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 13 19:45:41 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.BusinessEntity.
USING Progress.Lang.*.


CLASS Consultingwerk.BusinessEntityDesigner.Services.DatabaseSchemaBusinessEntity INHERITS BusinessEntity: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsDatabaseSchema.i }

    /*------------------------------------------------------------------------------
        Purpose:                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DatabaseSchemaBusinessEntity ():
        SUPER (DATASET dsDatabaseSchema:HANDLE).

    END CONSTRUCTOR.


    METHOD OVERRIDE PUBLIC VOID ReceiveData ():
        
/*        UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT IMPLEMENTED").*/

    END METHOD.

    METHOD OVERRIDE PUBLIC VOID ValidateData(  ):
        
/*        UNDO, THROW NEW Progress.Lang.AppError("METHOD NOT IMPLEMENTED").*/

    END METHOD.

END CLASS.