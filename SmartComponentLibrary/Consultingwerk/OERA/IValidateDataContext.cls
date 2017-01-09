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
    File        : IValidateDataContext
    Purpose     : Interface for parameter objects for the invokable version
                  of the ValidateData method in the Business Entity 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 23 02:06:21 CEST 2015
    Notes       : SCL-1030
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IValidateDataContext:  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the current field
        Notes:   Typically the field that caused the validation during a Leave event
                 May be empty
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CurrentFieldName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the comma delimited list of fields that have been 
                 processed by the user and thus should be validated
        Notes:   May be empty
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ProcessedFields AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the comma delimited list of names of the tables to validate
        Notes:   CAN-DO Style syntax, e.g. *
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Tables AS CHARACTER NO-UNDO 
    GET.
    SET. 

END INTERFACE.
