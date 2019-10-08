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
    File        : IThrowsErrorsOnInvalidSourceMapping
    Purpose     : Interface for Data Source Query manipulation routines
                  that allow turning on or off if errors are thrown for
                  invalid (unmapped) columns in the client side query 
                  string
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 18 22:35:34 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.OERA.Query.IThrowsErrorsOnInvalidSourceMapping:  

    /*------------------------------------------------------------------------------
        Purpose: Get and sets if this class throws an error when the client query 
                 string contains columns that are not mapped to a data source column                                                                     
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ThrowErrorsOnInvalidSourceMapping AS LOGICAL NO-UNDO 
    GET.
    SET. 
      
END INTERFACE.
