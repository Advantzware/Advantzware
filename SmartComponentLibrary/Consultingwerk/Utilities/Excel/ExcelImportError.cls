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
    File        : ExcelImportError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Feb 26 16:46:22 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.Exceptions.Exception.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Utilities.Excel.ExcelImportError INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the column number that caused the error                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Column AS INTEGER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the field name that caused the error                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the row number that caused the error                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Row AS INTEGER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the value that caused the error                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Value AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ExcepImportError class                                                                        
        Notes:                                           
        @param poInnerException The reference to the original error object
        @param pcFieldName The name of the imported field
        @param pcValue The value for the imported field
        @param piRow The current row
        @param piColumn The current column                             
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ExcelImportError (poInnerException AS Progress.Lang.Error,
                                         pcFieldName AS CHARACTER,
                                         pcValue AS CHARACTER,
                                         piRow AS INTEGER,
                                         piColumn AS INTEGER):
                                             
        SUPER (poInnerException, 
               SUBSTITUTE ("Error importing field &1 with value ~"&2~" from row &3, column &4.",
                           pcFieldName, pcValue, piRow, piColumn), 
               0).
        
        ASSIGN THIS-OBJECT:FieldName = pcFieldName
               THIS-OBJECT:Value     = pcValue 
               THIS-OBJECT:Row       = piRow 
               THIS-OBJECT:Column    = piColumn .         
    END CONSTRUCTOR.


END CLASS.