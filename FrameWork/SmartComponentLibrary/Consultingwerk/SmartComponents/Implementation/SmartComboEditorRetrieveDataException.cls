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
    File        : SmartComboEditorRetrieveDataException
    Purpose     : Exception thrown by the SmartComboEditor:RetrieveData method
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 04 10:27:20 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                     FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.SmartComboEditorRetrieveDataException 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityName of this exception
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityTable of this exception
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the QuerySort of this exception
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QuerySort AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the QueryString of this exception
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QueryString AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Exception class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartComboEditorRetrieveDataException (pcErrorString AS CHARACTER,
                                                              piMessageNumber AS INTEGER):        
    
        SUPER (pcErrorString, piMessageNumber).
    
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartComboEditorRetrieveDataException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartComboEditorRetrieveDataException (poInnerException AS Error,
                                                              pcErrorString AS CHARACTER,
                                                              piMessageNumber AS INTEGER):        
        
        SUPER (poInnerException, pcErrorString, piMessageNumber).
        
    END CONSTRUCTOR.

END CLASS.
