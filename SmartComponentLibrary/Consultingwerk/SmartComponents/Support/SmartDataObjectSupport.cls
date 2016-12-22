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
    File        : SmartDataObjectSupport
    Purpose     : Design Time Support for SmartDataObject based 
                  data binding
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 31 10:07:48 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Support.SmartDataObjectSupport   : 

    /*------------------------------------------------------------------------------
        Purpose: File-Open Dialog to pick an SmartDataObject Filename                                                                        
        Notes:   Used for the property sheets of SmartDataObjectAdapter and
                 SmartDataObjectBindingSource 
        @param pcFileName INPUT-OUTPUT The file name of the SmartDataObject   
        @return Logical value indicating if the user has chosen OK in the file open dialog                                                                    
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL FileOpenSmartDataObject (INPUT-OUTPUT pcFileName AS CHARACTER):
        
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lOk       AS LOGICAL NO-UNDO.
        
        IF pcFileName > "":U THEN 
            ASSIGN FILE-INFO:FILE-NAME = pcFileName
                   cFileName           = FILE-INFO:FULL-PATHNAME .
                   
        SYSTEM-DIALOG GET-FILE cFileName
            FILTERS "ADM2 SmartDataObjects (*.w)":U "*.w":U
            INITIAL-FILTER 1
            DEFAULT-EXTENSION "w":U
            MUST-EXIST 
            TITLE "Choose SmartDataObject"
            USE-FILENAME 
            UPDATE lOk .                    
        
        IF NOT lOk THEN RETURN lOk.              
        
        RUN adecomm/_relname.p (cFileName, 
                                "MUST-EXIST":U, 
                                OUTPUT cFileName).
               
        IF cFileName > "":U THEN          
            ASSIGN pcFileName = cFileName 
                   lOk        = TRUE .
        ELSE 
            ASSIGN lOk = FALSE . 
            
        RETURN lOk. 
        
    END METHOD.

 END CLASS.