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
    File        : LocalizationHelper
    Purpose     : Provides supporting routines to the localitzation services
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 10 10:04:36 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Framework.Localization.* FROM PROPATH . 
USING Consultingwerk.Util.*                           FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Localization.LocalizationHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the resource file for a given class 
        Notes:   
        @param pcClassName The name of the class
        @return The file name of the resource file
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ClassNameToResourceFileName (pcClassName AS CHARACTER):
        
        DEFINE VARIABLE cResourceFileName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iIndex            AS INTEGER   NO-UNDO .
        
        ASSIGN cResourceFileName = ClassHelper:ClassNameToFile (pcClassName) 
               iIndex            = R-INDEX (cResourceFileName, ".":U) 
               cResourceFileName = SUBSTRING (cResourceFileName, 1, iIndex) + "resx":U .
        
        FILE-INFORMATION:FILE-NAME = cResourceFileName .
        
        RETURN FILE-INFORMATION:FULL-PATHNAME . 
        
    END METHOD .

END CLASS.
