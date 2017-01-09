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
    File        : WriteFileEventArgs
    Purpose     : Evnet Argument for the WriteFile Event of the 
                  BusinessEntityDatasetController
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Wed Apr 01 23:12:06 CEST 2015
    Notes       : The Cancel property will skip the output of a single 
                  file but not stop the whole business entity generation 
                  process. This should be achieved by throwing an error 
                  from and event handler
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.WriteFileEventArgs 
    INHERITS CancelableEventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the FileName of the file beeing written
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Purpose of the file beeing written
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FilePurpose AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets a new file name 
        Notes:   Allows redirecting the output to a new file
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY NewFileName AS CHARACTER NO-UNDO 
	GET.
	SET. 

	/*------------------------------------------------------------------------------
	    Purpose: Constructor for the WriteFileEventArgs class
	    Notes:
        @param pcFilePurpose The purpose of the file being written
        @param pcFileName The name of the file being written
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC WriteFileEventArgs (pcFilePurpose AS CHARACTER,
                                           pcFileName AS CHARACTER):
		SUPER ().
		
		ASSIGN THIS-OBJECT:FilePurpose = pcFilePurpose 
		       THIS-OBJECT:FileName    = pcFileName .  
		
	END CONSTRUCTOR.

END CLASS.
