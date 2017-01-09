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
    File        : ITempFileManager
    Purpose     : Interface for services that support management of temporary
                  files. Temp files can be registered for delete on session
                  shutdown 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 21 19:11:57 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH . 

INTERFACE Consultingwerk.Framework.ITempFileManager:  

    /*------------------------------------------------------------------------------
        Purpose: Deletes all temp-files 
        Notes:   Typically called when ending the ABL session
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CleanUp ().

    /*------------------------------------------------------------------------------
        Purpose: Registers a temp-file for delete when the session ends 
        Notes:   
        @param pcFileName The name of the temp-file to delete
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterTempFile (pcFileName AS CHARACTER).

END INTERFACE.
