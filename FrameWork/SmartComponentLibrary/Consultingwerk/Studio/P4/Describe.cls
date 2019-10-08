/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : Describe
    Purpose     : Wrapper to the P4 Describe command
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jun 20 10:33:38 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Studio.P4.* FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.Studio.P4.Describe: 

    /*------------------------------------------------------------------------------
        Purpose: Write a changelist description to the specified file 
        Notes:   
        @param pcChange The number of the Changelist
        @param pcFileName The name of the file to export to  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ToFile (pcChange AS CHARACTER, 
                               pcFileName AS CHARACTER):
        
        DEFINE VARIABLE cDescription AS LONGCHAR  NO-UNDO .
    
        cDescription = THIS-OBJECT:ToLongchar (pcChange) . 
        
        COPY-LOB FROM cDescription TO FILE pcFileName .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a changelist description 
        Notes:   
        @param pcChange The number of the Changelist
        @return The LONGCHAR with the change list description
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR ToLongchar (pcChange AS CHARACTER):

        DEFINE VARIABLE cDescription AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO .

        INPUT THROUGH VALUE(SUBSTITUTE ("p4 describe -s &1":U, pcChange)) .
        
        REPEAT:
            IMPORT UNFORMATTED cLine . 
        
            ASSIGN cDescription = cDescription + cLine + "~n":U .
        
        END.
        
        RETURN cDescription.

    END METHOD .

END CLASS.
