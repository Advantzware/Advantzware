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
    File        : ContextDatasetFactory
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 19 12:55:58 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                 FROM PROPATH.
USING Consultingwerk.OERA.Context.*         FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .  

CLASS Consultingwerk.OERA.Context.ContextDatasetFactory 
    IMPLEMENTS IContextDatasetFactory: 

    { Consultingwerk/OERA/Context/dsContext.i }

    /*------------------------------------------------------------------------------
        Purpose: Factory method for the context dataset                                                                       
        Notes:   Returns the handle for the Context Dataset
                 Initializes the Client Progress Version Number
        @return The handle of the context dataset instance                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE CreateContextDataset ():
        
        IF NOT SESSION:REMOTE THEN DO:
            CREATE eSessionContext . 
            ASSIGN eSessionContext.ClientProVersion = PROVERSION 
                   eSessionContext.ClientType       = SESSION:CLIENT-TYPE . 
        END.
        
        RETURN DATASET dsContext:HANDLE .

    END METHOD.

END CLASS.
