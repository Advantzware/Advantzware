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
    File        : AppServerConnectionException
    Purpose     : Error class thrown when there is an issue connecting
                  to the AppServer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : Thu Apr 18 22:34:11 CEST 2013
    Notes       : Thrown from the AppServerServiceManager
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*           FROM PROPATH .
USING Consultingwerk.Framework.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Framework.Exceptions.AppServerConnectionException 
    INHERITS Exception
    IMPLEMENTS IErrorTitle
    {&SERIALIZABLE}:  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the AppServer Partition associated with
                 this error
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Partition AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Title to be used in the Error Dialog or Message Box 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Title AS CHARACTER NO-UNDO INIT "AppServer Connection"{&TRAN}
	GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AppServerConnectionException class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AppServerConnectionException ():
        
        SUPER ("Unable to connect to the AppServer."{&TRAN}, 0).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AppServerConnectionException class
        Notes:   
        @param pcPartition The AppServer Partition that failed to be connected
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC AppServerConnectionException (pcPartition AS CHARACTER):
	    
		SUPER (SUBSTITUTE ("Unable to connect to the AppServer partition named &1."{&TRAN},
		                   pcPartition),
		       0).
		
		THIS-OBJECT:Partition = pcPartition. 
		
	END CONSTRUCTOR.

END CLASS.
