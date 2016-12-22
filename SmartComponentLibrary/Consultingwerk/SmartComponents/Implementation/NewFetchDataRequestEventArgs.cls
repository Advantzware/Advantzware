/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : NewFetchDataRequestEventArgs
    Purpose     : Event argument class for the NewFetchDataRequest event    
                  of the SmartBusinessEntityAdapter class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 20 00:03:34 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.NewFetchDataRequestEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the FetchDataRequest object                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY FetchDataRequest AS IFetchDataRequest NO-UNDO 
	GET.
	PRIVATE SET. 

	/*------------------------------------------------------------------------------
		Purpose: Constructor for the NewFetchDataRequestEventArgs class  																	  
		Notes:  																	  
	    @param poFetchDataRequest The IFetchDataRequest instand for this object
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC NewFetchDataRequestEventArgs (poFetchDataRequest AS IFetchDataRequest):
		SUPER ().
		
		THIS-OBJECT:FetchDataRequest = poFetchDataRequest . 
		
	END CONSTRUCTOR.

END CLASS.
