/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ServiceNameMappingService
    Purpose     : Default implementation of IServiceNameMappingService 
                  Interface. Supports the mapping of the business service 
                  name (e.g. Business Entity or Business Task) to the 
                  actual class name 
    Syntax      : 
    Description : Supports mapping of short names to actual class names 
                  (SCL-1171)
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 04 22:31:04 CET 2016
    Notes       : Reads service mapping from a file .servicenamemapping
                  From Consultingwerk/OERA/ttServiceNameMapping.i
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH.

CLASS Consultingwerk.OERA.ServiceNameMappingService
    IMPLEMENTS IServiceNameMappingService: 

    {Consultingwerk/OERA/ttServiceNameMapping.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ServiceNameMappingService class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ServiceNameMappingService ():
		DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

		SUPER ().

        ASSIGN cFileName = FileHelper:FindFile (".servicenamemapping":U) .		
		
		IF cFileName > "":U THEN 
		    TEMP-TABLE ttServiceNameMapping:READ-XML ("file":U, cFileName, ?, ?, ?) .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the actual business service name for the providede class 
                 name        
        Notes:   
        @param pcServiceName The service name as provided by the consumer
        @return The name of the actual business service implementing class
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetBusinessServiceName (pcServiceName AS CHARACTER):

        IF NOT CAN-FIND (ttServiceNameMapping WHERE ttServiceNameMapping.SourceServiceName = pcServiceName) THEN 
            RETURN pcServiceName .

        FIND ttServiceNameMapping WHERE ttServiceNameMapping.SourceServiceName = pcServiceName  .

        RETURN ttServiceNameMapping.MappedServiceName .

    END .

END CLASS.
