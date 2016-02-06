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
    File        : XmlConfigurationProvider
    Purpose     : Configuration provider based on a XML file
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Aug 28 17:42:30 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*            FROM PROPATH . 
USING Consultingwerk.Framework.*  FROM PROPATH .  
USING Consultingwerk.Util.*       FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Framework.XmlConfigurationProvider 
    IMPLEMENTS IConfigurationProvider: 

    DEFINE TEMP-TABLE ttConfiguration NO-UNDO  
        FIELD ConfigurationName  AS CHARACTER 
        FIELD ConfigurationValue AS CHARACTER
        INDEX ConfigurationName IS UNIQUE ConfigurationName .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the XmlConfigurationProvider class
        Notes:   
        @param pcFileName The file name to read configuration data from
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC XmlConfigurationProvider (pcFileName AS CHARACTER):
		SUPER ().
		
        Consultingwerk.Assertion.FileAssert:Exists (pcFileName) .

        FILE-INFO:FILE-NAME = pcFileName .
		
        LogManager:WriteMessage(SUBSTITUTE ("########################################################################~n":U +
                                            "### Loading configuration~n":U + 
                                            "###    File Name:          &1~n":U +
                                            "###    Absolute File Name: &2~n":U + 
                                            "########################################################################~n":U, 
                                            pcFileName,
                                            FILE-INFO:FULL-PATHNAME), "ConfigurationProvider":U) .
		
		TEMP-TABLE ttConfiguration:READ-XML ("file":U,
		                                     FILE-INFO:FULL-PATHNAME,
		                                     "empty":U,
		                                     ?,
		                                     ?) .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a configuration setting
        Notes:   
        @param pcName The configuration property name
        @return The value for the configuration property or ? when not set
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetValue (pcName AS CHARACTER):
		
		FIND ttConfiguration WHERE ttConfiguration.ConfigurationName = pcName 
		    NO-LOCK NO-ERROR . 
		    
		IF AVAILABLE ttConfiguration THEN 
		    RETURN ttConfiguration.ConfigurationValue .
		ELSE 
		    RETURN ? .
		    
		FINALLY:
            ERROR-STATUS:ERROR = FALSE NO-ERROR . 		
        END FINALLY.    

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a configuration setting or the default value when the setting 
                 is not available
        Notes:   
        @param pcName The configuration property name
        @param pcDefault The default value to return when the setting is not availalbe
        @return The value for the configuration property or ? when not set
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetValue (pcName AS CHARACTER, 
	                                  pcDefault AS CHARACTER):
		
        FIND ttConfiguration WHERE ttConfiguration.ConfigurationName = pcName 
            NO-LOCK NO-ERROR . 
            
        IF AVAILABLE ttConfiguration THEN 
            RETURN ttConfiguration.ConfigurationValue .
        ELSE 
            RETURN pcDefault .
            
        FINALLY:
            ERROR-STATUS:ERROR = FALSE NO-ERROR .       
        END FINALLY.    

	END METHOD .

END CLASS.
