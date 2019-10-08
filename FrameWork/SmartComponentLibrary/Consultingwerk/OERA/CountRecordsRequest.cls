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
    File        : CountRecordsRequest
    Purpose     : Parameter object for count record requests
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 25 22:32:04 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.CountRecordsRequest 
    INHERITS FetchDataRequest 
    IMPLEMENTS ICountRecordsRequest: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the number of query results 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i NumResults INTEGER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the value that should be returned when stop-after occurs
        Notes:   
    ------------------------------------------------------------------------------*/       
    {Consultingwerk/JsonSerializableProperty.i StopAfterNumResults INTEGER} .    
    
    /*-------------------------------------------------------------------------------
        Purpose: Constructor for the CountRecordsRequest class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CountRecordsRequest ():
        SUPER ().
        
        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CountRecordsRequest class                                                                       
        Notes:    
        @param pcTables The tables to retrieve, Tables attribute
        @param pcQueries The CHR(1) delimited list of query strings per table, Queries attribute
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CountRecordsRequest (pcTables AS CHARACTER,
                                            pcQueries AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN 
            THIS-OBJECT:Tables     = REPLACE (pcTables, " ":U, "":U)
            THIS-OBJECT:Queries    = pcQueries .  
        
    END CONSTRUCTOR.
    
END CLASS.
