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
    File        : DataSourceQueryInfo
    Purpose     : Holder class for data source query information
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 27 17:28:13 CET 2013
    Notes       : Default IDataSourceQueryInfo implementation
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.DataSourceQueryInfo 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    IMPLEMENTS IDataSourceQueryInfo: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the client query of the current request
        Notes:   This is the query string that was provided by the consumer 
    ------------------------------------------------------------------------------*/  
    {Consultingwerk/JsonSerializableProperty.i ClientQuery CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets query that was used to prepare the data source query
        Notes:   This is the query string that combines the client query string and 
                 the source default query
    ------------------------------------------------------------------------------*/      
    {Consultingwerk/JsonSerializableProperty.i PrepareQuery CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the source default query
        Notes:   This is the query string that is maintained by the actual data access
                 class 
    ------------------------------------------------------------------------------*/     
    {Consultingwerk/JsonSerializableProperty.i SourceDefaultQuery CHARACTER} .
    	 
	/*------------------------------------------------------------------------------
	   Purpose: Constructor for the DataSourceQueryInfo class
	   Notes:
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC DataSourceQueryInfo ():
		SUPER ().
		
		THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
		
	END CONSTRUCTOR.

END CLASS.
