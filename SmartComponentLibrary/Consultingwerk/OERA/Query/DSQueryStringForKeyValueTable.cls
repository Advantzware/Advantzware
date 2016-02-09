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
    File        : DSQueryStringForKeyValueTable
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Feb 07 15:27:26 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.OERA.*            FROM PROPATH . 
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH.
USING Consultingwerk.OERA.Query.*      FROM PROPATH .  
USING Consultingwerk.Util.*            FROM PROPATH.
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.Query.DSQueryStringForKeyValueTable 
    INHERITS DSQueryString: 

    DEFINE VARIABLE mhInstance                    AS IDSQueryStringSource          NO-UNDO .
    DEFINE VARIABLE mcTable                       AS CHARACTER                     NO-UNDO .
    DEFINE VARIABLE oFetchDataByKeyTableParameter AS IFetchDataByKeyTableParameter NO-UNDO . 
    DEFINE VARIABLE cFieldPairs                   AS CHARACTER                     NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DSQueryStringForKeyValueTable class                                                                       
        Notes:   
        @param pcQueryString The query string from the current read request
        @param phInstance The reference to the owning DataAccess class (for call backs)
        @param pcTable The name of the temp-table this DSQueryString instance is providing source query manipulation for                                                                     
        @param plFilterUsingInnerJoin The value for the FilterUsingInnerJoin property
        @param poFetchDataByKeyTableParameter The IFetchDataByKeyTableParameter with the data for the current poFetchDataByKeyTableParameter AS FetchDataByKeyTable call
        @param pcFieldPairs The DATA-SOURCE field mapping
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DSQueryStringForKeyValueTable (pcQueryString                  AS CHARACTER,
                                                      phInstance                     AS DataAccess,
                                                      pcTable                        AS CHARACTER,
                                                      plFilterUsingInnerJoin         AS LOGICAL,
                                                      poFetchDataByKeyTableParameter AS IFetchDataByKeyTableParameter,
                                                      pcFieldPairs                   AS CHARACTER):

        SUPER (pcQueryString,
               phInstance,
               pcTable,
               plFilterUsingInnerJoin) .

        ASSIGN mhInstance                    = phInstance
               mcTable                       = pcTable
               oFetchDataByKeyTableParameter = poFetchDataByKeyTableParameter 
               cFieldPairs                   = pcFieldPairs .

    END CONSTRUCTOR . 

	/*------------------------------------------------------------------------------
        Purpose: Returns the default query string for the data source                                                                       
        Notes:   Injects the join creteria from the key value table to the source tables                                                 
        @return The default query string                 
	------------------------------------------------------------------------------*/
	METHOD OVERRIDE PUBLIC CHARACTER DefaultQuery ():
		
		DEFINE VARIABLE cQueryString     AS CHARACTER NO-UNDO .
	    DEFINE VARIABLE cNewQueryString  AS CHARACTER NO-UNDO .
		DEFINE VARIABLE i                AS INTEGER   NO-UNDO .
		DEFINE VARIABLE cSourceField     AS CHARACTER NO-UNDO .
		DEFINE VARIABLE cSourceTableName AS CHARACTER NO-UNDO .
		DEFINE VARIABLE hBuffer          AS HANDLE    NO-UNDO .
		DEFINE VARIABLE cAnd             AS CHARACTER NO-UNDO .
		DEFINE VARIABLE cJoin            AS CHARACTER NO-UNDO .
		
		ASSIGN cQueryString = SUPER:DefaultQuery() .
		
		IF mcTable = ENTRY (1, oFetchDataByKeyTableParameter:Tables) THEN DO:
		
		    IF cQueryString BEGINS "FOR":U THEN 
		        ASSIGN cQueryString = TRIM (SUBSTRING (cQueryString, 4)) .
		    ELSE                        
		        IF cQueryString BEGINS "PRESELECT":U THEN 
		            ASSIGN cQueryString = TRIM (SUBSTRING (cQueryString, 10)) .
		
		    IF cQueryString BEGINS "EACH":U OR cQueryString BEGINS "LAST":U THEN 
		        ASSIGN cQueryString = TRIM (SUBSTRING (cQueryString, 5)) .
		    ELSE 
	            IF cQueryString BEGINS "FIRST":U THEN 
                    ASSIGN cQueryString = TRIM (SUBSTRING (cQueryString, 6)) .
    	
    	    ASSIGN cSourceTableName               = ENTRY (1, cQueryString, " ":U) 
    	           ENTRY (1, cQueryString, " ":U) = "":U 
    	           cQueryString                   = TRIM (cQueryString) .
    	
    	    IF cQueryString BEGINS "WHERE":U THEN 
    	       ASSIGN cAnd         = "AND ":U
    	              cQueryString = TRIM (SUBSTRING (cQueryString, 6)) .
    	    ELSE 
    	        cAnd = " ":U . 
    	
    	    ASSIGN hBuffer = oFetchDataByKeyTableParameter:KeyValueTableHandle:DEFAULT-BUFFER-HANDLE .
    	
		    ASSIGN cNewQueryString = SUBSTITUTE ("FOR EACH &1, EACH &2 WHERE ":U, 
		                                         hBuffer:NAME,
		                                         cSourceTableName) .
        
            DO i = 1 TO hBuffer:NUM-FIELDS:
		
		        ASSIGN cSourceField = THIS-OBJECT:SourceColumn (cFieldPairs, 
		                                                        SUBSTITUTE ("&1.&2":U, 
                                                                            mcTable,
                                                                            hBuffer:BUFFER-FIELD (i):NAME)) 
                                                                            
                      cNewQueryString = cNewQueryString + 
                                        SUBSTITUTE ("&1 &2 = &3.&4 ":U,
                                                    (IF i > 1 THEN "AND":U ELSE "":U),
                                                    cSourceField,
                                                    hBuffer:NAME,
                                                    hBuffer:BUFFER-FIELD (i):NAME) .
		    END.
		
	        ASSIGN cQueryString = cNewQueryString + cAnd + cQueryString .
		END .

		RETURN cQueryString.
		
		CATCH err AS Progress.Lang.Error :
        	ErrorHelper:ShowErrorMessage(err) .	
        END CATCH.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the database field name matching a temp-table field name
                 contained in a consumers query string (query string vs. temp-table
                 definition) 
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of 
                 DataAccess:FetchData () query preparation                        
                 TO-DO: Provide code for alternative mapping                                             
        @param pcFieldPairs The field mapping of the data-source 
        @param pcColumn The temp table column name to return the source (database table) field name for             
        @return The database field name mapped to the passed in temp-table field
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER SourceColumn (pcFieldPairs AS CHARACTER,
                                          pcColumn AS CHARACTER):

        DEFINE VARIABLE iPos AS INTEGER NO-UNDO.
    
        ASSIGN iPos = LOOKUP(pcColumn, pcFieldPairs) .
        
        IF iPos MODULO 2 = 1 THEN  
            RETURN ENTRY(LOOKUP(pcColumn, pcFieldPairs) + 1, pcFieldPairs).    
        ELSE 
            UNDO, THROW NEW QueryStringForKeyValueTableException
                        (SUBSTITUTE ("No source field mapping available for column &1"{&TRAN},
                                     pcColumn), 0). 

    END METHOD.

        
END CLASS.

    