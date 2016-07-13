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
    File        : UserInterfaceCultureDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 30.05.2013 21:47:56 
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Framework.Localization.* FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH . 
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Localization.UserInterfaceCultureDataAccess INHERITS DataAccess: 

    { Consultingwerk/Windows/Framework/Localization/dsUserInterfaceCulture.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE VARIABLE cCachedCultureType AS CHARACTER NO-UNDO.

    DEFINE TEMP-TABLE ttUserInterfaceCulture NO-UNDO 
        LIKE eUserInterfaceCulture . 

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_ttUserInterfaceCulture FOR ttUserInterfaceCulture .
    @DefineDataSourcesEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserInterfaceCultureDataAccess class                                                                      
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.                                                                        
        @param phDataset The handle of the Business Entity dataset 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserInterfaceCultureDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        BindDataset (DATASET-HANDLE phDataset BIND) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():
        
        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsUserInterfaceCulture:HANDLE, FALSE) .
        
        @AttachDataSourcesStart.
        BUFFER eUserInterfaceCulture:ATTACH-DATA-SOURCE (DATA-SOURCE src_ttUserInterfaceCulture:HANDLE, "":U) .
        @AttachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received 
                 Dataset reference                                                                    
        Notes:   Invoked from the class constructor   
        @param dsUserInterfaceCulture DATASET to bind to                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsUserInterfaceCulture BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers                                                                      
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsUserInterfaceCulture:HANDLE, FALSE) .
    
        @DetachDataSourcesStart.
        BUFFER eUserInterfaceCulture:DETACH-DATA-SOURCE () .        
        @DetachDataSourcesEnd.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetches matching languages
        Notes:   
        @param poFetchDataRequest The IFetchDataRequest instance with the dat afor this call
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PUBLIC VOID FetchData (poFetchDataRequest AS Consultingwerk.OERA.IFetchDataRequest):
		
        DEFINE VARIABLE cCultureType     AS CHARACTER                       NO-UNDO INIT "NeutralCultures":U.
		
        DEFINE VARIABLE oQueryParser     AS Consultingwerk.QueryParser      NO-UNDO . 
        DEFINE VARIABLE oQueryExpression AS Consultingwerk.IQueryExpression NO-UNDO . 
        DEFINE VARIABLE oFieldExpression AS Consultingwerk.QueryExpression  NO-UNDO . 
                    
        IF poFetchDataRequest:Queries > "":U THEN DO:                    
                    
            oQueryParser = NEW Consultingwerk.QueryParser () .
            oQueryExpression = oQueryParser:ParseQueryString (poFetchDataRequest:Queries) .
            oFieldExpression = Consultingwerk.QueryExpression:GetExpressionForField ("eUserInterfaceCulture.CultureType":U, 
                                                                                     oQueryExpression) .                    
                        
            IF VALID-OBJECT (oFieldExpression) THEN DO:
                    
                IF TYPE-OF (oFieldExpression:SecondOperand, Consultingwerk.CharacterHolder) THEN 
                    cCultureType = CAST (oFieldExpression:SecondOperand, Consultingwerk.CharacterHolder):Value .                     
            END.
        END.		
		
        THIS-OBJECT:PopulateCultureTable (cCultureType) .
        		
		SUPER:FetchData(INPUT poFetchDataRequest).

	END METHOD .

	/*------------------------------------------------------------------------------
		Purpose: Populates the ttUserInterfaceCulture temp-table   																	  
		Notes:  																	
		@param pcCultureType The name of the System.Globalization.CultureTypes to retrieve cultures for  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateCultureTable (pcCultureType AS CHARACTER):
		
        DEFINE VARIABLE oCultures     AS "System.Globalization.CultureInfo[]":U NO-UNDO . 
        DEFINE VARIABLE oCultureTypes AS System.Globalization.CultureTypes      NO-UNDO. 
        DEFINE VARIABLE i             AS INTEGER                                NO-UNDO .

        IF pcCultureType = cCachedCultureType THEN 
            RETURN . 

        EMPTY TEMP-TABLE ttUserInterfaceCulture . 

        ASSIGN cCachedCultureType = pcCultureType .  

        oCultureTypes = {Consultingwerk/parse-enum.i System.Globalization.CultureTypes pcCultureType} .

        oCultures = System.Globalization.CultureInfo:GetCultures 
                        (oCultureTypes) .

        {Consultingwerk/foreach.i System.Globalization.CultureInfo oCulture in oCultures}
            
            CREATE ttUserInterfaceCulture.
            ASSIGN ttUserInterfaceCulture.DisplayName = oCulture:DisplayName 
                   ttUserInterfaceCulture.EnglishName = oCulture:EnglishName 
                   ttUserInterfaceCulture.Name        = oCulture:Name . 
        END.		

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the database field name matching a temp-table field name
                 contained in a consumers query string (query string vs. temp-table
                 definition) 
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of 
                 DataAccess:FetchData () query preparation                        
                 TO-DO: Provide code for alternative mapping                                             
        @param pcTable The temp table name to return the source (database table) field name for
        @param pcColumn The temp table column name to return the source (database table) field name for             
        @return The database field name mapped to the passed in temp-table field
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceColumn (pcTable AS CHARACTER, pcColumn AS CHARACTER):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.
        
        @SourceColumnCaseBlock.
        CASE pcTable:
            WHEN "eUserInterfaceCulture":U THEN
                cMap = DATA-SOURCE src_ttUserInterfaceCulture:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP . 

        END.
     
        RETURN ENTRY(LOOKUP(pccolumn,cMap) + 1,cMap).    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the base query string used to retrieve data for the temp
                 tables. This query string will be appended by the query provided by
                 the consumer (FetchDataRequest object).                                                                      
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of 
                 DataAccess:FetchData () query preparation                        
                 TO-DO: Provide code to return the DATA-SOURCE root query string                                             
        @param pcTable The temp table name to return the source default (database) query string for
        @return The base query string used to retrieve data for the given temp-table
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceDefaultQuery (pcTable AS CHARACTER):
    
        @SourceDefaultQueryCaseBlock.
        CASE pcTable:
            WHEN "eUserInterfaceCulture":U THEN
                RETURN "FOR EACH ttUserInterfaceCulture INDEXED-REPOSITION":U. 

        END. 
        
    END METHOD.

END CLASS.
