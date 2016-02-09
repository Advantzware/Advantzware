/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *    The original author of this class was Progress Software,        *
 *    Håvard Danielsen                                                *
 **********************************************************************/ 
/*------------------------------------------------------------------------
    File        : DSQueryStringProcedural
    Purpose     : Extends QueryString to provide query transformation 
                  for non OO utility procedure that manages multiple 
                  datasources as 4GL handles 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 03 08:24:02 CET 2010
    Notes       : This is the version for the depricated procedural OERA
                  backend code      
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW . 

{Consultingwerk/products.i}

USING Consultingwerk.*            FROM PROPATH . 
USING Consultingwerk.OERA.*       FROM PROPATH .
USING Consultingwerk.OERA.Query.* FROM PROPATH .
USING Consultingwerk.Util.*       FROM PROPATH .     
USING Progress.Lang.*             FROM PROPATH .
    
CLASS Consultingwerk.OERA.Query.DSQueryStringProcedural 
    INHERITS QueryString
    IMPLEMENTS IQueryMap, IDSQueryString
    USE-WIDGET-POOL:

    DEFINE PRIVATE VARIABLE mhProcedure AS HANDLE    NO-UNDO.
    DEFINE PRIVATE VARIABLE mcTable     AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DSQueryStringProcedural class                                                                       
        Notes:   
        @param pcQueryString The query string from the current read request
        @param phProcedure The handle of the owning data access object procedure (for call backs)
        @param pcTable The name of the temp-table this DSQueryStringProcedural instance is providing source query manipulation for                                                                     
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DSQueryStringProcedural (pcQueryString AS CHARACTER,
                                                phProcedure   AS HANDLE,
                                                pcTable       AS CHARACTER):
                                                    
        /* SUPER must be first call, so pass blank to postpone parsing untill 
           we have set the procedure handle for the call backs */
        SUPER ("":U, THIS-OBJECT).
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phProcedure WidgetTypeEnum:Procedure} .
        
        IF NOT ProcedureHelper:HasEntry (phProcedure, "SourceColumn":U, EntryTypeEnum:Function) OR
           NOT ProcedureHelper:HasEntry (phProcedure, "SourceDefaultQuery":U, EntryTypeEnum:Function) THEN 
            
            UNDO, THROW NEW AppError (SUBSTITUTE ("Procedure &1 needs to provide function SourceColumn and SourceDefaultQuery"{&TRAN}, 
                                                  phProcedure:FILE-NAME), 0) . 
        
        ASSIGN
            mhProcedure = phProcedure
            mcTable     = pcTable.
            
        ParseQuery (pcQuerySTring).
    END CONSTRUCTOR .
  
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DSQueryStringProcedural class                                                                       
        Notes:   
        @param pcQueryString The query string from the current read request
        @param phProcedure The handle of the owning data access object procedure (for call backs)
        @param pcTable The name of the temp-table this DSQueryStringProcedural instance is providing source query manipulation for                                                                     
        @param plFilterUsingInnerJoin The value for the FilterUsingInnerJoin property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DSQueryStringProcedural (pcQueryString AS CHARACTER,
                                                phProcedure   AS HANDLE,
                                                pcTable       AS CHARACTER,
                                                plFilterUsingInnerJoin AS LOGICAL):
        
        THIS-OBJECT (pcQueryString,
                     phProcedure,
                     pcTable) .                     
                                          
        THIS-OBJECT:FilterUsingInnerJoin = plFilterUsingInnerJoin . 
                     
    END CONSTRUCTOR .                                          
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the column from the source (database) column                                                                      
        Notes:   Invokes the sourceColumn function in the data access object                                                                
        @param pcColumn The name of the temp-table column
        @return The name of the source (database) column      
    ------------------------------------------------------------------------------*/   
    METHOD PUBLIC CHARACTER ColumnSource (pcColumn AS CHARACTER):
        
        RETURN DYNAMIC-FUNCTION ("SourceColumn":U IN mhProcedure, mcTable, pcColumn). 
        
    END.
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the default query string for the data source                                                                       
        Notes:   Invokes SourceDefaultQuery function in the data access object
        @return The default query string                 
    ------------------------------------------------------------------------------*/  
    METHOD PUBLIC CHARACTER DefaultQuery (): 
        
        RETURN DYNAMIC-FUNCTION ("SourceDefaultQuery":U IN mhProcedure, mcTable). 
        
    END.
END CLASS.
