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
    File        : PropathHelper
    Purpose     : Supporting Methods for dealing with the PROPATH 
    Syntax      : Static methods only, Protected constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 15 22:49:25 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW . 

{Consultingwerk/products.i}

USING Consultingwerk.Util.* .
USING Progress.Lang.*.

CLASS Consultingwerk.Util.PropathHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED PropathHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Tests if the current directory name is absolute of relative                                                                       
        Notes:   Absolute paths start with either a / (slash) or a double \\ (UNC)
                 or a letter followed by a colon             
        @param pcDirectory The name of the directory to test
        @return Logical value indicating if the directory is specified as an absolute path                                                            
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC LOGICAL IsAbsoluteDirectory (pcDirectory AS CHARACTER):
        
        DEFINE VARIABLE cFirstLetter  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSecondLetter AS CHARACTER NO-UNDO.
        
        IF pcDirectory BEGINS "/":U OR pcDirectory BEGINS "~\":U /* includes \\ for UNC */THEN 
            RETURN TRUE . 
            
        IF LENGTH (pcDirectory) < 2 THEN 
            RETURN FALSE . 
            
        ASSIGN cFirstLetter  = UPPER (SUBSTRING (pcDirectory, 1, 1)) 
               cSecondLetter = UPPER (SUBSTRING (pcDirectory, 2, 1)).                                
        
        IF ASC (cFirstLetter) >= 65 AND ASC (cFirstLetter) <= 90 AND 
           cSecondLetter = ":":U THEN 
            RETURN TRUE . 
            
        RETURN FALSE .     

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Turns a relative propath into an absolute path                                                                        
        Notes:
        @param pcBasePath The relative path to be used as a base for relative references
        @param pcPropath The propath to make absolute
        @return The propath with only absolute path references                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER MakeAbsolute (pcBasePath AS CHARACTER,
                                                 pcPropath AS CHARACTER):
        
        DEFINE VARIABLE cDelimiter AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cEntry     AS CHARACTER NO-UNDO.
        
        FILE-INFORMATION:FILE-NAME = pcBasePath . 
        
        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN . 
        ELSE 
            UNDO, THROW NEW AppError ("Invalid base path for method MakeAbsolute."{&TRAN}, 0) .
        
        ASSIGN pcBasePath = FILE-INFORMATION:FULL-PATHNAME 
               cDelimiter = ",":U . 
        
        DO i = 1 TO NUM-ENTRIES (pcPropath, cDelimiter):
            ASSIGN cEntry = ENTRY (i, pcPropath, cDelimiter) .            
        
            IF NOT IsAbsoluteDirectory (cEntry) THEN DO: 
                IF cEntry = ".":U THEN 
                    ASSIGN cEntry = pcBasePath . 
                ELSE DO: 
                    IF cEntry BEGINS "./":U OR cEntry BEGINS ".~\":U THEN 
                        cEntry = SUBSTRING (cEntry, 3) .
                    
                    ASSIGN cEntry = pcBasePath + "~\":U + cEntry . 
            
                    FILE-INFORMATION:FILE-NAME = cEntry . 
                    
                    IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN 
                        ASSIGN cEntry = FILE-INFORMATION:FULL-PATHNAME .
                END.
                                      
                ENTRY (i, pcPropath, cDelimiter) = cEntry .
            END.            
        END.

        RETURN pcPropath . 
    
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the propath delimiter for the current OS                                                                        
        Notes:   Accoording to the OpenEdge documentation, even UNIX sessions 
                 use the comma as the propath delimiter from within the AVM so this 
                 method is just left for backwards compatibility
        @return The comma as the propath                                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER PropathDelimiter():
        
        RETURN ",":U . 

    END METHOD.

END CLASS.
