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
    File        : NamedQueryParameterList
    Purpose     : List of parameters to named queries
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 13 12:32:58 CEST 2015
    Notes       : SCL-1018
                  Implemented as a List, as Dictionaries are currently not
                  serializable. But provides dictionary methods ContainsKey
                  and GetItem (by Name)
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Framework.Base.* FROM PROPATH .
USING Consultingwerk.OERA.*           FROM PROPATH .  
USING Progress.Lang.*                 FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.NamedQueryParameterList
    INHERITS GenericList
    {&SERIALIZABLE}: 

    {Consultingwerk/Framework/Base/GenericList.i NamedQueryParameterEntry}

    /*------------------------------------------------------------------------------
        Purpose: Adds a Character parameter to the list
        Notes:   
        @param pcName The name for the parameter 
        @param pcValue The value for the parameter
        @return The list itself (for fluent style coding)
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC NamedQueryParameterList Add (pcName AS CHARACTER, 
	                                           pcValue AS CHARACTER):
		
		THIS-OBJECT:Add (NEW NamedQueryParameterEntry (pcName, NEW CharacterHolder (pcValue))) . 
		
		RETURN THIS-OBJECT .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds a Date parameter to the list
        Notes:   
        @param pcName The name for the parameter 
        @param pdtValue The value for the parameter
        @return The list itself (for fluent style coding)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC NamedQueryParameterList Add (pcName AS CHARACTER, 
                                               pdtValue AS DATE):
        
        THIS-OBJECT:Add (NEW NamedQueryParameterEntry (pcName, NEW DateHolder (pdtValue))) . 
        
        RETURN THIS-OBJECT .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds a DateTime parameter to the list
        Notes:   
        @param pcName The name for the parameter 
        @param pdtValue The value for the parameter
        @return The list itself (for fluent style coding)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC NamedQueryParameterList Add (pcName AS CHARACTER, 
                                               pdtValue AS DATETIME):
        
        THIS-OBJECT:Add (NEW NamedQueryParameterEntry (pcName, NEW DateTimeHolder (pdtValue))) . 
        
        RETURN THIS-OBJECT .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds a DateTime-TZ parameter to the list
        Notes:   
        @param pcName The name for the parameter 
        @param pdtValue The value for the parameter
        @return The list itself (for fluent style coding)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC NamedQueryParameterList Add (pcName AS CHARACTER, 
                                               pdtValue AS DATETIME-TZ):
        
        THIS-OBJECT:Add (NEW NamedQueryParameterEntry (pcName, NEW DateTimeTzHolder (pdtValue))) . 
        
        RETURN THIS-OBJECT .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds a Decimal parameter to the list
        Notes:   
        @param pcName The name for the parameter 
        @param pdeValue The value for the parameter
        @return The list itself (for fluent style coding)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC NamedQueryParameterList Add (pcName AS CHARACTER, 
                                               pdeValue AS DECIMAL):
        
        THIS-OBJECT:Add (NEW NamedQueryParameterEntry (pcName, NEW DecimalHolder (pdeValue))) . 
        
        RETURN THIS-OBJECT .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds an Int64 parameter to the list
        Notes:   
        @param pcName The name for the parameter 
        @param piValue The value for the parameter
        @return The list itself (for fluent style coding)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC NamedQueryParameterList Add (pcName AS CHARACTER, 
                                               piValue AS INT64):
        
        THIS-OBJECT:Add (NEW NamedQueryParameterEntry (pcName, NEW Int64Holder (piValue))) . 
        
        RETURN THIS-OBJECT .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds an Integer parameter to the list
        Notes:   
        @param pcName The name for the parameter 
        @param piValue The value for the parameter
        @return The list itself (for fluent style coding)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC NamedQueryParameterList Add (pcName AS CHARACTER, 
                                               piValue AS INTEGER):
        
        THIS-OBJECT:Add (NEW NamedQueryParameterEntry (pcName, NEW IntegerHolder (piValue))) . 
        
        RETURN THIS-OBJECT .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds a Logical parameter to the list
        Notes:   
        @param pcName The name for the parameter 
        @param plValue The value for the parameter
        @return The list itself (for fluent style coding)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC NamedQueryParameterList Add (pcName AS CHARACTER, 
                                               plValue AS LOGICAL):
        
        THIS-OBJECT:Add (NEW NamedQueryParameterEntry (pcName, NEW LogicalHolder (plValue))) . 
        
        RETURN THIS-OBJECT .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds an Object parameter to the list
        Notes:   
        @param pcName The name for the parameter 
        @param poValue The value for the parameter
        @return The list itself (for fluent style coding)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC NamedQueryParameterList Add (pcName AS CHARACTER, 
                                               poValue AS Progress.Lang.Object):
        
        THIS-OBJECT:Add (NEW NamedQueryParameterEntry (pcName, poValue)) . 
        
        RETURN THIS-OBJECT .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Determines if an item with the specified key is contained in the List
        Notes:         
        @param pcKey The key to locate in the Dictionary
        @return Logical value indicating if the object is contained in the Dictionary                                                            
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ContainsKey (pcKey AS CHARACTER):
		
		{Consultingwerk/linqABL.i NamedQueryParameterEntry oItem in this-object where Name = pcKey}

            RETURN TRUE . 
		
		END.

        RETURN FALSE . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves the value of an item from the list
        Notes:    
        @param pcKey The key of the item to retrieve
        @return The value instance of the item with the specified key 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Lang.Object GetItem (pcKey AS CHARACTER):

        {Consultingwerk/linqABL.i NamedQueryParameterEntry oItem in this-object where Name = pcKey}

            RETURN oItem:Value . 
        
        END.

        RETURN ? . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves the value of an item from the list
        Notes:    
        @param pcKey The key of the item to retrieve
        @return The character value instnace of the item with the specified key 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetParameterValueAsCharacter (pcKey AS CHARACTER):
         
         DEFINE VARIABLE oValue AS Progress.Lang.Object NO-UNDO.
         
         IF NOT THIS-OBJECT:ContainsKey (pcKey) THEN  
             RETURN ? .
         
         oValue = THIS-OBJECT:GetItem(pcKey) .
         
         IF TYPE-OF (oValue, CharacterHolder) THEN 
             RETURN CAST (oValue, CharacterHolder):Value .
          
        IF TYPE-OF (oValue, DateHolder) THEN 
            RETURN STRING (CAST (oValue, DateHolder):Value) .
         
        IF TYPE-OF (oValue, DateTimeHolder) THEN 
            RETURN STRING (CAST (oValue, DateTimeHolder):Value) .
        
        IF TYPE-OF (oValue, DateTimeTzHolder) THEN 
            RETURN STRING (CAST (oValue, DateTimeTzHolder):Value) .
        
        IF TYPE-OF (oValue, DecimalHolder) THEN 
            RETURN STRING (CAST (oValue, DecimalHolder):Value) .
        
        IF TYPE-OF (oValue, Int64Holder) THEN 
            RETURN STRING (CAST (oValue, Int64Holder):Value) .
        
        IF TYPE-OF (oValue, IntegerHolder) THEN 
            RETURN STRING (CAST (oValue, IntegerHolder):Value) .
        
        IF TYPE-OF (oValue, LogicalHolder) THEN 
            RETURN STRING (CAST (oValue, LogicalHolder):Value) .
              
        RETURN ? . 

     END METHOD.
     
END CLASS.
