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
    File        : EnumHelperExt
    Purpose     : Provides Helper Functions for Enum types (ABL, .NET, 
                  Consultingwerk)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 21 13:09:58 CEST 2012
    Notes       : Class named EnumHelperExt to avoid naming conflicts 
                  with the Progress.Util.EnumHelper class, as both packages
                  (Consultingwerk.Util and Progress.Util) are likely candidates
                  for being used in an USING statement
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Assertion.*  FROM PROPATH . 
USING Consultingwerk.Exceptions.* FROM PROPATH . 
USING Consultingwerk.Util.*       FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Util.EnumHelperExt: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EnumHelperExt class                                                                       
        Notes:   Disallow instance creation                                  
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED EnumHelperExt ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Performs a logical Add on an Array of .NET Enum values
                 
        Notes:   
        @param poEnumArray An Array of Enum values
        @return The resulting Enum 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Enum Add (poEnumArray AS System.Enum EXTENT):

        DEFINE VARIABLE oResult AS System.Enum NO-UNDO . 
        DEFINE VARIABLE i       AS INTEGER     NO-UNDO.

        IF EXTENT (poEnumArray) >= 2 THEN .
        ELSE 
            UNDO, THROW NEW InvalidParameterValueException ("poEnumArray":U, 
                                                            SUBSTITUTE ("System.Enum[&1]":U, EXTENT (poEnumArray)),
                                                            "Consultingwerk.Util.EnumHelperExt":U) . 
        
        oResult = Progress.Util.EnumHelper:Add (poEnumArray[1],
                                                poEnumArray[2]) .
                                                
        DO i = 3 TO EXTENT (poEnumArray):
            oResult = Progress.Util.EnumHelper:Add (oResult,
                                                    poEnumArray[i]) .
        END .                                                     

        RETURN oResult .
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs a logical Add on an Array of .NET Enum values
                 
        Notes:   
        @param poEnum1 The first System.Enum for the Add operation
        @param poEnum2 The first System.Enum for the Add operation
        @param poEnum3 The first System.Enum for the Add operation
        @return The resulting Enum 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Enum Add (poEnum1 AS System.Enum,
                                          poEnum2 AS System.Enum,
                                          poEnum3 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 3 .
        
        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3 .

        RETURN EnumHelperExt:Add (oArray) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs a logical Add on an Array of .NET Enum values
                 
        Notes:   
        @param poEnum1 The first System.Enum for the Add operation
        @param poEnum2 The first System.Enum for the Add operation
        @param poEnum3 The first System.Enum for the Add operation
        @param poEnum4 The first System.Enum for the Add operation
        @return The resulting Enum 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Enum Add (poEnum1 AS System.Enum,
                                          poEnum2 AS System.Enum,
                                          poEnum3 AS System.Enum,
                                          poEnum4 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 4 .
        
        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4 .

        RETURN EnumHelperExt:Add (oArray) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs a logical Add on an Array of .NET Enum values
                 
        Notes:   
        @param poEnum1 The first System.Enum for the Add operation
        @param poEnum2 The first System.Enum for the Add operation
        @param poEnum3 The first System.Enum for the Add operation
        @param poEnum4 The first System.Enum for the Add operation
        @param poEnum5 The first System.Enum for the Add operation
        @return The resulting Enum 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Enum Add (poEnum1 AS System.Enum,
                                          poEnum2 AS System.Enum,
                                          poEnum3 AS System.Enum,
                                          poEnum4 AS System.Enum,
                                          poEnum5 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 5 .
        
        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4
               oArray[5] = poEnum5 .

        RETURN EnumHelperExt:Add (oArray) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns true, when the specified list of flagged .NET Enum values does
                 contain the the specified value (e.g. compaing DockStyle Top, Left 
        Notes:   
        @param poEnumList A list of flagged Enum values
        @param poValue A single Enum value
        @return True when the specified list of flagged Enum values does contain the specified value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL Contains (poEnumList AS System.Enum,
                                           poValue AS System.Enum):
        
         RETURN Progress.Util.EnumHelper:AreEqual (Progress.Util.EnumHelper:And (poEnumList,
                                                                                 poValue),
                                                   poValue) .
    END METHOD .

	/*------------------------------------------------------------------------------
 	   Purpose: Returns the .NET Enum member that represents the given integer value
	   Notes:   SCL-592
	   @param poType The Enum Member type to return the member from
	   @param piValue The value to return the enum member for
	   @return The enum member with the given value
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC System.Enum FromInteger (poType AS System.Type,
	                                              piValue AS INTEGER):
		
        DEFINE VARIABLE oValues AS System.Array NO-UNDO .
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poType """poType"":U"} .

        oValues = System.Enum:GetValues (poType) .

        {Consultingwerk/foreach.i System.Enum oEnum in oValues}

            IF System.Convert:ToInt32 (oEnum) = piValue THEN 
                RETURN oEnum .
        END.

		RETURN ? .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs a logical Or on an Array of .NET Enum values
                 
        Notes:   
        @param poEnumArray An Array of Enum values
        @return The resulting Enum 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Enum Or (poEnumArray AS System.Enum EXTENT):

        DEFINE VARIABLE oResult AS System.Enum NO-UNDO . 
        DEFINE VARIABLE i       AS INTEGER     NO-UNDO.

        IF EXTENT (poEnumArray) >= 2 THEN .
        ELSE 
            UNDO, THROW NEW InvalidParameterValueException ("poEnumArray":U, 
                                                            SUBSTITUTE ("System.Enum[&1]":U, EXTENT (poEnumArray)),
                                                            "Consultingwerk.Util.EnumHelperExt":U) . 
        
        oResult = Progress.Util.EnumHelper:Or (poEnumArray[1],
                                               poEnumArray[2]) .
                                                
        DO i = 3 TO EXTENT (poEnumArray):
            oResult = Progress.Util.EnumHelper:Or (oResult,
                                                   poEnumArray[i]) .
        END .                                                     

        RETURN oResult .
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs a logical Or on two .NET Enum values
                 
        Notes:   
        @param poEnum1 The first System.Enum for the Or operation
        @param poEnum2 The first System.Enum for the Or operation
        @return The resulting Enum 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum):

        RETURN Progress.Util.EnumHelper:Or (poEnum1, poEnum1) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs a logical Or on an Array of .NET Enum values
                 
        Notes:   
        @param poEnum1 The first System.Enum for the Or operation
        @param poEnum2 The first System.Enum for the Or operation
        @param poEnum3 The first System.Enum for the Or operation
        @return The resulting Enum 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 3 .
        
        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs a logical Or on an Array of .NET Enum values
                 
        Notes:   
        @param poEnum1 The first System.Enum for the Or operation
        @param poEnum2 The first System.Enum for the Or operation
        @param poEnum3 The first System.Enum for the Or operation
        @param poEnum4 The first System.Enum for the Or operation
        @return The resulting Enum 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum,
                                         poEnum4 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 4 .
        
        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs a logical Or on an Array of .NET Enum values
                 
        Notes:   
        @param poEnum1 The first System.Enum for the Or operation
        @param poEnum2 The first System.Enum for the Or operation
        @param poEnum3 The first System.Enum for the Or operation
        @param poEnum4 The first System.Enum for the Or operation
        @param poEnum5 The first System.Enum for the Or operation
        @return The resulting Enum 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum,
                                         poEnum4 AS System.Enum,
                                         poEnum5 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 5 .
        
        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4
               oArray[5] = poEnum5 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD . 
&ENDIF

END CLASS.
