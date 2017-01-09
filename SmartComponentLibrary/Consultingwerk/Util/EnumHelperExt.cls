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

    /**
     * Purpose: Constructor for the EnumHelperExt class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED EnumHelperExt ():
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Performs a logical Add on an Array of .NET Enum values
     *
     * Notes:
     * @param poEnumArray An Array of Enum values
     * @return The resulting Enum
     */
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

    /**
     * Purpose: Performs a logical Add on an Array of .NET Enum values
     *
     * Notes:
     * @param poEnum1 The first System.Enum for the Add operation
     * @param poEnum2 The first System.Enum for the Add operation
     * @param poEnum3 The first System.Enum for the Add operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Add (poEnum1 AS System.Enum,
                                          poEnum2 AS System.Enum,
                                          poEnum3 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 3 NO-UNDO .

        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3 .

        RETURN EnumHelperExt:Add (oArray) .

    END METHOD .

    /**
     * Purpose: Performs a logical Add on an Array of .NET Enum values
     *
     * Notes:
     * @param poEnum1 The first System.Enum for the Add operation
     * @param poEnum2 The first System.Enum for the Add operation
     * @param poEnum3 The first System.Enum for the Add operation
     * @param poEnum4 The first System.Enum for the Add operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Add (poEnum1 AS System.Enum,
                                          poEnum2 AS System.Enum,
                                          poEnum3 AS System.Enum,
                                          poEnum4 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 4 NO-UNDO .

        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4 .

        RETURN EnumHelperExt:Add (oArray) .

    END METHOD .

    /**
     * Purpose: Performs a logical Add on an Array of .NET Enum values
     *
     * Notes:
     * @param poEnum1 The first System.Enum for the Add operation
     * @param poEnum2 The first System.Enum for the Add operation
     * @param poEnum3 The first System.Enum for the Add operation
     * @param poEnum4 The first System.Enum for the Add operation
     * @param poEnum5 The first System.Enum for the Add operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Add (poEnum1 AS System.Enum,
                                          poEnum2 AS System.Enum,
                                          poEnum3 AS System.Enum,
                                          poEnum4 AS System.Enum,
                                          poEnum5 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 5 NO-UNDO .

        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4
               oArray[5] = poEnum5 .

        RETURN EnumHelperExt:Add (oArray) .

    END METHOD .

    /**
     * Purpose: Returns true, when the specified list of flagged .NET Enum values does
     *          contain the the specified value (e.g. compaing DockStyle Top, Left
     * Notes:
     * @param poEnumList A list of flagged Enum values
     * @param poValue A single Enum value
     * @return True when the specified list of flagged Enum values does contain the specified value
     */
    METHOD PUBLIC STATIC LOGICAL Contains (poEnumList AS System.Enum,
                                           poValue AS System.Enum):

         RETURN Progress.Util.EnumHelper:AreEqual (Progress.Util.EnumHelper:And (poEnumList,
                                                                                 poValue),
                                                   poValue) .
    END METHOD .

    /**
     * Purpose: Returns the .NET Enum member that represents the given integer value
     * Notes:   SCL-592
     * @param poType The Enum Member type to return the member from
     * @param piValue The value to return the enum member for
     * @return The enum member with the given value
     */
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

    /**
     * Purpose: Returns the Progress.Lang.Enum or Consultingwerk.Enum for
     *          the given string
     * Notes:
     * @param pcValue The Enum Name
     * @param poClass The Enum Class
     * @return The enum member
     */
    METHOD PUBLIC STATIC Progress.Lang.Object FromString (pcValue AS CHARACTER,
                                                          poClass AS Progress.Lang.Class):

    &IF NOT PROVERSION BEGINS "10.2":U &THEN

        IF poClass:IsA ("Consultingwerk.Enum":U) THEN

            RETURN DYNAMIC-INVOKE (poClass:TypeName, "FromString":U, pcValue) .

&IF DEFINED (NativeEnums) NE 0 &THEN
        ELSE IF poClass:IsA ("Progress.Lang.Enum":U) THEN

            RETURN Progress.Lang.Enum:ToObject (poClass:TypeName, pcValue) .
&ENDIF
        RETURN ? .
    &ELSE
    UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException ("FromString":U,
                                                                     "Consultingwerk.Util.EnumHelperExt":U) .
    &ENDIF

    END METHOD.

    /**
     * Purpose: Performs a logical Or on an Array of .NET Enum values
     * Notes:
     * @param poEnumArray An Array of Enum values
     * @return The resulting Enum
     */
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

    /**
     * Purpose: Performs a logical Or on two .NET Enum values
     * Notes:
     * @param poEnum1 The first System.Enum for the Or operation
     * @param poEnum2 The second System.Enum for the Or operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum):

        RETURN Progress.Util.EnumHelper:Or (poEnum1, poEnum1) .

    END METHOD .

    /**
     * Purpose: Performs a logical Or on an Array of .NET Enum values
     * Notes:
     * @param poEnum1 The first System.Enum for the Or operation
     * @param poEnum2 The second System.Enum for the Or operation
     * @param poEnum3 The third System.Enum for the Or operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 3 NO-UNDO .

        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD .

    /**
     * Purpose: Performs a logical Or on an Array of .NET Enum values
     * Notes:
     * @param poEnum1 The first System.Enum for the Or operation
     * @param poEnum2 The second System.Enum for the Or operation
     * @param poEnum3 The third System.Enum for the Or operation
     * @param poEnum4 The fourth System.Enum for the Or operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum,
                                         poEnum4 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 4 NO-UNDO .

        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD .

    /**
     * Purpose: Performs a logical Or on an Array of .NET Enum values
     * Notes:
     * @param poEnum1 The first System.Enum for the Or operation
     * @param poEnum2 The second System.Enum for the Or operation
     * @param poEnum3 The third System.Enum for the Or operation
     * @param poEnum4 The fourth System.Enum for the Or operation
     * @param poEnum5 The fifth System.Enum for the Or operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum,
                                         poEnum4 AS System.Enum,
                                         poEnum5 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 5 NO-UNDO .

        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4
               oArray[5] = poEnum5 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD .

    /**
     * Purpose: Performs a logical Or on an Array of .NET Enum values
     * Notes:
     * @param poEnum1 The first System.Enum for the Or operation
     * @param poEnum2 The second System.Enum for the Or operation
     * @param poEnum3 The third System.Enum for the Or operation
     * @param poEnum4 The fourth System.Enum for the Or operation
     * @param poEnum5 The fifth System.Enum for the Or operation
     * @param poEnum6 The sixth System.Enum for the Or operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum,
                                         poEnum4 AS System.Enum,
                                         poEnum5 AS System.Enum,
                                         poEnum6 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 6 NO-UNDO .

        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4
               oArray[5] = poEnum5
               oArray[6] = poEnum6 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD .

    /**
     * Purpose: Performs a logical Or on an Array of .NET Enum values
     * Notes:
     * @param poEnum1 The first System.Enum for the Or operation
     * @param poEnum2 The second System.Enum for the Or operation
     * @param poEnum3 The third System.Enum for the Or operation
     * @param poEnum4 The fourth System.Enum for the Or operation
     * @param poEnum5 The fifth System.Enum for the Or operation
     * @param poEnum6 The sixth System.Enum for the Or operation
     * @param poEnum7 The seventh System.Enum for the Or operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum,
                                         poEnum4 AS System.Enum,
                                         poEnum5 AS System.Enum,
                                         poEnum6 AS System.Enum,
                                         poEnum7 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 7 NO-UNDO .

        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4
               oArray[5] = poEnum5
               oArray[6] = poEnum6
               oArray[7] = poEnum7 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD .

    /**
     * Purpose: Performs a logical Or on an Array of .NET Enum values
     * Notes:
     * @param poEnum1 The first System.Enum for the Or operation
     * @param poEnum2 The second System.Enum for the Or operation
     * @param poEnum3 The third System.Enum for the Or operation
     * @param poEnum4 The fourth System.Enum for the Or operation
     * @param poEnum5 The fifth System.Enum for the Or operation
     * @param poEnum6 The sixth System.Enum for the Or operation
     * @param poEnum7 The seventh System.Enum for the Or operation
     * @param poEnum8 The eigth System.Enum for the Or operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum,
                                         poEnum4 AS System.Enum,
                                         poEnum5 AS System.Enum,
                                         poEnum6 AS System.Enum,
                                         poEnum7 AS System.Enum,
                                         poEnum8 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 8 NO-UNDO .

        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4
               oArray[5] = poEnum5
               oArray[6] = poEnum6
               oArray[7] = poEnum7
               oArray[8] = poEnum8 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD .

    /**
     * Purpose: Performs a logical Or on an Array of .NET Enum values
     * Notes:
     * @param poEnum1 The first System.Enum for the Or operation
     * @param poEnum2 The second System.Enum for the Or operation
     * @param poEnum3 The third System.Enum for the Or operation
     * @param poEnum4 The fourth System.Enum for the Or operation
     * @param poEnum5 The fifth System.Enum for the Or operation
     * @param poEnum6 The sixth System.Enum for the Or operation
     * @param poEnum7 The seventh System.Enum for the Or operation
     * @param poEnum8 The eigth System.Enum for the Or operation
     * @param poEnum9 The nineth System.Enum for the Or operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum,
                                         poEnum4 AS System.Enum,
                                         poEnum5 AS System.Enum,
                                         poEnum6 AS System.Enum,
                                         poEnum7 AS System.Enum,
                                         poEnum8 AS System.Enum,
                                         poEnum9 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 9 NO-UNDO .

        ASSIGN oArray[1] = poEnum1
               oArray[2] = poEnum2
               oArray[3] = poEnum3
               oArray[4] = poEnum4
               oArray[5] = poEnum5
               oArray[6] = poEnum6
               oArray[7] = poEnum7
               oArray[8] = poEnum8
               oArray[9] = poEnum9 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD .

    /**
     * Purpose: Performs a logical Or on an Array of .NET Enum values
     * Notes:
     * @param poEnum1 The first System.Enum for the Or operation
     * @param poEnum2 The second System.Enum for the Or operation
     * @param poEnum3 The third System.Enum for the Or operation
     * @param poEnum4 The fourth System.Enum for the Or operation
     * @param poEnum5 The fifth System.Enum for the Or operation
     * @param poEnum6 The sixth System.Enum for the Or operation
     * @param poEnum7 The seventh System.Enum for the Or operation
     * @param poEnum8 The eigth System.Enum for the Or operation
     * @param poEnum9 The nineth System.Enum for the Or operation
     * @param poEnum10 The tenth System.Enum for the Or operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum,
                                         poEnum4 AS System.Enum,
                                         poEnum5 AS System.Enum,
                                         poEnum6 AS System.Enum,
                                         poEnum7 AS System.Enum,
                                         poEnum8 AS System.Enum,
                                         poEnum9 AS System.Enum,
                                         poEnum10 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 10 NO-UNDO .

        ASSIGN oArray[1]  = poEnum1
               oArray[2]  = poEnum2
               oArray[3]  = poEnum3
               oArray[4]  = poEnum4
               oArray[5]  = poEnum5
               oArray[6]  = poEnum6
               oArray[7]  = poEnum7
               oArray[8]  = poEnum8
               oArray[9]  = poEnum9
               oArray[10] = poEnum10 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD .

    /**
     * Purpose: Performs a logical Or on an Array of .NET Enum values
     * Notes:
     * @param poEnum1 The first System.Enum for the Or operation
     * @param poEnum2 The second System.Enum for the Or operation
     * @param poEnum3 The third System.Enum for the Or operation
     * @param poEnum4 The fourth System.Enum for the Or operation
     * @param poEnum5 The fifth System.Enum for the Or operation
     * @param poEnum6 The sixth System.Enum for the Or operation
     * @param poEnum7 The seventh System.Enum for the Or operation
     * @param poEnum8 The eigth System.Enum for the Or operation
     * @param poEnum9 The nineth System.Enum for the Or operation
     * @param poEnum10 The tenth System.Enum for the Or operation
     * @param poEnum11 The eleventh System.Enum for the Or operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum,
                                         poEnum4 AS System.Enum,
                                         poEnum5 AS System.Enum,
                                         poEnum6 AS System.Enum,
                                         poEnum7 AS System.Enum,
                                         poEnum8 AS System.Enum,
                                         poEnum9 AS System.Enum,
                                         poEnum10 AS System.Enum,
                                         poEnum11 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 11 NO-UNDO .

        ASSIGN oArray[1]  = poEnum1
               oArray[2]  = poEnum2
               oArray[3]  = poEnum3
               oArray[4]  = poEnum4
               oArray[5]  = poEnum5
               oArray[6]  = poEnum6
               oArray[7]  = poEnum7
               oArray[8]  = poEnum8
               oArray[9]  = poEnum9
               oArray[10] = poEnum10
               oArray[11] = poEnum11 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD .

    /**
     * Purpose: Performs a logical Or on an Array of .NET Enum values
     * Notes:
     * @param poEnum1 The first System.Enum for the Or operation
     * @param poEnum2 The second System.Enum for the Or operation
     * @param poEnum3 The third System.Enum for the Or operation
     * @param poEnum4 The fourth System.Enum for the Or operation
     * @param poEnum5 The fifth System.Enum for the Or operation
     * @param poEnum6 The sixth System.Enum for the Or operation
     * @param poEnum7 The seventh System.Enum for the Or operation
     * @param poEnum8 The eigth System.Enum for the Or operation
     * @param poEnum9 The nineth System.Enum for the Or operation
     * @param poEnum10 The tenth System.Enum for the Or operation
     * @param poEnum11 The eleventh System.Enum for the Or operation
     * @param poEnum12 The twelveth System.Enum for the Or operation
     * @return The resulting Enum
     */
    METHOD PUBLIC STATIC System.Enum Or (poEnum1 AS System.Enum,
                                         poEnum2 AS System.Enum,
                                         poEnum3 AS System.Enum,
                                         poEnum4 AS System.Enum,
                                         poEnum5 AS System.Enum,
                                         poEnum6 AS System.Enum,
                                         poEnum7 AS System.Enum,
                                         poEnum8 AS System.Enum,
                                         poEnum9 AS System.Enum,
                                         poEnum10 AS System.Enum,
                                         poEnum11 AS System.Enum,
                                         poEnum12 AS System.Enum):

        DEFINE VARIABLE oArray AS System.Enum EXTENT 12 NO-UNDO .

        ASSIGN oArray[1]  = poEnum1
               oArray[2]  = poEnum2
               oArray[3]  = poEnum3
               oArray[4]  = poEnum4
               oArray[5]  = poEnum5
               oArray[6]  = poEnum6
               oArray[7]  = poEnum7
               oArray[8]  = poEnum8
               oArray[9]  = poEnum9
               oArray[10] = poEnum10
               oArray[11] = poEnum11
               oArray[12] = poEnum12 .

        RETURN EnumHelperExt:Or (oArray) .

    END METHOD .
&ENDIF

END CLASS.
