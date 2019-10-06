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
    File        : ResourceFileHelper
    Purpose     : Provides support for windows .NET .resx file
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 01 19:01:22 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Consultingwerk.Util.Support.*          FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING System.Collections.*                   FROM ASSEMBLY .
USING System.Resources.*                     FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Util.ResourceFileHelper:

    /**
     * Purpose: Constructor for the ResourceFileHelper class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED ResourceFileHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Returns a CharacterList with the data element names in the resx file
     * Notes:
     * @param pcFileName The name of the resx file
     * @return The CharacterList of data element names
     */
    METHOD PUBLIC STATIC CharacterList GetElementNames (pcFileName AS CHARACTER):

        DEFINE VARIABLE oParser AS ResxFileParser NO-UNDO .

        oParser = NEW ResxFileParser () .

        RETURN oParser:ParseResxFile (pcFileName) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oParser) .
        END FINALLY.

    END METHOD .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Returns if the provide .resx File is Localizable
     * Notes:
     * @param pcFileName The file name of the .resx file
     * @return Logical vlaue indicating if the .resx file is localizable
     */
    METHOD PUBLIC STATIC LOGICAL IsLocalizable (pcFileName AS CHARACTER):

        DEFINE VARIABLE oMetaEnumerator AS IDictionaryEnumerator NO-UNDO .
        DEFINE VARIABLE oMetaEntry      AS DictionaryEntry       NO-UNDO .

        DEFINE VARIABLE oResourceReader AS ResXResourceReader    NO-UNDO .

        ASSIGN oResourceReader = NEW ResXResourceReader (FileHelper:FindFile (pcFileName))

        oMetaEnumerator = oResourceReader:GetMetadataEnumerator () .
        oMetaEnumerator:Reset () .

        DO WHILE oMetaEnumerator:MoveNext():

            oMetaEntry = CAST (oMetaEnumerator:Current, DictionaryEntry) .

            IF UNBOX (oMetaEntry:Key) = "$this.Localizable":U THEN
                RETURN UNBOX (oMetaEntry:Value) .
        END .

        RETURN FALSE .

    END METHOD.
&ENDIF
END CLASS.
