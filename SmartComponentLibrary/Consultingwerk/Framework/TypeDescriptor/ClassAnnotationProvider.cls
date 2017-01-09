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
    File        : ClassAnnotationProvider
    Purpose     : Provides Annotation information for ABL classes at runtime
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 01 21:52:36 CEST 2015
    Notes       : Based on the output of the Consultingwerk.Studio.ExtractClassAnnotations
                  utility
                  http://confluence.consultingwerkcloud.com/wiki/display/SCL/The+Annotation+based+Type+Descriptor
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Assertion.*                FROM PROPATH .
USING Consultingwerk.Framework.*                FROM PROPATH .
USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.Framework.TypeDescriptor.ClassAnnotationProvider
    IMPLEMENTS IClassAnnotationProvider:

    DEFINE TEMP-TABLE ttClassCache NO-UNDO
        FIELD ClassName                AS CHARACTER
        FIELD ClassAnnotationContainer AS Progress.Lang.Object
        INDEX ClassName IS UNIQUE ClassName .

    DEFINE VARIABLE dtCacheAge   AS DATETIME-TZ NO-UNDO INITIAL ? .
    DEFINE VARIABLE iMaxCacheAge AS INTEGER     NO-UNDO INITIAL 0 .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassAnnotationProvider class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassAnnotationProvider ():
        SUPER ().

        DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .

        oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

        IF VALID-OBJECT (oConfigurationProvider) THEN DO:

            iMaxCacheAge = DataTypeHelper:ToInteger (oConfigurationProvider:GetValue ("AnnotationCacheMaxAge":U)) .

            IF iMaxCacheAge = ? THEN
                ASSIGN iMaxCacheAge = 0 .
        END.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Clears the cache
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClearCache ():

        EMPTY TEMP-TABLE ttClassCache .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the ClassAnnotationContainer with the annotations of the
                 given class
        Notes:
        @param poClass The class to return the Annotations for
        @return The ClassAnnotationContainer with the annotations of the given class
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ClassAnnotationContainer GetClassAnnotations (poClass AS Progress.Lang.Class):

        DEFINE VARIABLE cFileName                 AS CHARACTER                NO-UNDO .
        DEFINE VARIABLE cAnnotationFileName       AS CHARACTER                NO-UNDO .

        DEFINE VARIABLE lcSerialized              AS LONGCHAR                 NO-UNDO .
        DEFINE VARIABLE oClassAnnotationContainer AS ClassAnnotationContainer NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass """poClass"":U"}

        /* Check cache age */
        IF iMaxCacheAge > 0 AND dtCacheAge <> ? THEN DO:

            IF INTERVAL (NOW, dtCacheAge, "seconds":U) > iMaxCacheAge THEN DO:
                THIS-OBJECT:ClearCache() .
                ASSIGN dtCacheAge = ? .
            END.
        END.

        IF CAN-FIND (ttClassCache WHERE ttClassCache.ClassName = poClass:TypeName) THEN DO:
            FIND ttClassCache WHERE ttClassCache.ClassName = poClass:TypeName .

            IF VALID-OBJECT (ttClassCache.ClassAnnotationContainer) THEN
                RETURN CAST (ttClassCache.ClassAnnotationContainer, ClassAnnotationContainer) .
        END.

        ASSIGN cFileName           = ClassHelper:ClassNameToFile (poClass:TypeName)
               cAnnotationFileName = FileHelper:ReplaceFileExtension (cFileName, "annotations":U).

        IF FileHelper:Exists (cAnnotationFileName) THEN DO:
            ASSIGN dtCacheAge          = NOW
                   cAnnotationFileName = FileHelper:FindFile (cAnnotationFileName) .

            FIX-CODEPAGE (lcSerialized) = Codepages:UTF-8 .

            COPY-LOB FROM FILE cAnnotationFileName TO lcSerialized .

            oClassAnnotationContainer = CAST (Serializable:DeserializeInstance(lcSerialized), ClassAnnotationContainer) .

            IF NOT CAN-FIND (ttClassCache WHERE ttClassCache.ClassName = oClassAnnotationContainer:ClassName) THEN DO:
                CREATE ttClassCache .
                ASSIGN ttClassCache.ClassName = oClassAnnotationContainer:ClassName .
            END.
            ELSE
                FIND ttClassCache WHERE ttClassCache.ClassName = oClassAnnotationContainer:ClassName .

            ASSIGN ttClassCache.ClassAnnotationContainer = oClassAnnotationContainer .

            RETURN oClassAnnotationContainer .
        END.

        RETURN ? .

    END METHOD .

END CLASS.
