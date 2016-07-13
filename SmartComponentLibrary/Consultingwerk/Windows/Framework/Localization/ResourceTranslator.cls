/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ResourceTranslator
    Purpose     : Supports reading and writing of resx files for translation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 06 23:03:10 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*             FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .  
USING Consultingwerk.Util.*                  FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .
USING System.Collections.*                   FROM ASSEMBLY . 
USING System.Resources.*                     FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.Framework.Localization.ResourceTranslator: 

    DEFINE VARIABLE oResourceReader AS System.Resources.ResXResourceReader NO-UNDO .

    { Consultingwerk/SmartFramework/Localization/eStringResources.i }
    
    /*------------------------------------------------------------------------------
        Purpose: Returns if the resource file is Localizable 
        Notes:   Based on the value and availbility of the $this.Localizable 
                 meta entry
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY IsLocalizable AS LOGICAL NO-UNDO 
	GET:

        DEFINE VARIABLE oMetaEnumerator AS IDictionaryEnumerator NO-UNDO .
        DEFINE VARIABLE oMetaEntry      AS DictionaryEntry       NO-UNDO .

        oMetaEnumerator = oResourceReader:GetMetadataEnumerator () .
        oMetaEnumerator:Reset () .
        
        DO WHILE oMetaEnumerator:MoveNext():
            
            oMetaEntry = CAST (oMetaEnumerator:Current, DictionaryEntry) .

            IF UNBOX (oMetaEntry:Key) = "$this.Localizable":U THEN 
                RETURN UNBOX (oMetaEntry:Value) .         
        END .	
        
        RETURN FALSE . 
        	
	END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the current resource file name
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ResourceFileName AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

	/*------------------------------------------------------------------------------
		Purpose: Constructor for the ResourceTranslator class  																	  
		Notes:  											
		@param pcResourceFile The name of the source resource file to work with						  
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ResourceTranslator (pcResourceFile AS CHARACTER):
		SUPER ().
		
		FileAssert:Exists (pcResourceFile) .
		
		ASSIGN oResourceReader              = NEW ResXResourceReader (FileHelper:FindFile (pcResourceFile)) 
		       THIS-OBJECT:ResourceFileName = pcResourceFile .

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
		Purpose: Returns a CharacterList of the Names of the System.String resources								  
		Notes: 
		@return The CharacterList of String resource names																	  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC CharacterList GetResourceNames ():
		
		DEFINE VARIABLE oList AS CharacterList NO-UNDO . 
		
		oList = NEW CharacterList ("":U, CHR (1)) . 

        {Consultingwerk/foreach.i DictionaryEntry oEntry in oResourceReader}

            IF TYPE-OF (oEntry:Value, System.String) THEN 
                oList:Add (UNBOX (oEntry:Key)) .

        END.

        RETURN oList . 

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterDictionary of the System.String resources                                  
        Notes: 
        @return The CharacterDictionary of String resources                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterDictionary GetResourceStrings ():
        
        DEFINE VARIABLE oDictionary AS CharacterDictionary NO-UNDO . 
        
        oDictionary = NEW CharacterDictionary ("":U, CHR (1),
                                               "":U, CHR (1)) . 

        {Consultingwerk/foreach.i DictionaryEntry oEntry in oResourceReader}

            IF TYPE-OF (oEntry:Value, System.String) THEN 
                oDictionary:Add (UNBOX (oEntry:Key),
                                 UNBOX (oEntry:Value)) .

        END.

        RETURN oDictionary . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterDictionary of the System.String resources                                  
        Notes: 
        @param eStringResources OUTPUT TABLE The eStringResources temp-table with the translations by key                                                            
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetResourceStrings (OUTPUT TABLE eStringResources):
        
        EMPTY TEMP-TABLE eStringResources .

        {Consultingwerk/foreach.i DictionaryEntry oEntry in oResourceReader}

            IF TYPE-OF (oEntry:Value, System.String) THEN DO:
                CREATE eStringResources.
                ASSIGN eStringResources.ResourceKey = oEntry:Key
                       eStringResources.OriginalString = oEntry:Value .
            END.
        END.

    END METHOD.
    
	/*------------------------------------------------------------------------------
		Purpose: Translates the Resource File using the given Translation Dictionary  																	  
		Notes:  		
		@param pcTargetFile The file name of the output file
		@param poTranslations The CharacterDictionary with the translations by key															  
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID Translate (pcTargetFile AS CHARACTER,
	                              poTranslations AS CharacterDictionary):
		
		DEFINE VARIABLE oResourceWriter  AS ResXResourceWriter    NO-UNDO . 
		DEFINE VARIABLE oMetaEnumerator  AS IDictionaryEnumerator NO-UNDO .
		DEFINE VARIABLE oMetaEntry       AS DictionaryEntry       NO-UNDO .
		DEFINE VARIABLE cKey             AS CHARACTER             NO-UNDO .
		
		ObjectAssert:IsValid (poTranslations, "Translation Dictionary":U) .
		
		oResourceWriter = NEW ResXResourceWriter (pcTargetFile) .

        oMetaEnumerator = oResourceReader:GetMetadataEnumerator () .
        oMetaEnumerator:Reset () .
        
        DO WHILE oMetaEnumerator:MoveNext():
            
            oMetaEntry = CAST (oMetaEnumerator:Current, DictionaryEntry) .
            
            oResourceWriter:AddMetadata (UNBOX (oMetaEntry:Key),
                                         oMetaEntry:Value) .
            
        END .

        {Consultingwerk/foreach.i DictionaryEntry oEntry in oResourceReader}

            ASSIGN cKey = UNBOX (oEntry:Key) . 

            IF poTranslations:ContainsKey (cKey) THEN 
                oResourceWriter:AddResource (cKey,
                                             poTranslations:GetValue (cKey)) .
            ELSE 
                oResourceWriter:AddResource (cKey,
                                             oEntry:Value) .
        END.
        
        oResourceWriter:Generate () .
        oResourceWriter:Close () .
        oResourceWriter:Dispose () .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Translates the Resource File using the given Translation Dictionary                                                                      
        Notes:          
        @param pcTargetFile The file name of the output file
        @param eStringResources The eStringResources temp-table with the translations by key                                                            
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Translate (pcTargetFile AS CHARACTER,
                                  TABLE FOR eStringResources):
        
        DEFINE VARIABLE oResourceWriter  AS ResXResourceWriter    NO-UNDO . 
        DEFINE VARIABLE oMetaEnumerator  AS IDictionaryEnumerator NO-UNDO .
        DEFINE VARIABLE oMetaEntry       AS DictionaryEntry       NO-UNDO .
        DEFINE VARIABLE cKey             AS CHARACTER             NO-UNDO .
        
        oResourceWriter = NEW ResXResourceWriter (pcTargetFile) .

        oMetaEnumerator = oResourceReader:GetMetadataEnumerator () .
        oMetaEnumerator:Reset () .
        
        DO WHILE oMetaEnumerator:MoveNext():
            
            oMetaEntry = CAST (oMetaEnumerator:Current, DictionaryEntry) .
            
            oResourceWriter:AddMetadata (UNBOX (oMetaEntry:Key),
                                         oMetaEntry:Value) .
            
        END .

        {Consultingwerk/foreach.i DictionaryEntry oEntry in oResourceReader}

            ASSIGN cKey = UNBOX (oEntry:Key) . 

            FIND eStringResources WHERE eStringResources.ResourceKey      = cKey
                                    AND eStringResources.TranslatedString > "":U 
                NO-ERROR .                                     

            IF AVAILABLE eStringResources THEN 
                oResourceWriter:AddResource (cKey,
                                             eStringResources.TranslatedString) .
            ELSE 
                oResourceWriter:AddResource (cKey,
                                             oEntry:Value) .
        END.
        
        oResourceWriter:Generate () .
        oResourceWriter:Close () .
        oResourceWriter:Dispose () .

    END METHOD.

END CLASS.
