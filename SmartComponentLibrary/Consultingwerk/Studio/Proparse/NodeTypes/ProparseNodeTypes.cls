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
    File        : ProparseNodeTypes
    Purpose     : Enumeration of Proparse NodeTypes
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 23 07:04:36 CEST 2015
    Notes       : Properties prefixed with N_ to avoid conflicts with ABL
                  keywords
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Studio.Proparse.NodeTypes.* FROM PROPATH .  
USING Progress.Lang.*                            FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Studio.Proparse.NodeTypes.ProparseNodeTypes: 

    {Consultingwerk/Studio/Proparse/NodeTypes/ttNodeTypes.i &ACCESS=STATIC}
    
    /*------------------------------------------------------------------------------
        Purpose: Node type for the _cbit keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AACBIT AS CHARACTER INIT "AACBIT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the _control keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AACONTROL AS CHARACTER INIT "AACONTROL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the _list keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AALIST AS CHARACTER INIT "AALIST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the _memory keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AAMEMORY AS CHARACTER INIT "AAMEMORY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the _msg keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AAMSG AS CHARACTER INIT "AAMSG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the _pcontrol keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AAPCONTROL AS CHARACTER INIT "AAPCONTROL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the _serial-num keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AASERIAL AS CHARACTER INIT "AASERIAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the _trace keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AATRACE AS CHARACTER INIT "AATRACE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the absolute keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ABSOLUTE AS CHARACTER INIT "ABSOLUTE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the abstract keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ABSTRACT AS CHARACTER INIT "ABSTRACT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the accelerator keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ACCELERATOR AS CHARACTER INIT "ACCELERATOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the accumulate keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ACCUMULATE AS CHARACTER INIT "ACCUMULATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the active-form keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ACTIVEFORM AS CHARACTER INIT "ACTIVEFORM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the active-window keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ACTIVEWINDOW AS CHARACTER INIT "ACTIVEWINDOW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the add keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ADD AS CHARACTER INIT "ADD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the add-interval keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ADDINTERVAL AS CHARACTER INIT "ADDINTERVAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the advise keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ADVISE AS CHARACTER INIT "ADVISE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the alert-box keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ALERTBOX AS CHARACTER INIT "ALERTBOX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the alias keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ALIAS AS CHARACTER INIT "ALIAS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the all keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ALL AS CHARACTER INIT "ALL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the allow-replication keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ALLOWREPLICATION AS CHARACTER INIT "ALLOWREPLICATION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the alter keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ALTER AS CHARACTER INIT "ALTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the alternate-key keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ALTERNATEKEY AS CHARACTER INIT "ALTERNATEKEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ambiguous keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AMBIGUOUS AS CHARACTER INIT "AMBIGUOUS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the analyze keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ANALYZE AS CHARACTER INIT "ANALYZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the and keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AND AS CHARACTER INIT "AND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ansi-only keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ANSIONLY AS CHARACTER INIT "ANSIONLY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the any keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ANY AS CHARACTER INIT "ANY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the anywhere keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ANYWHERE AS CHARACTER INIT "ANYWHERE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the append keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_APPEND AS CHARACTER INIT "APPEND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the application keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_APPLICATION AS CHARACTER INIT "APPLICATION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the apply keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_APPLY AS CHARACTER INIT "APPLY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the array-message keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ARRAYMESSAGE AS CHARACTER INIT "ARRAYMESSAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the as keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AS AS CHARACTER INIT "AS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the asc keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ASC AS CHARACTER INIT "ASC":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ascending keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ASCENDING AS CHARACTER INIT "ASCENDING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ask-overwrite keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ASKOVERWRITE AS CHARACTER INIT "ASKOVERWRITE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the assembly keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ASSEMBLY AS CHARACTER INIT "ASSEMBLY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the assign keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ASSIGN AS CHARACTER INIT "ASSIGN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the asynchronous keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ASYNCHRONOUS AS CHARACTER INIT "ASYNCHRONOUS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the at keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AT AS CHARACTER INIT "AT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the attachment keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ATTACHMENT AS CHARACTER INIT "ATTACHMENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the attr-space keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ATTRSPACE AS CHARACTER INIT "ATTRSPACE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the audit-control keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AUDITCONTROL AS CHARACTER INIT "AUDITCONTROL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the audit-enabled keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AUDITENABLED AS CHARACTER INIT "AUDITENABLED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the audit-policy keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AUDITPOLICY AS CHARACTER INIT "AUDITPOLICY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the authorization keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AUTHORIZATION AS CHARACTER INIT "AUTHORIZATION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the auto-completion keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AUTOCOMPLETION AS CHARACTER INIT "AUTOCOMPLETION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the auto-end-key keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AUTOENDKEY AS CHARACTER INIT "AUTOENDKEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the auto-go keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AUTOGO AS CHARACTER INIT "AUTOGO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the automatic keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AUTOMATIC AS CHARACTER INIT "AUTOMATIC":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the auto-return keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AUTORETURN AS CHARACTER INIT "AUTORETURN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the available keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AVAILABLE AS CHARACTER INIT "AVAILABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the average keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AVERAGE AS CHARACTER INIT "AVERAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the avg keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_AVG AS CHARACTER INIT "AVG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the background keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BACKGROUND AS CHARACTER INIT "BACKGROUND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the backwards keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BACKWARDS AS CHARACTER INIT "BACKWARDS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the base64 keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BASE64 AS CHARACTER INIT "BASE64":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the base64-decode keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BASE64DECODE AS CHARACTER INIT "BASE64DECODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the base64-encode keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BASE64ENCODE AS CHARACTER INIT "BASE64ENCODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the base-key keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BASEKEY AS CHARACTER INIT "BASEKEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the batch-size keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BATCHSIZE AS CHARACTER INIT "BATCHSIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the before-hide keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BEFOREHIDE AS CHARACTER INIT "BEFOREHIDE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the before-table keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BEFORETABLE AS CHARACTER INIT "BEFORETABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the begins keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BEGINS AS CHARACTER INIT "BEGINS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the bell keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BELL AS CHARACTER INIT "BELL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the between keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BETWEEN AS CHARACTER INIT "BETWEEN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the bgcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BGCOLOR AS CHARACTER INIT "BGCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the big-endian keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BIGENDIAN AS CHARACTER INIT "BIGENDIAN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the bigint keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BIGINT AS CHARACTER INIT "BIGINT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the binary keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BINARY AS CHARACTER INIT "BINARY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the bind keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BIND AS CHARACTER INIT "BIND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the bind-where keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BINDWHERE AS CHARACTER INIT "BINDWHERE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the blank keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BLANK AS CHARACTER INIT "BLANK":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the blob keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BLOB AS CHARACTER INIT "BLOB":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the block-level keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BLOCKLEVEL AS CHARACTER INIT "BLOCKLEVEL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the both keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BOTH AS CHARACTER INIT "BOTH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the bottom keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BOTTOM AS CHARACTER INIT "BOTTOM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the box keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BOX AS CHARACTER INIT "BOX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the break keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BREAK AS CHARACTER INIT "BREAK":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the browse keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BROWSE AS CHARACTER INIT "BROWSE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the btos keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BTOS AS CHARACTER INIT "BTOS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the buffer keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BUFFER AS CHARACTER INIT "BUFFER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the buffer-chars keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BUFFERCHARS AS CHARACTER INIT "BUFFERCHARS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the buffer-compare keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BUFFERCOMPARE AS CHARACTER INIT "BUFFERCOMPARE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the buffer-copy keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BUFFERCOPY AS CHARACTER INIT "BUFFERCOPY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the buffer-lines keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BUFFERLINES AS CHARACTER INIT "BUFFERLINES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the buffer-name keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BUFFERNAME AS CHARACTER INIT "BUFFERNAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the button keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BUTTON AS CHARACTER INIT "BUTTON":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the buttons keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BUTTONS AS CHARACTER INIT "BUTTONS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the by keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BY AS CHARACTER INIT "BY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the by-pointer keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BYPOINTER AS CHARACTER INIT "BYPOINTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the by-reference keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BYREFERENCE AS CHARACTER INIT "BYREFERENCE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the byte keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BYTE AS CHARACTER INIT "BYTE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the by-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BYVALUE AS CHARACTER INIT "BYVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the by-variant-pointer keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_BYVARIANTPOINTER AS CHARACTER INIT "BYVARIANTPOINTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the cache keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CACHE AS CHARACTER INIT "CACHE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the cache-size keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CACHESIZE AS CHARACTER INIT "CACHESIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the call keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CALL AS CHARACTER INIT "CALL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the cancel-button keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CANCELBUTTON AS CHARACTER INIT "CANCELBUTTON":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the can-do keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CANDO AS CHARACTER INIT "CANDO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the can-find keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CANFIND AS CHARACTER INIT "CANFIND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the can-query keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CANQUERY AS CHARACTER INIT "CANQUERY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the can-set keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CANSET AS CHARACTER INIT "CANSET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the caps keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CAPS AS CHARACTER INIT "CAPS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the case keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CASE AS CHARACTER INIT "CASE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the case-sensitive keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CASESENSITIVE AS CHARACTER INIT "CASESENSITIVE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the cast keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CAST AS CHARACTER INIT "CAST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the catch keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CATCH AS CHARACTER INIT "CATCH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the cdecl keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CDECL_KW AS CHARACTER INIT "CDECL_KW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the centered keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CENTERED AS CHARACTER INIT "CENTERED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the chained keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CHAINED AS CHARACTER INIT "CHAINED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the character keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CHARACTER AS CHARACTER INIT "CHARACTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the characterlength keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CHARACTERLENGTH AS CHARACTER INIT "CHARACTERLENGTH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the check keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CHECK AS CHARACTER INIT "CHECK":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the choose keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CHOOSE AS CHARACTER INIT "CHOOSE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the chr keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CHR AS CHARACTER INIT "CHR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the class keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CLASS AS CHARACTER INIT "CLASS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the clear keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CLEAR AS CHARACTER INIT "CLEAR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the client-principal keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CLIENTPRINCIPAL AS CHARACTER INIT "CLIENTPRINCIPAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the clipboard keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CLIPBOARD AS CHARACTER INIT "CLIPBOARD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the clob keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CLOB AS CHARACTER INIT "CLOB":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the close keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CLOSE AS CHARACTER INIT "CLOSE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the codebase-locator keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CODEBASELOCATOR AS CHARACTER INIT "CODEBASELOCATOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the codepage keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CODEPAGE AS CHARACTER INIT "CODEPAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the codepage-convert keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CODEPAGECONVERT AS CHARACTER INIT "CODEPAGECONVERT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the collate keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLLATE AS CHARACTER INIT "COLLATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the col-of keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLOF AS CHARACTER INIT "COLOF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the colon keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLON AS CHARACTER INIT "COLON":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the colon-aligned keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLONALIGNED AS CHARACTER INIT "COLONALIGNED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the color keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLOR AS CHARACTER INIT "COLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the color-table keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLORTABLE AS CHARACTER INIT "COLORTABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the column keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLUMN AS CHARACTER INIT "COLUMN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the column-bgcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLUMNBGCOLOR AS CHARACTER INIT "COLUMNBGCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the column-codepage keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLUMNCODEPAGE AS CHARACTER INIT "COLUMNCODEPAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the column-dcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLUMNDCOLOR AS CHARACTER INIT "COLUMNDCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the column-fgcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLUMNFGCOLOR AS CHARACTER INIT "COLUMNFGCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the column-font keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLUMNFONT AS CHARACTER INIT "COLUMNFONT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the column-label keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLUMNLABEL AS CHARACTER INIT "COLUMNLABEL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the column-of keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLUMNOF AS CHARACTER INIT "COLUMNOF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the column-pfcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLUMNPFCOLOR AS CHARACTER INIT "COLUMNPFCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the columns keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COLUMNS AS CHARACTER INIT "COLUMNS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the combo-box keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COMBOBOX AS CHARACTER INIT "COMBOBOX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the com-handle keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COMHANDLE AS CHARACTER INIT "COMHANDLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the command keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COMMAND AS CHARACTER INIT "COMMAND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the compare keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COMPARE AS CHARACTER INIT "COMPARE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the compares keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COMPARES AS CHARACTER INIT "COMPARES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the compile keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COMPILE AS CHARACTER INIT "COMPILE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the compiler keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COMPILER AS CHARACTER INIT "COMPILER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the complete keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COMPLETE AS CHARACTER INIT "COMPLETE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the com-self keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COMSELF AS CHARACTER INIT "COMSELF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the config-name keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONFIGNAME AS CHARACTER INIT "CONFIGNAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the connect keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONNECT AS CHARACTER INIT "CONNECT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the connected keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONNECTED AS CHARACTER INIT "CONNECTED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the constructor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONSTRUCTOR AS CHARACTER INIT "CONSTRUCTOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the contains keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONTAINS AS CHARACTER INIT "CONTAINS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the contents keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONTENTS AS CHARACTER INIT "CONTENTS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the context keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONTEXT AS CHARACTER INIT "CONTEXT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the context-help keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONTEXTHELP AS CHARACTER INIT "CONTEXTHELP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the context-help-file keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONTEXTHELPFILE AS CHARACTER INIT "CONTEXTHELPFILE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the context-help-id keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONTEXTHELPID AS CHARACTER INIT "CONTEXTHELPID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the context-popup keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONTEXTPOPUP AS CHARACTER INIT "CONTEXTPOPUP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the control keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONTROL AS CHARACTER INIT "CONTROL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the control-frame keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONTROLFRAME AS CHARACTER INIT "CONTROLFRAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the convert keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONVERT AS CHARACTER INIT "CONVERT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the convert-3d-colors keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CONVERT3DCOLORS AS CHARACTER INIT "CONVERT3DCOLORS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the copy-dataset keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COPYDATASET AS CHARACTER INIT "COPYDATASET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the copy-lob keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COPYLOB AS CHARACTER INIT "COPYLOB":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the copy-temp-table keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COPYTEMPTABLE AS CHARACTER INIT "COPYTEMPTABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the count keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COUNT AS CHARACTER INIT "COUNT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the count-of keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_COUNTOF AS CHARACTER INIT "COUNTOF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the create keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CREATE AS CHARACTER INIT "CREATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the create-like-sequential keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CREATELIKESEQUENTIAL AS CHARACTER INIT "CREATELIKESEQUENTIAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the create-test-file keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CREATETESTFILE AS CHARACTER INIT "CREATETESTFILE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the currency keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CURRENCY AS CHARACTER INIT "CURRENCY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the current keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CURRENT AS CHARACTER INIT "CURRENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the current-changed keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CURRENTCHANGED AS CHARACTER INIT "CURRENTCHANGED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the current-environment keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CURRENTENVIRONMENT AS CHARACTER INIT "CURRENTENVIRONMENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the current-language keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CURRENTLANGUAGE AS CHARACTER INIT "CURRENTLANGUAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the current-query keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CURRENTQUERY AS CHARACTER INIT "CURRENTQUERY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the current-result-row keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CURRENTRESULTROW AS CHARACTER INIT "CURRENTRESULTROW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the current-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CURRENTVALUE AS CHARACTER INIT "CURRENTVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the current-window keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CURRENTWINDOW AS CHARACTER INIT "CURRENTWINDOW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the cursor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_CURSOR AS CHARACTER INIT "CURSOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the database keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATABASE AS CHARACTER INIT "DATABASE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the data-bind keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATABIND AS CHARACTER INIT "DATABIND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the data-relation keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATARELATION AS CHARACTER INIT "DATARELATION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dataservers keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATASERVERS AS CHARACTER INIT "DATASERVERS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dataset keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATASET AS CHARACTER INIT "DATASET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dataset-handle keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATASETHANDLE AS CHARACTER INIT "DATASETHANDLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the data-source keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATASOURCE AS CHARACTER INIT "DATASOURCE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the data-source-modified keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATASOURCEMODIFIED AS CHARACTER INIT "DATASOURCEMODIFIED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the data-source-rowid keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATASOURCEROWID AS CHARACTER INIT "DATASOURCEROWID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the date keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATE AS CHARACTER INIT "DATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the datetime keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATETIME AS CHARACTER INIT "DATETIME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the datetime-tz keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DATETIMETZ AS CHARACTER INIT "DATETIMETZ":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the day keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DAY AS CHARACTER INIT "DAY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dbcodepage keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DBCODEPAGE AS CHARACTER INIT "DBCODEPAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dbcollation keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DBCOLLATION AS CHARACTER INIT "DBCOLLATION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dbims keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DBIMS AS CHARACTER INIT "DBIMS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dbname keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DBNAME AS CHARACTER INIT "DBNAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dbparam keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DBPARAM AS CHARACTER INIT "DBPARAM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the db-remote-host keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DBREMOTEHOST AS CHARACTER INIT "DBREMOTEHOST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dbrestrictions keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DBRESTRICTIONS AS CHARACTER INIT "DBRESTRICTIONS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dbtaskid keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DBTASKID AS CHARACTER INIT "DBTASKID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dbtype keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DBTYPE AS CHARACTER INIT "DBTYPE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dbversion keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DBVERSION AS CHARACTER INIT "DBVERSION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DCOLOR AS CHARACTER INIT "DCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dde keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DDE AS CHARACTER INIT "DDE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the deblank keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEBLANK AS CHARACTER INIT "DEBLANK":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the debug keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEBUG AS CHARACTER INIT "DEBUG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the debugger keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEBUGGER AS CHARACTER INIT "DEBUGGER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the debug-list keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEBUGLIST AS CHARACTER INIT "DEBUGLIST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the decimal keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DECIMAL AS CHARACTER INIT "DECIMAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the decimals keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DECIMALS AS CHARACTER INIT "DECIMALS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the declare keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DECLARE AS CHARACTER INIT "DECLARE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the decrypt keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DECRYPT AS CHARACTER INIT "DECRYPT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the default keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEFAULT AS CHARACTER INIT "DEFAULT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the default-button keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEFAULTBUTTON AS CHARACTER INIT "DEFAULTBUTTON":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the default-extension keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEFAULTEXTENSION AS CHARACTER INIT "DEFAULTEXTENSION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the default-noxlate keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEFAULTNOXLATE AS CHARACTER INIT "DEFAULTNOXLATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the default-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEFAULTVALUE AS CHARACTER INIT "DEFAULTVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the default-window keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEFAULTWINDOW AS CHARACTER INIT "DEFAULTWINDOW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the defer-lob-fetch keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEFERLOBFETCH AS CHARACTER INIT "DEFERLOBFETCH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the define keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEFINE AS CHARACTER INIT "DEFINE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the defined keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DEFINED AS CHARACTER INIT "DEFINED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the delegate keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DELEGATE AS CHARACTER INIT "DELEGATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the delete-character keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DELETECHARACTER AS CHARACTER INIT "DELETECHARACTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the delete-result-list-entry keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DELETERESULTLISTENTRY AS CHARACTER INIT "DELETERESULTLISTENTRY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the delete keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DELETE_KW AS CHARACTER INIT "DELETE_KW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the delimiter keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DELIMITER AS CHARACTER INIT "DELIMITER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the descending keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DESCENDING AS CHARACTER INIT "DESCENDING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the deselection keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DESELECTION AS CHARACTER INIT "DESELECTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the destructor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DESTRUCTOR AS CHARACTER INIT "DESTRUCTOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dialog-box keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DIALOGBOX AS CHARACTER INIT "DIALOGBOX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dialog-help keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DIALOGHELP AS CHARACTER INIT "DIALOGHELP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dictionary keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DICTIONARY AS CHARACTER INIT "DICTIONARY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dir keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DIR AS CHARACTER INIT "DIR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the disable keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DISABLE AS CHARACTER INIT "DISABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the disable-auto-zap keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DISABLEAUTOZAP AS CHARACTER INIT "DISABLEAUTOZAP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the disabled keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DISABLED AS CHARACTER INIT "DISABLED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the disconnect keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DISCONNECT AS CHARACTER INIT "DISCONNECT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the display keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DISPLAY AS CHARACTER INIT "DISPLAY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the distinct keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DISTINCT AS CHARACTER INIT "DISTINCT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the do keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DO AS CHARACTER INIT "DO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dos keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DOS AS CHARACTER INIT "DOS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the double keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DOUBLE AS CHARACTER INIT "DOUBLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the down keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DOWN AS CHARACTER INIT "DOWN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the drop keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DROP AS CHARACTER INIT "DROP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the drop-down keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DROPDOWN AS CHARACTER INIT "DROPDOWN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the drop-down-list keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DROPDOWNLIST AS CHARACTER INIT "DROPDOWNLIST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the drop-file-notify keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DROPFILENOTIFY AS CHARACTER INIT "DROPFILENOTIFY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the drop-target keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DROPTARGET AS CHARACTER INIT "DROPTARGET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dump keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DUMP AS CHARACTER INIT "DUMP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dynamic keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DYNAMIC AS CHARACTER INIT "DYNAMIC":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dynamic-cast keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DYNAMICCAST AS CHARACTER INIT "DYNAMICCAST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dynamic-current-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DYNAMICCURRENTVALUE AS CHARACTER INIT "DYNAMICCURRENTVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dynamic-function keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DYNAMICFUNCTION AS CHARACTER INIT "DYNAMICFUNCTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dynamic-invoke keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DYNAMICINVOKE AS CHARACTER INIT "DYNAMICINVOKE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dynamic-new keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DYNAMICNEW AS CHARACTER INIT "DYNAMICNEW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the dynamic-next-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_DYNAMICNEXTVALUE AS CHARACTER INIT "DYNAMICNEXTVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the each keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EACH AS CHARACTER INIT "EACH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the echo keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ECHO AS CHARACTER INIT "ECHO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the edge-chars keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EDGECHARS AS CHARACTER INIT "EDGECHARS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the edge-pixels keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EDGEPIXELS AS CHARACTER INIT "EDGEPIXELS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the editing keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EDITING AS CHARACTER INIT "EDITING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the editor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EDITOR AS CHARACTER INIT "EDITOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the edit-undo keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EDITUNDO AS CHARACTER INIT "EDITUNDO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the else keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ELSE AS CHARACTER INIT "ELSE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the empty keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EMPTY AS CHARACTER INIT "EMPTY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the enable keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ENABLE AS CHARACTER INIT "ENABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the enabled-fields keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ENABLEDFIELDS AS CHARACTER INIT "ENABLEDFIELDS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the encode keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ENCODE AS CHARACTER INIT "ENCODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the encrypt keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ENCRYPT AS CHARACTER INIT "ENCRYPT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the encryption-salt keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ENCRYPTIONSALT AS CHARACTER INIT "ENCRYPTIONSALT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the end keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_END AS CHARACTER INIT "END":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the end-key keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ENDKEY AS CHARACTER INIT "ENDKEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the end-move keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ENDMOVE AS CHARACTER INIT "ENDMOVE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the end-resize keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ENDRESIZE AS CHARACTER INIT "ENDRESIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the end-row-resize keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ENDROWRESIZE AS CHARACTER INIT "ENDROWRESIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the entered keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ENTERED AS CHARACTER INIT "ENTERED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the entry keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ENTRY AS CHARACTER INIT "ENTRY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the eq keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EQ AS CHARACTER INIT "EQ":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the error keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ERROR AS CHARACTER INIT "ERROR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the error-code keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ERRORCODE AS CHARACTER INIT "ERRORCODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the error-stack-trace keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ERRORSTACKTRACE AS CHARACTER INIT "ERRORSTACKTRACE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the error-status keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ERRORSTATUS AS CHARACTER INIT "ERRORSTATUS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the escape keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ESCAPE AS CHARACTER INIT "ESCAPE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the etime keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ETIME_KW AS CHARACTER INIT "ETIME_KW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the event keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EVENT AS CHARACTER INIT "EVENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the event-procedure keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EVENTPROCEDURE AS CHARACTER INIT "EVENTPROCEDURE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the events keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EVENTS AS CHARACTER INIT "EVENTS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the except keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXCEPT AS CHARACTER INIT "EXCEPT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the exclusive-id keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXCLUSIVEID AS CHARACTER INIT "EXCLUSIVEID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the exclusive-lock keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXCLUSIVELOCK AS CHARACTER INIT "EXCLUSIVELOCK":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the exclusive-web-user keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXCLUSIVEWEBUSER AS CHARACTER INIT "EXCLUSIVEWEBUSER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the execute keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXECUTE AS CHARACTER INIT "EXECUTE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the exists keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXISTS AS CHARACTER INIT "EXISTS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the exp keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXP AS CHARACTER INIT "EXP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the expand keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXPAND AS CHARACTER INIT "EXPAND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the expandable keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXPANDABLE AS CHARACTER INIT "EXPANDABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the explicit keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXPLICIT AS CHARACTER INIT "EXPLICIT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the export keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXPORT AS CHARACTER INIT "EXPORT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the extended keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXTENDED AS CHARACTER INIT "EXTENDED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the extent keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXTENT AS CHARACTER INIT "EXTENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the external keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_EXTERNAL AS CHARACTER INIT "EXTERNAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the false-leaks keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FALSELEAKS AS CHARACTER INIT "FALSELEAKS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the false keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FALSE_KW AS CHARACTER INIT "FALSE_KW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the fetch keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FETCH AS CHARACTER INIT "FETCH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the fgcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FGCOLOR AS CHARACTER INIT "FGCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the field keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FIELD AS CHARACTER INIT "FIELD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the fields keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FIELDS AS CHARACTER INIT "FIELDS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the file keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FILE AS CHARACTER INIT "FILE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the file-information keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FILEINFORMATION AS CHARACTER INIT "FILEINFORMATION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the fill keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FILL AS CHARACTER INIT "FILL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the fill-in keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FILLIN AS CHARACTER INIT "FILLIN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the filters keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FILTERS AS CHARACTER INIT "FILTERS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the final keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FINAL AS CHARACTER INIT "FINAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the finally keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FINALLY AS CHARACTER INIT "FINALLY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the find keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FIND AS CHARACTER INIT "FIND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the find-case-sensitive keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FINDCASESENSITIVE AS CHARACTER INIT "FINDCASESENSITIVE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the finder keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FINDER AS CHARACTER INIT "FINDER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the find-global keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FINDGLOBAL AS CHARACTER INIT "FINDGLOBAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the find-next-occurrence keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FINDNEXTOCCURRENCE AS CHARACTER INIT "FINDNEXTOCCURRENCE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the find-prev-occurrence keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FINDPREVOCCURRENCE AS CHARACTER INIT "FINDPREVOCCURRENCE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the find-select keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FINDSELECT AS CHARACTER INIT "FINDSELECT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the find-wrap-around keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FINDWRAPAROUND AS CHARACTER INIT "FINDWRAPAROUND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the first keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FIRST AS CHARACTER INIT "FIRST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the first-form keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FIRSTFORM AS CHARACTER INIT "FIRSTFORM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the first-of keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FIRSTOF AS CHARACTER INIT "FIRSTOF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the fit-last-column keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FITLASTCOLUMN AS CHARACTER INIT "FITLASTCOLUMN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the fixchar keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FIXCHAR AS CHARACTER INIT "FIXCHAR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the fix-codepage keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FIXCODEPAGE AS CHARACTER INIT "FIXCODEPAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the fixed-only keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FIXEDONLY AS CHARACTER INIT "FIXEDONLY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the flat-button keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FLATBUTTON AS CHARACTER INIT "FLATBUTTON":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the float keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FLOAT AS CHARACTER INIT "FLOAT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the focus keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FOCUS AS CHARACTER INIT "FOCUS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the font keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FONT AS CHARACTER INIT "FONT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the font-based-layout keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FONTBASEDLAYOUT AS CHARACTER INIT "FONTBASEDLAYOUT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the font-table keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FONTTABLE AS CHARACTER INIT "FONTTABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the for keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FOR AS CHARACTER INIT "FOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the force-file keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FORCEFILE AS CHARACTER INIT "FORCEFILE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the foreign-key-hidden keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FOREIGNKEYHIDDEN AS CHARACTER INIT "FOREIGNKEYHIDDEN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the format keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FORMAT AS CHARACTER INIT "FORMAT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the forminput keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FORMINPUT AS CHARACTER INIT "FORMINPUT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the form-long-input keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FORMLONGINPUT AS CHARACTER INIT "FORMLONGINPUT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the forwards keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FORWARDS AS CHARACTER INIT "FORWARDS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frame keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FRAME AS CHARACTER INIT "FRAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frame-col keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FRAMECOL AS CHARACTER INIT "FRAMECOL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frame-db keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FRAMEDB AS CHARACTER INIT "FRAMEDB":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frame-down keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FRAMEDOWN AS CHARACTER INIT "FRAMEDOWN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frame-field keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FRAMEFIELD AS CHARACTER INIT "FRAMEFIELD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frame-file keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FRAMEFILE AS CHARACTER INIT "FRAMEFILE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frame-index keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FRAMEINDEX AS CHARACTER INIT "FRAMEINDEX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frame-line keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FRAMELINE AS CHARACTER INIT "FRAMELINE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frame-name keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FRAMENAME AS CHARACTER INIT "FRAMENAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frame-row keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FRAMEROW AS CHARACTER INIT "FRAMEROW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frame-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FRAMEVALUE AS CHARACTER INIT "FRAMEVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the frequency keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FREQUENCY AS CHARACTER INIT "FREQUENCY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the from keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FROM AS CHARACTER INIT "FROM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the from-current keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FROMCURRENT AS CHARACTER INIT "FROMCURRENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the function keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FUNCTION AS CHARACTER INIT "FUNCTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the function-call-type keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_FUNCTIONCALLTYPE AS CHARACTER INIT "FUNCTIONCALLTYPE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ge keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GE AS CHARACTER INIT "GE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the generate-md5 keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GENERATEMD5 AS CHARACTER INIT "GENERATEMD5":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the generate-pbe-key keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GENERATEPBEKEY AS CHARACTER INIT "GENERATEPBEKEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the generate-pbe-salt keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GENERATEPBESALT AS CHARACTER INIT "GENERATEPBESALT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the generate-random-key keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GENERATERANDOMKEY AS CHARACTER INIT "GENERATERANDOMKEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the generate-uuid keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GENERATEUUID AS CHARACTER INIT "GENERATEUUID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GET AS CHARACTER INIT "GET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-attr-call-type keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETATTRCALLTYPE AS CHARACTER INIT "GETATTRCALLTYPE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-bits keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETBITS AS CHARACTER INIT "GETBITS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-buffer-handle keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETBUFFERHANDLE AS CHARACTER INIT "GETBUFFERHANDLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-byte keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETBYTE AS CHARACTER INIT "GETBYTE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-byte-order keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETBYTEORDER AS CHARACTER INIT "GETBYTEORDER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-bytes keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETBYTES AS CHARACTER INIT "GETBYTES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-cgi-list keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETCGILIST AS CHARACTER INIT "GETCGILIST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-cgi-long-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETCGILONGVALUE AS CHARACTER INIT "GETCGILONGVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-cgi-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETCGIVALUE AS CHARACTER INIT "GETCGIVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-codepages keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETCODEPAGES AS CHARACTER INIT "GETCODEPAGES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-collations keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETCOLLATIONS AS CHARACTER INIT "GETCOLLATIONS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-config-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETCONFIGVALUE AS CHARACTER INIT "GETCONFIGVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-dir keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETDIR AS CHARACTER INIT "GETDIR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-double keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETDOUBLE AS CHARACTER INIT "GETDOUBLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-file keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETFILE AS CHARACTER INIT "GETFILE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-float keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETFLOAT AS CHARACTER INIT "GETFLOAT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-int64 keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETINT64 AS CHARACTER INIT "GETINT64":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-key-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETKEYVALUE AS CHARACTER INIT "GETKEYVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-license keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETLICENSE AS CHARACTER INIT "GETLICENSE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-long keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETLONG AS CHARACTER INIT "GETLONG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-pointer-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETPOINTERVALUE AS CHARACTER INIT "GETPOINTERVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-short keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETSHORT AS CHARACTER INIT "GETSHORT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-size keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETSIZE AS CHARACTER INIT "GETSIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-string keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETSTRING AS CHARACTER INIT "GETSTRING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-unsigned-long keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETUNSIGNEDLONG AS CHARACTER INIT "GETUNSIGNEDLONG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the get-unsigned-short keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GETUNSIGNEDSHORT AS CHARACTER INIT "GETUNSIGNEDSHORT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the global keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GLOBAL AS CHARACTER INIT "GLOBAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the go-on keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GOON AS CHARACTER INIT "GOON":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the go-pending keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GOPENDING AS CHARACTER INIT "GOPENDING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the grant keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GRANT AS CHARACTER INIT "GRANT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the graphic-edge keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GRAPHICEDGE AS CHARACTER INIT "GRAPHICEDGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the group keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GROUP AS CHARACTER INIT "GROUP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the group-box keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GROUPBOX AS CHARACTER INIT "GROUPBOX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the gt keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GTHAN AS CHARACTER INIT "GTHAN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the guid keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_GUID AS CHARACTER INIT "GUID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the handle keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HANDLE AS CHARACTER INIT "HANDLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the having keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HAVING AS CHARACTER INIT "HAVING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the header keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HEADER AS CHARACTER INIT "HEADER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the height keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HEIGHT AS CHARACTER INIT "HEIGHT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the help keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HELP AS CHARACTER INIT "HELP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the help-topic keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HELPTOPIC AS CHARACTER INIT "HELPTOPIC":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the hex-decode keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HEXDECODE AS CHARACTER INIT "HEXDECODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the hex-encode keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HEXENCODE AS CHARACTER INIT "HEXENCODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the hide keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HIDE AS CHARACTER INIT "HIDE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the hint keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HINT AS CHARACTER INIT "HINT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the horizontal keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HORIZONTAL AS CHARACTER INIT "HORIZONTAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the host-byte-order keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HOSTBYTEORDER AS CHARACTER INIT "HOSTBYTEORDER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the html-end-of-line keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HTMLENDOFLINE AS CHARACTER INIT "HTMLENDOFLINE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the html-frame-begin keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HTMLFRAMEBEGIN AS CHARACTER INIT "HTMLFRAMEBEGIN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the html-frame-end keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HTMLFRAMEEND AS CHARACTER INIT "HTMLFRAMEEND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the html-header-begin keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HTMLHEADERBEGIN AS CHARACTER INIT "HTMLHEADERBEGIN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the html-header-end keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HTMLHEADEREND AS CHARACTER INIT "HTMLHEADEREND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the html-title-begin keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HTMLTITLEBEGIN AS CHARACTER INIT "HTMLTITLEBEGIN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the html-title-end keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_HTMLTITLEEND AS CHARACTER INIT "HTMLTITLEEND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the if keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IF AS CHARACTER INIT "IF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the image keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IMAGE AS CHARACTER INIT "IMAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the image-down keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IMAGEDOWN AS CHARACTER INIT "IMAGEDOWN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the image-insensitive keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IMAGEINSENSITIVE AS CHARACTER INIT "IMAGEINSENSITIVE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the image-size keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IMAGESIZE AS CHARACTER INIT "IMAGESIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the image-size-chars keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IMAGESIZECHARS AS CHARACTER INIT "IMAGESIZECHARS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the image-size-pixels keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IMAGESIZEPIXELS AS CHARACTER INIT "IMAGESIZEPIXELS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the image-up keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IMAGEUP AS CHARACTER INIT "IMAGEUP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the implements keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IMPLEMENTS AS CHARACTER INIT "IMPLEMENTS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the import keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IMPORT AS CHARACTER INIT "IMPORT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the increment-exclusive-id keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INCREMENTEXCLUSIVEID AS CHARACTER INIT "INCREMENTEXCLUSIVEID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the index keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INDEX AS CHARACTER INIT "INDEX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the indexed-reposition keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INDEXEDREPOSITION AS CHARACTER INIT "INDEXEDREPOSITION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the index-hint keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INDEXHINT AS CHARACTER INIT "INDEXHINT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the indicator keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INDICATOR AS CHARACTER INIT "INDICATOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the information keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INFORMATION AS CHARACTER INIT "INFORMATION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the inherit-bgcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INHERITBGCOLOR AS CHARACTER INIT "INHERITBGCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the inherit-fgcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INHERITFGCOLOR AS CHARACTER INIT "INHERITFGCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the inherits keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INHERITS AS CHARACTER INIT "INHERITS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the initial keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INITIAL AS CHARACTER INIT "INITIAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the initial-dir keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INITIALDIR AS CHARACTER INIT "INITIALDIR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the initial-filter keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INITIALFILTER AS CHARACTER INIT "INITIALFILTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the initiate keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INITIATE AS CHARACTER INIT "INITIATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the inner keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INNER AS CHARACTER INIT "INNER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the inner-chars keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INNERCHARS AS CHARACTER INIT "INNERCHARS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the inner-lines keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INNERLINES AS CHARACTER INIT "INNERLINES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the input keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INPUT AS CHARACTER INIT "INPUT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the input-output keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INPUTOUTPUT AS CHARACTER INIT "INPUTOUTPUT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the insert keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INSERT AS CHARACTER INIT "INSERT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the int64 keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INT64 AS CHARACTER INIT "INT64":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the integer keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INTEGER AS CHARACTER INIT "INTEGER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the interface keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INTERFACE AS CHARACTER INIT "INTERFACE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the interval keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INTERVAL AS CHARACTER INIT "INTERVAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the into keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_INTO AS CHARACTER INIT "INTO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the in keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IN_KW AS CHARACTER INIT "IN_KW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the is keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IS AS CHARACTER INIT "IS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the is-attr-space keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ISATTRSPACE AS CHARACTER INIT "ISATTRSPACE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the is-codepage-fixed keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ISCODEPAGEFIXED AS CHARACTER INIT "ISCODEPAGEFIXED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the is-column-codepage keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ISCOLUMNCODEPAGE AS CHARACTER INIT "ISCOLUMNCODEPAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the is-lead-byte keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ISLEADBYTE AS CHARACTER INIT "ISLEADBYTE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the iso-date keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ISODATE AS CHARACTER INIT "ISODATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the item keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ITEM AS CHARACTER INIT "ITEM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the iunknown keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_IUNKNOWN AS CHARACTER INIT "IUNKNOWN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the join keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_JOIN AS CHARACTER INIT "JOIN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the join-by-sqldb keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_JOINBYSQLDB AS CHARACTER INIT "JOINBYSQLDB":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the kblabel keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_KBLABEL AS CHARACTER INIT "KBLABEL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the keep-messages keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_KEEPMESSAGES AS CHARACTER INIT "KEEPMESSAGES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the keep-tab-order keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_KEEPTABORDER AS CHARACTER INIT "KEEPTABORDER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the key keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_KEY AS CHARACTER INIT "KEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the key-code keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_KEYCODE AS CHARACTER INIT "KEYCODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the key-function keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_KEYFUNCTION AS CHARACTER INIT "KEYFUNCTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the key-label keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_KEYLABEL AS CHARACTER INIT "KEYLABEL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the keys keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_KEYS AS CHARACTER INIT "KEYS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the keyword keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_KEYWORD AS CHARACTER INIT "KEYWORD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the keyword-all keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_KEYWORDALL AS CHARACTER INIT "KEYWORDALL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the label keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LABEL AS CHARACTER INIT "LABEL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the label-bgcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LABELBGCOLOR AS CHARACTER INIT "LABELBGCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the label-dcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LABELDCOLOR AS CHARACTER INIT "LABELDCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the label-fgcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LABELFGCOLOR AS CHARACTER INIT "LABELFGCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the label-font keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LABELFONT AS CHARACTER INIT "LABELFONT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the landscape keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LANDSCAPE AS CHARACTER INIT "LANDSCAPE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the languages keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LANGUAGES AS CHARACTER INIT "LANGUAGES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the large keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LARGE AS CHARACTER INIT "LARGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the large-to-small keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LARGETOSMALL AS CHARACTER INIT "LARGETOSMALL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the last keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LAST AS CHARACTER INIT "LAST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the last-batch keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LASTBATCH AS CHARACTER INIT "LASTBATCH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the last-event keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LASTEVENT AS CHARACTER INIT "LASTEVENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the last-form keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LASTFORM AS CHARACTER INIT "LASTFORM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the last-key keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LASTKEY AS CHARACTER INIT "LASTKEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the last-of keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LASTOF AS CHARACTER INIT "LASTOF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the lc keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LC AS CHARACTER INIT "LC":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ldbname keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LDBNAME AS CHARACTER INIT "LDBNAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the le keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LE AS CHARACTER INIT "LE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the leak-detection keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LEAKDETECTION AS CHARACTER INIT "LEAKDETECTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the leave keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LEAVE AS CHARACTER INIT "LEAVE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the left keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LEFT AS CHARACTER INIT "LEFT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the left-aligned keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LEFTALIGNED AS CHARACTER INIT "LEFTALIGNED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the left-trim keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LEFTTRIM AS CHARACTER INIT "LEFTTRIM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the length keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LENGTH AS CHARACTER INIT "LENGTH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the library keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LIBRARY AS CHARACTER INIT "LIBRARY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the like keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LIKE AS CHARACTER INIT "LIKE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the like-sequential keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LIKESEQUENTIAL AS CHARACTER INIT "LIKESEQUENTIAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the line-counter keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LINECOUNTER AS CHARACTER INIT "LINECOUNTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the list-events keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LISTEVENTS AS CHARACTER INIT "LISTEVENTS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the listing keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LISTING AS CHARACTER INIT "LISTING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the list-item-pairs keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LISTITEMPAIRS AS CHARACTER INIT "LISTITEMPAIRS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the list-items keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LISTITEMS AS CHARACTER INIT "LISTITEMS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the list-query-attrs keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LISTQUERYATTRS AS CHARACTER INIT "LISTQUERYATTRS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the list-set-attrs keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LISTSETATTRS AS CHARACTER INIT "LISTSETATTRS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the list-widgets keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LISTWIDGETS AS CHARACTER INIT "LISTWIDGETS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the little-endian keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LITTLEENDIAN AS CHARACTER INIT "LITTLEENDIAN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the load keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LOAD AS CHARACTER INIT "LOAD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the load-picture keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LOADPICTURE AS CHARACTER INIT "LOADPICTURE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the lob-dir keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LOBDIR AS CHARACTER INIT "LOBDIR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the locked keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LOCKED AS CHARACTER INIT "LOCKED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the log keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LOG AS CHARACTER INIT "LOG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the logical keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LOGICAL AS CHARACTER INIT "LOGICAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the log-manager keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LOGMANAGER AS CHARACTER INIT "LOGMANAGER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the long keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LONG AS CHARACTER INIT "LONG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the longchar keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LONGCHAR AS CHARACTER INIT "LONGCHAR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the lookahead keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LOOKAHEAD AS CHARACTER INIT "LOOKAHEAD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the lookup keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LOOKUP AS CHARACTER INIT "LOOKUP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the lt keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_LTHAN AS CHARACTER INIT "LTHAN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the machine-class keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MACHINECLASS AS CHARACTER INIT "MACHINECLASS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the map keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MAP AS CHARACTER INIT "MAP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the margin-extra keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MARGINEXTRA AS CHARACTER INIT "MARGINEXTRA":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the mark-new keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MARKNEW AS CHARACTER INIT "MARKNEW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the mark-row-state keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MARKROWSTATE AS CHARACTER INIT "MARKROWSTATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the matches keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MATCHES AS CHARACTER INIT "MATCHES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the max-chars keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MAXCHARS AS CHARACTER INIT "MAXCHARS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the maximize keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MAXIMIZE AS CHARACTER INIT "MAXIMIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the max keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MAXIMUM AS CHARACTER INIT "MAXIMUM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the maximum-level keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MAXIMUMLEVEL AS CHARACTER INIT "MAXIMUMLEVEL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the max-rows keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MAXROWS AS CHARACTER INIT "MAXROWS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the max-size keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MAXSIZE AS CHARACTER INIT "MAXSIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the max-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MAXVALUE AS CHARACTER INIT "MAXVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the md5-digest keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MD5DIGEST AS CHARACTER INIT "MD5DIGEST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the member keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MEMBER AS CHARACTER INIT "MEMBER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the memptr keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MEMPTR AS CHARACTER INIT "MEMPTR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the menu keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MENU AS CHARACTER INIT "MENU":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the menu-bar keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MENUBAR AS CHARACTER INIT "MENUBAR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the menu-item keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MENUITEM AS CHARACTER INIT "MENUITEM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the merge-by-field keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MERGEBYFIELD AS CHARACTER INIT "MERGEBYFIELD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the message keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MESSAGE AS CHARACTER INIT "MESSAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the message-line keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MESSAGELINE AS CHARACTER INIT "MESSAGELINE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the message-lines keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MESSAGELINES AS CHARACTER INIT "MESSAGELINES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the method keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_METHOD AS CHARACTER INIT "METHOD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the minimum keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MINIMUM AS CHARACTER INIT "MINIMUM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the min-size keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MINSIZE AS CHARACTER INIT "MINSIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the min-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MINVALUE AS CHARACTER INIT "MINVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the modulo keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MODULO AS CHARACTER INIT "MODULO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the month keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MONTH AS CHARACTER INIT "MONTH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the mouse keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MOUSE AS CHARACTER INIT "MOUSE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the mouse-pointer keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MOUSEPOINTER AS CHARACTER INIT "MOUSEPOINTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the mpe keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MPE AS CHARACTER INIT "MPE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the mtime keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MTIME AS CHARACTER INIT "MTIME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the multiple keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MULTIPLE AS CHARACTER INIT "MULTIPLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the multiple-key keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MULTIPLEKEY AS CHARACTER INIT "MULTIPLEKEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the must-exist keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_MUSTEXIST AS CHARACTER INIT "MUSTEXIST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the namespace-prefix keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NAMESPACEPREFIX AS CHARACTER INIT "NAMESPACEPREFIX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the namespace-uri keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NAMESPACEURI AS CHARACTER INIT "NAMESPACEURI":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the native keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NATIVE AS CHARACTER INIT "NATIVE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ne keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NE AS CHARACTER INIT "NE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the nested keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NESTED AS CHARACTER INIT "NESTED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the new keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NEW AS CHARACTER INIT "NEW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the new-instance keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NEWINSTANCE AS CHARACTER INIT "NEWINSTANCE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the next keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NEXT AS CHARACTER INIT "NEXT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the next-prompt keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NEXTPROMPT AS CHARACTER INIT "NEXTPROMPT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the next-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NEXTVALUE AS CHARACTER INIT "NEXTVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NO AS CHARACTER INIT "NO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-apply keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOAPPLY AS CHARACTER INIT "NOAPPLY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-array-message keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOARRAYMESSAGE AS CHARACTER INIT "NOARRAYMESSAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-assign keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOASSIGN AS CHARACTER INIT "NOASSIGN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-attr-list keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOATTRLIST AS CHARACTER INIT "NOATTRLIST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-attr-space keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOATTRSPACE AS CHARACTER INIT "NOATTRSPACE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-auto-validate keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOAUTOVALIDATE AS CHARACTER INIT "NOAUTOVALIDATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-bind-where keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOBINDWHERE AS CHARACTER INIT "NOBINDWHERE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-box keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOBOX AS CHARACTER INIT "NOBOX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-column-scrolling keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOCOLUMNSCROLLING AS CHARACTER INIT "NOCOLUMNSCROLLING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-console keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOCONSOLE AS CHARACTER INIT "NOCONSOLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-convert keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOCONVERT AS CHARACTER INIT "NOCONVERT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-convert-3d-colors keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOCONVERT3DCOLORS AS CHARACTER INIT "NOCONVERT3DCOLORS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-current-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOCURRENTVALUE AS CHARACTER INIT "NOCURRENTVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-debug keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NODEBUG AS CHARACTER INIT "NODEBUG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-drag keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NODRAG AS CHARACTER INIT "NODRAG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-echo keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOECHO AS CHARACTER INIT "NOECHO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-empty-space keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOEMPTYSPACE AS CHARACTER INIT "NOEMPTYSPACE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-error keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOERROR_KW AS CHARACTER INIT "NOERROR_KW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-fill keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOFILL AS CHARACTER INIT "NOFILL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-focus keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOFOCUS AS CHARACTER INIT "NOFOCUS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-help keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOHELP AS CHARACTER INIT "NOHELP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-hide keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOHIDE AS CHARACTER INIT "NOHIDE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-index-hint keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOINDEXHINT AS CHARACTER INIT "NOINDEXHINT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-inherit-bgcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOINHERITBGCOLOR AS CHARACTER INIT "NOINHERITBGCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-inherit-fgcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOINHERITFGCOLOR AS CHARACTER INIT "NOINHERITFGCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-join-by-sqldb keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOJOINBYSQLDB AS CHARACTER INIT "NOJOINBYSQLDB":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-labels keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOLABELS AS CHARACTER INIT "NOLABELS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-lobs keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOLOBS AS CHARACTER INIT "NOLOBS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-lock keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOLOCK AS CHARACTER INIT "NOLOCK":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-lookahead keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOLOOKAHEAD AS CHARACTER INIT "NOLOOKAHEAD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-map keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOMAP AS CHARACTER INIT "NOMAP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-message keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOMESSAGE AS CHARACTER INIT "NOMESSAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the none keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NONE AS CHARACTER INIT "NONE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-pause keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOPAUSE AS CHARACTER INIT "NOPAUSE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-prefetch keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOPREFETCH AS CHARACTER INIT "NOPREFETCH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-return-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NORETURNVALUE AS CHARACTER INIT "NORETURNVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the normal keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NORMAL AS CHARACTER INIT "NORMAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the normalize keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NORMALIZE AS CHARACTER INIT "NORMALIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-row-markers keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOROWMARKERS AS CHARACTER INIT "NOROWMARKERS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-scrollbar-vertical keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOSCROLLBARVERTICAL AS CHARACTER INIT "NOSCROLLBARVERTICAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-separate-connection keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOSEPARATECONNECTION AS CHARACTER INIT "NOSEPARATECONNECTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-separators keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOSEPARATORS AS CHARACTER INIT "NOSEPARATORS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the not keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOT AS CHARACTER INIT "NOT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-tab-stop keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOTABSTOP AS CHARACTER INIT "NOTABSTOP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the not-active keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOTACTIVE AS CHARACTER INIT "NOTACTIVE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-underline keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOUNDERLINE AS CHARACTER INIT "NOUNDERLINE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-undo keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOUNDO AS CHARACTER INIT "NOUNDO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-validate keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOVALIDATE AS CHARACTER INIT "NOVALIDATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the now keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOW AS CHARACTER INIT "NOW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-wait keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOWAIT AS CHARACTER INIT "NOWAIT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the no-word-wrap keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NOWORDWRAP AS CHARACTER INIT "NOWORDWRAP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the null keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NULL_KW AS CHARACTER INIT "NULL_KW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the num-aliases keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NUMALIASES AS CHARACTER INIT "NUMALIASES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the num-copies keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NUMCOPIES AS CHARACTER INIT "NUMCOPIES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the num-dbs keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NUMDBS AS CHARACTER INIT "NUMDBS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the num-entries keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NUMENTRIES AS CHARACTER INIT "NUMENTRIES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the numeric keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NUMERIC AS CHARACTER INIT "NUMERIC":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the num-results keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_NUMRESULTS AS CHARACTER INIT "NUMRESULTS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the object keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OBJECT AS CHARACTER INIT "OBJECT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the octet-length keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OCTETLENGTH AS CHARACTER INIT "OCTETLENGTH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the of keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OF AS CHARACTER INIT "OF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the off keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OFF AS CHARACTER INIT "OFF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ok keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OK AS CHARACTER INIT "OK":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ok-cancel keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OKCANCEL AS CHARACTER INIT "OKCANCEL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the old keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OLD AS CHARACTER INIT "OLD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the on keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ON AS CHARACTER INIT "ON":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the only keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ONLY AS CHARACTER INIT "ONLY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the open keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OPEN AS CHARACTER INIT "OPEN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the opsys keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OPSYS AS CHARACTER INIT "OPSYS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the option keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OPTION AS CHARACTER INIT "OPTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the or keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OR AS CHARACTER INIT "OR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the order keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ORDER AS CHARACTER INIT "ORDER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ordered-join keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ORDEREDJOIN AS CHARACTER INIT "ORDEREDJOIN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ordinal keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ORDINAL AS CHARACTER INIT "ORDINAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os2 keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OS2 AS CHARACTER INIT "OS2":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os400 keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OS400 AS CHARACTER INIT "OS400":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os-append keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OSAPPEND AS CHARACTER INIT "OSAPPEND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os-command keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OSCOMMAND AS CHARACTER INIT "OSCOMMAND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os-copy keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OSCOPY AS CHARACTER INIT "OSCOPY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os-create-dir keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OSCREATEDIR AS CHARACTER INIT "OSCREATEDIR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os-delete keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OSDELETE AS CHARACTER INIT "OSDELETE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os-dir keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OSDIR AS CHARACTER INIT "OSDIR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os-drives keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OSDRIVES AS CHARACTER INIT "OSDRIVES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os-error keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OSERROR AS CHARACTER INIT "OSERROR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os-getenv keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OSGETENV AS CHARACTER INIT "OSGETENV":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the os-rename keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OSRENAME AS CHARACTER INIT "OSRENAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the otherwise keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OTHERWISE AS CHARACTER INIT "OTHERWISE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the outer keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OUTER AS CHARACTER INIT "OUTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the outer-join keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OUTERJOIN AS CHARACTER INIT "OUTERJOIN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the output keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OUTPUT AS CHARACTER INIT "OUTPUT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the overlay keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OVERLAY AS CHARACTER INIT "OVERLAY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the override keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_OVERRIDE AS CHARACTER INIT "OVERRIDE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the page keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PAGE AS CHARACTER INIT "PAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the page-bottom keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PAGEBOTTOM AS CHARACTER INIT "PAGEBOTTOM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the paged keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PAGED AS CHARACTER INIT "PAGED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the page-number keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PAGENUMBER AS CHARACTER INIT "PAGENUMBER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the page-size keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PAGESIZE_KW AS CHARACTER INIT "PAGESIZE_KW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the page-top keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PAGETOP AS CHARACTER INIT "PAGETOP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the page-width keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PAGEWIDTH AS CHARACTER INIT "PAGEWIDTH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the parameter keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PARAMETER AS CHARACTER INIT "PARAMETER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the parent keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PARENT AS CHARACTER INIT "PARENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the partial-key keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PARTIALKEY AS CHARACTER INIT "PARTIALKEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the pascal keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PASCAL_KW AS CHARACTER INIT "PASCAL_KW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the password-field keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PASSWORDFIELD AS CHARACTER INIT "PASSWORDFIELD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the pause keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PAUSE AS CHARACTER INIT "PAUSE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the pbe-hash-algorithm keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PBEHASHALGORITHM AS CHARACTER INIT "PBEHASHALGORITHM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the pbe-key-rounds keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PBEKEYROUNDS AS CHARACTER INIT "PBEKEYROUNDS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the pdbname keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PDBNAME AS CHARACTER INIT "PDBNAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the performance keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PERFORMANCE AS CHARACTER INIT "PERFORMANCE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the persistent keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PERSISTENT AS CHARACTER INIT "PERSISTENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the pfcolor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PFCOLOR AS CHARACTER INIT "PFCOLOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the pinnable keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PINNABLE AS CHARACTER INIT "PINNABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the portrait keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PORTRAIT AS CHARACTER INIT "PORTRAIT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the position keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_POSITION AS CHARACTER INIT "POSITION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the precision keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PRECISION AS CHARACTER INIT "PRECISION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the prefer-dataset keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PREFERDATASET AS CHARACTER INIT "PREFERDATASET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the preprocess keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PREPROCESS AS CHARACTER INIT "PREPROCESS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the preselect keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PRESELECT AS CHARACTER INIT "PRESELECT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the prev keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PREV AS CHARACTER INIT "PREV":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the primary keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PRIMARY AS CHARACTER INIT "PRIMARY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the printer keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PRINTER AS CHARACTER INIT "PRINTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the printer-setup keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PRINTERSETUP AS CHARACTER INIT "PRINTERSETUP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the private keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PRIVATE AS CHARACTER INIT "PRIVATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the privileges keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PRIVILEGES AS CHARACTER INIT "PRIVILEGES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the procedure keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROCEDURE AS CHARACTER INIT "PROCEDURE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the procedure-call-type keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROCEDURECALLTYPE AS CHARACTER INIT "PROCEDURECALLTYPE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the process keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROCESS AS CHARACTER INIT "PROCESS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the proc-handle keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROCHANDLE AS CHARACTER INIT "PROCHANDLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the proc-status keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROCSTATUS AS CHARACTER INIT "PROCSTATUS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the proc-text keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROCTEXT AS CHARACTER INIT "PROCTEXT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the proc-text-buffer keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROCTEXTBUFFER AS CHARACTER INIT "PROCTEXTBUFFER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the profiler keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROFILER AS CHARACTER INIT "PROFILER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the program-name keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROGRAMNAME AS CHARACTER INIT "PROGRAMNAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the progress keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROGRESS AS CHARACTER INIT "PROGRESS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the prompt keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROMPT AS CHARACTER INIT "PROMPT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the prompt-for keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROMPTFOR AS CHARACTER INIT "PROMPTFOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the promsgs keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROMSGS AS CHARACTER INIT "PROMSGS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the propath keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROPATH AS CHARACTER INIT "PROPATH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the property keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROPERTY AS CHARACTER INIT "PROPERTY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the protected keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROTECTED AS CHARACTER INIT "PROTECTED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the proversion keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PROVERSION AS CHARACTER INIT "PROVERSION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the public keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUBLIC AS CHARACTER INIT "PUBLIC":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the publish keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUBLISH AS CHARACTER INIT "PUBLISH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUT AS CHARACTER INIT "PUT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-bits keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTBITS AS CHARACTER INIT "PUTBITS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-byte keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTBYTE AS CHARACTER INIT "PUTBYTE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-bytes keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTBYTES AS CHARACTER INIT "PUTBYTES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-double keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTDOUBLE AS CHARACTER INIT "PUTDOUBLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-float keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTFLOAT AS CHARACTER INIT "PUTFLOAT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-int64 keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTINT64 AS CHARACTER INIT "PUTINT64":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-key-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTKEYVALUE AS CHARACTER INIT "PUTKEYVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-long keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTLONG AS CHARACTER INIT "PUTLONG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-short keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTSHORT AS CHARACTER INIT "PUTSHORT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-string keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTSTRING AS CHARACTER INIT "PUTSTRING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-unsigned-long keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTUNSIGNEDLONG AS CHARACTER INIT "PUTUNSIGNEDLONG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the put-unsigned-short keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_PUTUNSIGNEDSHORT AS CHARACTER INIT "PUTUNSIGNEDSHORT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the query keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_QUERY AS CHARACTER INIT "QUERY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the query-close keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_QUERYCLOSE AS CHARACTER INIT "QUERYCLOSE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the query-off-end keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_QUERYOFFEND AS CHARACTER INIT "QUERYOFFEND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the query-tuning keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_QUERYTUNING AS CHARACTER INIT "QUERYTUNING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the question keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_QUESTION AS CHARACTER INIT "QUESTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the quit keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_QUIT AS CHARACTER INIT "QUIT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the quoter keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_QUOTER AS CHARACTER INIT "QUOTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the radio-buttons keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RADIOBUTTONS AS CHARACTER INIT "RADIOBUTTONS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the radio-set keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RADIOSET AS CHARACTER INIT "RADIOSET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the random keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RANDOM AS CHARACTER INIT "RANDOM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the raw keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RAW AS CHARACTER INIT "RAW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the raw-transfer keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RAWTRANSFER AS CHARACTER INIT "RAWTRANSFER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the rcode-information keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RCODEINFORMATION AS CHARACTER INIT "RCODEINFORMATION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the read keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_READ AS CHARACTER INIT "READ":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the read-available keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_READAVAILABLE AS CHARACTER INIT "READAVAILABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the read-exact-num keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_READEXACTNUM AS CHARACTER INIT "READEXACTNUM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the readkey keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_READKEY AS CHARACTER INIT "READKEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the read-only keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_READONLY AS CHARACTER INIT "READONLY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the real keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REAL AS CHARACTER INIT "REAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the recid keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RECID AS CHARACTER INIT "RECID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the record-length keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RECORDLENGTH AS CHARACTER INIT "RECORDLENGTH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the rectangle keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RECTANGLE AS CHARACTER INIT "RECTANGLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the recursive keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RECURSIVE AS CHARACTER INIT "RECURSIVE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the reference-only keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REFERENCEONLY AS CHARACTER INIT "REFERENCEONLY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the rejected keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REJECTED AS CHARACTER INIT "REJECTED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the relation-fields keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RELATIONFIELDS AS CHARACTER INIT "RELATIONFIELDS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the release keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RELEASE AS CHARACTER INIT "RELEASE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the repeat keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REPEAT AS CHARACTER INIT "REPEAT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the replace keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REPLACE AS CHARACTER INIT "REPLACE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the replication-create keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REPLICATIONCREATE AS CHARACTER INIT "REPLICATIONCREATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the replication-delete keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REPLICATIONDELETE AS CHARACTER INIT "REPLICATIONDELETE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the replication-write keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REPLICATIONWRITE AS CHARACTER INIT "REPLICATIONWRITE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the reposition keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REPOSITION AS CHARACTER INIT "REPOSITION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the reposition-backward keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REPOSITIONBACKWARD AS CHARACTER INIT "REPOSITIONBACKWARD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the reposition-forward keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REPOSITIONFORWARD AS CHARACTER INIT "REPOSITIONFORWARD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the reposition-mode keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REPOSITIONMODE AS CHARACTER INIT "REPOSITIONMODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the reposition-to-row keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REPOSITIONTOROW AS CHARACTER INIT "REPOSITIONTOROW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the reposition-to-rowid keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REPOSITIONTOROWID AS CHARACTER INIT "REPOSITIONTOROWID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the request keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REQUEST AS CHARACTER INIT "REQUEST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the restart-row keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RESTARTROW AS CHARACTER INIT "RESTARTROW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the result keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RESULT AS CHARACTER INIT "RESULT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the retain keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RETAIN AS CHARACTER INIT "RETAIN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the retain-shape keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RETAINSHAPE AS CHARACTER INIT "RETAINSHAPE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the retry keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RETRY AS CHARACTER INIT "RETRY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the retry-cancel keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RETRYCANCEL AS CHARACTER INIT "RETRYCANCEL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the return keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RETURN AS CHARACTER INIT "RETURN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the returns keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RETURNS AS CHARACTER INIT "RETURNS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the return-to-start-dir keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RETURNTOSTARTDIR AS CHARACTER INIT "RETURNTOSTARTDIR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the return-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RETURNVALUE AS CHARACTER INIT "RETURNVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the reverse-from keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REVERSEFROM AS CHARACTER INIT "REVERSEFROM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the revert keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REVERT AS CHARACTER INIT "REVERT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the revoke keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_REVOKE AS CHARACTER INIT "REVOKE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the rgb-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RGBVALUE AS CHARACTER INIT "RGBVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the right keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RIGHT AS CHARACTER INIT "RIGHT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the right-aligned keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RIGHTALIGNED AS CHARACTER INIT "RIGHTALIGNED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the right-trim keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RIGHTTRIM AS CHARACTER INIT "RIGHTTRIM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the r-index keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RINDEX AS CHARACTER INIT "RINDEX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the round keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROUND AS CHARACTER INIT "ROUND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the rounded keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROUNDED AS CHARACTER INIT "ROUNDED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the routine-level keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROUTINELEVEL AS CHARACTER INIT "ROUTINELEVEL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the row keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROW AS CHARACTER INIT "ROW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the row-created keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROWCREATED AS CHARACTER INIT "ROWCREATED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the row-deleted keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROWDELETED AS CHARACTER INIT "ROWDELETED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the row-height keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROWHEIGHTCHARS AS CHARACTER INIT "ROWHEIGHTCHARS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the row-height-pixels keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROWHEIGHTPIXELS AS CHARACTER INIT "ROWHEIGHTPIXELS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the rowid keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROWID AS CHARACTER INIT "ROWID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the row-modified keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROWMODIFIED AS CHARACTER INIT "ROWMODIFIED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the row-of keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROWOF AS CHARACTER INIT "ROWOF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the row-state keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROWSTATE AS CHARACTER INIT "ROWSTATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the row-unmodified keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_ROWUNMODIFIED AS CHARACTER INIT "ROWUNMODIFIED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the rule keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RULE AS CHARACTER INIT "RULE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the run keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RUN AS CHARACTER INIT "RUN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the run-procedure keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_RUNPROCEDURE AS CHARACTER INIT "RUNPROCEDURE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the save keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAVE AS CHARACTER INIT "SAVE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the save-as keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAVEAS AS CHARACTER INIT "SAVEAS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the savecache keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAVECACHE AS CHARACTER INIT "SAVECACHE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-attributes keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXATTRIBUTES AS CHARACTER INIT "SAXATTRIBUTES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-complete keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXCOMPLETE AS CHARACTER INIT "SAXCOMPLETE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-parser-error keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXPARSERERROR AS CHARACTER INIT "SAXPARSERERROR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-reader keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXREADER AS CHARACTER INIT "SAXREADER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-running keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXRUNNING AS CHARACTER INIT "SAXRUNNING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-uninitialized keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXUNINITIALIZED AS CHARACTER INIT "SAXUNINITIALIZED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-write-begin keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXWRITEBEGIN AS CHARACTER INIT "SAXWRITEBEGIN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-write-complete keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXWRITECOMPLETE AS CHARACTER INIT "SAXWRITECOMPLETE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-write-content keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXWRITECONTENT AS CHARACTER INIT "SAXWRITECONTENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-write-element keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXWRITEELEMENT AS CHARACTER INIT "SAXWRITEELEMENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-write-error keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXWRITEERROR AS CHARACTER INIT "SAXWRITEERROR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-write-idle keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXWRITEIDLE AS CHARACTER INIT "SAXWRITEIDLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-writer keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXWRITER AS CHARACTER INIT "SAXWRITER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sax-write-tag keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SAXWRITETAG AS CHARACTER INIT "SAXWRITETAG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the schema keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SCHEMA AS CHARACTER INIT "SCHEMA":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the screen keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SCREEN AS CHARACTER INIT "SCREEN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the screen-io keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SCREENIO AS CHARACTER INIT "SCREENIO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the screen-lines keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SCREENLINES AS CHARACTER INIT "SCREENLINES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the scroll keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SCROLL AS CHARACTER INIT "SCROLL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the scrollable keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SCROLLABLE AS CHARACTER INIT "SCROLLABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the scrollbar-horizontal keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SCROLLBARHORIZONTAL AS CHARACTER INIT "SCROLLBARHORIZONTAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the scrollbar-vertical keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SCROLLBARVERTICAL AS CHARACTER INIT "SCROLLBARVERTICAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the scrolling keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SCROLLING AS CHARACTER INIT "SCROLLING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sdbname keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SDBNAME AS CHARACTER INIT "SDBNAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the search keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SEARCH AS CHARACTER INIT "SEARCH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the search-self keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SEARCHSELF AS CHARACTER INIT "SEARCHSELF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the search-target keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SEARCHTARGET AS CHARACTER INIT "SEARCHTARGET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the section keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SECTION AS CHARACTER INIT "SECTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the security-policy keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SECURITYPOLICY AS CHARACTER INIT "SECURITYPOLICY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the seek keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SEEK AS CHARACTER INIT "SEEK":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the select keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SELECT AS CHARACTER INIT "SELECT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the selection keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SELECTION AS CHARACTER INIT "SELECTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the selection-list keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SELECTIONLIST AS CHARACTER INIT "SELECTIONLIST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the self keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SELF AS CHARACTER INIT "SELF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the send keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SEND AS CHARACTER INIT "SEND":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the send-sql-statement keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SENDSQLSTATEMENT AS CHARACTER INIT "SENDSQLSTATEMENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the separate-connection keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SEPARATECONNECTION AS CHARACTER INIT "SEPARATECONNECTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the separators keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SEPARATORS AS CHARACTER INIT "SEPARATORS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the serialize-hidden keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SERIALIZEHIDDEN AS CHARACTER INIT "SERIALIZEHIDDEN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the serialize-name keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SERIALIZENAME AS CHARACTER INIT "SERIALIZENAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the server keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SERVER AS CHARACTER INIT "SERVER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the server-socket keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SERVERSOCKET AS CHARACTER INIT "SERVERSOCKET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the session keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SESSION AS CHARACTER INIT "SESSION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the set keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SET AS CHARACTER INIT "SET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the set-attr-call-type keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SETATTRCALLTYPE AS CHARACTER INIT "SETATTRCALLTYPE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the set-byte-order keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SETBYTEORDER AS CHARACTER INIT "SETBYTEORDER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the set-contents keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SETCONTENTS AS CHARACTER INIT "SETCONTENTS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the set-current-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SETCURRENTVALUE AS CHARACTER INIT "SETCURRENTVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the set-db-client keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SETDBCLIENT AS CHARACTER INIT "SETDBCLIENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the set-pointer-value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SETPOINTERVALUE AS CHARACTER INIT "SETPOINTERVALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the set-size keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SETSIZE AS CHARACTER INIT "SETSIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the setuserid keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SETUSERID AS CHARACTER INIT "SETUSERID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sha1-digest keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SHA1DIGEST AS CHARACTER INIT "SHA1DIGEST":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the shared keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SHARED AS CHARACTER INIT "SHARED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the share-lock keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SHARELOCK AS CHARACTER INIT "SHARELOCK":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the short keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SHORT AS CHARACTER INIT "SHORT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the show-stats keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SHOWSTATS AS CHARACTER INIT "SHOWSTATS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the side-labels keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SIDELABELS AS CHARACTER INIT "SIDELABELS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the signature keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SIGNATURE AS CHARACTER INIT "SIGNATURE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the silent keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SILENT AS CHARACTER INIT "SILENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the simple keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SIMPLE AS CHARACTER INIT "SIMPLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the single keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SINGLE AS CHARACTER INIT "SINGLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the size keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SIZE AS CHARACTER INIT "SIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the size-chars keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SIZECHARS AS CHARACTER INIT "SIZECHARS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the size-pixels keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SIZEPIXELS AS CHARACTER INIT "SIZEPIXELS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the skip keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SKIP AS CHARACTER INIT "SKIP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the skip-deleted-record keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SKIPDELETEDRECORD AS CHARACTER INIT "SKIPDELETEDRECORD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the slider keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SLIDER AS CHARACTER INIT "SLIDER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the smallint keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SMALLINT AS CHARACTER INIT "SMALLINT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the soap-header keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SOAPHEADER AS CHARACTER INIT "SOAPHEADER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the soap-header-entryref keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SOAPHEADERENTRYREF AS CHARACTER INIT "SOAPHEADERENTRYREF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the socket keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SOCKET AS CHARACTER INIT "SOCKET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the some keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SOME AS CHARACTER INIT "SOME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sort keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SORT AS CHARACTER INIT "SORT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the source keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SOURCE AS CHARACTER INIT "SOURCE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the source-procedure keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SOURCEPROCEDURE AS CHARACTER INIT "SOURCEPROCEDURE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the space keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SPACE AS CHARACTER INIT "SPACE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sql keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SQL AS CHARACTER INIT "SQL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sqrt keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SQRT AS CHARACTER INIT "SQRT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ssl-server-name keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SSLSERVERNAME AS CHARACTER INIT "SSLSERVERNAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the start keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_START AS CHARACTER INIT "START":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the starting keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STARTING AS CHARACTER INIT "STARTING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the start-move keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STARTMOVE AS CHARACTER INIT "STARTMOVE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the start-resize keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STARTRESIZE AS CHARACTER INIT "STARTRESIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the start-row-resize keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STARTROWRESIZE AS CHARACTER INIT "STARTROWRESIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the static keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STATIC AS CHARACTER INIT "STATIC":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the status keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STATUS AS CHARACTER INIT "STATUS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the status-bar keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STATUSBAR AS CHARACTER INIT "STATUSBAR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the stdcall keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STDCALL_KW AS CHARACTER INIT "STDCALL_KW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the stomp-detection keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STOMPDETECTION AS CHARACTER INIT "STOMPDETECTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the stomp-frequency keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STOMPFREQUENCY AS CHARACTER INIT "STOMPFREQUENCY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the stop keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STOP AS CHARACTER INIT "STOP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the stop-after keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STOPAFTER AS CHARACTER INIT "STOPAFTER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the stored-procedure keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STOREDPROCEDURE AS CHARACTER INIT "STOREDPROCEDURE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the stream keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STREAM AS CHARACTER INIT "STREAM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the stream-handle keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STREAMHANDLE AS CHARACTER INIT "STREAMHANDLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the stream-io keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STREAMIO AS CHARACTER INIT "STREAMIO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the stretch-to-fit keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STRETCHTOFIT AS CHARACTER INIT "STRETCHTOFIT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the string keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STRING AS CHARACTER INIT "STRING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the string-xref keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_STRINGXREF AS CHARACTER INIT "STRINGXREF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sub-average keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUBAVERAGE AS CHARACTER INIT "SUBAVERAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sub-count keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUBCOUNT AS CHARACTER INIT "SUBCOUNT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sub-maximum keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUBMAXIMUM AS CHARACTER INIT "SUBMAXIMUM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sub-menu keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUBMENU AS CHARACTER INIT "SUBMENU":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sub-menu-help keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUBMENUHELP AS CHARACTER INIT "SUBMENUHELP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sub-minimum keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUBMINIMUM AS CHARACTER INIT "SUBMINIMUM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the subscribe keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUBSCRIBE AS CHARACTER INIT "SUBSCRIBE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the substitute keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUBSTITUTE AS CHARACTER INIT "SUBSTITUTE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the substring keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUBSTRING AS CHARACTER INIT "SUBSTRING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sub-total keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUBTOTAL AS CHARACTER INIT "SUBTOTAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the sum keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUM AS CHARACTER INIT "SUM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the summary keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUMMARY AS CHARACTER INIT "SUMMARY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the super keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SUPER AS CHARACTER INIT "SUPER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the symmetric-encryption-algorithm keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SYMMETRICENCRYPTIONALGORITHM AS CHARACTER INIT "SYMMETRICENCRYPTIONALGORITHM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the symmetric-encryption-iv keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SYMMETRICENCRYPTIONIV AS CHARACTER INIT "SYMMETRICENCRYPTIONIV":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the symmetric-encryption-key keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SYMMETRICENCRYPTIONKEY AS CHARACTER INIT "SYMMETRICENCRYPTIONKEY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the symmetric-support keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SYMMETRICSUPPORT AS CHARACTER INIT "SYMMETRICSUPPORT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the system-dialog keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SYSTEMDIALOG AS CHARACTER INIT "SYSTEMDIALOG":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the system-help keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_SYSTEMHELP AS CHARACTER INIT "SYSTEMHELP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the table keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TABLE AS CHARACTER INIT "TABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the table-handle keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TABLEHANDLE AS CHARACTER INIT "TABLEHANDLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the table-number keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TABLENUMBER AS CHARACTER INIT "TABLENUMBER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the target keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TARGET AS CHARACTER INIT "TARGET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the target-procedure keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TARGETPROCEDURE AS CHARACTER INIT "TARGETPROCEDURE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the temp-table keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TEMPTABLE AS CHARACTER INIT "TEMPTABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the term keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TERMINAL AS CHARACTER INIT "TERMINAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the terminate keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TERMINATE AS CHARACTER INIT "TERMINATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the text keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TEXT AS CHARACTER INIT "TEXT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the text-cursor keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TEXTCURSOR AS CHARACTER INIT "TEXTCURSOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the text-seg-growth keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TEXTSEGGROW AS CHARACTER INIT "TEXTSEGGROW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the then keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_THEN AS CHARACTER INIT "THEN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the this-object keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_THISOBJECT AS CHARACTER INIT "THISOBJECT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the this-procedure keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_THISPROCEDURE AS CHARACTER INIT "THISPROCEDURE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the three-d keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_THREED AS CHARACTER INIT "THREED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the through keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_THROUGH AS CHARACTER INIT "THROUGH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the throw keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_THROW AS CHARACTER INIT "THROW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the tic-marks keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TICMARKS AS CHARACTER INIT "TICMARKS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the time keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TIME AS CHARACTER INIT "TIME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the timestamp keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TIMESTAMP AS CHARACTER INIT "TIMESTAMP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the timezone keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TIMEZONE AS CHARACTER INIT "TIMEZONE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the title keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TITLE AS CHARACTER INIT "TITLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the to keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TO AS CHARACTER INIT "TO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the today keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TODAY AS CHARACTER INIT "TODAY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the toggle-box keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TOGGLEBOX AS CHARACTER INIT "TOGGLEBOX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the tool-bar keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TOOLBAR AS CHARACTER INIT "TOOLBAR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the tooltip keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TOOLTIP AS CHARACTER INIT "TOOLTIP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the top keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TOP AS CHARACTER INIT "TOP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the topic keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TOPIC AS CHARACTER INIT "TOPIC":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the top-nav-query keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TOPNAVQUERY AS CHARACTER INIT "TOPNAVQUERY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the top-only keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TOPONLY AS CHARACTER INIT "TOPONLY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the to-rowid keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TOROWID AS CHARACTER INIT "TOROWID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the total keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TOTAL AS CHARACTER INIT "TOTAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the trailing keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TRAILING AS CHARACTER INIT "TRAILING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the trans keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TRANSACTION AS CHARACTER INIT "TRANSACTION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the transaction-mode keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TRANSACTIONMODE AS CHARACTER INIT "TRANSACTIONMODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the trans-init-procedure keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TRANSINITPROCEDURE AS CHARACTER INIT "TRANSINITPROCEDURE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the transparent keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TRANSPARENT AS CHARACTER INIT "TRANSPARENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the trigger keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TRIGGER AS CHARACTER INIT "TRIGGER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the triggers keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TRIGGERS AS CHARACTER INIT "TRIGGERS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the trim keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TRIM AS CHARACTER INIT "TRIM":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the true keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TRUE_KW AS CHARACTER INIT "TRUE_KW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the truncate keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TRUNCATE AS CHARACTER INIT "TRUNCATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the ttcodepage keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TTCODEPAGE AS CHARACTER INIT "TTCODEPAGE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the type-of keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_TYPEOF AS CHARACTER INIT "TYPEOF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the unbox keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNBOX AS CHARACTER INIT "UNBOX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the unbuffered keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNBUFFERED AS CHARACTER INIT "UNBUFFERED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the underline keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNDERLINE AS CHARACTER INIT "UNDERLINE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the undo keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNDO AS CHARACTER INIT "UNDO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the unformatted keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNFORMATTED AS CHARACTER INIT "UNFORMATTED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the union keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNION AS CHARACTER INIT "UNION":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the unique keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNIQUE AS CHARACTER INIT "UNIQUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the unique-match keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNIQUEMATCH AS CHARACTER INIT "UNIQUEMATCH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the unix keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNIX AS CHARACTER INIT "UNIX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the unless-hidden keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNLESSHIDDEN AS CHARACTER INIT "UNLESSHIDDEN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the unload keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNLOAD AS CHARACTER INIT "UNLOAD":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the unsigned-byte keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNSIGNEDBYTE AS CHARACTER INIT "UNSIGNEDBYTE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the unsigned-short keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNSIGNEDSHORT AS CHARACTER INIT "UNSIGNEDSHORT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the unsubscribe keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UNSUBSCRIBE AS CHARACTER INIT "UNSUBSCRIBE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the up keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UP AS CHARACTER INIT "UP":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the update keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_UPDATE AS CHARACTER INIT "UPDATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the url-decode keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_URLDECODE AS CHARACTER INIT "URLDECODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the url-encode keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_URLENCODE AS CHARACTER INIT "URLENCODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the use keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_USE AS CHARACTER INIT "USE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the use-dict-exps keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_USEDICTEXPS AS CHARACTER INIT "USEDICTEXPS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the use-filename keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_USEFILENAME AS CHARACTER INIT "USEFILENAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the use-index keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_USEINDEX AS CHARACTER INIT "USEINDEX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the user keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_USER AS CHARACTER INIT "USER":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the use-revvideo keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_USEREVVIDEO AS CHARACTER INIT "USEREVVIDEO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the userid keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_USERID AS CHARACTER INIT "USERID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the use-text keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_USETEXT AS CHARACTER INIT "USETEXT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the use-underline keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_USEUNDERLINE AS CHARACTER INIT "USEUNDERLINE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the use-widget-pool keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_USEWIDGETPOOL AS CHARACTER INIT "USEWIDGETPOOL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the using keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_USING AS CHARACTER INIT "USING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the v6frame keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_V6FRAME AS CHARACTER INIT "V6FRAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the validate keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VALIDATE AS CHARACTER INIT "VALIDATE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the valid-event keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VALIDEVENT AS CHARACTER INIT "VALIDEVENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the valid-handle keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VALIDHANDLE AS CHARACTER INIT "VALIDHANDLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the valid-object keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VALIDOBJECT AS CHARACTER INIT "VALIDOBJECT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the value keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VALUE AS CHARACTER INIT "VALUE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the value-changed keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VALUECHANGED AS CHARACTER INIT "VALUECHANGED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the values keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VALUES AS CHARACTER INIT "VALUES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the variable keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VARIABLE AS CHARACTER INIT "VARIABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the verbose keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VERBOSE AS CHARACTER INIT "VERBOSE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the vertical keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VERTICAL AS CHARACTER INIT "VERTICAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the view keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VIEW AS CHARACTER INIT "VIEW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the view-as keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VIEWAS AS CHARACTER INIT "VIEWAS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the vms keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VMS AS CHARACTER INIT "VMS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the void keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_VOID AS CHARACTER INIT "VOID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the wait keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WAIT AS CHARACTER INIT "WAIT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the wait-for keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WAITFOR AS CHARACTER INIT "WAITFOR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the warning keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WARNING AS CHARACTER INIT "WARNING":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the web-context keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WEBCONTEXT AS CHARACTER INIT "WEBCONTEXT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the weekday keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WEEKDAY AS CHARACTER INIT "WEEKDAY":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the when keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WHEN AS CHARACTER INIT "WHEN":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the where keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WHERE AS CHARACTER INIT "WHERE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the while keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WHILE AS CHARACTER INIT "WHILE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the widget keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WIDGET AS CHARACTER INIT "WIDGET":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the widget-handle keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WIDGETHANDLE AS CHARACTER INIT "WIDGETHANDLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the widget-id keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WIDGETID AS CHARACTER INIT "WIDGETID":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the widget-pool keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WIDGETPOOL AS CHARACTER INIT "WIDGETPOOL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the width keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WIDTH AS CHARACTER INIT "WIDTH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the width-chars keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WIDTHCHARS AS CHARACTER INIT "WIDTHCHARS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the width-pixels keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WIDTHPIXELS AS CHARACTER INIT "WIDTHPIXELS":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the window keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WINDOW AS CHARACTER INIT "WINDOW":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the window-delayed-minimize keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WINDOWDELAYEDMINIMIZE AS CHARACTER INIT "WINDOWDELAYEDMINIMIZE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the window-maximized keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WINDOWMAXIMIZED AS CHARACTER INIT "WINDOWMAXIMIZED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the window-minimized keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WINDOWMINIMIZED AS CHARACTER INIT "WINDOWMINIMIZED":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the window-name keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WINDOWNAME AS CHARACTER INIT "WINDOWNAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the window-normal keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WINDOWNORMAL AS CHARACTER INIT "WINDOWNORMAL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the with keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WITH AS CHARACTER INIT "WITH":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the word-index keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WORDINDEX AS CHARACTER INIT "WORDINDEX":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the work-table keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WORKTABLE AS CHARACTER INIT "WORKTABLE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the write keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_WRITE AS CHARACTER INIT "WRITE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the x keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_X AS CHARACTER INIT "X":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the xcode keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_XCODE AS CHARACTER INIT "XCODE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the x-document keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_XDOCUMENT AS CHARACTER INIT "XDOCUMENT":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the xml-data-type keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_XMLDATATYPE AS CHARACTER INIT "XMLDATATYPE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the xml-node-name keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_XMLNODENAME AS CHARACTER INIT "XMLNODENAME":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the xml-node-type keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_XMLNODETYPE AS CHARACTER INIT "XMLNODETYPE":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the x-noderef keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_XNODEREF AS CHARACTER INIT "XNODEREF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the x-of keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_XOF AS CHARACTER INIT "XOF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the xref keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_XREF AS CHARACTER INIT "XREF":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the xref-xml keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_XREFXML AS CHARACTER INIT "XREFXML":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the y keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_Y AS CHARACTER INIT "Y":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the year keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_YEAR AS CHARACTER INIT "YEAR":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the yes keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_YES AS CHARACTER INIT "YES":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the yes-no keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_YESNO AS CHARACTER INIT "YESNO":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the yes-no-cancel keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_YESNOCANCEL AS CHARACTER INIT "YESNOCANCEL":U NO-UNDO GET. 

    /*------------------------------------------------------------------------------
        Purpose: Node type for the y-of keyword 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY N_YOF AS CHARACTER INIT "YOF":U NO-UNDO GET. 


    /*------------------------------------------------------------------------------
        Purpose: Private default constructor for the ProparseNodeTypes class 
        Notes:   Disallow instance creation                                                                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE ProparseNodeTypes ():
        SUPER ().
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the ABL Keyword for a given proparse Node 
        Notes:   
        @param pcNodeType The Proparse Node type
        @return The ABL keyword
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER AblKeyword (pcNodeType AS CHARACTER):
        
        IF NOT CAN-FIND (FIRST ttNodeTypes) THEN 
            ProparseNodeTypes:ImportNodeTypes () .
        
        FIND ttNodeTypes WHERE ttNodeTypes.NodeType = pcNodeType .
        
        RETURN UPPER (ttNodeTypes.AblKeyWord) .
        
        CATCH err AS Progress.Lang.Error:
            UNDO, THROW NEW ProparseNodeTypesException (err, "Invalid proparse node type."{&TRAN}, 0) . 
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the minimum number of characters the ABL keyword can be 
                 abbreviated with 
        Notes:   
        @param pcNodeType The Proparse Node type
        @return The minimum number of characters the ABL keyword can be abbreviated with
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER MinAbbreviated (pcNodeType AS CHARACTER):
        
        IF NOT CAN-FIND (FIRST ttNodeTypes) THEN 
            ProparseNodeTypes:ImportNodeTypes () .
        
        FIND ttNodeTypes WHERE ttNodeTypes.NodeType = pcNodeType .
        
        RETURN ttNodeTypes.MinAbbreviated .
        
        CATCH err AS Progress.Lang.Error:
            UNDO, THROW NEW ProparseNodeTypesException (err, "Invalid proparse node type."{&TRAN}, 0) . 
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the proparse node type for a given ABL keyword 
        Notes:   
        @param pcAblKeyword The ABL keyword
        @return The propery node type
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER NodeType (pcAblKeyword AS CHARACTER):
        
        IF NOT CAN-FIND (FIRST ttNodeTypes) THEN 
            ProparseNodeTypes:ImportNodeTypes () .
        
        FIND ttNodeTypes WHERE ttNodeTypes.AblKeyWord = pcAblKeyword .
        
        RETURN ttNodeTypes.NodeType . 
        
        CATCH err AS Progress.Lang.Error:
            UNDO, THROW NEW ProparseNodeTypesException (err, "Invalid proparse node type."{&TRAN}, 0) . 
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Imports the NodeTypes from the file Consultingwerk/Studio/Proparse/NodeTypes/nodetypes.xml
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID ImportNodeTypes ():
        
        TEMP-TABLE ttNodeTypes:READ-XML ("FILE":U, 
                                         "Consultingwerk/Studio/Proparse/NodeTypes/nodetypes.xml":U,
                                         ?, ?, ?) .
        
    END METHOD .

END CLASS.
