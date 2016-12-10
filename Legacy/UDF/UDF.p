/* UDF.p - user defined fields extract

   purpose: process to extract user defined field values
   syntax:  {UDF/ttUDF.i} <--- include defines temp-table ttUDF
            ...
            RUN UDF/UDF.p (<udf group>, <table.rec_key>, OUTPUT TABLE ttUDF).
            ...
   author:  Ron Stark
   date:    12.10.2016
*/   

{UDF/ttUDF.i}

DEFINE INPUT  PARAMETER ipcMFGroup AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcRecKey  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttUDF.

{methods/defines/miscflds.i &NEW="NEW"}

EMPTY TEMP-TABLE ttUDF.

FIND FIRST mfgroup NO-LOCK
     WHERE mfgroup.mfgroup_data BEGINS ipcMFGroup
     NO-ERROR.
IF NOT AVAILABLE mfgroup THEN RETURN.

RUN pGetMFData.
RUN pBuildttUDF.

PROCEDURE pGetMFData:
    OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat").
    FOR EACH mfdata NO-LOCK
        WHERE mfdata.mfgroup_data EQ ipcMFGroup
        :
      PUT UNFORMATTED mfdata.miscflds_data SKIP.
    END.
    OUTPUT CLOSE.
    INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat") NO-ECHO.
    REPEAT:
      CREATE ttAttrb.
      IMPORT ttAttrb.
    END.
    INPUT CLOSE.
    IF ttAttrb.attr_type EQ "" THEN DELETE ttAttrb.
END PROCEDURE.

PROCEDURE pBuildttUDF:
    DEFINE VARIABLE cName      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLabel     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormat    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    
    FOR EACH ttAttrb
        BREAK BY ttAttrb.attr_tab
              BY ttAttrb.attr_order
              BY ttAttrb.attr_x
              BY ttAttrb.attr_y
        :
        IF FIRST-OF(ttAttrb.attr_tab) THEN DO:
            ASSIGN
                cLabel            = ENTRY(1,ENTRY(ttAttrb.attr_tab,mfgroup.mfgroup_tab),"|")
                cValue            = STRING(ttAttrb.attr_tab)
                cFormat           = "x(" + STRING(MAX(LENGTH(cLabel),LENGTH(cValue))) + ")"
                idx               = 0
                .
            RUN pCreatettUDF (ipcRecKey,
                              ttAttrb.attr_tab,
                              idx,
                              "tab",
                              "character",
                              cFormat,
                              cLabel,
                              cValue
                              ).
        END. /* first of tab */
        IF NOT CAN-DO("Rectangle,Text",ttAttrb.attr_type) THEN DO:
            FIND FIRST mfvalues
                 WHERE mfvalues.rec_key EQ ipcRecKey
                   AND mfvalues.mf_id   EQ ttAttrb.attr_id
                 NO-ERROR.
            IF AVAILABLE mfvalues THEN DO:
                ASSIGN
                    cName   = IF ttAttrb.attr_name EQ "" THEN ttAttrb.attr_id
                              ELSE ttAttrb.attr_name
                    cLabel  = IF ttAttrb.attr_label EQ "" THEN cName
                              ELSE ttAttrb.attr_label
                    cFormat = IF CAN-DO("Editor,Radio-Set,Slider",ttAttrb.attr_type) THEN ?
                              ELSE ttAttrb.attr_settings
                    cValue  = IF mfvalues.mf_value EQ ? AND ttAttrb.attr_datatype NE "date" THEN ""
                              ELSE mfvalues.mf_value
                    idx     = idx + 1
                    .
                IF ttAttrb.attr_datatype EQ "character" THEN
                cFormat = "x(" + STRING(MAX(LENGTH(cLabel),LENGTH(cValue))) + ")".
                IF ttAttrb.attr_datatype EQ "logical" THEN
                cFormat = "yes/no".
                RUN pCreatettUDF (ipcRecKey,
                                  ttAttrb.attr_tab,
                                  idx,
                                  ttAttrb.attr_id,
                                  LC(ttAttrb.attr_datatype),
                                  cFormat,
                                  cLabel,
                                  cValue
                                  ).
            END. /* avail mfvalues */
        END. /* not rect or text */
    END. /* each ttAttrb */
END PROCEDURE.

PROCEDURE pCreatettUDF:
    DEFINE INPUT PARAMETER ipcRecKey   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiTab      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrder    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDataType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFormat   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLabel    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcValue    AS CHARACTER NO-UNDO.

    CREATE ttUDF.
    ASSIGN
        ttUDF.udfRecKey   = ipcRecKey
        ttUDF.udfTab      = ipiTab
        ttUDF.udfOrder    = ipiOrder
        ttUDF.udfID       = ipcID
        ttUDF.udfDataType = ipcDataType
        ttUDF.udfFormat   = ipcFormat
        ttUDF.udfLabel    = ipcLabel
        ttUDF.udfValue    = ipcValue
        .
END PROCEDURE.
