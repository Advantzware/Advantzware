/* est/frmotvar.i */

def {1} temp-table tt-frmout NO-UNDO
    field est-no    as cha
    field cust-no   as cha
    field part-no   as cha
    field ship-id   as cha
    field part-dscr1 as cha
    field stack-no  as cha
    field style     as cha
    field flute     as cha
    field test      as cha
    field board     as dec
    FIELD cal       AS DEC
    field procat    as dec
    field len       as dec      
    field wid       as dec
    field dep       as dec
    field form-no       as int
    field blank-no  as int     
    field i-col     as int     
    field i-pass    as int
    field i-coat    as int
    field i-coat-p  as int
    field yld-qty   as int
    field f-col     as int
    field f-pass    as int
    field f-coat    as int
    field f-coat-p as int
    FIELD pur-man   AS cha
    FIELD est-date AS CHAR
    field colr-dscr as char
    field glu-cod   as char
    field bndl-cod as char
    field plat-cod as char
    field bndng-cod  as char 
    field tab as log FORMAT "In/Out"
    field bord as char 
    FIELD quantity AS INT
    FIELD cat AS CHAR
    field copy-qty as int extent 20 
    field copy-rel as int extent 20  		
    .
