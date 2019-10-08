&SCOPED-DEFINE include "~
account,~
ap-buy,~
ap-ctrl,~
ap-ledger,~
ar-ctrl,~
bank,~
buyer,~
carr-mtx,~
carrier,~
ce-ctrl,~
costtype,~
crew,~
currency,~
cust,~
cust-markup,~
cust-part,~
cust-prod-sales,~
custype,~
db-ctrl,~
dept,~
e-item,~
e-item-cust,~
e-item-vend,~
e-itemfg,~
e-itemfg-vend,~
emp,~
fg-act,~
fg-bin,~
fg-ctrl,~
fgcat,~
flute,~
gl-ctrl,~
gl-rpt,~
gl-rptd,~
item,~
item-bom,~
item-spec,~
itemfg,~
itemfg-bom,~
itemfg-ink,~
itemfg-loc,~
itemfgdtl,~
jc-ctrl,~
loadtag,~
loc,~
mach,~
mach-adder,~
mach-calendar,~
mat,~
mat-act,~
matprep,~
mmtx,~
mmtx2,~
mmty,~
mstd,~
oe-ctrl,~
oe-prmtx,~
period,~
po-ctrl,~
prep,~
procat,~
prod,~
prodl,~
reftable,~
rm-bin,~
rm-ctrl,~
routing,~
routing-mtx,~
shift,~
shipto,~
sman,~
sman-mtx,~
soldto,~
stack-flute,~
stack-size,~
stax,~
stax-group,~
std-code,~
style,~
sys-ctrl,~
terms,~
terr,~
test-red,~
usercomp,~
vend,~
ventype~
"

DEF BUFFER bf-field FOR _field.

OUTPUT TO c:\tmp\asiindex.lst.

PUT "ASI Unique Index List " SKIP.

FOR EACH _file WHERE NOT _hidden AND CAN-DO({&include},_file-name),
    FIRST bf-field OF _file WHERE bf-field._field-name = "company",
    EACH _index OF _file WHERE _index._unique,
    EACH _index-field OF _index,
    EACH _field OF _index-field 
    NO-LOCK BREAK BY _file._file-name BY _index._index-name .
    .
    DISP _file-name FORM "x(15)" WHEN FIRST-OF(_file._file-name)
         _index-name FORM "x(15)" WHEN FIRST-OF(_index._index-name)
         _unique WHEN FIRST-OF(_index._index-name)
         _field._field-name 
        WITH DOWN STREAM-IO.

END.
