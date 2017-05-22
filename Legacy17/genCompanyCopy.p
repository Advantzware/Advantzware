/* genCompanyCopy.p */

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
e-item,~
e-item-cust,~
e-item-vend,~
e-itemfg,~
e-itemfg-vend,~
emp,~
fg-act,~
fg-bin,~
fg-ctrl,~
fg-set,~
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
mat-act,~
matprep,~
mmtx,~
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

OUTPUT TO 'custom/companyCopy.i'.
PUT UNFORMATTED '/* custom/company.i - used in custom/companyCopy.w */' SKIP
  '/* auto generated from genCompanyCopy.p on ' TODAY ' @ ' STRING(TIME,'hh:mm:ss am') ' */' SKIP(1)
  'PROCEDURE startCopy:' SKIP.
FOR EACH _file NO-LOCK WHERE _file._hidden EQ NO AND CAN-DO({&include},_file._file-name),
  FIRST _field OF _file NO-LOCK WHERE _field._field-name EQ 'company':
  PUT UNFORMATTED '  RUN ' _file._file-name 'Copy.' SKIP.
END.
PUT UNFORMATTED 'END PROCEDURE.' SKIP.
FOR EACH _file NO-LOCK WHERE _file._hidden EQ NO AND CAN-DO({&include},_file._file-name),
  FIRST _field OF _file NO-LOCK WHERE _field._field-name EQ 'company':
  PUT UNFORMATTED SKIP(1)
    'PROCEDURE ' _file._file-name 'Copy:' SKIP
    '  DEFINE BUFFER b' _file._file-name ' FOR ' _file._file-name '.' SKIP.
  IF SEARCH('custom/cc_' + _file._file-name + 'Defs.i') NE ? THEN
  PUT UNFORMATTED '  ~{custom/cc_' _file._file-name 'Defs.i}' SKIP.
  PUT UNFORMATTED SKIP(1)
    '  RUN showMsg (~'' _file._file-name '~',NO).' SKIP
    '  IF CAN-FIND(FIRST ' _file._file-name ' WHERE '
    _file._file-name '.company EQ ipCompanyTo) THEN' SKIP
    '  FOR EACH ' _file._file-name ' EXCLUSIVE-LOCK WHERE '
    _file._file-name '.company EQ ipCompanyTo:' SKIP
    '    DELETE ' _file._file-name '.' SKIP
    '  END.' SKIP
    '  FOR EACH ' _file._file-name ' NO-LOCK WHERE '
    _file._file-name '.company EQ ipCompanyFrom:' SKIP
    '    CREATE b' _file._file-name '.' SKIP.
  IF SEARCH('custom/cc_' + _file._file-name + '.i') NE ? THEN
  PUT UNFORMATTED '    ~{custom/cc_' _file._file-name '.i}' SKIP.
  ELSE
  PUT UNFORMATTED
    '    BUFFER-COPY ' _file._file-name ' EXCEPT company TO b'
    _file._file-name SKIP
    '      ASSIGN b' _file._file-name '.company = ipCompanyTo.' SKIP.
  PUT UNFORMATTED
    '  END.' SKIP
    '  RUN showMsg (~'~',YES).' SKIP
    'END PROCEDURE.' SKIP.
END.
OUTPUT CLOSE.

COMPILE custom/companyCopy.w SAVE.
