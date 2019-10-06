
def {1} shared buffer xoe-ord for oe-ord.

def NEW shared var v-tax-rate as dec format ">,>>9.99<<<".
def NEW shared var v-frt-tax-rate like v-tax-rate.

def {1} shared var v-frel like oe-relh.release# init 0.
def {1} shared var v-trel like v-frel init 999999.
def {1} shared var v-fdat like oe-relh.rel-date format "99/99/9999" init today.
def {1} shared var v-tdat like v-fdat.
def {1} shared var v-fcus like oe-relh.cust-no init 0.
def {1} shared var v-tcus like v-fcus init "zzzzzzzz".
def {1} shared var v-ford like oe-rell.ord-no init 0.
def {1} shared var v-tord like v-ford init 99999999.

{oe/relcheck.i {1}}
