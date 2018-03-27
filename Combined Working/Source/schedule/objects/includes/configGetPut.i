/* configGetPut.i - used in configVersion.i */

&IF '{1}' EQ 'get' &THEN
{{&includes}/config.i {1} {2} {3} {4} {5} "{&version1}"}
{{&includes}/config.i {1} {2} {3} {4} {5} "{&version2}"}
{{&includes}/config.i {1} {2} {3} {4} {5} "{&version3}"}
{{&includes}/config.i {1} {2} {3} {4} {5} "{&version4}"}
{{&includes}/config.i {1} {2} {3} {4} {5} "{&version5}"}
{{&includes}/config.i {1} {2} {3} {4} {5} "{&version6}"}
{{&includes}/config.i {1} {2} {3} {4} {5} "{&version7}"}
{{&includes}/config.i {1} {2} {3} {4} {5} "{&version8}"}
{{&includes}/config.i {1} {2} {3} {4} {5} "{&version9}"}
&ENDIF

{{&includes}/config.i {1} {2} {3} {4} {5} "{&version}"}
