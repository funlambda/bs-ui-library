// Generated by BUCKLESCRIPT VERSION 2.2.0, PLEASE EDIT WITH CARE
'use strict';

var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

var LeftRight = /* module */[];

function getOption(x) {
  if (x) {
    return x[0];
  } else {
    return Pervasives.failwith("option has no value");
  }
}

function float_of_string_opt(s) {
  try {
    return /* Some */[Caml_format.caml_float_of_string(s)];
  }
  catch (_ex){
    return /* None */0;
  }
}

exports.LeftRight = LeftRight;
exports.getOption = getOption;
exports.float_of_string_opt = float_of_string_opt;
/* No side effect */
