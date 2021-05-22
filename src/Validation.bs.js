// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

function fromErrors(getErrors) {
  return {
          _0: getErrors,
          [Symbol.for("name")]: "Validator"
        };
}

function all(validators) {
  var allGetErrors = List.map((function (validator) {
          return validator._0;
        }), validators);
  var validateLogic = function (subject) {
    return List.flatten(List.map((function (getError) {
                      return Curry._1(getError, subject);
                    }), allGetErrors));
  };
  return {
          _0: validateLogic,
          [Symbol.for("name")]: "Validator"
        };
}

function first(validators) {
  var allValidation = all(validators);
  var validateLogic = function (subject) {
    try {
      return Curry._1(allValidation._0, subject);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Failure") {
        return /* [] */0;
      }
      throw exn;
    }
  };
  return {
          _0: validateLogic,
          [Symbol.for("name")]: "Validator"
        };
}

function run(validator, source) {
  return Curry._1(validator._0, source);
}

exports.fromErrors = fromErrors;
exports.all = all;
exports.first = first;
exports.run = run;
/* No side effect */
