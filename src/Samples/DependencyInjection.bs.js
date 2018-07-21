// Generated by BUCKLESCRIPT VERSION 4.0.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");

function ServiceFunctor(C) {
  var apply = function (arg) {
    return Curry._1(C[/* execute */0], arg) + "Functor";
  };
  return /* module */Block.localModule(["apply"], [apply]);
}

function Application(S) {
  var run = function (arg) {
    return Curry._1(S[/* apply */0], arg) + "Application";
  };
  return /* module */Block.localModule(["run"], [run]);
}

exports.ServiceFunctor = ServiceFunctor;
exports.Application = Application;
/* No side effect */