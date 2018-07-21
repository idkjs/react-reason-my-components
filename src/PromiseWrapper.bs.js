// Generated by BUCKLESCRIPT VERSION 4.0.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

function PromiseWrapper() {
  var component = ReasonReact.reducerComponent("PromiseWrapper");
  var make = function (promise, whenSuccess, whenError, whenPending, _) {
    return /* record */Block.record([
              "debugName",
              "reactClassInternal",
              "handedOffState",
              "willReceiveProps",
              "didMount",
              "didUpdate",
              "willUnmount",
              "willUpdate",
              "shouldUpdate",
              "render",
              "initialState",
              "retainedProps",
              "reducer",
              "subscriptions",
              "jsElementWrapped"
            ], [
              component[/* debugName */0],
              component[/* reactClassInternal */1],
              component[/* handedOffState */2],
              component[/* willReceiveProps */3],
              (function (self) {
                  promise.then((function (value) {
                            return Promise.resolve(Curry._1(self[/* send */3], /* Completed */Block.variant("Completed", 0, [value])));
                          })).catch((function (error) {
                          return Promise.resolve(Curry._1(self[/* send */3], /* Error */Block.variant("Error", 1, [error])));
                        }));
                  return /* () */0;
                }),
              component[/* didUpdate */5],
              component[/* willUnmount */6],
              component[/* willUpdate */7],
              component[/* shouldUpdate */8],
              (function (self) {
                  var match = self[/* state */1];
                  if (match !== undefined) {
                    var result = match;
                    if (result.tag) {
                      return Curry._1(whenError, result[0]);
                    } else {
                      return Curry._1(whenSuccess, result[0]);
                    }
                  } else {
                    return whenPending;
                  }
                }),
              (function () {
                  return undefined;
                }),
              component[/* retainedProps */11],
              (function (action, _) {
                  if (action.tag) {
                    var state = /* Failure */Block.variant("Failure", 1, [action[0]]);
                    return /* Update */Block.variant("Update", 0, [state]);
                  } else {
                    var state$1 = /* Success */Block.variant("Success", 0, [action[0]]);
                    return /* Update */Block.variant("Update", 0, [state$1]);
                  }
                }),
              component[/* subscriptions */13],
              component[/* jsElementWrapped */14]
            ]);
  };
  return /* module */Block.localModule([
            "component",
            "make"
          ], [
            component,
            make
          ]);
}

exports.PromiseWrapper = PromiseWrapper;
/* ReasonReact Not a pure module */