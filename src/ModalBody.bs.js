// Generated by BUCKLESCRIPT VERSION 4.0.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var React = require("react");
var ReasonReact = require("reason-react/src/ReasonReact.js");

var component = ReasonReact.statelessComponent("ModalBodyComponent");

function make(_children) {
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
            component[/* didMount */4],
            component[/* didUpdate */5],
            component[/* willUnmount */6],
            component[/* willUpdate */7],
            component[/* shouldUpdate */8],
            (function () {
                return React.createElement("div", {
                            className: "modal-body"
                          }, _children);
              }),
            component[/* initialState */10],
            component[/* retainedProps */11],
            component[/* reducer */12],
            component[/* subscriptions */13],
            component[/* jsElementWrapped */14]
          ]);
}

exports.component = component;
exports.make = make;
/* component Not a pure module */
