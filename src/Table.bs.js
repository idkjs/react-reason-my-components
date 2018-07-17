// Generated by BUCKLESCRIPT VERSION 4.0.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var ReasonReact = require("reason-react/src/ReasonReact.js");

var component = ReasonReact.statelessComponent("FlexibleTable");

function Table(T) {
  var make = function (tableName, datas, headers, _) {
    return /* record */[
            /* debugName */component[/* debugName */0],
            /* reactClassInternal */component[/* reactClassInternal */1],
            /* handedOffState */component[/* handedOffState */2],
            /* willReceiveProps */component[/* willReceiveProps */3],
            /* didMount */component[/* didMount */4],
            /* didUpdate */component[/* didUpdate */5],
            /* willUnmount */component[/* willUnmount */6],
            /* willUpdate */component[/* willUpdate */7],
            /* shouldUpdate */component[/* shouldUpdate */8],
            /* render */(function () {
                var bodyRows = $$Array.of_list(List.map(Curry._1(T[/* render */1], headers), datas));
                var headerRow = $$Array.of_list(List.map(T[/* renderHeader */0], headers));
                return React.createElement("table", {
                            name: tableName
                          }, React.createElement("thead", undefined, React.createElement("tr", undefined, headerRow)), React.createElement("tbody", undefined, bodyRows));
              }),
            /* initialState */component[/* initialState */10],
            /* retainedProps */component[/* retainedProps */11],
            /* reducer */component[/* reducer */12],
            /* subscriptions */component[/* subscriptions */13],
            /* jsElementWrapped */component[/* jsElementWrapped */14]
          ];
  };
  return /* module */[/* make */make];
}

function render(headers, data) {
  var row = $$Array.of_list(List.map((function (header) {
              if (header) {
                return React.createElement("td", undefined, React.createElement("div", undefined, data[/* text */0]));
              } else {
                return React.createElement("td", undefined, React.createElement("div", undefined, data[/* id */1]));
              }
            }), headers));
  return React.createElement("tr", undefined, row);
}

function renderHeader(header) {
  if (header) {
    return React.createElement("th", undefined, "Name");
  } else {
    return React.createElement("th", undefined, "ID");
  }
}

var TodoTableDefine = /* module */[
  /* render */render,
  /* renderHeader */renderHeader
];

function make(tableName, datas, headers, _) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function () {
              var bodyRows = $$Array.of_list(List.map((function (param) {
                          return render(headers, param);
                        }), datas));
              var headerRow = $$Array.of_list(List.map(renderHeader, headers));
              return React.createElement("table", {
                          name: tableName
                        }, React.createElement("thead", undefined, React.createElement("tr", undefined, headerRow)), React.createElement("tbody", undefined, bodyRows));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

var TodoTable = /* module */[/* make */make];

var TableSample = /* module */[
  /* TodoTableDefine */TodoTableDefine,
  /* TodoTable */TodoTable
];

ReasonReact.element(undefined, undefined, make("todoTable", /* :: */[
          /* record */[
            /* text */"",
            /* id */"id"
          ],
          /* [] */0
        ], /* :: */[
          /* ID */0,
          /* :: */[
            /* Name */1,
            /* [] */0
          ]
        ], /* array */[]));

exports.component = component;
exports.Table = Table;
exports.TableSample = TableSample;
/* component Not a pure module */
