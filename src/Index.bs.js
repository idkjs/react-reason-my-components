// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Js_option = require("bs-platform/lib/js/js_option.js");
var ReactDOMRe = require("reason-react/src/legacy/ReactDOMRe.bs.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var CamlinternalOO = require("bs-platform/lib/js/camlinternalOO.js");
var Page$ReactTemplate = require("./Page.bs.js");
var Helpers$ReactTemplate = require("./Helpers.bs.js");
var Routing$ReactTemplate = require("./Routing.bs.js");
var Tooltip$ReactTemplate = require("./Tooltip.bs.js");
var TreeData$ReactTemplate = require("./TreeData.bs.js");
var UrlParser$ReactTemplate = require("./UrlParser.bs.js");
var ModalSample$ReactTemplate = require("./ModalSample.bs.js");
var PromiseWrapper$ReactTemplate = require("./PromiseWrapper.bs.js");
var IncrementalSearch$ReactTemplate = require("./IncrementalSearch.bs.js");
var TodoFlexibleTable$ReactTemplate = require("./Samples/TodoFlexibleTable.bs.js");
var ExecuteSomeService = require("./externalJsModules/ExecuteSomeService");

function andThen(a, b, v) {
  return Curry._1(b, Curry._1(a, v));
}

function toFragment(fn) {
  return Curry.__1(fn);
}

function execute(prim) {
  return ExecuteSomeService.execute(prim);
}

var executeResult = ExecuteSomeService.execute(100);

console.log(executeResult);

function executeNullable(prim) {
  return ExecuteSomeService.executeNullable(prim);
}

var executeNullableResult = ExecuteSomeService.executeNullable(100);

console.log(executeNullableResult);

var executeNullableResultOpt = executeNullableResult === undefined ? undefined : Caml_option.some(executeNullableResult);

function appendOne(param) {
  return 1 + param | 0;
}

var r = Belt_Option.map(Belt_Option.map(Js_option.map((function (a) {
                return 1 + a | 0;
              }), Js_option.map((function (a) {
                    return 1 + a | 0;
                  }), executeNullableResultOpt)), appendOne), appendOne);

console.log(r);

function executePromise(prim) {
  return ExecuteSomeService.executePromise(prim);
}

var executePromiseResult = ExecuteSomeService.executePromise(100);

executePromiseResult.then(function (param) {
        return Promise.resolve(100 + param | 0);
      }).then(function (param) {
      return Promise.resolve((console.log(param), undefined));
    });

var $$class = CamlinternalOO.create_table(["name"]);

var name = CamlinternalOO.get_method_label($$class, "name");

CamlinternalOO.set_method($$class, name, (function (self$1) {
        return "hoge";
      }));

CamlinternalOO.init_class($$class);

var sampleArgObj = CamlinternalOO.create_object_opt(undefined, $$class);

function executeArgObj(prim) {
  return ExecuteSomeService.executeArgObj(prim);
}

var argJsObjSample = {
  name: "RYO"
};

console.log(ExecuteSomeService.executeArgObj(argJsObjSample));

console.log(argJsObjSample.name);

console.log("りょうた");

console.log("りょうた");

var IntPromiseWrapperDef = {};

var IntPromiseWrapper = PromiseWrapper$ReactTemplate.PromiseWrapper(IntPromiseWrapperDef);

function whenSuccess(v) {
  return React.createElement("div", undefined, String(v));
}

function whenError(param) {
  return React.createElement("div", undefined, React.createElement("p", undefined, "ERROR!"));
}

var whenPending = React.createElement("div", undefined, React.createElement("p", undefined, "NowLoading..."));

function r$1(param) {
  
}

function timePromise(count) {
  return new Promise((function (resolve, param) {
                setTimeout((function (param) {
                        return resolve(count);
                      }), count);
                
              }));
}

var allEntities = {
  hd: "Apple",
  tl: {
    hd: "Banana",
    tl: {
      hd: "All",
      tl: {
        hd: "Berry",
        tl: /* [] */0
      }
    }
  }
};

function findEntities(text) {
  return new Promise((function (resolve, param) {
                return resolve(List.filter(function (__x) {
                                  return Helpers$ReactTemplate.$$String.contain(__x, text);
                                })(allEntities));
              }));
}

var TextIncrementalSearchDef = {
  allEntities: allEntities,
  findEntities: findEntities
};

var TextIncrementalSearch = IncrementalSearch$ReactTemplate.IncrementalSearch({
      findEntities: findEntities
    });

function SamplePage(Resolver) {
  var initialize = function (resource) {
    return {
            value: resource[0],
            name: resource[1]
          };
  };
  var render = function (t) {
    var name = t.name;
    var valueStr = String(t.value);
    return name + valueStr;
  };
  var loadResource = function (arg) {
    return Curry._1(Resolver.loadResource, 1009).then(function (value) {
                return Promise.resolve([
                            value,
                            String(value) + arg
                          ]);
              });
  };
  return {
          componentName: "SampleIntPage",
          initialize: initialize,
          render: render,
          loadResource: loadResource
        };
}

var loadResource = timePromise;

var SamplePageResolverImpl = {
  loadResource: loadResource
};

var ResourcePromiseWrapperDef = {};

function initialize(resource) {
  return {
          value: resource[0],
          name: resource[1]
        };
}

function render(t) {
  var name = t.name;
  var valueStr = String(t.value);
  return name + valueStr;
}

function loadResource$1(arg) {
  return timePromise(1009).then(function (value) {
              return Promise.resolve([
                          value,
                          String(value) + arg
                        ]);
            });
}

var partial_arg = {
  componentName: "SampleIntPage",
  initialize: initialize,
  render: render,
  loadResource: loadResource$1
};

var partial_arg$1 = Page$ReactTemplate.Make;

var SamplePageImpl = (function (param) {
      return partial_arg$1(partial_arg, param);
    })(ResourcePromiseWrapperDef);

function add(x, y, z) {
  return (x + y | 0) + z | 0;
}

var addFiveOops = add(5, 4, 4);

function add2(x, y, z) {
  return (x + y | 0) + z | 0;
}

var addFiveOops2 = add2(5, 4, 4);

function add3(x, y, z) {
  return (x + y | 0) + z | 0;
}

function urlToRoute(url, param) {
  var route = url.path;
  if (route) {
    switch (route.hd) {
      case "about" :
          if (route.tl) {
            console.log(route);
            return /* NotFound */2;
          } else {
            return /* About */1;
          }
      case "src" :
          var match = route.tl;
          if (match) {
            if (match.hd === "index.html") {
              if (match.tl) {
                console.log(route);
                return /* NotFound */2;
              } else {
                return /* Home */0;
              }
            }
            console.log(route);
            return /* NotFound */2;
          }
          console.log(route);
          return /* NotFound */2;
      default:
        console.log(route);
        return /* NotFound */2;
    }
  } else {
    console.log(route);
    return /* NotFound */2;
  }
}

function transition(route) {
  switch (route) {
    case /* Home */0 :
        return timePromise(3000).then(function (v) {
                    return Promise.resolve(React.createElement("div", undefined, String(v)));
                  });
    case /* About */1 :
        return Promise.resolve(React.createElement("div", undefined, "About"));
    case /* NotFound */2 :
        return Promise.resolve(React.createElement("div", undefined, "NF"));
    
  }
}

var SampleRouting = {
  urlToRoute: urlToRoute,
  transition: transition
};

var SampleApp = Routing$ReactTemplate.Application(SampleRouting);

function userFactory(name) {
  return {
          name: name,
          id: name
        };
}

function identity(user) {
  return user.id;
}

function showLeaf(user) {
  return React.createElement("p", {
              key: user.id
            }, user.name);
}

var UserLeaf = {
  identity: identity,
  showLeaf: showLeaf
};

var UserTree = TreeData$ReactTemplate.Tree(UserLeaf);

var leaf1 = Curry._1(UserTree.leaf, {
      name: "user1",
      id: "user1"
    });

var leaf2 = Curry._1(UserTree.leaf, {
      name: "user2",
      id: "user2"
    });

var leaf3 = Curry._1(UserTree.leaf, {
      name: "user3",
      id: "user3"
    });

var leaf4 = Curry._1(UserTree.leaf, {
      name: "user4",
      id: "user4"
    });

var leaf5 = Curry._1(UserTree.leaf, {
      name: "user5",
      id: "user5"
    });

var node1Tmp = Curry._1(UserTree.node, {
      hd: leaf1,
      tl: {
        hd: leaf2,
        tl: /* [] */0
      }
    });

var node2 = Curry._1(UserTree.node, {
      hd: leaf4,
      tl: /* [] */0
    });

var node1 = Curry._1(UserTree.node, {
      hd: node1Tmp,
      tl: {
        hd: leaf5,
        tl: /* [] */0
      }
    });

var tree = Curry._1(UserTree.node, {
      hd: node1,
      tl: {
        hd: node2,
        tl: {
          hd: leaf3,
          tl: /* [] */0
        }
      }
    });

UrlParser$ReactTemplate.Sample.start(undefined);

ReactDOMRe.renderToElementWithId(React.createElement("div", undefined, React.createElement(Tooltip$ReactTemplate.make, {
              position: /* Down */2,
              size: /* Large */2,
              text: "IAM TOOLTIP",
              parent: React.createElement("button", {
                    className: "target"
                  }, "HELLO")
            }), React.createElement(TodoFlexibleTable$ReactTemplate.TodoTableSample.make, {}), React.createElement(IntPromiseWrapper.make, {
              promise: timePromise(1000),
              whenSuccess: whenSuccess,
              whenError: whenError,
              whenPending: whenPending
            }), React.createElement(ModalSample$ReactTemplate.make, {}), React.createElement(TextIncrementalSearch.make, {
              searchResultView: (function (results) {
                  return React.createElement("ul", undefined, $$Array.of_list(List.map((function (text) {
                                        return React.createElement("li", {
                                                    key: text
                                                  }, text);
                                      }), results)));
                })
            }), React.createElement(SamplePageImpl.make, {
              loadResourceArg: "HOGE"
            }), React.createElement(SampleApp.make, {
              initialPage: React.createElement("div", undefined, "INITIAL"),
              onError: (function (param) {
                  console.error("ERROR!");
                  
                }),
              onStartTransition: (function (param) {
                  console.log("start_transition");
                  
                }),
              onFinishTransition: (function (param) {
                  console.log("finish_transition");
                  
                })
            }), Curry._1(UserTree.showTree, tree)), "index");

function plus(a, b) {
  return a + b | 0;
}

var PromiseWrapper = PromiseWrapper$ReactTemplate.PromiseWrapper;

var $great$great = andThen;

var $tilde$great = toFragment;

var addFiveOops3 = 13;

var userTreeSample = tree;

exports.PromiseWrapper = PromiseWrapper;
exports.andThen = andThen;
exports.$great$great = $great$great;
exports.toFragment = toFragment;
exports.$tilde$great = $tilde$great;
exports.execute = execute;
exports.executeResult = executeResult;
exports.executeNullable = executeNullable;
exports.executeNullableResult = executeNullableResult;
exports.executePromise = executePromise;
exports.executePromiseResult = executePromiseResult;
exports.sampleArgObj = sampleArgObj;
exports.executeArgObj = executeArgObj;
exports.IntPromiseWrapperDef = IntPromiseWrapperDef;
exports.IntPromiseWrapper = IntPromiseWrapper;
exports.whenSuccess = whenSuccess;
exports.whenError = whenError;
exports.whenPending = whenPending;
exports.r = r$1;
exports.timePromise = timePromise;
exports.TextIncrementalSearchDef = TextIncrementalSearchDef;
exports.TextIncrementalSearch = TextIncrementalSearch;
exports.SamplePage = SamplePage;
exports.SamplePageResolverImpl = SamplePageResolverImpl;
exports.ResourcePromiseWrapperDef = ResourcePromiseWrapperDef;
exports.SamplePageImpl = SamplePageImpl;
exports.add = add;
exports.addFiveOops = addFiveOops;
exports.add2 = add2;
exports.addFiveOops2 = addFiveOops2;
exports.add3 = add3;
exports.addFiveOops3 = addFiveOops3;
exports.SampleRouting = SampleRouting;
exports.SampleApp = SampleApp;
exports.userFactory = userFactory;
exports.UserLeaf = UserLeaf;
exports.UserTree = UserTree;
exports.userTreeSample = userTreeSample;
exports.plus = plus;
/* executeResult Not a pure module */
