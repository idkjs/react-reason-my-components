include FlexibleTableHeader;

type element;

[@bs.send]
external addEventListener: (Dom.element, string, unit => unit) => unit =
  "addEventListener";

type tableCell('column) = {
  column: 'column,
  style: ReactDOMRe.style,
};

module type TableDef = {
  type column;
  type relationDataType;
  type header = headerItem(column);
  type cell = tableCell(column);
  let dataRowStyle: (list(header), relationDataType) => string;
  let headerRowStyle: list(header) => string;
  let footerRowStyle: list(header) => string;

  let defaultHeader: column => header;
};

module FlexibleTable = (T: TableDef) => {
  type state = {
    tableWidth: float,
    mutable tableDom: React.ref(Js.Nullable.t(Dom.element)),
  };
  type action =
    | DetectedTableSize(float);
  let defaultHeaders = headerColumns =>
    headerColumns |> List.map(T.defaultHeader);
  let headerItemToCell = (header: T.header): T.cell => {
    let width = getWidthSize(header.size);
    let px = "px";
    {
      column: header.column,
      style: ReactDOMRe.Style.make(~width={j|$width$px|j}, ()),
    };
  };

  [@react.component]
  let make =
      (
        ~datas: list('data),
        ~headerItems: list(T.header),
        ~row: (list(T.cell), 'data) => React.element,
        ~header: list(T.cell) => React.element,
        ~footer: list(T.cell) => React.element,
        ~tableClassName: string,
        (),
      ) => {
    let initialState = {
      tableWidth: 0.0,
      tableDom: React.useRef(Js.Nullable.null),
    };
    let (state, dispatch) =
      React.useReducer(
        (state, action) =>
          switch (action) {
          | DetectedTableSize(tableWidth) => {...state, tableWidth}
          },
        initialState,
      );
    // let theRef = React.useRef((None));
    let apperTable = (theRef) => {
      let domOpt = Js.Nullable.toOption(theRef);
      let tableDomToAction = tableDom => {
        let tableWidth: float =
          ReactDOMRe.domElementToObj(tableDom)##clientWidth;
        if (tableWidth != state.tableWidth) {
          DetectedTableSize(tableWidth) |> dispatch;
        };
      };
      switch (domOpt) {
      | Some(dom) =>
        state.tableDom = (dom);
        let tableDom = dom;

        addEventListener(state.tableDom, "transitionend", () =>
          switch (state.tableDom) {
          | Some(dom) => tableDomToAction(dom)
          | None => ()
          }
        );

        tableDomToAction(tableDom);
      | None => ()
      };
    };
    React.useEffect1(
      () => {
        apperTable(state.tableDom);
        None;
      },
      [|state|],
    );
    let cells =
      headerItems
      |> getWidthSizeByTableWidthSize(state.tableWidth)
      |> List.map(headerItemToCell);
    let bodyRows =
      datas |> List.map(row(cells)) |> Array.of_list |> ReasonReact.array;

    let header = cells |> header;
    let footer = cells |> footer;
    <table
      className=tableClassName ref={ReactDOMRe.Ref.domRef(state.tableDom)}>
      <thead> header </thead>
      <tbody> bodyRows </tbody>
      <tfoot> footer </tfoot>
    </table>;
  };
};
