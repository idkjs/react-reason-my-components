[@react.component]
let make = (~title, ()) =>
  <div className="modal-header">
    {ReasonReact.cloneElement(
       title,
       ~props={"className": "modal-title"},
       [||],
     )}
    _children
  </div>;
