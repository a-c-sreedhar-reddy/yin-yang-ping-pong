let name a = a;;

let canvas_element =
  Webapi.Dom.Document.getElementById "canvas" Webapi.Dom.document
in

match canvas_element with Some _ -> () | None -> ()
