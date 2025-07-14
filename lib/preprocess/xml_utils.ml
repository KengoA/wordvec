let strip_xml_tags s =
  let re = Str.regexp "<[^>]+>" in
  Str.global_replace re " " s

let xml_to_txt ~input_xml ~output_txt =
  let ic = open_in input_xml in
  let oc = open_out output_txt in
  (try
     while true do
       let line = input_line ic in
       let clean = strip_xml_tags line in
       output_string oc clean ;
       output_char oc '\n'
     done
   with End_of_file -> ()) ;
  close_in ic ;
  close_out oc
