(** Tests for the definition extractor. *)

open Coverage.Definition_extractor

(** Helper to parse and extract definitions from a string. *)
let extract_from_string (content : string) : definition list =
  let parse_result = Syntax.Read.parse_string ~filename:"test.el" content in
  extract_definitions parse_result.sexps

(** {1 Private Name Detection} *)

let test_private_name_with_double_dash () =
  Alcotest.(check bool)
    "double dash is private" true
    (is_private_name "my-pkg--private")

let test_private_name_single_dash () =
  Alcotest.(check bool)
    "single dash is public" false
    (is_private_name "my-pkg-public")

let test_private_name_no_dash () =
  Alcotest.(check bool) "no dash is public" false (is_private_name "mypkg")

(** {1 Function Extraction} *)

let test_extract_defun () =
  let defs = extract_from_string "(defun my-fn () nil)" in
  Alcotest.(check int) "one definition" 1 (List.length defs);
  Alcotest.(check string) "name" "my-fn" (List.hd defs).name;
  Alcotest.(check bool) "is function" true ((List.hd defs).kind = Function)

let test_extract_defsubst () =
  let defs = extract_from_string "(defsubst my-subst () nil)" in
  Alcotest.(check int) "one definition" 1 (List.length defs);
  Alcotest.(check string) "name" "my-subst" (List.hd defs).name;
  Alcotest.(check bool) "is function" true ((List.hd defs).kind = Function)

let test_extract_cl_defun () =
  let defs = extract_from_string "(cl-defun my-cl-fn () nil)" in
  Alcotest.(check int) "one definition" 1 (List.length defs);
  Alcotest.(check string) "name" "my-cl-fn" (List.hd defs).name;
  Alcotest.(check bool) "is function" true ((List.hd defs).kind = Function)

(** {1 Macro Extraction} *)

let test_extract_defmacro () =
  let defs = extract_from_string "(defmacro my-macro () nil)" in
  Alcotest.(check int) "one definition" 1 (List.length defs);
  Alcotest.(check string) "name" "my-macro" (List.hd defs).name;
  Alcotest.(check bool) "is macro" true ((List.hd defs).kind = Macro)

let test_extract_cl_defmacro () =
  let defs = extract_from_string "(cl-defmacro my-cl-macro () nil)" in
  Alcotest.(check int) "one definition" 1 (List.length defs);
  Alcotest.(check string) "name" "my-cl-macro" (List.hd defs).name;
  Alcotest.(check bool) "is macro" true ((List.hd defs).kind = Macro)

(** {1 Variable Extraction} *)

let test_extract_defvar () =
  let defs = extract_from_string "(defvar my-var nil)" in
  Alcotest.(check int) "one definition" 1 (List.length defs);
  Alcotest.(check string) "name" "my-var" (List.hd defs).name;
  Alcotest.(check bool) "is variable" true ((List.hd defs).kind = Variable)

let test_extract_defcustom () =
  let defs =
    extract_from_string "(defcustom my-custom nil \"doc\" :type 'string)"
  in
  Alcotest.(check int) "one definition" 1 (List.length defs);
  Alcotest.(check string) "name" "my-custom" (List.hd defs).name;
  Alcotest.(check bool) "is variable" true ((List.hd defs).kind = Variable)

let test_extract_defconst () =
  let defs = extract_from_string "(defconst my-const 42)" in
  Alcotest.(check int) "one definition" 1 (List.length defs);
  Alcotest.(check string) "name" "my-const" (List.hd defs).name;
  Alcotest.(check bool) "is variable" true ((List.hd defs).kind = Variable)

let test_extract_defvar_local () =
  let defs = extract_from_string "(defvar-local my-local nil)" in
  Alcotest.(check int) "one definition" 1 (List.length defs);
  Alcotest.(check string) "name" "my-local" (List.hd defs).name;
  Alcotest.(check bool) "is variable" true ((List.hd defs).kind = Variable)

(** {1 Face Extraction} *)

let test_extract_defface () =
  let defs = extract_from_string "(defface my-face '((t :bold t)) \"doc\")" in
  Alcotest.(check int) "one definition" 1 (List.length defs);
  Alcotest.(check string) "name" "my-face" (List.hd defs).name;
  Alcotest.(check bool) "is face" true ((List.hd defs).kind = Face)

(** {1 Struct Extraction} *)

let test_extract_cl_defstruct () =
  let defs = extract_from_string "(cl-defstruct person name age)" in
  let names = List.map (fun d -> d.name) defs in
  Alcotest.(check bool) "has person" true (List.mem "person" names);
  Alcotest.(check bool) "has make-person" true (List.mem "make-person" names);
  Alcotest.(check bool) "has person-p" true (List.mem "person-p" names);
  Alcotest.(check bool) "has copy-person" true (List.mem "copy-person" names);
  Alcotest.(check bool) "has person-name" true (List.mem "person-name" names);
  Alcotest.(check bool) "has person-age" true (List.mem "person-age" names)

let test_extract_cl_defstruct_with_options () =
  let defs =
    extract_from_string "(cl-defstruct (point (:constructor nil)) x y)"
  in
  let names = List.map (fun d -> d.name) defs in
  Alcotest.(check bool) "has point" true (List.mem "point" names);
  Alcotest.(check bool) "has point-x" true (List.mem "point-x" names);
  Alcotest.(check bool) "has point-y" true (List.mem "point-y" names)

(** {1 Class Extraction} *)

let test_extract_defclass () =
  let defs =
    extract_from_string
      "(defclass widget () ((name :accessor widget-name) (value)))"
  in
  let names = List.map (fun d -> d.name) defs in
  Alcotest.(check bool) "has widget" true (List.mem "widget" names);
  Alcotest.(check bool) "has widget-p" true (List.mem "widget-p" names);
  Alcotest.(check bool) "has widget-name" true (List.mem "widget-name" names)

(** {1 Private Detection in Extraction} *)

let test_extract_private_function () =
  let defs = extract_from_string "(defun my-pkg--private () nil)" in
  Alcotest.(check bool) "marked private" true (List.hd defs).is_private

let test_extract_public_function () =
  let defs = extract_from_string "(defun my-pkg-public () nil)" in
  Alcotest.(check bool) "marked public" false (List.hd defs).is_private

(** {1 Multiple Definitions} *)

let test_extract_multiple () =
  let defs =
    extract_from_string
      "(defun fn1 () nil)\n(defvar var1 nil)\n(defun fn2 () nil)"
  in
  Alcotest.(check int) "three definitions" 3 (List.length defs)

(** {1 Count Functions} *)

let test_count_public () =
  let defs =
    extract_from_string "(defun public1 () nil)\n(defun pkg--private () nil)"
  in
  Alcotest.(check int) "one public" 1 (count_public defs)

let test_count_private () =
  let defs =
    extract_from_string "(defun public1 () nil)\n(defun pkg--private () nil)"
  in
  Alcotest.(check int) "one private" 1 (count_private defs)

(** {1 Test Suites} *)

let private_name_tests =
  [
    Alcotest.test_case "double dash" `Quick test_private_name_with_double_dash;
    Alcotest.test_case "single dash" `Quick test_private_name_single_dash;
    Alcotest.test_case "no dash" `Quick test_private_name_no_dash;
  ]

let function_tests =
  [
    Alcotest.test_case "defun" `Quick test_extract_defun;
    Alcotest.test_case "defsubst" `Quick test_extract_defsubst;
    Alcotest.test_case "cl-defun" `Quick test_extract_cl_defun;
  ]

let macro_tests =
  [
    Alcotest.test_case "defmacro" `Quick test_extract_defmacro;
    Alcotest.test_case "cl-defmacro" `Quick test_extract_cl_defmacro;
  ]

let variable_tests =
  [
    Alcotest.test_case "defvar" `Quick test_extract_defvar;
    Alcotest.test_case "defcustom" `Quick test_extract_defcustom;
    Alcotest.test_case "defconst" `Quick test_extract_defconst;
    Alcotest.test_case "defvar-local" `Quick test_extract_defvar_local;
  ]

let face_tests = [ Alcotest.test_case "defface" `Quick test_extract_defface ]

let struct_tests =
  [
    Alcotest.test_case "cl-defstruct" `Quick test_extract_cl_defstruct;
    Alcotest.test_case "cl-defstruct options" `Quick
      test_extract_cl_defstruct_with_options;
  ]

let class_tests = [ Alcotest.test_case "defclass" `Quick test_extract_defclass ]

let private_extraction_tests =
  [
    Alcotest.test_case "private function" `Quick test_extract_private_function;
    Alcotest.test_case "public function" `Quick test_extract_public_function;
  ]

let misc_tests =
  [
    Alcotest.test_case "multiple defs" `Quick test_extract_multiple;
    Alcotest.test_case "count public" `Quick test_count_public;
    Alcotest.test_case "count private" `Quick test_count_private;
  ]

let () =
  Alcotest.run "definition_extractor"
    [
      ("private-name", private_name_tests);
      ("functions", function_tests);
      ("macros", macro_tests);
      ("variables", variable_tests);
      ("faces", face_tests);
      ("structs", struct_tests);
      ("classes", class_tests);
      ("private-extraction", private_extraction_tests);
      ("misc", misc_tests);
    ]
