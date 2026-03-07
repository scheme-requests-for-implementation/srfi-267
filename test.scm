(define (test name expected raw)
  (if (string=? expected raw)
      (begin
        (display name)
        (display " passed")
        (newline))
      (begin
        (display name)
        (display "failed:")
        (newline)
        (write expected)
        (newline)
        (display "!=")
        (newline)
        (write raw)
        (newline))))

(test "empty" "" #"""")
(test "single character" "a" #""a"")
(test "no escapes" "\\" #""\"")
(test "Inserting double quote using custom delimiters" "\"" #"-"""-")
(test "Inserting double quote using space" " \" " #"" " "")
(test "Inserting backslash and double quote using custom delimiters" "\\\"" #"-"\""-")
(test "Quoted string using custom delimiters" "#\"()\"" #"-"#"()""-")
(test "Embedded raw string" "#\"\"a\"\"" #"-"#""a"""-")
(test "ends with double quote" "ends with \\\"" #"-"ends with \""-")
(test "multiline string" "multiline\nstring" #""multiline
string"")
(test "no whitespace stripping" "\n    no whitespace stripping" #""
    no whitespace stripping"")

(test "SRFI 264 example" "\\(?(\\d{3})\\D{0,3}(\\d{3})\\D{0,3}(\\d{4})" #""\(?(\d{3})\D{0,3}(\d{3})\D{0,3}(\d{4})"")
(test "wisp example"
      "\ndefine: hello-name name\n  string-append \"Hello,\" name \"!\"\n"
      #"wisp-EOS"
define: hello-name name
  string-append "Hello," name "!"
"wisp-EOS")

(test "parse-url example"
      #""Given a URL as a string, returns a Parsed-URL record with the
components of that URL.

(parse-url "https://example.org/~smith/?record")
=> #<Parsed-URL protocol: "https" domain: "example.org"
                path: "/~smith/" query: "?record">""
      "Given a URL as a string, returns a Parsed-URL record with the\ncomponents of that URL.\n\n(parse-url \"https://example.org/~smith/?record\")\n=> #<Parsed-URL protocol: \"https\" domain: \"example.org\"\n                path: \"/~smith/\" query: \"?record\">")
